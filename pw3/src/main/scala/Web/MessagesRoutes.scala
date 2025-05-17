package Web

import Chat.{
  AnalyzerService,
  Parser,
  TokenizerService,
  UnexpectedTokenException
}
import Data.MessageService.Username
import Data.{AccountService, MessageService, Session, SessionService}
import Utils.WebsocketBroadcastChannelManager
import Web.Decorators.getSession
import Web.Layouts.*
import cask.endpoints.postJson
import cask.model.Response
import cask.util.Ws
import scalatags.Text.all.*
import upickle.default.*

/** Assembles the routes dealing with the message board:
  *   - One route to display the home page
  *   - One route to send the new messages as JSON
  *   - One route to subscribe with websocket to new messages
  *
  * @param log
  */
class MessagesRoutes(
    tokenizerSvc: TokenizerService,
    analyzerSvc: AnalyzerService,
    msgSvc: MessageService,
    accountSvc: AccountService,
    sessionSvc: SessionService,
)(implicit
    val cc: castor.Context,
    val log: cask.Logger,
) extends cask.Routes:
  import MessagesRoutes.*

  private val channelManager = WebsocketBroadcastChannelManager()

  @getSession(sessionSvc)
  @cask.get("/")
  def index()(session: Session) =
    homePage(session)

  @getSession(sessionSvc)
  @cask.postJson("/send")
  def sendMessage(msg: String)(session: Session) =
    (for
      trimmedMsg <- Some(msg.trim)
        .filter(_.nonEmpty)
        .toRight(MessageResponse(false, "Please provide a message."))
      user <- session.getCurrentUser
        .toRight(
          MessageResponse(false, "You must be logged in to send a message."),
        )
    yield trimmedMsg match {
      case SentMessage.BotMention(content) =>
        handleBotMessage(session, user, content, "bot")

      case SentMessage.UserMention(username, content) =>
        msgSvc.add(
          user,
          message(content, user, Some(username)),
          Some(username),
        )
        notifyLatestMessages()
        Success

      case content @ _ =>
        msgSvc.add(user, message(content, user))
        notifyLatestMessages()
        Success
    }).merge

  @cask.websocket("/subscribe")
  def subscribe(): cask.WebsocketResult =
    cask.WsHandler { chan =>
      notifyLatestMessages(chan)
      channelManager.subscribe(chan)
    }

  @cask.get("/clearHistory")
  def clearHistory() =
    msgSvc.deleteHistory()
    notifyLatestMessages()
    cask.Redirect("/")

  private def handleBotMessage(
      session: Session,
      user: Username,
      content: String,
      mention: String,
  ): MessageResponse =
    util
      .Try {
        val tokens = tokenizerSvc.tokenize(content).toList
        val stmt = Parser(tokens.iterator).parsePhrases()
        val msgId = msgSvc.add(
          user,
          message(content, user, Some(mention)),
          Some(mention),
          Some(stmt),
        )

        val reply = analyzerSvc.reply(session)(stmt)
        msgSvc.add(
          BotName,
          message(reply, BotName),
          replyToId = Some(msgId),
        )

        notifyLatestMessages()
        Success
      }
      .recover { case e: UnexpectedTokenException =>
        MessageResponse(false, e.getMessage)
      }
      .get

  private def notifyLatestMessages(
      chan: castor.Actor[Ws.Event] = channelManager,
  ): Unit =
    msgSvc.getLatestMessages(20).map(_._2.render) match {
      case Nil      => chan.send(Ws.Text(noMessagesMessage.render))
      case messages => chan.send(Ws.Text(messages.mkString("\n")))
    }

  initialize()
end MessagesRoutes

object MessagesRoutes:
  /** The response to the send API endpoint to be serialized as JSON */
  case class MessageResponse(success: Boolean, err: String) derives ReadWriter

  // Helper response object for success
  private object Success extends MessageResponse(true, "")

  // Helper object for message handling
  private object SentMessage {
    object BotMention {
      def unapply(msg: String): Option[String] = {
        val botPattern = """^@bot\s+(.+)$""".r
        msg match {
          case botPattern(content) => Some(content)
          case _                   => None
        }
      }
    }

    object UserMention {
      def unapply(msg: String): Option[(String, String)] = {
        val mentionPattern = """^@(\w+)\s+(.+)$""".r
        msg match {
          case mentionPattern(username, content) if username != "bot" =>
            Some((username, content))
          case _ => None
        }
      }
    }
  }
