import Web.{UsersRoutes, MessagesRoutes, StaticRoutes}
import Chat.*
import Data.*
import Utils.*

object MainChatroom extends cask.Main:
  val tokenizerSvc: TokenizerService = new TokenizerService(
    Dictionary.dictionary,
  )
  val productSvc: ProductService = new ProductImpl()
  val sessionSvc: SessionService = new SessionImpl()
  val accountSvc: AccountService = new AccountImpl()
  val msgSvc: MessageService = new MessageImpl()
  val analyzerSvc: AnalyzerService = new AnalyzerService(productSvc, accountSvc)

  val allRoutes = Seq(
    StaticRoutes(),
    UsersRoutes(accountSvc, sessionSvc),
    MessagesRoutes(tokenizerSvc, analyzerSvc, msgSvc, accountSvc, sessionSvc),
  )

  override def port: Int = 8980
