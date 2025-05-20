package Web

import Data.MessageService.Username
import Data.Session
import scalatags.Text.all.*
import scalatags.Text.tags2

/** Assembles the method used to layout ScalaTags
  */
object Layouts:
  // You can use it to store your methods to generate ScalaTags.

  /** A navbar item is a (label, URI) tuple. */
  type NavbarItem = (String, String)

  val BotName: String = "Bot-tender"

  // The list of CSS stylesheets to include in the HTML page
  private lazy val CSS_STYLESHEETS = Seq(
    "/public/css/main.css",
  )

  // The list of JS scripts to include in the HTML page
  private lazy val JS_SCRIPTS = Seq(
    "/public/js/main.js",
  )

  /** Generic function to generate a simple HTML page
    */
  def page(
      pageHeader: Tag,
      jsScripts: Seq[String] = Nil,
  )(
      pageContent: Frag*,
  ): doctype =
    doctype("html"):
      html(
        head(
          meta(charset := "utf-8"),
          meta(
            name := "viewport",
            content := "width=device-width, initial-scale=1",
          ),
          tags2.title(BotName),
          for css <- CSS_STYLESHEETS
          yield link(rel := "stylesheet", href := css),
        ),
        body(
          pageHeader,
          div(cls := "content", pageContent),
          for js <- jsScripts yield script(src := js),
        ),
      )

  /** Generates the home page with the message board and the form to send new
    * @param session
    *   the active session of the user
    * @return
    *   the generated HTML page
    */
  def homePage(session: Session): doctype = page(
    navbar(session),
    JS_SCRIPTS
  )(
    messageBoard(loadingMessage),
    messageForm(),
  )

  /** Generates the login/register page with, optionally, the given error
    * messages
    * @param loginError
    *   the error message to display for the login form
    * @param registerError
    *   the error message to display for the register form
    * @return
    *   the generated HTML page
    */
  def loginPage(
      loginError: String = "",
      registerError: String = "",
  ): doctype = page(
    navbar(messageBoardNavItem),
  )(
    formSection(
      "Login",
      usernameForm(loginError)(action := "/login"),
    ),
    formSection(
      "Register",
      usernameForm(registerError)(action := "/register"),
    ),
  )

  /** Generates the action success page with the given message
    * @param headerItems
    *   navbar items to display in the header
    * @param message
    *   the message to display
    * @return
    *   the generated HTML page
    */
  def actionSuccessPage(
      headerItems: NavbarItem*,
  )(
      message: String,
  ): doctype = page(
    navbar(messageBoardNavItem, headerItems*),
  )(
    p(message),
  )

  /** Generates the navbar for the given session (if the user is logged in, the
    * name and log out link is shown), the log in link is shown otherwise.
    * @param session
    *   the active session
    * @return
    *   the generated navbar tag
    */
  def navbar(session: Session): Tag =
    session.getCurrentUser
      .map(u => navbar((s"Hello $u !", ""), logoutNavItem))
      .getOrElse(navbar(loginNavItem))

  /** Generate the navbar with the given title and navigation items
    * @param item
    *   a pair of (label, link) for the first item, mandatory
    * @param more
    *   a sequence of pairs of (label, link) for the other items
    * @return
    *   the generated nav tag
    */
  def navbar(item: NavbarItem, more: NavbarItem*): Tag =
    navbar(more.prepended(item))
  private def navbar(navItems: Seq[NavbarItem]): Tag = tags2.nav(
    a(cls := "nav-brand", BotName),
    for (label, link) <- navItems
    yield div(
      cls := "nav-item",
      if (link == "") span(label)
      else a(label, href := link),
    ),
  )

  /** Generates a navbar item for the message board
    */
  lazy val messageBoardNavItem: NavbarItem = ("Go to the message board", "/")

  /** Generates a navbar item for the login page
    */
  lazy val loginNavItem: NavbarItem = ("Log in", "/login")

  /** Generates a navbar item for the logout page
    */
  lazy val logoutNavItem: NavbarItem = ("Log out", "/logout")

  /** Generates the message board
    * @return
    *   the generated div tag
    */
  def messageBoard(innerContent: Modifier*): Tag =
    div(id := "boardMessage", innerContent)

  /** Generates a message with the given content, author and mention
    * @param content
    *   The content
    * @param author
    *   The author of the message
    * @param mention
    *   The mention of the message
    * @return
    *   the generated div tag
    */
  def message(
      content: String,
      author: Username,
      mention: Option[Username] = None,
  ): Frag = div(
    cls := "msg",
    span(author, cls := "author"),
    span(
      cls := "msg-content",
      mention.map(m => span(s"@$m ", cls := "mention")),
      content,
    ),
  )

  /** Generates a default message for the board that is displayed while the
    * messages are loading
    * @return
    *   the generated div tag
    */
  lazy val loadingMessage: Tag = p(
    "Please wait, the messages are loading...",
    style := "text-align: center;",
  )

  /** Generates a default message for the board that is displayed when no
    * messages are in the history
    *
    * @return
    *   the generated div tag
    */
  lazy val noMessagesMessage: Tag = p(
    "No messages have been sent yet",
    style := "text-align: center;",
  )

  /** Generates a form section with the given title and content
    * @param title
    *   the title of the section
    * @param content
    *   the content of the section
    * @return
    *   the generated div tag
    */
  def formSection(title: String, content: Modifier*): Tag =
    div(
      h2(title),
      content,
    )

  /** Generates the form to send a new message
    * @param errorContent
    *   optionally, the error message to display
    * @return
    *   the generated form tag
    */
  def messageForm(errorContent: Modifier*): Tag =
    inputForm("Your message:")(
      "Write your message",
      name := "message",
    )(
      errorContent,
    )(
      onsubmit := "submitMessageForm(); return false",
    )

  /** Generates the form to send a new message
    * @param errorContent
    *   optionally, the error message to display
    * @return
    *   the generated form tag
    */
  def usernameForm(errorContent: Modifier*): Tag =
    inputForm("Username:")(
      "Enter your username",
      name := "username",
    )(
      errorContent,
    )(
      method := "post",
    )

  /** Generates the form to send a new message
    * @param labelContent
    *   the label text
    * @param inputPlaceholder
    *   the input placeholder text
    * @param errorContent
    *   optionally, the error message attributes to display
    * @return
    *   the generated form tag
    */
  private def inputForm(
      labelContent: String,
  )(
      inputPlaceholder: String,
      inputModifiers: Modifier*,
  )(
      errorContent: Modifier*,
  ): Tag = form(
    id := "msgForm",
    div(id := "errorDiv", cls := "errorMsg", errorContent),
    label(`for` := "messageInput", labelContent),
    input(
      id := "messageInput",
      `type` := "text",
      placeholder := inputPlaceholder,
      required,
      inputModifiers,
    ),
    input(`type` := "submit", value := "Send"),
  )
end Layouts
