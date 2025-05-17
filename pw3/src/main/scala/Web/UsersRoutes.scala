package Web

import Data.{AccountService, Session, SessionService}
import Web.Decorators.getSession
import Web.Layouts.*
import cask.model.Response
import scalatags.Text.all.*

/** Assembles the routes dealing with the users:
  *   - One route to display the login form and register form page
  *   - One route to process the login form and display the login success page
  *   - One route to process the register form and display the register success
  *     page
  *   - One route to logout and display the logout success page
  *
  * The username of the current session user is stored inside a cookie called
  * `username`.
  */
class UsersRoutes(
    accountSvc: AccountService,
    sessionSvc: SessionService,
)(implicit
    val log: cask.Logger,
) extends cask.Routes:

  @cask.get("/login")
  def login() = loginPage()

  @getSession(sessionSvc)
  @cask.postForm("/login")
  def loginForm(username: String)(session: Session) =
    if accountSvc.isAccountExisting(username)
    then
      accountSvc.setCurrent(username, session)
      actionSuccessPage(logoutNavItem)(
        s"You are now logged in as $username!",
      )
    else loginPage(loginError = "The specified user does not exist.")

  @getSession(sessionSvc)
  @cask.postForm("/register")
  def registerForm(username: String)(session: Session) =
    if !accountSvc.isAccountExisting(username)
    then
      accountSvc.setCurrent(username, session)
      actionSuccessPage(logoutNavItem)(
        s"You are now registered and logged in as $username!",
      )
    else loginPage(registerError = "The specified user already exists.")

  @getSession(sessionSvc)
  @cask.route("/logout", methods = Seq("post", "get"))
  def logout()(session: Session) =
    session.reset()
    actionSuccessPage(loginNavItem)("You are now logged out!")

  initialize()
end UsersRoutes
