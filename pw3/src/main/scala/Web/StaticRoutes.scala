package Web

/** Assembles the routes dealing with static files.
  */
class StaticRoutes()(implicit
    val log: cask.Logger,
) extends cask.Routes:
  @cask.staticResources("/public/css")
  def staticStylesheets() = "css"

  @cask.staticResources("/public/js")
  def staticScripts() = "js"

  initialize()
end StaticRoutes
