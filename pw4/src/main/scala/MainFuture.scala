import Chat.*
import Data.*
import Utils.*
import Web.{MessagesRoutes, StaticRoutes, UsersRoutes}

object MainFuture extends cask.Main:
  val tokenizerSvc = new TokenizerService(Dictionary.dictionary)
  val sessionSvc: SessionService = new SessionImpl()
  val productSvc: ProductService = new ProductImpl()
  val accountSvc: AccountService = new AccountImpl()
  val orderSvc: OrderService = new OrderImpl(productSvc)
  val analyzerSvc = new AnalyzerService(productSvc, orderSvc, accountSvc)
  val msgSvc: MessageService = new MessageConcurrentImpl(new MessageImpl())

  val allRoutes = Seq(
    StaticRoutes(),
    UsersRoutes(accountSvc, sessionSvc),
    MessagesRoutes(tokenizerSvc, analyzerSvc, msgSvc, accountSvc, sessionSvc),
  )

  override def port: Int = 8980
