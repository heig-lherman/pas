package Chat

import Chat.ExprTree.*
import Data.OrderService.Product
import Data.ProductService.{BrandName, ProductName}
import Data.{AccountService, OrderService, ProductService, Session}

class AnalyzerService(
    productSvc: ProductService,
    orderSvc: OrderService,
    accountSvc: AccountService,
):
  import AnalyzerService.*
  given OrderService = orderSvc
  given AccountService = accountSvc

  /** Return the output text of the current node, in order to write it in
    * console.
    *
    * @return
    *   the output text of the current node
    */
  def reply(
      session: Session,
      orderHandling: OrderHandling = OrderHandling.Synchronous,
  )(
      t: Statement,
  ): String =
    t match
      case Greeting =>
        ensureAuthorized(session, denyMessage = "Hello !") { user =>
          s"Hello $user !"
        }

      case BalanceRequest =>
        ensureAuthorized(session) { user =>
          f"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(user)}%2.2f."
        }

      case OrderRequest(products) =>
        ensureAuthorized(session) { user =>
          orderHandling.handleOrder(user, evaluateExpression(products))
        }

      case PriceRequest(products) =>
        val totalPrice = orderSvc.resolvePrice(evaluateExpression(products))
        f"Cela coûte CHF $totalPrice%2.2f."

  /** Evaluates the program expressions resulting in a list of products.
    *
    * @param expr
    *   the expression to evaluate
    * @return
    *   the list of products
    */
  private def evaluateExpression(expr: Expression): List[Product] =
    expr match
      case OrExpression(left, right) =>
        val lProducts = evaluateExpression(left)
        val rProducts = evaluateExpression(right)
        if orderSvc.resolvePrice(lProducts) < orderSvc.resolvePrice(rProducts)
        then lProducts
        else rProducts

      case AndExpression(left, right) =>
        (evaluateExpression(left) ++ evaluateExpression(right))
          .groupMapReduce((_, bn, pn) => (bn, pn))(_._1)(_ + _)
          .map { case ((bn, pn), amount) => (amount, bn, pn) }
          .toList

      case ProductExpression(amount, ProductLiteral(productName, brandName)) =>
        List(
          (
            amount.amount,
            productName,
            brandName.getOrElse(productSvc.getDefaultBrand(productName)),
          ),
        )
end AnalyzerService

private object AnalyzerService:
  /** Ensures that the user is connected before executing the authorized call.
    *
    * @param session
    *   the current session
    * @param authorizedCall
    *   the call to execute if the user is connected
    * @return
    *   the result of the authorized call or an error message
    */
  private def ensureAuthorized(
      session: Session,
      denyMessage: => String = "Veuillez d'abord vous identifier.",
  )(
      authorizedCall: String => String,
  ): String =
    session.getCurrentUser match
      case Some(username) => authorizedCall(username)
      case None           => denyMessage
