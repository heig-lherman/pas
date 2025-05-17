package Chat

import Chat.ExprTree.*
import Data.ProductService.{BrandName, ProductName}
import Data.{AccountService, ProductService, Session}

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import AnalyzerService.*

  /** Return the output text of the current node, in order to write it in
    * console.
    *
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: Statement): String =
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
          val productList = evaluateExpression(products)
          val totalPrice = resolvePrice(productList)
          accountSvc.purchase(user, totalPrice) match {
            case Some(remainingBalance) =>
              s"Voici donc ${formatProducts(productList)}! " +
                f"Cela coûte CHF $totalPrice%2.2f et votre nouveau solde est de CHF $remainingBalance%2.2f."
            case None =>
              "Vous n'avez pas assez d'argent pour acheter ces produits."
          }
        }

      case PriceRequest(products) =>
        val totalPrice = resolvePrice(evaluateExpression(products))
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
        if resolvePrice(lProducts) < resolvePrice(rProducts) then lProducts
        else rProducts

      case AndExpression(left, right) =>
        evaluateExpression(left) ++ evaluateExpression(right)

      case ProductExpression(amount, ProductLiteral(productName, brandName)) =>
        List(
          (
            amount.amount,
            productName,
            brandName.getOrElse(productSvc.getDefaultBrand(productName)),
          ),
        )

  /** Resolves the price of a list of products.
    *
    * @param products
    *   the list of products
    * @return
    *   the total price of the products
    */
  private def resolvePrice(products: List[Product]): Double =
    products.foldLeft(0.0) { case (acc, (amount, productName, brandName)) =>
      acc + productSvc.getPrice(productName, brandName) * amount
    }
end AnalyzerService

private object AnalyzerService:
  private type Product = (Int, ProductName, BrandName)

  /** Formats a list of products into a human-readable string.
    *
    * @param products
    *   the list of products
    * @return
    *   the formatted string
    */
  private def formatProducts(products: List[Product]): String = {
    val formatProduct: Product => String = p => s"${p._1} ${p._2} ${p._3}"
    products match
      case Nil         => ""
      case head :: Nil => formatProduct(head)
      case list =>
        val formatted = list.map(formatProduct)
        formatted.init.mkString(", ") + " et " + formatted.last
  }

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
