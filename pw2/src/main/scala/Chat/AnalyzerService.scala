package Chat

import Data.ProductService.{BrandName, ProductName}
import Data.{AccountService, ProductService, Session}

private object AnalyzerService:
  private type Product = (Int, ProductName, BrandName)

class AnalyzerService(productSvc: ProductService, accountSvc: AccountService):
  import ExprTree.*
  import AnalyzerService.Product

  /** Return the output text of the current node, in order to write it in
    * console.
    *
    * @return
    *   the output text of the current node
    */
  def reply(session: Session)(t: Statement): String =
    t match
      case Greeting =>
        session.getCurrentUser match
          case Some(username) => s"Hello $username !"
          case None           => "Hello !"

      case BalanceRequest =>
        session.getCurrentUser match
          case Some(username) =>
            f"Le montant actuel de votre solde est de CHF ${accountSvc.getAccountBalance(username)}%2.2f."
          case None => "Vous devez être connecté pour connaître votre solde."

      case OrderRequest(products) =>
        session.getCurrentUser match
          case Some(username) =>
            val productList = evaluateExpression(products)
            val totalPrice = resolvePrice(productList)
            accountSvc.purchase(username, totalPrice) match {
              case Some(remainingBalance) =>
                s"Voici donc ${formatProducts(productList)}! " +
                  f"Cela coûte CHF $totalPrice%2.2f et votre nouveau solde est de CHF $remainingBalance%2.2f."
              case None =>
                "Vous n'avez pas assez d'argent pour acheter ces produits."
            }
          case None => "Vous devez être connecté pour faire une commande."

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

      case p @ ProductExpression(_, _) => List(evaluateProduct(p))

  /** Evaluates a product expression resulting in a product tuple.
    *
    * @param expr
    *   the product expression to evaluate
    * @return
    *   the product
    */
  // TODO: check if need to check the product brand validity here
  private def evaluateProduct(expr: ProductExpression): Product = {
    val ProductExpression(amount, ProductLiteral(productName, brandName)) = expr

    (
      amount.amount,
      productName,
      brandName.getOrElse(productSvc.getDefaultBrand(productName)),
    )
  }

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
end AnalyzerService
