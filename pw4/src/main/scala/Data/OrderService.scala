package Data

import Data.ProductService.{BrandName, ProductName, ProductionException}

import scala.concurrent.{ExecutionContext, Future}

object OrderService:
  /** A product is a tuple containing the quantity, the name and the brand of
    * the product
    */
  type Product = (Int, ProductName, BrandName)

  /** An order is a tuple containing the total price and a list of products.
    */
  type Order = (Double, Seq[Product])

  /** Exception thrown when an order completely fails.
    *
    * @param message
    *   the error message
    */
  class ProductionFailureException(message: String) extends Exception(message)

  /** Formats a list of products into a human-readable string.
    *
    * @param products
    *   the list of products
    * @return
    *   the formatted string
    */
  def formatProducts(products: Seq[Product]): String = {
    val formatProduct: Product => String = p => s"${p._1} ${p._2} ${p._3}"
    products match
      case Nil              => ""
      case (0, _, _) :: Nil => "strictement rien :("
      case head :: Nil      => formatProduct(head)
      case list =>
        val formatted = list.filter(_._1 > 0).map(formatProduct)
        formatted.init.mkString(", ") + " et " + formatted.last
  }

trait OrderService:
  import OrderService.*

  /** Given the product service is available, resolves the price of a list of
    * products.
    *
    * @param products
    *   the list of products
    * @return
    *   the total price of the products
    */
  def resolvePrice(products: List[Product]): Double

  /** Allows to place an order for a list of products.
    *
    * Products are fulfilled in parallel when they are of different types, else
    * they are sequentially provided.
    *
    * @param products
    *   the products to order
    * @return
    *   a future that completes with the order once it is successfully placed,
    *   - returns a failed future if none of the products were successfully
    *     ordered, expected failure is [[ProductionFailureException]]
    *   - returns left-sided result if the order was partially fulfilled,
    *   - returns right-sided result if the order was fully fulfilled.
    */
  def placeOrder(
      products: Seq[Product],
  )(implicit
      ec: scala.concurrent.ExecutionContext,
  ): Future[Either[Order, Order]]

class OrderImpl(val productService: ProductService) extends OrderService:
  import OrderService.*

  override def resolvePrice(products: List[Product]): Double =
    products.foldLeft(0.0) { case (acc, (amount, productName, brandName)) =>
      acc + productService.getPrice(productName, brandName) * amount
    }

  override def placeOrder(
      products: Seq[Product],
  )(implicit
      ec: ExecutionContext,
  ): Future[Either[Order, Order]] = {
    def processProduct(product: Product): Future[(Int, Double, Boolean)] =
      product match {
        case (qty, pn, bn) =>
          (0 until qty).foldLeft(Future.successful((0, 0.0, false))) {
            case (acc, _) =>
              acc.flatMap { case (count, total, incomplete) =>
                productService
                  .produce(pn, bn)
                  .map(price => (count + 1, total + price, incomplete))
                  .recover { case _: ProductionException =>
                    (count, total, true)
                  }
              }
          }
      }

    Future
      .sequence(products.map { case (qty, pn, bn) =>
        processProduct((qty, pn, bn)).map { case (count, total, incomplete) =>
          (incomplete, total, if (count > 0) Seq((count, pn, bn)) else Nil)
        }
      })
      .map(results =>
        val (incomplete, total, products) = results
          .reduceLeft((a, b) => (a._1 || b._1, a._2 + b._2, a._3 ++ b._3))
        if products.nonEmpty
        then Either.cond(!incomplete, (total, products), (total, products))
        else
          throw new ProductionFailureException(
            "Aucun produit n'a pu être produit.",
          ),
      )
  }
