package Chat

import Data.OrderService.{Product, formatProducts}
import Data.{AccountService, OrderService}

import scala.util.{Failure, Success}

sealed trait OrderHandling:
  /** Handle the order of a product.
    *
    * @param session
    *   The session of the current user
    * @param products
    *   The list of products to order
    * @return
    *   A message indicating the result of the order
    */
  def handleOrder(
      user: String,
      products: List[Product],
  )(using
      orderSvc: OrderService,
      accountSvc: AccountService,
  ): String

object OrderHandling:

  case object Synchronous extends OrderHandling:
    override def handleOrder(
        user: String,
        products: List[Product],
    )(using
        orderSvc: OrderService,
        accountSvc: AccountService,
    ): String =
      val totalPrice = orderSvc.resolvePrice(products)
      accountSvc.purchase(user, totalPrice) match {
        case Some(remainingBalance) =>
          s"Voici donc ${formatProducts(products)}! " +
            f"Cela coûte CHF $totalPrice%2.2f et votre nouveau solde est de CHF $remainingBalance%2.2f."
        case None =>
          "Vous n'avez pas assez d'argent pour acheter ces produits."
      }
  end Synchronous

  case class Asynchronous(
      replyHandler: String => Unit,
  ) extends OrderHandling:
    import scala.concurrent.ExecutionContext.Implicits.global

    override def handleOrder(
        user: String,
        products: List[Product],
    )(using
        orderSvc: OrderService,
        accountSvc: AccountService,
    ): String =
      val totalPrice = orderSvc.resolvePrice(products)
      if accountSvc.getAccountBalance(user) < totalPrice
      then "Vous n'avez pas assez d'argent pour acheter ces produits."
      else
        orderSvc
          .placeOrder(products)
          .map(
            _.fold(
              (total, partialProducts) =>
                (
                  total,
                  f"La commande de ${formatProducts(products)} est partiellement prête. Voici ${formatProducts(partialProducts)}. Cela coûte CHF $total%2.2f.",
                ),
              _ =>
                (
                  totalPrice,
                  f"La commande de ${formatProducts(products)} est prête. Cela coûte CHF $totalPrice%2.2f.",
                ),
            ),
          )
          .map((total, result) =>
            accountSvc.purchase(user, total) match {
              case Some(remainingBalance) =>
                f"$result Votre nouveau solde est de CHF $remainingBalance%2.2f."
              case None =>
                s"La commande de ${formatProducts(products)} est prête mais vous avez dépensé votre argent, le bot-tender n'est pas très content."
            },
          )
          .recover { case _: OrderService.ProductionFailureException =>
            s"La commande de ${formatProducts(products)} ne peut pas être délivrée."
          }
          .andThen {
            case Success(msg) => replyHandler(msg)
            case Failure(e) =>
              println(s"Erreur lors de la commande: ${e.getMessage}")
              replyHandler(
                s"La commande de ${formatProducts(products)} a échoué pour une raison inconnue.",
              )
          }
        s"Votre commande est en cours de préparation: ${formatProducts(products)}"
