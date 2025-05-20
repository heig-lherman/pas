package Data

import Data.ProductService.*
import Utils.FutureOps

import scala.collection.SeqMap
import scala.concurrent.Future
import scala.concurrent.duration.*

object ProductService:
  type BrandName = String
  type ProductName = String

  /** Exception thrown when a product fails to fulfill.
    *
    * @param message
    *   the error message
    */
  class ProductionException(message: String) extends Exception(message)

  /** The properties of a product and its brand.
    *
    * @param price
    *   the price of the product
    * @param mean
    *   the mean of the gaussian curve for production time
    * @param std
    *   the std of the gaussian curve for production time
    * @param successRate
    *   the chance that the production succeeds
    */
  case class ProductProperties(
      price: Double,
      mean: FiniteDuration,
      std: FiniteDuration,
      successRate: Double,
  )

  /** The available products and their brands with their prices.
    *
    * NOTE: exposed here as a constant map to be able to use this when
    * tokenizing the user input, which will disambiguate products and brands.
    */
  val AvailableProducts
      : Map[ProductName, SeqMap[BrandName, ProductProperties]] =
    Map(
      "biere" -> SeqMap(
        "boxer" -> ProductProperties(1.00, 2.seconds, 500.millis, 0.95),
        "farmer" -> ProductProperties(1.00, 4.seconds, 1.second, 0.35),
        "wittekop" -> ProductProperties(2.00, 3.seconds, 750.millis, 0.55),
        "punkipa" -> ProductProperties(3.00, 5.seconds, 1.seconds, 0.85),
        "jackhammer" -> ProductProperties(3.00, 5.seconds, 500.millis, 0.60),
        "tenebreuse" -> ProductProperties(4.00, 6.seconds, 250.millis, 0.70),
      ),
      "croissant" -> SeqMap(
        "maison" -> ProductProperties(2.00, 1.5.seconds, 200.millis, 0.75),
        "cailler" -> ProductProperties(2.00, 2.seconds, 600.millis, 0.85),
      ),
    )

trait ProductService:
  /** Get the price of a given product and brand
    *
    * @param product
    *   the name of the product
    * @param brand
    *   the name of the brand
    * @return
    *   the price in CHF
    */
  def getPrice(product: ProductName, brand: BrandName): Double

  /** Get the default brand for a given product
    *
    * @param product
    *   the given product
    * @return
    *   the default brand
    */
  def getDefaultBrand(product: ProductName): BrandName

  /** For all available products list all their available brands.
    *
    * @return
    *   a map of all available products and all their brands.
    */
  def products: Map[ProductName, Iterable[BrandName]]

  /** Produce a product with a given brand.
    *
    * @param product
    *   the product to produce
    * @param brand
    *   the brand of the product
    * @return
    *   a future that completes with the price of the product once it is
    *   successfully produced
    */
  def produce(
      product: ProductName,
      brand: BrandName,
  )(implicit
      ec: scala.concurrent.ExecutionContext,
  ): Future[Double]

class ProductImpl extends ProductService:
  import ProductService.*

  private def getProductProperties(
      product: ProductName,
      brand: BrandName,
  ): ProductProperties =
    AvailableProducts
      .getOrElse(
        product,
        throw new NoSuchElementException(s"Produit inconnu: $product"),
      )
      .getOrElse(
        brand,
        throw new NoSuchElementException(
          s"Marque inconnue pour le produit $product: $brand",
        ),
      )

  override def getPrice(product: ProductName, brand: BrandName): Double =
    getProductProperties(product, brand)._1

  override def getDefaultBrand(product: ProductName): BrandName =
    AvailableProducts
      .getOrElse(
        product,
        throw new NoSuchElementException(s"Produit inconnu: $product"),
      )
      .head
      ._1

  override def products: Map[ProductName, Iterable[BrandName]] =
    AvailableProducts.view.mapValues(_.keys).toMap

  override def produce(
      product: ProductName,
      brand: BrandName,
  )(implicit
      ec: scala.concurrent.ExecutionContext,
  ): Future[Double] =
    val properties = getProductProperties(product, brand)
    FutureOps
      .randomSchedule(properties.mean, properties.std, properties.successRate)
      .map(_ => properties.price)
      .recover { case _: Exception =>
        throw new ProductionException(
          s"Échec de la production du produit $product de la marque $brand",
        )
      }

end ProductImpl
