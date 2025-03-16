package Data

import Data.ProductService.*

object ProductService:
  type BrandName = String
  type ProductName = String

  /** The available products and their brands with their prices.
    *
    * NOTE: exposed here as a constant map to be able to use this when
    * tokenizing the user input, which will disambiguate products and brands.
    */
  val AvailableProducts: Map[ProductName, Seq[(BrandName, Double)]] =
    Map(
      "biere" -> Seq(
        ("boxer", 1.00),
        ("farmer", 1.00),
        ("wittekop", 2.00),
        ("punkipa", 3.00),
        ("jackhammer", 3.00),
        ("tenebreuse", 4.00),
      ),
      "croissant" -> Seq(
        ("maison", 2.00),
        ("cailler", 2.00),
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

class ProductImpl extends ProductService:
  import ProductService.*

  override def getPrice(product: ProductName, brand: BrandName): Double =
    AvailableProducts
      .getOrElse(
        product,
        throw new NoSuchElementException(s"Produit inconnu: $product"),
      )
      .find(_._1 == brand)
      .map(_._2)
      .getOrElse(
        throw new NoSuchElementException(
          s"Marque inconnue pour le produit $product: $brand",
        ),
      )

  override def getDefaultBrand(product: ProductName): BrandName =
    AvailableProducts
      .getOrElse(
        product,
        throw new NoSuchElementException(s"Produit inconnu: $product"),
      )
      .head
      ._1

  override def products: Map[ProductName, Iterable[BrandName]] =
    AvailableProducts.view.mapValues(_.map(_._1)).toMap

end ProductImpl
