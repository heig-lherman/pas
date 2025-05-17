package Chat

import Chat.ExprTree.*
import Data.ProductService.{BrandName, ProductName}

import scala.language.implicitConversions

/** A DSL for building ExprTree instances in a clean, fluent way for tests.
  */
object TreeDSL {
  // Statements
  def greeting: Greeting.type = Greeting
  def priceRequest(products: Expression): PriceRequest = PriceRequest(products)
  def orderRequest(products: Expression): OrderRequest = OrderRequest(products)
  def balanceRequest: BalanceRequest.type = BalanceRequest

  // Extension methods for product literals
  extension (s: ProductName) {

    /** Creates a product literal with a brand.
      *
      * Example: "croissant" by "cailler"
      */
    def by(brand: BrandName): ProductLiteral = ProductLiteral(s, Some(brand))

    /** Creates a product literal without a brand.
      *
      * Example: "croissant".withoutBrand
      */
    def withoutBrand: ProductLiteral = ProductLiteral(s, None)

    /** Shorthand for creating a product literal without a brand.
      *
      * Example: "croissant".product
      */
    def product: ProductLiteral = ProductLiteral(s, None)
  }

  // Extension methods for amount literals
  extension (i: Int) {

    /** Creates a product expression with the specified amount and product.
      *
      * Example: 2 of "croissant".product
      */
    def of(product: ProductLiteral): ProductExpression =
      ProductExpression(AmountLiteral(i), product)

    /** Shorthand for creating an amount literal.
      *
      * Example: 2.amount
      */
    def amount: AmountLiteral = AmountLiteral(i)
  }

  // Extension methods for expressions
  extension (left: Expression) {

    /** Creates an AND expression.
      */
    def and(right: Expression): AndExpression = AndExpression(left, right)

    /** Creates an OR expression.
      */
    def or(right: Expression): OrExpression = OrExpression(left, right)
  }

  implicit def stringToProductLiteral(s: String): ProductLiteral =
    ProductLiteral(s, None)

  implicit def intToAmountLiteral(i: Int): AmountLiteral = AmountLiteral(i)
}
