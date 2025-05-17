package Chat

import Data.ProductService.{BrandName, ProductName}

/** This sealed trait represents a node of the tree.
  */
sealed trait ExprTree

/** Declarations of the nodes' types.
  */
object ExprTree:

  // ---- Program Statements

  /** Statements are a combination of expressions forming a final phrase given
    * by the user.
    */
  sealed trait Statement extends ExprTree

  /** Represents a simple greeting statement.
    */
  case object Greeting extends Statement

  /** Represents a request for the price of one or multiple products.
    *
    * @param products
    *   child expression for identifying the products
    */
  case class PriceRequest(products: Expression) extends Statement

  /** Represents a request for ordering one or multiple products.
    *
    * @param products
    *   child expression for identifying the products
    */
  case class OrderRequest(products: Expression) extends Statement

  /** Represents a request for the balance of the account.
    */
  case object BalanceRequest extends Statement

  // ---- Program Expressions

  /** Expressions are valued nodes that can be evaluated to a result, they can
    * either be a primary or a combination of expressions.
    */
  sealed trait Expression extends ExprTree

  /** Binary expressions reflect a two-sided operation linked by a logical
    * operator.
    */
  sealed trait BinaryExpression extends Expression {
    val left: Expression
    val right: Expression
  }

  /** Represents a logical AND operation between two expressions.
    *
    * @param left
    *   the left-hand side expression
    * @param right
    *   the right-hand side expression
    */
  case class AndExpression(left: Expression, right: Expression)
      extends BinaryExpression

  /** Represents a logical OR operation between two expressions.
    *
    * @param left
    *   the left-hand side expression
    * @param right
    *   the right-hand side expression
    */
  case class OrExpression(left: Expression, right: Expression)
      extends BinaryExpression

  /** Represents a product expression, which is a combination of an amount, a
    * product and an optional brand.
    *
    * @param amount
    *   the amount
    * @param product
    *   the product
    */
  case class ProductExpression(
      amount: AmountLiteral,
      product: ProductLiteral,
  ) extends Expression

  // ---- Primaries

  /** In our formal language, the only primaries available are literals which
    * can either be a numerical quantitative value or a character sequence
    * representing a product or a brand.
    *
    * Literals are the smallest part of the tree, in most cases are the leaves
    * of an expression branch.
    */
  sealed trait Literal extends ExprTree

  /** Represents a quantity, given as integer value.
    *
    * @param amount
    *   the quantity
    */
  case class AmountLiteral(amount: Int) extends Literal

  /** Represents a product literal.
    *
    * This is a primary because the syntax is unequivocal, and in no manner
    * adhering to the formal language this could be further decomposed.
    *
    * @param product
    *   the product name
    * @param brand
    *   the brand name, optional
    */
  case class ProductLiteral(
      product: ProductName,
      brand: Option[BrandName] = Option.empty,
  ) extends Literal
