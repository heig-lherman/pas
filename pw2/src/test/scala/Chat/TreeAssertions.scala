package Chat

import Chat.ExprTree.*

/** Tree comparison utility that provides detailed diff reports.
  */
object TreeDiff {

  /** Finds the differences between two expression trees.
    *
    * @param expected
    *   The expected tree
    * @param actual
    *   The actual tree
    * @return
    *   A list of differences, empty if trees are equal
    */
  def diff(expected: ExprTree, actual: ExprTree): List[String] =
    findDifferences(expected, actual)

  private def findDifferences(
      expected: ExprTree,
      actual: ExprTree,
      path: String = "",
  ): List[String] = {
    (expected, actual) match {
      case (e, a) if e.getClass != a.getClass =>
        List(
          s"$path: Expected ${prettyClassName(e)}, got ${prettyClassName(a)}",
        )

      case (Greeting, Greeting) => Nil

      case (PriceRequest(eProducts), PriceRequest(aProducts)) =>
        findDifferences(eProducts, aProducts, s"${pathPrefix(path)}products")

      case (OrderRequest(eProducts), OrderRequest(aProducts)) =>
        findDifferences(eProducts, aProducts, s"${pathPrefix(path)}products")

      case (BalanceRequest, BalanceRequest) => Nil

      case (AndExpression(eLeft, eRight), AndExpression(aLeft, aRight)) =>
        findDifferences(eLeft, aLeft, s"${pathPrefix(path)}left") ++
          findDifferences(eRight, aRight, s"${pathPrefix(path)}right")

      case (OrExpression(eLeft, eRight), OrExpression(aLeft, aRight)) =>
        findDifferences(eLeft, aLeft, s"${pathPrefix(path)}left") ++
          findDifferences(eRight, aRight, s"${pathPrefix(path)}right")

      case (
            ProductExpression(eAmount, eProduct),
            ProductExpression(aAmount, aProduct),
          ) =>
        findDifferences(eAmount, aAmount, s"${pathPrefix(path)}amount") ++
          findDifferences(eProduct, aProduct, s"${pathPrefix(path)}product")

      case (AmountLiteral(eAmount), AmountLiteral(aAmount)) =>
        if (eAmount != aAmount)
          List(s"$path: Expected amount $eAmount, got $aAmount")
        else Nil

      case (
            ProductLiteral(eProduct, eBrand),
            ProductLiteral(aProduct, aBrand),
          ) =>
        val productDiff =
          if (eProduct != aProduct)
            List(s"$path: Expected product name '$eProduct', got '$aProduct'")
          else Nil
        val brandDiff =
          if (eBrand != aBrand)
            List(
              s"$path: Expected brand ${formatOptional(eBrand)}, got ${formatOptional(aBrand)}",
            )
          else Nil
        productDiff ++ brandDiff

      case _ => List(s"$path: Unexpected tree structure mismatch")
    }
  }

  private def pathPrefix(path: String): String =
    if (path.isEmpty) "" else s"$path."

  private def prettyClassName(obj: Any): String = obj.getClass.getSimpleName

  private def formatOptional(opt: Option[String]): String = opt match {
    case Some(value) => s"'$value'"
    case None        => "none"
  }

  /** Converts a tree to a pretty string representation.
    */
  def stringify(tree: ExprTree): String = {
    tree match {
      case Greeting               => "Greeting"
      case PriceRequest(products) => s"PriceRequest(${stringify(products)})"
      case OrderRequest(products) => s"OrderRequest(${stringify(products)})"
      case BalanceRequest         => "BalanceRequest"
      case AndExpression(left, right) =>
        s"(${stringify(left)} AND ${stringify(right)})"
      case OrExpression(left, right) =>
        s"(${stringify(left)} OR ${stringify(right)})"
      case ProductExpression(AmountLiteral(amount), product) =>
        s"$amount × ${stringify(product)}"
      case AmountLiteral(amount)                => amount.toString
      case ProductLiteral(product, None)        => product
      case ProductLiteral(product, Some(brand)) => s"$product ($brand)"
      case _                                    => tree.toString
    }
  }

  /** Converts a tree to a concise Scala code representation that could be used
    * to recreate it.
    */
  def toCode(tree: ExprTree): String = tree match {
    case Greeting               => "greeting"
    case PriceRequest(products) => s"priceRequest(${toCode(products)})"
    case OrderRequest(products) => s"orderRequest(${toCode(products)})"
    case BalanceRequest         => "balanceRequest"
    case AndExpression(left, right) =>
      s"(${toCode(left)}).and(${toCode(right)})"
    case OrExpression(left, right) =>
      s"(${toCode(left)}).or(${toCode(right)})"
    case ProductExpression(AmountLiteral(amount), product) =>
      s"$amount.of(${toCode(product)})"
    case AmountLiteral(amount)         => s"$amount.amount"
    case ProductLiteral(product, None) => s"\"$product\".product"
    case ProductLiteral(product, Some(brand)) =>
      s"\"$product\".by(\"$brand\")"
    case _ => tree.toString
  }
}

/** MUnit assertions for ExprTree testing.
  */
trait TreeAssertions extends munit.Assertions {

  /** Asserts that two expression trees are equal, providing detailed error
    * messages when they differ.
    */
  def assertTreeEquals(
      expected: ExprTree,
      actual: ExprTree,
      clue: String = "",
  ): Unit = {
    val differences = TreeDiff.diff(expected, actual)
    if (differences.nonEmpty) {
      val cluePart = if (clue.nonEmpty) s"$clue: " else ""
      fail(
        s"""${cluePart}Trees are not equal.
           |
           |Expected structure: ${TreeDiff.stringify(expected)}
           |Actual structure:   ${TreeDiff.stringify(actual)}
           |
           |Differences:
           |${differences.map(d => s"  - $d").mkString("\n")}
           |
           |DSL code to create expected tree:
           |  ${TreeDiff.toCode(expected)}
           |""".stripMargin,
      )
    }
  }
}
