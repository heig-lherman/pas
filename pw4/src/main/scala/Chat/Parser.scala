package Chat

import Chat.ExprTree.*
import Chat.Token.*

import scala.annotation.tailrec

class UnexpectedTokenException(msg: String) extends Exception(msg) {}

class Parser(tokensIt: Iterator[(String, Token)]):

  // Start the process by reading the first token.
  var curTuple: (String, Token) = tokensIt.next()

  def curValue: String = curTuple._1
  def curToken: Token = curTuple._2

  /** Reads the next token and assigns it into the global variable curTuple */
  def readToken(): Unit = curTuple = tokensIt.next()

  /** "Eats" the expected token and returns it value, or terminates with an
    * error.
    */
  private def eat(token: Token): String =
    if token == curToken then
      val tmp = curValue
      readToken()
      tmp
    else expected(token)

  /** Complains that what was found was not expected. The method accepts
    * arbitrarily many arguments of type Token
    */
  private def expected(token: Token, more: Token*): Nothing =
    expected(more.prepended(token))
  private def expected(tokens: Seq[Token]): Nothing =
    val expectedTokens = tokens.mkString(" or ")
    throw new UnexpectedTokenException(
      s"Expected: $expectedTokens, found: $curToken",
    )

  // --- Program parsing

  /** the root method of the parser: parses an entry phrase */
  def parsePhrases(): Statement =
    if curToken == BONJOUR then
      readToken()
      if curToken == EOL then return Greeting

    curToken match
      case QUEL | COMBIEN => parsePriceRequest()
      case JE             => parseUserAction()
      case _              => expected(BONJOUR, QUEL, COMBIEN, JE)

  // --- Statement parsing

  /** From the current token expected to be [[QUEL]] or [[COMBIEN]], parse a
    * price request sentence
    *
    * @return
    *   the parsed PriceRequest
    */
  private def parsePriceRequest(): Statement =
    // Eat both possible sentence paths
    if curToken == QUEL then
      readToken()
      eat(ETRE)
      eat(LE)
      eat(PRIX)
      eat(DE)
    else if curToken == COMBIEN then
      readToken()
      eat(COUTER)
    else expected(QUEL, COMBIEN)

    PriceRequest(parseExpression())

  /** From the current token expected to be [[JE]], parse a user action sentence
    *
    * @return
    *   the parsed OrderRequest or BalanceRequest
    */
  private def parseUserAction(): Statement =
    eat(JE)
    eat(VOULOIR)

    if curToken == COMMANDER then
      readToken()
      OrderRequest(parseExpression())
    else if curToken == CONNAITRE then
      readToken()
      eat(MON)
      eat(SOLDE)
      BalanceRequest
    else expected(COMMANDER, CONNAITRE)

  // --- Expression parsing

  /** Parse the product expression from the sentence, can be either multiple
    * products or a single product.
    *
    * @return
    *   the parsed product expression
    */
  private def parseExpression(): Expression = parseLogicExpression()

  /** Parse a logic expression from the sentence, if present. Logic expressions
    * are OR and AND binary expressions, each have no precedence between them,
    * and should be processed from left to right.
    *
    * @return
    *   the parsed logic expression, if present. Otherwise, the next expression
    *   in the precedence chain.
    */
  private def parseLogicExpression(): Expression = {
    @tailrec
    def parseRest(left: Expression): Expression = curToken match {
      case OU | ET =>
        val op = curToken
        readToken()
        val right = parseProductExpression()
        op match {
          case OU => parseRest(OrExpression(left, right))
          case ET => parseRest(AndExpression(left, right))
          case _  => expected(OU, ET)
        }
      case _ => left
    }

    parseRest(parseProductExpression())
  }

  /** Parse a single product expression from the sentence
    *
    * @return
    *   the parsed product expression
    */
  private def parseProductExpression(): Expression =
    ProductExpression(
      parseAmountLiteral(),
      parseProductLiteral(),
    )

  // --- Primary parsing

  /** Parse an amount literal from the current NUM token
    *
    * @return
    *   the parsed amount literal
    */
  private def parseAmountLiteral(): AmountLiteral =
    val amount = eat(NUM).toInt
    AmountLiteral(amount)

  /** Parses a product literal from the product token and an optional brand
    * token, if present.
    *
    * @return
    *   the parsed product literal
    */
  private def parseProductLiteral(): ProductLiteral =
    val product = eat(PRODUIT)
    ProductLiteral(
      product,
      if curToken == MARQUE then Some(eat(MARQUE)) else None,
    )
