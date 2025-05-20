import Chat.*
import Data.*
import Utils.*

import scala.io.StdIn

object MainParser:
  /** Convert the user input to lower case, then take an action depending on the
    * value.
    *
    * @param tokenizerSvc
    *   The service used to tokenize the input
    * @param analyzerSvc
    *   The service used to analyze the expression tree
    * @param session
    *   The session instance that stores the current user
    * @param input
    *   The user input
    * @return
    *   whether the input loop should continue or not
    */
  def evaluateInput(
      tokenizerSvc: TokenizerService,
      analyzerSvc: AnalyzerService,
      session: Session,
  )(input: String): Boolean =
    input.toLowerCase match
      case "quitter" =>
        println("Adieu.")
        false // close loop
      case s =>
        try
          val tokenized = tokenizerSvc.tokenize(s)

          val parser = new Parser(tokenized)
          val expr = parser.parsePhrases()

          val printResult = analyzerSvc.reply(session)(expr)

          println(printResult)
        catch
          case e: UnexpectedTokenException =>
            println(s"Invalid input. ${e.getMessage}")
          case i: NotImplementedError =>
            println("The code is not yet implemented")
        true // continue loop
  end evaluateInput

  def main(args: Array[String]): Unit =
    val tokenizerSvc = new TokenizerService(Dictionary.dictionary)
    val sessionSvc: SessionService = new SessionImpl()
    val productSvc: ProductService = new ProductImpl()
    val accountSvc: AccountService = new AccountImpl()
    val orderSvc: OrderService = new OrderImpl(productSvc)
    val analyzerSvc = new AnalyzerService(productSvc, orderSvc, accountSvc)

    println("Bienvenue au Chill-Out !")

    val session = sessionSvc.create()
    // TODO: uncomment when you want to have an authenticated user
    // accountSvc.setCurrent("John", session)

    while
      print("> ")
      evaluateInput(tokenizerSvc, analyzerSvc, session)(StdIn.readLine)
    do ()
  end main
end MainParser
