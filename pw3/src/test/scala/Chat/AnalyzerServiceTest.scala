package Chat

import Chat.TreeDSL.*
import Data.ProductService.{BrandName, ProductName}
import Data.{AccountService, ProductService, Session}
import munit.FunSuite

class AnalyzerServiceTest extends FunSuite {

  class SimpleProductService extends ProductService {
    override def getPrice(product: ProductName, brand: BrandName): Double =
      (product, brand) match {
        case ("product1", "brandA") => 10.0
        case ("product1", "brandB") => 15.0
        case ("product2", "brandC") => 5.0
        case ("product2", "brandD") => 7.5
        case _ => 1.0 // fallback price
      }

    override def getDefaultBrand(product: ProductName): BrandName =
      product match {
        case "product1" => "brandA"
        case "product2" => "brandC"
        case _ => "default"
      }

    override def products: Map[ProductName, Iterable[BrandName]] =
      Map(
        "product1" -> List("brandA", "brandB"),
        "product2" -> List("brandC", "brandD")
      )
  }

  class TestAccountService extends AccountService {
    // Track account modification calls for testing
    var purchaseCalls: List[(String, Double)] = List()
    var balanceRequests: List[String] = List()

    private val fixedBalances = Map(
      "user1" -> 100.0,
      "user2" -> 20.0,
      "insufficientUser" -> 5.0
    )

    override def getAccountBalance(user: String): Double = {
      balanceRequests = balanceRequests :+ user
      fixedBalances.getOrElse(user, 0.0)
    }

    override def purchase(user: String, amount: Double): Option[Double] = {
      purchaseCalls = purchaseCalls :+ (user, amount)
      val balance = getAccountBalance(user)
      if (balance >= amount) Some(balance - amount) else None
    }

    // Methods below have minimal implementations for the interface
    override def addAccount(user: String, balance: Double): Unit = {}
    override def isAccountExisting(user: String): Boolean = fixedBalances.contains(user)
    override def setCurrent(user: String, session: Session): Unit = session.setCurrentUser(user)
  }

  // Test helpers to verify expression evaluation behavior
  test("OR expression should select the cheaper option") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    // Test OR expression where left side is cheaper
    val leftCheaperExpr = priceRequest(
      (1 of ("product2" by "brandC")) or // 5.0
        (1 of ("product1" by "brandA"))    // 10.0
    )
    assertEquals(analyzer.reply(session)(leftCheaperExpr), "Cela coûte CHF 5.00.")

    // Test OR expression where right side is cheaper
    val rightCheaperExpr = priceRequest(
      (1 of ("product1" by "brandB")) or // 15.0
        (2 of ("product2" by "brandC"))    // 10.0
    )
    assertEquals(analyzer.reply(session)(rightCheaperExpr), "Cela coûte CHF 10.00.")

    // Test complex nested OR where middle option is cheapest
    val nestedOrExpr = priceRequest(
      (2 of ("product1" by "brandA")) or // 20.0
        (1 of ("product2" by "brandC")) or // 5.0
        (1 of ("product1" by "brandB"))    // 15.0
    )
    assertEquals(analyzer.reply(session)(nestedOrExpr), "Cela coûte CHF 5.00.")
  }

  test("AND expression should combine all products") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    val andExpr = priceRequest(
      (1 of ("product1" by "brandA")) and // 10.0
        (2 of ("product2" by "brandC"))     // 10.0
    )
    assertEquals(analyzer.reply(session)(andExpr), "Cela coûte CHF 20.00.")
  }

  test("Default brand should be used when not specified") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    val exprWithDefaultBrand = priceRequest(1 of "product1".product) // Should use brandA
    assertEquals(analyzer.reply(session)(exprWithDefaultBrand), "Cela coûte CHF 10.00.")
  }

  test("Account service should be called with correct parameters on order") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    session.setCurrentUser("user1")

    val orderExpr = orderRequest(
      (1 of ("product1" by "brandA")) and // 10.0
        (1 of ("product2" by "brandC"))     // 5.0
    )

    analyzer.reply(session)(orderExpr)

    // Verify that purchase was called with the correct parameters
    assertEquals(accountService.purchaseCalls.size, 1)
    assertEquals(accountService.purchaseCalls.head._1, "user1")
    assertEquals(accountService.purchaseCalls.head._2, 15.0) // Total price
  }

  test("Order should fail when user has insufficient balance") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    session.setCurrentUser("insufficientUser") // Has balance of 5.0

    val expensiveOrderExpr = orderRequest(1 of ("product1" by "brandB")) // Costs 15.0
    val result = analyzer.reply(session)(expensiveOrderExpr)

    assertEquals(
      result,
      "Vous n'avez pas assez d'argent pour acheter ces produits."
    )
  }

  test("Product formatting should handle various numbers of products") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    session.setCurrentUser("user1") // Has sufficient balance

    // Single product
    val singleProductOrder = orderRequest(2 of ("product1" by "brandA"))
    val singleResult = analyzer.reply(session)(singleProductOrder)
    assert(singleResult.contains("2 product1 brandA!"))

    // Two products
    val twoProductOrder = orderRequest(
      (1 of ("product1" by "brandA")) and
        (3 of ("product2" by "brandC"))
    )
    val twoResult = analyzer.reply(session)(twoProductOrder)
    assert(twoResult.contains("1 product1 brandA et 3 product2 brandC!"))

    // Three products
    val threeProductOrder = orderRequest(
      (1 of ("product1" by "brandA")) and
        (2 of ("product1" by "brandB")) and
        (3 of ("product2" by "brandC"))
    )
    val threeResult = analyzer.reply(session)(threeProductOrder)
    assert(threeResult.contains("1 product1 brandA, 2 product1 brandB et 3 product2 brandC!"))
  }

  test("Complex nested expressions with AND and OR") {
    val productService = new SimpleProductService()
    val accountService = new TestAccountService()
    val analyzer = new AnalyzerService(productService, accountService)
    val session = new Session("test-session")

    val complexExpr = priceRequest(
      ((1 of ("product1" by "brandA")) or (2 of ("product2" by "brandC"))) and
        ((2 of ("product1" by "brandB")) or (1 of ("product2" by "brandD")))
    )

    // Expected calculations:
    // Left side: min(10.0, 10.0) = 10.0 (both options equal)
    // Right side: min(30.0, 7.5) = 7.5
    // Combined: 10.0 + 7.5 = 17.5

    assertEquals(analyzer.reply(session)(complexExpr), "Cela coûte CHF 17.50.")
  }
}
