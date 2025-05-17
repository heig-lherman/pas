package Chat

import Chat.TreeDSL.*
import Data.*
import munit.FunSuite

class AnalyzerServiceIntegrationTest extends FunSuite {

  // Test setup
  var productService: ProductService = _
  var accountService: AccountService = _
  var session: Session = _
  var analyzer: AnalyzerService = _

  override def beforeEach(context: munit.BeforeEach): Unit = {
    productService = new ProductImpl()
    accountService = new AccountImpl()
    session = new Session("test_session")
    analyzer = new AnalyzerService(productService, accountService)

    accountService.addAccount("poor_user", 1.00)
  }

  // Greeting tests
  test("reply with greeting when no user is logged in") {
    val result = analyzer.reply(session) { greeting }
    assertEquals(result, "Hello !")
  }

  test("reply with personalized greeting when user is logged in") {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) { greeting }
    assertEquals(result, "Hello test_user !")
  }

  // Balance request tests
  test(
    "reply with error message for balance request when no user is logged in",
  ) {
    val result = analyzer.reply(session) { balanceRequest }
    assertEquals(result, "Veuillez d'abord vous identifier.")
  }

  test("reply with balance when user is logged in") {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) { balanceRequest }
    assertEquals(result, "Le montant actuel de votre solde est de CHF 30.00.")
  }

  // Price request tests
  test("reply with price for a single product") {
    val result = analyzer.reply(session) {
      priceRequest(2 of "croissant".product)
    }

    assertEquals(result, "Cela coûte CHF 4.00.")
  }

  test("reply with price for a product with specified brand") {
    val result = analyzer.reply(session) {
      priceRequest(1 of ("croissant" by "cailler"))
    }

    assertEquals(result, "Cela coûte CHF 2.00.")
  }

  test("reply with price for products joined with AND") {
    val result = analyzer.reply(session) {
      priceRequest((2 of "croissant".product) and (1 of "biere".product))
    }

    assertEquals(result, "Cela coûte CHF 5.00.") // 2*2.00 + 1*1.00
  }

  test("reply with price of cheaper option for products joined with OR") {
    val result = analyzer.reply(session) {
      priceRequest((3 of "croissant".product) or (2 of "biere".product))
    }

    // Expected: picks the cheaper option
    // 3 croissants at 2.00 each = 6.00
    // 2 beers at 1.00 each = 2.00
    // Should pick the 2 beers
    assertEquals(result, "Cela coûte CHF 2.00.")
  }

  // Order request tests
  test("reply with error message for order request when no user is logged in") {
    val result = analyzer.reply(session) {
      orderRequest(2 of "croissant".product)
    }

    assertEquals(result, "Veuillez d'abord vous identifier.")
  }

  test(
    "reply with success for order request when user has sufficient balance",
  ) {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) {
      orderRequest(2 of "croissant".product)
    }

    // Initial balance = 30.00 (default new account balance)
    // 2 croissants at 2.00 each = 4.00
    // New balance = 26.00
    assertEquals(
      result,
      "Voici donc 2 croissant maison! Cela coûte CHF 4.00 et votre nouveau solde est de CHF 26.00.",
    )
  }

  test(
    "reply with failure for order request when user has insufficient balance",
  ) {
    accountService.setCurrent("poor_user", session)
    val result = analyzer.reply(session) {
      orderRequest(2 of "biere".product)
    }

    // Initial balance = 1.00
    // 2 beers at 1.00 each = 2.00
    // Insufficient funds
    assertEquals(
      result,
      "Vous n'avez pas assez d'argent pour acheter ces produits.",
    )
  }

  test(
    "reply with correct formatted products for order with multiple products",
  ) {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) {
      orderRequest(
        (1 of ("croissant" by "cailler")) and (1 of ("biere" by "wittekop")),
      )
    }

    // 1 croissant at 2.00 + 1 beer at 2.00 = 4.00
    // Initial balance = 30.00, new balance = 26.00
    assertEquals(
      result,
      "Voici donc 1 croissant cailler et 1 biere wittekop! Cela coûte CHF 4.00 et votre nouveau solde est de CHF 26.00.",
    )
  }

  test("reply with correct product selection for order with OR expression") {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) {
      orderRequest((3 of "croissant".product) or (2 of "biere".product))
    }

    // 3 croissants at 2.00 each = 6.00
    // 2 beers at 1.00 each = 2.00
    // Should pick the 2 beers as they're cheaper
    assertEquals(
      result,
      "Voici donc 2 biere boxer! Cela coûte CHF 2.00 et votre nouveau solde est de CHF 28.00.",
    )
  }

  test("reply with complex order combining AND and OR expressions") {
    accountService.setCurrent("test_user", session)
    val result = analyzer.reply(session) {
      orderRequest(
        ((1 of ("croissant" by "cailler")) or (2 of ("croissant" by "maison")))
          and
            (1 of ("biere" by "wittekop")),
      )
    }

    // Option 1: 1 Cailler croissant at 2.00 = 2.00
    // Option 2: 2 Maison croissants at 2.00 each = 4.00
    // Should pick Option 1 (cheaper) + 1 Wittekop beer at 2.00 = 4.00
    // Initial balance = 30.00, new balance = 26.00
    assertEquals(
      result,
      "Voici donc 1 croissant cailler et 1 biere wittekop! Cela coûte CHF 4.00 et votre nouveau solde est de CHF 26.00.",
    )
  }

  test("complex nested OR expressions should pick the cheapest option") {
    accountService.setCurrent("test_user", session)
    val expr = orderRequest(
      (1 of ("biere" by "boxer")) or
        ((2 of ("croissant" by "maison")) or (1 of ("croissant" by "cailler"))),
    )
    val result = analyzer.reply(session)(expr)

    // Option 1: 1 Boxer beer at 1.00 = 1.00
    // Option 2: 2 Maison croissants at 2.00 each = 4.00
    // Option 3: 1 Cailler croissant at 2.00 = 2.00
    // Should pick Option 1 (cheapest)
    assertEquals(
      result,
      "Voici donc 1 biere boxer! Cela coûte CHF 1.00 et votre nouveau solde est de CHF 29.00.",
    )
  }

  test("complex nested AND expressions should combine all products") {
    accountService.setCurrent("test_user", session)
    val expr = orderRequest(
      (1 of ("biere" by "boxer")) and
        ((1 of ("croissant" by "maison")) and (1 of ("croissant" by "cailler"))),
    )
    val result = analyzer.reply(session)(expr)

    // 1 Boxer beer at 1.00 + 1 Maison croissant at 2.00 + 1 Cailler croissant at 2.00 = 5.00
    // Initial balance = 30.00, new balance = 25.00
    assertEquals(
      result,
      "Voici donc 1 biere boxer, 1 croissant maison et 1 croissant cailler! Cela coûte CHF 5.00 et votre nouveau solde est de CHF 25.00.",
    )
  }
}
