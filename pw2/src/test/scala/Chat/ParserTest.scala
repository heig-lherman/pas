package Chat

import Chat.TokenizerService.withEol
import Chat.TreeDSL.*
import munit.FunSuite

class ParserTest extends FunSuite with TreeAssertions {

  test("parse simple greeting") {
    val tokens = List(
      ("bonjour", Token.BONJOUR),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = greeting
    assertTreeEquals(expected, result)
  }

  test("parse balance request with greeting") {
    val tokens = List(
      ("bonjour", Token.BONJOUR),
      ("je", Token.JE),
      ("veux", Token.VOULOIR),
      ("connaitre", Token.CONNAITRE),
      ("mon", Token.MON),
      ("solde", Token.SOLDE),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = balanceRequest
    assertTreeEquals(expected, result)
  }

  test("parse balance request without greeting") {
    val tokens = List(
      ("je", Token.JE),
      ("veux", Token.VOULOIR),
      ("connaitre", Token.CONNAITRE),
      ("mon", Token.MON),
      ("solde", Token.SOLDE),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = balanceRequest
    assertTreeEquals(expected, result)
  }

  test("parse price request for a product with brand") {
    val tokens = List(
      ("combien", Token.COMBIEN),
      ("coute", Token.COUTER),
      ("1", Token.NUM),
      ("croissant", Token.PRODUIT),
      ("cailler", Token.MARQUE),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = priceRequest(1 of ("croissant" by "cailler"))
    assertTreeEquals(expected, result)
  }

  test("parse price request for multiple products") {
    val tokens = List(
      ("bonjour", Token.BONJOUR),
      ("quel", Token.QUEL),
      ("est", Token.ETRE),
      ("le", Token.LE),
      ("prix", Token.PRIX),
      ("de", Token.DE),
      ("1", Token.NUM),
      ("biere", Token.PRODUIT),
      ("boxer", Token.MARQUE),
      ("et", Token.ET),
      ("2", Token.NUM),
      ("biere", Token.PRODUIT),
      ("farmer", Token.MARQUE),
      ("ou", Token.OU),
      ("1", Token.NUM),
      ("biere", Token.PRODUIT),
      ("tenebreuse", Token.MARQUE),
      ("ou", Token.OU),
      ("2", Token.NUM),
      ("biere", Token.PRODUIT),
      ("jackhammer", Token.MARQUE),
      ("et", Token.ET),
      ("3", Token.NUM),
      ("croissant", Token.PRODUIT),
      ("cailler", Token.MARQUE),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = priceRequest(
      ((1 of ("biere" by "boxer")) and (2 of ("biere" by "farmer")))
        or (1 of ("biere" by "tenebreuse"))
        or (2 of ("biere" by "jackhammer"))
        and (3 of ("croissant" by "cailler")),
    )
    assertTreeEquals(expected, result)
  }

  test("parse order request") {
    val tokens = List(
      ("je", Token.JE),
      ("voudrais", Token.VOULOIR),
      ("commander", Token.COMMANDER),
      ("1", Token.NUM),
      ("biere", Token.PRODUIT),
      ("boxer", Token.MARQUE),
      ("et", Token.ET),
      ("2", Token.NUM),
      ("croissant", Token.PRODUIT),
      ("cailler", Token.MARQUE),
    ).iterator.withEol

    val result = Parser(tokens).parsePhrases()

    val expected = orderRequest(
      (1 of ("biere" by "boxer")) and (2 of ("croissant" by "cailler")),
    )
    assertTreeEquals(expected, result)
  }

  test("throws on unsupported balance syntax") {
    val tokens = List(
      ("quel", Token.QUEL),
      ("est", Token.ETRE),
      ("mon", Token.MON),
      ("solde", Token.SOLDE),
    ).iterator.withEol

    val result = intercept[UnexpectedTokenException] {
      Parser(tokens).parsePhrases()
    }

    assertEquals(result.getMessage, "Expected: LE, found: MON")
  }

  test("throws on empty sentence") {
    val tokens = List().iterator.withEol

    val result = intercept[UnexpectedTokenException] {
      Parser(tokens).parsePhrases()
    }

    assertEquals(
      result.getMessage,
      "Expected: BONJOUR or QUEL or COMBIEN or JE, found: EOL",
    )
  }
}
