package Chat

import munit.FunSuite
import Chat.Token.*
import Utils.Dictionary

class TokenizerServiceTest extends FunSuite {
  val tokenizer = new TokenizerService(Dictionary.dictionary)

  def tokenizeToList(input: String): List[(String, Token)] =
    tokenizer.tokenize(input).toList

  test("tokenize simple greeting") {
    val input = "bonjour"
    val expected = List(
      "bonjour" -> BONJOUR,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("tokenize complex sentence") {
    val input = "je veux commander 1 bière"
    val expected = List(
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "commander" -> COMMANDER,
      "1" -> NUM,
      "biere" -> PRODUIT,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("handle unknown words") {
    val input = "xyz abc"
    val expected = List(
      "xyz" -> UNKNOWN,
      "abc" -> UNKNOWN,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("normalize input properly") {
    val input = "Bonjour, je veux Commander!"
    val expected = List(
      "bonjour" -> BONJOUR,
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "commander" -> COMMANDER,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("apply dictionary replacements") {
    val input = "je voudrais connaître mon solde"
    val expected = List(
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "connaitre" -> CONNAITRE,
      "mon" -> MON,
      "solde" -> SOLDE,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("recognize brands") {
    val input = "tenebreuse boxer"
    val expected = List(
      "tenebreuse" -> MARQUE,
      "boxer" -> MARQUE,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("recognize products") {
    val input = "biere croissant"
    val expected = List(
      "biere" -> PRODUIT,
      "croissant" -> PRODUIT,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("recognize numbers") {
    val input = "je veux 5 bieres"
    val expected = List(
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "5" -> NUM,
      "biere" -> PRODUIT,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("handle apostrophes") {
    val input = "j'aimerais"
    val expected = List(
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("handle multiple spaces") {
    val input = "je   veux  commander"
    val expected = List(
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "commander" -> COMMANDER,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("handle empty input") {
    val input = ""
    val expected = List(
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }

  test("handle long input") {
    val input =
      "bonjour je voudrais commander 5 biere boxer et 3 croissant cailler"
    val expected = List(
      "bonjour" -> BONJOUR,
      "je" -> JE,
      "vouloir" -> VOULOIR,
      "commander" -> COMMANDER,
      "5" -> NUM,
      "biere" -> PRODUIT,
      "boxer" -> MARQUE,
      "et" -> ET,
      "3" -> NUM,
      "croissant" -> PRODUIT,
      "cailler" -> MARQUE,
      "EOL" -> EOL,
    )
    assertEquals(tokenizeToList(input), expected)
  }
}
