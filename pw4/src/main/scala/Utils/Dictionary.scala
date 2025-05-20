package Utils

/** Contains the dictionary of the application, which is used to validate and
  * normalize words entered by the user.
  */
object Dictionary:
  // This dictionary is a Map object that contains valid words as keys and their normalized equivalents as values (e.g.
  // we want to normalize the words "veux" and "aimerais" in one unique term: "vouloir").
  val dictionary: Map[String, String] = Map(
    // - Bonjour
    "bonjour" -> "bonjour",
    "hello" -> "bonjour",
    "yo" -> "bonjour",
    // - Je
    "je" -> "je",
    "j" -> "je",
    // - Mon
    "mon" -> "mon",
    "ma" -> "mon",
    "mes" -> "mon",
    // - Le
    "le" -> "le",
    "la" -> "le",
    "les" -> "le",
    // - De
    "de" -> "de",
    "du" -> "de",
    "des" -> "de",
    // - Combien
    "combien" -> "combien",
    // - Quel
    "quel" -> "quel",
    "quelle" -> "quel",
    "quels" -> "quel",
    "quelles" -> "quel",
    // - Etre
    "être" -> "etre",
    "suis" -> "etre",
    "es" -> "etre",
    "est" -> "etre",
    "sommes" -> "etre",
    "etes" -> "etre",
    "sont" -> "etre",
    // - Vouloir
    "veux" -> "vouloir",
    "voudrais" -> "vouloir",
    "aimerais" -> "vouloir",
    "souhaite" -> "vouloir",
    "souhaîte" -> "vouloir",
    "souhaiterais" -> "vouloir",
    // - Couter
    "couter" -> "couter",
    "cout" -> "couter",
    "coute" -> "couter",
    "coutent" -> "couter",
    "coûter" -> "couter",
    "coûte" -> "couter",
    "coûtent" -> "couter",
    // - Prix
    "prix" -> "prix",
    "montant" -> "prix",
    "tarif" -> "prix",
    // - Commander
    "commander" -> "commander",
    "acheter" -> "commander",
    "prendre" -> "commander",
    // - Connaitre
    "connaitre" -> "connaitre",
    "connaître" -> "connaitre",
    "savoir" -> "connaitre",
    "connais" -> "connaitre",
    "sais" -> "connaitre",
    // - Produits
    "bière" -> "biere",
    "bieres" -> "biere",
    "bières" -> "biere",
    "croissant" -> "croissant",
    "croissants" -> "croissant",
    // - Marques
    "ténébreuse" -> "tenebreuse",
    // - Solde
    "solde" -> "solde",
    "argent" -> "solde",
    "porte-monnaie" -> "solde",
    "compte" -> "solde",
    "banque" -> "solde",
    // - Logique
    "et" -> "et",
    "ou" -> "ou",
  )
end Dictionary
