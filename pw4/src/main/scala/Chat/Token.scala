package Chat

enum Token:
  case // Terms
    BONJOUR,
    // Articles
    JE,
    MON,
    LE,
    DE,
    // Interrogative adjectives
    COMBIEN,
    QUEL,
    // Actions
    ETRE,
    VOULOIR,
    COUTER,
    COMMANDER,
    CONNAITRE,
    // Logic Operators
    ET,
    OU,
    // Products
    PRODUIT,
    MARQUE,
    SOLDE,
    PRIX,
    // Util
    NUM,
    EOL, // Use EOL to indicate the end of the line (i.e. there are no more tokens for this input)
    UNKNOWN // Use UNKNOWN if there is no match with any other tokens or words
end Token
