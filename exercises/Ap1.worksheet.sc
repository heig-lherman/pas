/*
 * 1. Déclaration de variables
 * Déclarez les variables mutables nom et prenom de type String, avec la valeur vide comme valeur
 * par défaut.
 */
var nom = ""
var prenom = ""

/*
 * 2. Affectation de variables
 * Affectez votre nom et votre prénom aux deux variables déclarées dans l'exercice précédent.
 */
nom = "Herman"
prenom = "Loïc"
    

/*
 * 3. Immuables vs mutables
 * Déclarez la variable immuable dateDeNaissance de type String, avec la valeur vide comme valeur par défaut.
 * — Affectez votre date de naissance à la variable dateDeNaissance.
 * — Est-ce possible ? Pourquoi ?
 * — Si ça ne fonctionne pas, faites en sorte que la variable dateDeNaissance possède comme valeur,
 *   votre date de naissance.
 */
val dateDeNaissance: String = "2001-05-11"


/*
 * 4. Affichage
 * Affichez, à l'aide de la fonction println(...) et des variables nom, prenom et dateDeNaissance le
 * texte :
 * Bonjour, je m'appelle <<prenom>> <<nom>> et je suis né le <<dateDeNaissance>>.
 */
val msg = s"Bonjour, je m'appelle ${prenom} ${nom} et je suis né le ${dateDeNaissance}."
println(msg)

/*
 * 5. Boucle I
 * En utilisant une boucle while et la variable compteur, affichez 5 fois le message de l'exercice 4.
 */
var count = 0
while count < 5 do
    println(msg)
    count += 1

/*
 * 6. Boucle II
 * En utilisant une boucle for, affichez 5 fois le message de l'exercice 4.
 */
for _ <- 1 to 5 do println(msg)

/*
 * 7. Boucle III
 * En utilisant une boucle for, et une autre condition d'arrêt qu'à l'exercice précédent,
 * affichez 5 fois le message de l'exercice 4.
 */
for _ <- 0 until 5 do println(msg)


/*
 * 8. Condition booléenne
 * En utilisant les variables compteur, nom et prénom, utilisez une condition booléenne pour afficher
 * votre nom si le compteur est égal à 0, sinon votre prénom.
 */
if count == 0 then println(nom) else println(prenom)

/*
 * 9. Match cases
 * Réimplémentez l'exercice précédent en remplaçant les conditions booléennes par un match cases
 * Scala.
 */
count = 0
count match {
    case 0 => println(nom)
    case _ => println(prenom)
}
