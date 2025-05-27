/** # Future
  */

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure, Random}
import scala.util.Try

/** ## Exercice 1
  *
  * La fonction f ci-dessous permet de créer un Future[Int] à titre d'exemple
  * pour les exercices de cette partie. Elle réussit ou échoue de manière
  * aléatoire.
  *
  * Compléter la méthode f pour afficher la réussite/échec des futures avec des
  * messages appropriés comme :
  *   - success for f1 with value 898
  *   - f2 failed with Boxed Exception
  *
  * Implémenter les variantes suivantes en utilisant la méthode f pour créer les
  * futures et commenter l'ordre d'execution :
  *
  *   a. Créer 3 futures, f1, f2 et f3 indépendement les uns des autres.
  *   a. Créer un future f5 seulement après la réussite d'un premier future f4.
  *   a. Créer un future f8 seulement après la réussite d'un future f7 et
  *      celui-ci seulement après la réussite d'un future f6 mais seulement si
  *      la valeur de résultat de f6 est supérieure à 70.
  *   a. Créer une List[Future] contenant les 3 futures, f9, f10, et f11 créés
  *      en parallèle et afficher la somme de leur résultats ou un message
  *      d'erreur. Trouver dans la librairie scala.concurrent une méthode pour
  *      assembler les 3 future en parallèle.
  *   a. Créer une List[Future] contenant les 3 futures, f12, f13, et f14 créés
  *      en parallèle et afficher le resultat du premier future qui a terminé
  *      (par example "f12 a terminé en premier") ou afficher message d'échec.
  *      Trouver dans la librairie scala.concurrent une méthode pour accéder aux
  *      résultat du premier Future qui termine.
  *   a. Créer un future f16 seulement après l'echec d'un premier future f15.
  *
  * Attention, pour avoir le temps d'afficher les résutlats, la thread
  * principale doit avoir Thread.sleep(400) à la fin.
  */

var durations = Iterator.continually(Random.nextInt(100))
var successes = Iterator.continually(Random.nextDouble() > 0.7)
def f(name: String): Future[(Int, String)] = {
  val duration = durations.next()
  val success = successes.next()
  println(s"creating future $name")
  val f = Future {
    Thread.sleep(duration)
    if (success) (duration, name) else throw new Exception(s"did not succeed")
  }

  f.onComplete {
    case Success((duration, name)) =>
      println(s"success for $name with value $duration")
    case Failure(exception) =>
      println(s"$name failed with ${exception.getMessage}")
  }

  f
}

// a.
val f1 = f("f1")
val f2 = f("f2")
val f3 = f("f3")

// b.
val f4 = f("f4")
val f5 = f4.flatMap(_ => f("f5"))

// c.
val f8 =
  for
    f6 @ (duration, _) <- f("f6")
    if duration > 700
    f7 <- f("f7")
    f8 <- f("f8")
  yield f8

// d.
val fd = Future
  .sequence(List(f("f9"), f("f10"), f("f11")))
  .andThen {
    case Success(rs) =>
      println(s"f9 + f10 + f11 = ${rs.map(_._1).sum}")
    case Failure(exc) =>
      println(s"at least one future did not complete: ${exc.getMessage}")
  }

// e.
val fe = Future
  .firstCompletedOf(List(f("f12"), f("f13"), f("f14")))
  .andThen {
    case Success((_, name)) =>
      println(s"$name a fini en premier")
    case Failure(exception) =>
      println(s"no future completed: ${exception.getMessage}")
  }

// f.
val f15 = f("f15")
val f16 = f15.recoverWith(_ => f(f"f19"))

Thread.sleep(1000)

/** ## Exercice 2
  *
  * Ecrire une fonction exists qui prend en paramètre un Future et un predicat
  * et qui retourne un nouveau Future[Boolean].
  *
  *   - Le Future résultat est complété avec true si et seulement si le future
  *     original est complété et que le prédicat retourne true, sinon le future
  *     résultat est complété avec false,
  *   - Si le future original échoue, le future résultat échoue aussi
  */

def exists[A](f: Future[A])(p: A => Boolean): Future[Boolean] =
  for rf <- f
  yield p(rf)

val p = (x: Int) => x == 2
println(Await.result(exists(Future.successful { 1 })(p), 1.second) == false)
println(Await.result(exists(Future.successful { 2 })(p), 1.second) == true)

Try { Await.result(exists(Future.failed { ??? })(p), 1.second) }.isFailure

/** ## Exercice 3
  *
  * Prenons la méthode `getWeather(url: String)` qui effectue une requête vers
  * un service web spécifié par l’url pour récupérer en un mot ("sunny",
  * "rainy", "windy", etc.) la météo actuelle.
  *
  * Prenons la liste `urls` qui liste les urls alternatifs qui donne accès à la
  * même fonctionnalité.
  *
  * Implémenter la méthode `successiveFallbacks` qui prend une liste d’urls en
  * paramètre et qui lance des `getWeather` pour chaque url les uns après la
  * terminaison des autres tant que le précèdent n’a pas réussi. La méthode
  * retourne le résultat du premier `getWeather` à réussir. Si aucun
  * `getWeather` ne réussi, elle retourne l’erreur du premier échec.
  */

def getWeather(url: String): Future[String] =
  if url.contains("fail") then
    Future.failed { new Exception("call to API did not succeed") }
  else if url.contains("yverdon") then Future.failed { new Exception("foggy") }
  else
    Future.successful {
      val weathers = List("sunny", "rainy", "cloudy", "windy", "stormy")

      Thread.sleep(Random.nextInt(100))
      weathers(Random.nextInt(weathers.length))
    }

val urls: List[String] = List(
  "example.com/fail",
  "weatherapi.example.com/yverdon",
  "weatherapi.example.com",
  "forecast.example.com",
  "example.com/fail"
)

def successiveFallbacks(urls: List[String]): Future[String] = urls match {
  case Nil => Future.failed(new Exception("No url remaining"))
  case url :: remaining =>
    getWeather(url).fallbackTo(successiveFallbacks(remaining))
}

println(Await.result(successiveFallbacks(urls), 1.second))
