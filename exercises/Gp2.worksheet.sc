object Ex1 {
  // Définir une fonction `describe` qui accepte un paramètre pouvant être un Int, une 
  // String ou un Boolean.
  //
  // La fonction retourne une description de la valeur :
  //
  // - Pour Int : "Ceci est un entier : [valeur]"
  // - Pour String : "Ceci est une chaîne : [valeur]"
  // - Pour Boolean : "Ceci est un booléen : [valeur]"
  def describe(value: Int | String | Boolean): String =
    value match
      case i: Int => s"Ceci est un entier : $i"
      case s: String => s"Ceci est une chaîne : $s"
      case b: Boolean => s"Ceci est un booléen : $b"

  println(describe(42))
  println(describe("Hello"))
  println(describe(true))
}

Ex1

object Ex2 {
  // En utilisant uniquement les traits ci-dessous, créer une fonction `greet` avec le type 
  // de paramètre d'entrée et de sortie adaptés qui retourne le nom et l'age d'une entitée.
  //
  // L'entitée peut soit directement avoir un age ou elle a une date de naissance.
  //
  // Créer des entités et tester la fonction.
  //
  // Astuce: Vous pouvez utiliser `ChronoUnit.DAYS.between` pour retrouver l'age à partir 
  // de 2 `Instant`.
  //
  // Par example: greet(e1) //> res0: String = Hello, John! You are 24 years old.

  import java.time.Instant
  import java.time.temporal.ChronoUnit

  trait HasName {
    def name: String
  }

  trait HasAge {
    def age: Int
  }

  trait HasDob {
    def dob: Instant
  }

  def greet(entity: HasName & (HasAge | HasDob)): String =
    val age = entity match
      case e: HasAge => e.age
      case e: HasDob => ChronoUnit.DAYS.between(e.dob, Instant.now()) / 365
    s"Hello, ${entity.name}! You are $age years old."

  case class Person(name: String, age: Int) extends HasName, HasAge
  case class PersonWithDob(name: String, dob: Instant) extends HasName, HasDob

  val e1 = Person("John", 24)
  val e2 = PersonWithDob("Alice", Instant.parse("2000-01-01T00:00:00Z"))

  val e3 = new HasName with HasDob:
    def name: String = "Bob"
    def dob: Instant = Instant.parse("1995-05-15T00:00:00Z")
  println(greet(e1))
  println(greet(e2))
  println(greet(e3))
}

Ex2

object Ex3 {
  // Corriger les types de la file immuable (immutable queue) ci-dessous afin qu'elle gère 
  // correctement la variance tout en respectant la sécurité des types (type safety).

  trait Queue[+T] {
    def enqueue[U >: T](x: U): Queue[U]
    def head: T
    def tail: Queue[T]
  }

  object Queue {
    def apply[T](xs: T*): Queue[T] =
      QueueImpl(xs.toList, Nil)

    // Private implementation class
    private class QueueImpl[+T](
        private val leading: List[T],
        private val trailing: List[T]
    ) extends Queue[T] {
      // Ensures leading list is non-empty when needed
      private def mirror: QueueImpl[T] =
        if leading.isEmpty then QueueImpl(trailing.reverse, Nil)
        else this

      def head: T = mirror.leading.head

      def tail: Queue[T] =
        val q = mirror
        QueueImpl(q.leading.tail, q.trailing)

      def enqueue[U >: T](x: U): Queue[U] =
        QueueImpl(leading, x :: trailing)
    }
  }

  trait Fruit {}
  trait Apple extends Fruit {}
  trait Banana extends Fruit {}

  val queue = Queue[Apple](new Apple {}, new Apple {})
  val test: Queue[Fruit] = queue.enqueue(new Banana {})
}

Ex3
