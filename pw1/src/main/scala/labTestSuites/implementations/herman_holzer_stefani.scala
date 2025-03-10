package labTestSuites.implementations.herman_holzer_stefani

import labTestSuites.*

import scala.annotation.tailrec

/** Référence all the implementations so that they can be used in the tests
  * SHOULD NOT BE MODIFIED !!!
  */
val all = Seq(
  X,
  B1,
  B2,
  B3,
  B4,
  C1,
  C2,
  C3,
  C4,
  C5,
  C6,
  C7,
  C8,
  C9,
  C10,
  D1,
  D2,
  D3,
  D4,
  D5,
  D6,
  D7,
  D8,
  D9,
  D10,
  D11,
  D12,
  D13,
  D14,
  D15,
  D16,
)

//
// BELOW ARE THE OBJECTS IN WHICH YOU WILL
// ADD THE IMPLEMENTATIONS OF EACH EXERCICE
//
// ⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟⮟

/** Example of the implementation of an exercice
  */
object X extends Implementations[X]:
  /** Assign each implementation to a variable. It will simplify changing their
    * order.
    */
  val recursive = new X:
    def square(x: Double): Double = x * x
    def power(x: Double, n: Int): Double =
      require(n >= 0)
      if n == 0 then 1.0
      else if n % 2 == 0 then square(power(x, n / 2))
      else x * power(x, n - 1)

  /** List here all your implementations of the current exercice.
    *
    * Only the first implementation in the sequence will be tested by your test
    * suite. If you want to test another implementation, move it to the top of
    * the sequence.
    */
  val correct: Seq[X] = Seq(
    recursive,
  )

/** Ecrivez une fonction récursive qui prend une List[Int] en paramètre et qui
  * somme tous les éléments de la liste, sans utiliser la fonction définie
  * Math.sum.
  */
object B1 extends Implementations[B1]:
  val tailrecImpl = new B1:
    def func(ls: List[Int]): Int = sum(0, ls)

    @tailrec
    private def sum(cur: Int, ls: List[Int]): Int = {
      if ls.isEmpty then cur
      else sum(cur + ls.head, ls.tail)
    }

  val correct = Seq(
    tailrecImpl,
  )

/** Définissez un Vector[Int] contenant les valeurs de 1 à 1000.
  *
  * Ecrivez une fonction qui prend un Int en paramètre.
  *   - Si cet Int est un multiple de trois, retourne son cube.
  *   - Si cet Int est un multiple de deux, retourne son carré.
  *   - Si cet Int est impair, retourne -1. Appliquez cette fonction à toutes
  *     les valeurs du vecteur.
  */
object B2 extends Implementations[B2]:
  val basicImpl = new B2:
    val vector = (1 to 1000).toVector

    def func(v: Int): Int = {
      v match {
        case v if v % 3 == 0 => v * v * v
        case v if v % 2 == 0 => v * v
        case _               => -1
      }
    }

    val apply = vector.map(func)

  val correct = Seq(
    basicImpl,
  )

/** Définissez un Vector[Int] contenant les valeurs de 1 à 10.
  *
  * Ecrivez une fonction qui prend un Int en paramètre, et retourne le factoriel
  * de ce nombre, en utilisant une implémentation récursive.
  *
  * Ecrivez une fonction qui prend un Int en paramètre, et retourne le nombre de
  * fibonacci corres- pondant, en utilisant une implémentation récursive.
  *
  * Ecrivez la même fonction en utilisant une implémentation récursive
  * terminale.
  *
  * Appliquez les fonctions au vecteur comme suit :
  *   - Si le nombre est pair, lui appliquer la fonction factoriel.
  *   - Si le nombre est impair, lui appliquer la fonction fibonacci.
  */
object B3 extends Implementations[B3]:
  val basicImpl = new B3:
    val vector = Vector.range(1, 11)

    def fact(x: Int): BigInt = {
      x match {
        case 0 | 1 => 1
        case _     => x * fact(x - 1)
      }
    }

    def fib(x: Int): BigInt = {
      x match {
        case x if x < 0 => -1
        case 0          => 0
        case 1          => 1
        case _          => fib(x - 1) + fib(x - 2)
      }
    }

    def fibRec(x: Int): BigInt = {
      @tailrec
      def fibonacciHelper(n: Int, previous: Int, current: Int): BigInt = {
        n match {
          case v if v < 0 => -1
          case 0          => previous
          case _          => fibonacciHelper(n - 1, current, previous + current)
        }
      }

      fibonacciHelper(x, 0, 1)
    }

    def factOrFib(value: Int): BigInt = {
      if value % 2 == 0 then fact(value)
      else fibRec(value)
    }

    val apply = vector.map {
      case v if v % 2 == 0 => fact(v)
      case v               => fib(v)
    }

  val correct = Seq(
    basicImpl,
  )

/** L’exponentiation rapide est un algorithme utilisé pour calculer rapidement
  * de grandes puissances entières :
  *
  * b^(2n) = (b^2)^n = (b^n)^2 , b^(2n+1) = b * b^(2n)
  *
  * Écrire une fonction récursive terminale fastExp qui implémente
  * l’exponentiation rapide.
  *
  * def fastExp(base: Int, exp: Int): Int = ???
  */
object B4 extends Implementations[B4]:
  val basicImpl = new B4:
    def fastExp(base: Int, exp: Int): Int = {
      @tailrec
      def fastExpHelper(base: Int, exp: Int, acc: Int): Int = {
        exp match {
          case 0               => acc
          case e if e < 0      => -1
          case e if e % 2 == 0 => fastExpHelper(base * base, e / 2, acc)
          case e               => fastExpHelper(base, e - 1, acc * base)
        }
      }

      fastExpHelper(base, exp, 1)
    }

  val correct = Seq(
    basicImpl,
  )

/** On souhaite créer une Map[Int, String] à partir d’un Vector[String] et d’une
  * List[Int], ou les clés sont les éléments de la liste et les valeurs les
  * éléments du vecteur (en se basant sur la collection la plus petite).
  *   - Ecrivez une première fonction récursive pour combiner les clés et les
  *     valeurs. Cette dernière prend le vecteur et la liste en paramètre et
  *     retourne une List[(Int, String)].
  *   - Ecrivez une seconde fonction qui prend le retour de la première fonction
  *     en paramètre et qui retourne la Map[Int, String].
  */
object C1 extends Implementations[C1]:
  val basicImpl = new C1:
    def zip(v: Vector[String], l: List[Int]): List[(Int, String)] =
      @tailrec
      def tupleListCreate(
          vec: Vector[String],
          lst: List[Int],
          acc: List[(Int, String)],
      ): List[(Int, String)] =
        (vec, lst) match
          case (v +: vs, l :: ls) => tupleListCreate(vs, ls, (l -> v) :: acc)
          case _                  => acc.reverse

      tupleListCreate(v, l, Nil)

    def toMap(t: List[(Int, String)]): Map[Int, String] = Map.from(t)

  val correct = Seq(
    basicImpl,
  )

/** Implémenter l’opération init avec le pattern matching.
  *
  * Rappel : init retourne une liste contenant tous les éléments sauf le
  * dernier. Une exception est lancée si la liste est vide.
  *
  * {{{
  *   def init[T](xs: List[T]): List[T] = xs match
  *     case List() => throw new Exception("init of empty list")
  *     case List(x) => ???
  *     case y :: ys => ???
  * }}}
  */
object C2 extends Implementations[C2]:
  val basicImpl = new C2:
    def init[T](xs: List[T]): List[T] = xs match
      case List()  => throw new Exception("init of empty list")
      case List(x) => Nil
      case x :: xs => x :: init(xs)

  val correct = Seq(
    basicImpl,
  )

/** Trouver l’avant dernier élément d’une liste.
  *
  * Exemple : penultimate(List(1, 1, 2, 3, 5, 8)) //> res0: Int = 5
  *
  * def penultimate[T](xs: List[T]) = ???
  */
object C3 extends Implementations[C3]:
  val basicImpl = new C3:
    def penultimate[T](xs: List[T]) =
      require(xs.length >= 2, "List should have at least 2 elements")
      xs(xs.length - 2)

  val correct = Seq(
    basicImpl,
  )

/** Détecter si une liste est un palindrome.
  *
  * Exemple : isPalindrome(List(1, 2, 3, 2, 1)) //> 3res0: Boolean = true
  *
  * def isPalindrome[T](xs: List[T]) = ???
  */
object C4 extends Implementations[C4]:
  val basicImpl = new C4:
    def isPalindrome[T](xs: List[T]) = xs == xs.reverse

  val correct = Seq(
    basicImpl,
  )

/** Enlever le nième élément d’une liste xs. Si n est hors des limites de la
  * liste retourner tous simplement xs.
  *
  * def removeAt[T](n: Int, xs: List[T]) = ???
  *
  * Exemple d’utilisation :
  * {{{removeAt(1, List('a', 'b', 'c', 'd')) //> res1: List[Char] = List(a, c, d)}}}
  */
object C5 extends Implementations[C5]:
  val basicImpl = new C5:
    def removeAt[T](n: Int, xs: List[T]) =
      if (n < 0) xs
      else
        xs.splitAt(n) match
          case (pre, Nil)       => xs
          case (pre, _ :: tail) => pre ::: tail

  val correct = Seq(
    basicImpl,
  )

/** Ecrire une fonction pack, qui prend les éléments duplicatas consécutifs dans
  * une liste et les regroupe en plusieurs sous-listes.
  *
  * Par exemple, pack(List("a", "a", "a", "b", "c", "c", "a")) doit retourner :
  * List( List("a", "a", "a"), List("b"), List("c", "c"), List("a"))
  *
  * Vous pouvez compléter l’implémentation suivante :
  *
  * {{{
  *   def pack[T](xs: List[T]): List[List[T]] = xs match
  *     case Nil => Nil
  *     case y :: ys => ???
  * }}}
  */
object C6 extends Implementations[C6]:
  val basicImpl = new C6:
    def pack[T](xs: List[T]): List[List[T]] = xs match
      case Nil => Nil
      case y :: ys =>
        val (group, rest) = xs.span(_ == y)
        group :: pack(rest)

  val correct = Seq(
    basicImpl,
  )

/** En utilisant pack, écrire une fonction encode qui produit le ”run length
  * encoding” (codage par plage) d’une liste.
  *
  * L’idée est d’encoder les n duplicatas consécutifs d’un élément x comme un
  * paire (x, n).
  *
  * Par exemple encode(List("a", "a", "a", "b", "c", "c", "a")) doit retourner :
  * List(("a", 3), ("b", 1), ("c", 2), ("a", 1))
  */
object C7 extends Implementations[C7]:
  val basicImpl = new C7:
    private def pack[T](xs: List[T]): List[List[T]] =
      C6.correct.head.pack(xs)

    def encode[T](xs: List[T]): List[(T, Int)] =
      pack(xs).map(group => (group.head, group.length))

  val correct = Seq(
    basicImpl,
  )

/** Implémenter une fonction récursive qui sépare une liste donnée en deux
  * sous-listes dont la première est de taille n. Vous ne devez pas utiliser les
  * méthodes take et drop.
  *
  * Compléter uniquement les parties (a), (b) et (c).
  *
  * {{{
  *   def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) =
  *     (n, ls) match
  *       case (_, Nil) => ??? // a
  *       case (0, list) => ??? // b
  *       case (n, h :: tail) => ??? // c
  * }}}
  */
object C8 extends Implementations[C8]:
  val basicImpl = new C8:
    def splitRecursive[A](n: Int, ls: List[A]): (List[A], List[A]) =
      (n, ls) match
        case (0, list) => (Nil, list)
        case (_, Nil) =>
          throw new IllegalArgumentException(
            "n must be less than or equal to the length of the list",
          )
        case (n, h :: tail) =>
          val (pre, post) = splitRecursive(n - 1, tail)
          (h :: pre, post)

  val correct = Seq(
    basicImpl,
  )

/** Ecrire la méthode decode qui permet de décoder une liste encodée (l’inverse
  * de l’exercice d’encodage de la question 7) seulement en utilisant le pattern
  * matching.
  *
  * Par exemple : decode(List(("a", 3), ("b", 1), ("c", 2), ("a", 1))) doit
  * retourner : List("a", "a", "a", "b", "c", "c", "a")
  */
object C9 extends Implementations[C9]:
  val tailrecImpl = new C9:
    def decode[T](xs: List[(T, Int)]): List[T] = {
      @tailrec
      def decodeHelper(
          acc: List[T],
          xs: List[(T, Int)],
      ): List[T] = xs match
        case Nil            => acc
        case (x, n) :: tail => decodeHelper(acc ++ List.fill(n)(x), tail)

      decodeHelper(Nil, xs)
    }

  val basicRecImpl = new C9:
    def decode[T](xs: List[(T, Int)]): List[T] = xs match
      case Nil            => Nil
      case (x, n) :: tail => List.fill(n)(x) ++ decode(tail)

  val flatmapMatch = new C9:
    def decode[T](xs: List[(T, Int)]): List[T] = xs.flatMap { case (x, n) =>
      List.fill(n)(x)
    }

  val correct = Seq(
    tailrecImpl,
    basicRecImpl,
    flatmapMatch,
  )

/** Implémenter la fonction takeWhileStrictlyIncreasing qui, étant donné une
  * liste d’entiers, l, retourne la plus grande suite des éléments qui sont
  * strictement croissante.
  *
  * Quelques exemples d'utilisation :
  *   - {{{takeWhileStrictlyIncreasing(List(1, 8, 9, 9, 10, 2, 3)) //> List(1, 8, 9)}}}
  *   - {{{takeWhileStrictlyIncreasing(List(9, 10, 1, 2, 3, 11, 12, 13)) //> List(9, 10)}}}
  *   - {{{takeWhileStrictlyIncreasing(List()) //> List()}}}
  *
  * {{{def takeWhileStrictlyIncreasing(list: List[Int]): List[Int] = ???}}}
  */
object C10 extends Implementations[C10]:
  val basicImpl = new C10:
    def takeWhileStrictlyIncreasing(list: List[Int]): List[Int] = list match
      case x :: y :: xs if y > x => x :: takeWhileStrictlyIncreasing(y :: xs)
      case x :: _                => List(x)
      case Nil                   => Nil

  val correct = Seq(
    basicImpl,
  )

/** Ecrivez une fonction qui prend un vecteur et une liste d’entiers en
  * paramètre, et qui retourne un Vector[List[Int]], où chaque liste du vecteur
  * est la liste initiale multipliée par une des composantes du vecteur.
  */
object D1 extends Implementations[D1]:
  val doubleMap = new D1:
    def func(v: Vector[Int], l: List[Int]): Vector[List[Int]] =
      v.map(x => l.map(y => y * x))

  val correct = Seq(
    doubleMap,
  )

/** Utilisez la fonction reduceLeft pour calculer la valeur maximum d’une liste
  * d’entiers.
  */
object D2 extends Implementations[D2]:
  val simpleMax = new D2:
    def max(l: List[Int]): Int = l.reduceLeft((a, b) => if a > b then a else b)

  val correct = Seq(
    simpleMax,
  )

/** Evaluez 1.to(10).reduceLeft(_ * _). Qu’est-ce que vous obtenez ? Ecrire une
  * fonction qui calcule n! de cette façon.
  */
object D3 extends Implementations[D3]:
  val matchedFactorial = new D3:
    def func(n: Int): BigInt = n match
      case n if n < 0 =>
        throw new IllegalArgumentException("n must be positive")
      case 0 => BigInt(1)
      case 1 => BigInt(1)
      // noinspection SimplifiableFoldOrReduce - we explicitly want to use reduceLeft
      case _ => 1.to(n).map(BigInt(_)).reduceLeft(_ * _)

  val correct = Seq(
    matchedFactorial,
  )

/** Maintenant, on aimerait calculer 2**n avec la même astuce. Comment obtenir
  * une séquence de n copie d’un nombre ? Astuce : map. Quelle est votre
  * fonction qui calcule 2**n ?
  */
object D4 extends Implementations[D4]:
  val powerOfTwo = new D4:
    def baseTwoPower(n: Int): BigInt = n match
      case n if n < 0 =>
        throw new IllegalArgumentException("n must be positive")
      case 0 => BigInt(1)
      // noinspection SimplifiableFoldOrReduce - we explicitly want to use reduceLeft
      case _ => 1.to(n).map(_ => BigInt(2)).reduceLeft(_ * _)

  val bigintPow = new D4:
    def baseTwoPower(n: Int): BigInt = BigInt(2).pow(n)

  val correct = Seq(
    powerOfTwo,
    bigintPow,
  )

/** Etant donné une Seq[String], comment peut-on utiliser reduceLeft pour les
  * concaténer avec des espaces entre les deux ? Écrivez une fonction catSpace
  * qui fait cela. Par exemple, catSpace(Vector("I", "have", "a", "dream"))
  * deverait retourner la chaine ”I have a dream”.
  */
object D5 extends Implementations[D5]:
  val optionedCatSpace = new D5:
    def catSpace(xs: Seq[String]): String =
      xs.reduceLeftOption(_ + " " + _).getOrElse("")

  val correct = Seq(
    optionedCatSpace,
  )

/** En utilisant foldLeft (ou foldRight) écrivez une fonction reverse pour
  * inverser l’ordre des éléments d’une liste.
  */
object D6 extends Implementations[D6]:
  val left = new D6:
    def reverse[T](l: List[T]): List[T] =
      l.foldLeft(List.empty[T])((acc, x) => x :: acc)

  val right = new D6:
    def reverse[T](l: List[T]): List[T] =
      l.foldRight(List.empty[T])((x, acc) => acc :+ x)

  val correct = Seq(
    left,
    right,
  )

/** Étant donné une matrice représentée comme une liste de listes, écrivez une
  * fonction qui renvoie la première, ou une colonne arbitraire de la matrice.
  *
  * def firstColumn(xs: List[List[Int]]): List[Int] = ???
  *
  * def column(xs: * List[List[Int]], col: Int): List[Int] = ???
  */
object D7 extends Implementations[D7]:
  val basicImpl = new D7:
    def firstColumn(xs: List[List[Int]]): List[Int] =
      xs.map(_.head)
    def column(xs: List[List[Int]], col: Int): List[Int] =
      require(col >= 0)
      xs.map(_(col))

  val correct = Seq(
    basicImpl,
  )

/** Etant donné une matrice représentée comme une liste de listes, écrivez une
  * fonction qui renvoie la diagonal de la matrice.
  *
  * def diagonal(xs: List[List[Int]]): List[Int] = ???
  */
object D8 extends Implementations[D8]:
  val zipImpl = new D8:
    def diagonal(xs: List[List[Int]]): List[Int] =
      require(
        xs.map(_.length).distinct.length <= 1,
        "Matrix should be rectangular",
      )
      xs.zipWithIndex.collect { case (row, i) if i < row.length => row(i) }

  val correct = Seq(
    zipImpl,
  )

/** Étant donné une matrice représentée comme une liste de listes, écrivez une
  * fonction qui vérifie si la matrice a une ligne composée uniquement de zéros:
  * def hasZeroRow(matrix: List[List[Int]]): Boolean = ???
  */
object D9 extends Implementations[D9]:
  val basicImpl = new D9:
    def hasZeroRow(matrix: List[List[Int]]): Boolean =
      matrix.exists(_.forall(_ == 0))

  val correct = Seq(
    basicImpl,
  )

/** Ecrivez un test pour savoir si un nombre est premier. Un nombre n est
  * premier si les seuls diviseurs de n sont 1 et n lui-même. Restez simple,
  * l’efficacité n’est pas importante pour l’instant:
  *
  * def isPrime(x: Int): Boolean = ???
  */
object D10 extends Implementations[D10]:
  val basicImpl = new D10:
    def isPrime(x: Int): Boolean =
      require(x >= 0)
      x match
        case 0 | 1 => false
        case 2     => true
        case _ =>
          2.to(x / 2).forall(x % _ != 0)

  val correct = Seq(
    basicImpl,
  )

/** Étant donné la liste des chaînes, recherchez toutes les lignes plus longues
  * qu’une longueur minimale
  *
  * def linesLonger(lines: List[String], len: Int): List[String]= ???
  */
object D11 extends Implementations[D11]:
  val basicImpl = new D11:
    def linesLonger(lines: List[String], len: Int): List[String] =
      lines.filter(_.length > len)

  val correct = Seq(
    basicImpl,
  )

/** Étant donné la liste des chaînes, trouvez la longueur de la ligne la plus
  * longue. Écrivez cette fonction à l’aide de l’opérateur foldLeft. Pouvez-vous
  * également utiliser l’opérateur reduceLeft ?
  *
  * def longestLineLength(lines: List[String]): Int = ???
  */
object D12 extends Implementations[D12]:
  val withFoldLeft = new D12:
    def longestLineLength(lines: List[String]): Int =
      lines.foldLeft(0)(_ max _.length)

  val withReduceLeft = new D12:
    // noinspection SimplifiableFoldOrReduce - we explicitly want to use reduceLeft
    def longestLineLength(lines: List[String]): Int =
      lines.map(_.length).reduceLeft(_ max _)

  val correct = Seq(
    withFoldLeft,
    withReduceLeft,
  )

/** Étant donné la liste des chaînes, supprimez toutes les lignes vides.
  *
  * def elimEmptyLines(lines: List[String]): List[String] = ???
  */
object D13 extends Implementations[D13]:
  val basicImpl = new D13:
    def elimEmptyLines(lines: List[String]): List[String] =
      lines.filter(_.trim.nonEmpty)

  val correct = Seq(
    basicImpl,
  )

/** Étant donné la liste des chaînes, recherchez la ligne la plus longue (s’il y
  * en a plusieurs de même taille retourner la dernière).
  *
  * def longestLine(lines: List[String]): String = ???
  */
object D14 extends Implementations[D14]:
  val basicImpl = new D14:
    def longestLine(lines: List[String]): String =
      lines.reverse.maxBy(_.length)

  val correct = Seq(
    basicImpl,
  )

/** En utilisant la fonction foldRight, implémenter la fonction compress qui
  * permet d’éliminer les éléments successifs identiques dans une liste et de
  * retourner une nouvelle liste sans les répétition des éléments consécutifs
  * (voir l’exemple fourni).
  *
  * compress(List('a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e',
  * 'e', 'e')) //> res0: List[Char] = List('a', 'b', 'c', 'a', 'd', 'e')
  *
  * def compress[A](ls: List[A]): List[A] = ???
  */
object D15 extends Implementations[D15]:
  val basicImpl = new D15:
    def compress[A](ls: List[A]): List[A] =
      ls.foldRight(List.empty[A]) { (x, acc) =>
        acc match
          case Nil => List(x)
          case h :: t =>
            if x == h then acc
            else x :: acc
      }

  val correct = Seq(
    basicImpl,
  )

/** En utilisant la fonction foldLeft, implémenter la méthode averageOfDoubles
  * qui calcule la moyenne d’une liste non-vide de nombres réels. Attention :
  * Vous ne devez pas utiliser d’autres fonctions que foldLeft et la division.
  * Exemple d’application :
  *
  * averageOfDoubles(List(2.0, 2.5, 4.5) //res5: Double \= 3.0
  *
  * def averageOfDoubles(l: List[Double]): Double = ???
  */
object D16 extends Implementations[D16]:
  val basicImpl = new D16:
    def averageOfDoubles(l: List[Double]): Double =
      require(l.nonEmpty, "List should not be empty")
      val (sum, count) = l.foldLeft((0.0, 0)) { case ((s, c), x) =>
        (s + x, c + 1)
      }
      sum / count

  val correct = Seq(
    basicImpl,
  )
