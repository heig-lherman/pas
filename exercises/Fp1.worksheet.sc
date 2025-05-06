import scala.annotation.tailrec

/** 1. Ecrire une fonction qui prend un LazyList en paramètre et le transforme en une List. On vous demande de ne pas
 * utiliser la méthode LazyList.toList.
 */
def toList[A](s: LazyList[A]): List[A] = s ++: Nil

/** 2. Ecrire une fonction qui prend un LazyList en paramètre et retourne le LazyList contenant seulement les n premiers
 * éléments. On vous demande de ne pas utiliser la méthode LazyList.take.
 */
def take[A](s: LazyList[A], n: Int): LazyList[A] = s match {
  case LazyList() => LazyList.empty
  case _ if n <= 0 => LazyList.empty
  case x #:: xs => x #:: take(xs, n - 1)
}

/** 3. Ecrire une fonction forAll qui prend un LazyList et un prédicat en paramètre et qui détermine si
 * tous les éléments de LazyList satisfont le prédicat. Attention : on vous demande de ne pas utiliser
 * la méthode && ni la méthode forAll. */
@tailrec
def forAll[A](l: LazyList[A])(p: A => Boolean): Boolean = l match {
  case LazyList() => true
  case x #:: xs => p(x) && forAll(xs)(p)
}

/** 4. En utilisant les LazyList, écrire une fonction FirstNFibs qui prend un nombre entier n en paramètre et qui
 * retourne les n premiers nombres de Fibonacci. */
def firstNFibs(n: Int): List[Int] = {
  def fib(a: Int, b: Int): LazyList[Int] = a #:: fib(b, a + b)

  fib(0, 1).take(n).toList
}

/** 5. (a) Compléter la fonction def createAbLazyList: LazyList[String] qui retourne une LazyList infinie des
 * permutations de 'a' et 'b', y compris la String vide ''. */
def createAbLazyList: LazyList[String] =
  "" #:: (for
    s <- createAbLazyList
    c <- List("a", "b")
  yield s + c)

/** 5. (b) Ecrire la fonction def nPalindroms(n: Int, s: LazyList[String]): List[String] permettant de retourner une
 * liste des n premiers palindromes à partir d’une LazyList infinie de la forme présentée plus haut. */
def nPalindroms(n: Int, s: LazyList[String]): List[String] = s.filter(x => x == x.reverse).take(n).toList

/** 6. On appelle suite de Syracuse une suite d’entiers naturels définie de la manière suivante : on part
 * d’un nombre entier plus grand que zéro ; sil est pair, on le divise par 2 ; sil est impair, on le multiplie
 * par 3 et on ajoute 1. En répétant lopération, on obtient une suite d’entiers positifs dont chacun ne
 * dépend que de son prédécesseur.
 *
 * Par exemple, à partir de 14, on construit la suite des nombres : 14, 7, 22, 11, 34, 17, 52, 26, 13, 40,
 * 20, 10, 5, 16, 8, 4, 2, 1, 4, 2, ...
 *
 * Implémenter la méthode syracusN qui génére un LazyList infinie correspondant à la suite syracus
 * qui par le nombre n. */
def syracusN(n: Int): LazyList[Int] =
  n #:: (if n % 2 == 0 then syracusN(n / 2) else syracusN(3 * n + 1))
