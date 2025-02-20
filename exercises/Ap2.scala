import scala.annotation.tailrec

/** 
 * 10. Boucle I
 * Implémentez une boucle while qui n’affiche que les nombres impairs et multiple de 3 ou de 5, compris entre 1 et 100.
 */
@main def p10a() = 
    var i = 0
    while i <= 100 do
        if i % 2 != 0 && (i % 3 == 0 || i % 5 == 0) then
            println(i)
        i += 1

@main def p10b() =
    for i <- 1 to 100 if i % 2 != 0 && (i % 3 == 0 || i % 5 == 0) do println(i)

/**
 * 11. Fonction I
 * Implémentez une fonction, prenant deux Int en paramètre, qui affiche le plus grand des deux paramètres, en une seule ligne de code.
 */
@main def p11() =
    def fn(x: Int, y: Int) = if x > y then x else y

    println(fn(10, 20))
    println(fn(20, 10))

/**
 * 12. Fonction II
 * Implémentez une fonction qui prend un String en paramètre, si ce paramètre vaut
 *   — "Hello", la fonction affiche "World".
 *   — "World", la fonction affiche "Hello".
 *   — Sinon, la fonction affiche "Goodbye".
 * Sans utiliser de conditions booléennes.
 */
@main def p12() =
    def fn(a: String) = println(
        a match {
            case "Hello" => "World"
            case "World" => "Hello"
            case _ => "Goodbye"
        }
    )

    fn("Hello")
    fn("World")
    fn("Something else")
        
/**
 * 13. Boucle II
 * Implémentez la boucle ci-dessous, en une seule boucle for Scala.
 * for (int i = 0; i < 3; ++i) {
 *   for (int j = 0; j <= 3; ++j) {
 *     for (int k = 0; k < 4; ++k) {
 *       System.out.println("Hello World !");
 *     }
 *   }
 * }
 */
@main def p13() =
    for i <- 0 to 2; j <- 0 to 3; k <- 0 to 3 do println("Hello World !")

/*
 * 14. Match cases I
 * Implémentez un match cases sur un String, qui vérifie dans l’ordre :
 *   — Si la longueur du String est impaire, affiche la longueur.
 *   — Si le String est égal à "Hello World!", l’affiche.
 *   — Sinon, affiche "Goodbye".
 */
@main def p14() =
    def m(s: String) = s match {
        case a if a.size % 2 != 0 => println(a.size)
        case "Hello World!" => println(s)
        case _ => println("Goodbye")
    }

    m("abc")
    m("ab")
    m("Hello World!")

/* 
 * 15. Boucle III & Match cases II
 * A l’aide d’une variable mutable compteur valant 0, implémentez une boucle while ayant pour
 * condition, tant que le compteur est plus petit que 10. Sans utiliser de conditions booléennes,
 * incrémentez le compteur comme suit :
 *   — Si le compteur est pair, on l’incrémente de 3.
 *   — Si le compteur est impair, on l’incrémente de 1.
 * Combien de fois la boucle est-elle exécutée ? Quelle est la valeur du compteur après ses exécutions ?
 */
@main def p15() =
    var c = 0
    var count = 0
    while c < 10 do 
        count += 1
        c % 2 match {
            case 0 => c += 3
            case _ => c += 1
        }

    println(count)
    println(c)

/*
 * 16. Fonctions III & Match cases III
 * Implémentez une fonction qui prend un Int en paramètre.
 *   — Si x est plus grand que 100, retourne la valeur de x.
 *   — Si x est un multiple de 7, appelle la fonction avec x + 8.
 *   — Si x est impair, appelle la fonction avec x + 12.
 *   — Sinon, appelle la fonction avec x + 1.
 * Appelez cette fonction avec la variable immuable x ayant comme valeur 0.
 *   — Quelle est la valeur finale retournée par la fonction ?
 *   — Est-ce la nouvelle valeur de x ?
 *   — Combien de fois la fonction est-elle exécutée ?
 *   — Pouvons-nous appeler la fonction à l’intérieur de celle-ci sans explicitement
 *     retourner une valeur de type Int
 */
@main def p16() =
    var count = 0
    def fn(x: Int): Int = 
        count += 1
        x match {
            case a if x > 100 => x
            case a if a % 7 == 100 => fn(a + 8)
            case a if a % 2 != 0 => fn(a + 12)
            case _ => fn(x +1)
        }

    val x = 0
    val res = fn(x)
    println(s"1. ${res}")
    println(s"2. ${x == res}")
    println(s"3. ${count}")
    // 4. Non

/*
 * 17. Fonction IV
 * Implémentez une fonction qui prends trois Int (x, y, z) en paramètre.
 *   — Si z est pair, retournez la somme des carrés de x et y.
 *   — Si z est impair, retourner le carré de la somme de x et de y.
 *   — Si z vaut 0, retournez la somme des cas z pair et z impair.
 * Appelez la fonction avec x = 2, y = 3, et z = 1, puis 2, puis 0.
 *   — Est-ce que le cas z = 0 est correct ? Pourquoi ?
 *   — S’il n’est pas correct, modifiez le code pour obtenir le bon résultat.
 */
@main def p17() =
    def fn(x: Int, y: Int, z: Int): Int = z match {
        case 0 => fn(x, y, 1) + fn(x, y, 2)
        case a if a % 2 == 0 => x * x + y * y
        case a if a % 2 != 0 => (x + y) * (x + y)
    }

    println(fn(2, 3, 1))
    println(fn(2, 3, 2))
    println(fn(2, 3, 0))

/* 
 * 18. Fonction V
 * Implémentez deux versions d’une fonction qui prends deux Int (x et y) en paramètre, une fois avec
 * des conditions booléennes et une fois avec des match cases.
 *   — Si x == y, retourne x.
 *   — Si x < y, retourne y.
 *   — Si x > y, alors :
 *      — Si x est impair, retourne 2x + 3y.
 *      — Si y est impair et x est pair, retourne 4x - 7y.
 *      — Si x est un multiple de 3 et y un multiple de 4, retourne x2 + y3
 *   — Si x < y et x > 4, retourne 2x.
 *   — Si x == y et y == 0, retourne 42.
 *   — Dans tous les autres cas, retourne 9
 * Faites en sorte que toutes les conditions soient vérifiables.
 * Hint : il faut faire attention à l’ordre.
 */
@main def p18() =
    def fn1(x: Int, y: Int) = 
        if x == y then
            if y == 0 then 42
            else x
        else if x < y then
            if x > 4 then 2 * x
            else y
        else
            if x % 2 != 0 then 2 * x + 3 * y
            else if y % 2 != 0 && x % 2 == 0 then 4 * x - 7 * y
            else if x % 3 == 0 && y % 4 == 0 then x * x + y * y
            else 9
    
    def fn2(x: Int, y: Int) = (x, y) match {
        case (x, y) if x == y => y match {
            case 0 => 42
            case _ => x
        }
        case (x, y) if x < y => x match {
            case x if x > 4 => 2 * x
            case _ => y
        }
        case _ => (x, y) match {
            case (x, y) if x % 2 != 0 => 2 * x + 3 * y
            case (x, y) if y % 2 != 0 && x % 2 == 0 => 4 * x - 7 * y
            case (x, y) if x % 3 == 0 && y % 4 == 0 => x * x + y * y
            case _ => 9
        }
    }

    // x == y cases
    println(s"fn1(0, 0) = ${fn1(0, 0)}") // -> 42
    println(s"fn2(0, 0) = ${fn2(0, 0)}") // -> 42
    println(s"fn1(5, 5) = ${fn1(5, 5)}") // -> 5
    println(s"fn2(5, 5) = ${fn2(5, 5)}") // -> 5
    println()

    // x < y cases
    println(s"fn1(3, 7) = ${fn1(3, 7)}") // -> 7 (x <= 4)
    println(s"fn2(3, 7) = ${fn2(3, 7)}") // -> 7 (x <= 4)
    println(s"fn1(6, 10) = ${fn1(6, 10)}") // -> 12 (x > 4)
    println(s"fn2(6, 10) = ${fn2(6, 10)}") // -> 12 (x > 4)
    println()

    // x > y with odd x cases
    println(s"fn1(7, 4) = ${fn1(7, 4)}") // -> 2*7 + 3*4
    println(s"fn2(7, 4) = ${fn2(7, 4)}") // -> 2*7 + 3*4
    println()

    // x > y with even x and odd y cases
    println(s"fn1(8, 5) = ${fn1(8, 5)}") // -> 4*8 - 7*5
    println(s"fn2(8, 5) = ${fn2(8, 5)}") // -> 4*8 - 7*5
    println()

    // x > y with x divisible by 3 and y divisible by 4 cases
    println(s"fn1(6, 4) = ${fn1(6, 4)}") // -> 6*6 + 4*4
    println(s"fn2(6, 4) = ${fn2(6, 4)}") // -> 6*6 + 4*4
    println()

    // default case
    println(s"fn1(4, 2) = ${fn1(4, 2)}") // -> 9
    println(s"fn2(4, 2) = ${fn2(4, 2)}") // -> 9
    println()

/* 
 * 19. Fonction VI
 * Implémentez une fonction qui retourne la somme des n premiers nombres de Fibonacci.
 * Appelez cette fonction avec n = 20.
 */
@main def p19() =
    def fn(n: Int): Long =
        lazy val fib: Stream[Int] = 0 #:: 1 #:: fib.zip(fib.tail).map(_ + _)
        return fib.take(n).sum

    def fnrec(n: Int) =
        @tailrec
        def seq(n: Int, a: Long = 0, b: Long = 1, acc: Long = 0): Long = n match {
            case 0 => acc
            case _ => seq(n - 1, b, a + b, acc + a)
        }

        seq(n)

    println(fn(20))
    println(fnrec(20))
