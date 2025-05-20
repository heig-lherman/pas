## Choix architecturaux

Les changements architecturaux ont été réfléchi en amont et de sorte que les changements nécessaires
dans la base de code existante soient minimes, en essayant de restreindre les changements aux
composants internes des services.

### Propriétés de production

Le service `ProductService` a été revu pour incorporer dans la map des produits disponibles des
propriétés de production (les paramètres de la méthode `randomSchedule`).

Une méthode `produce` qui retourne un `Future[Double]` a été ajoutée à la classe `ProductService`.
Cette méthode permet d'obtenir un future qui se complètera après son temps de production.
La valeur de retour correspond au prix du produit, pour s'aligner avec la méthode synchrone. En cas
d'échec de la production, le future échouera avec une exception `Product.ProductionException`.

### Gestion des commandes

Un nouveau service `OrderService` a été créé pour gérer les commandes, il prend dans sa seule
méthode `placeOrder` une liste de `Product` et retourne un `Future[Either[Order, Order]]`.

Si tous les produits ont pu être produits, le service retournera un `Future[Right[Order]]`
contenant le prix total et la liste des produits de taille égale à celle demandée.
Si la totalité des produits n'a pas pu être produite, le service retournera un `Future.failed`
contenant une exception `Order.ProductionFailureException`.
Si certains produits n'ont pas pu être produits, le service retournera un `Future[Left[Order]]`
contenant le total partiel et la liste partielle des produits produits.

L'utilisation de `Either` permet d'avoir une façon simple et fonctionnelle de gérer le cas où le
retour serait incomplet. Pour rappel, Scala considère une valeur droite comme étant un succès.

```scala
object OrderService:
  //              qty  brand      product
  type Product = (Int, BrandName, ProductName)

  //            total   successful products
  type Order = (Double, List[Product])
```

### Intégration dans le système actuel

Pour éviter de devoir restructurer l'entièreté du système, tout en gardant les responsabilités
séparées, l'API offerte par le `OrderService` devrait pouvoir être facilement intégrée dans le
`AnalyzerService`.

Nous allons ajouter un trait `OrderHandling` qui contiendra la méthode `handleOrder` reprenant la
gestion des commandes telle qu'implémentée jusqu'ici dans un objet `OrderHandling.Synchronous`.
Une classe supplémentaire `OrderHandling.Asynchronous` sera ajoutée pour gérer les commandes de
manière asynchrone en utilisant le `OrderService` et un callback pour gérer les résultats.

Pour simplifier la gestion des services, nous pouvons utiliser les paramètres contextuels pour
injecter les deux services qui seront nécessaires dans l'implémentation de `OrderHandling`.

La nouvelle dépendance de `AnalyzerService` sur `OrderService` demandera de modifier tous les
fichiers `Main*.scala` pour instancier et injecter le service dans le `AnalyzerService`.

Nous avons également effectué quelques refactors pour centraliser la gestion du formattage des
différents éléments concernant les commandes dans un object companion au service `OrderService`.
