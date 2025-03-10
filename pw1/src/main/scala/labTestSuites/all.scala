package labTestSuites

import implementations._

/** This sequence allows the assistant to retrieve and centralize all the
  * implementations of all the groups while correcting.
  */
val allImplementations = Seq(
  herman_holzer_stefani.all,
)

def getImplementations(): Seq[Exercice] =
  allImplementations.flatten.flatMap(impls => impls.correct)
