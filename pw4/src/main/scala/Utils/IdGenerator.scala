package Utils

/** A trait for generating unique IDs.
  * @tparam T
  *   the type of the ID
  */
trait IdGenerator[+T] {
  def nextId(): T
  def reset(): Unit
}

/** A simple implementation of IdGenerator that generates Long IDs.
  */
class LongIdGenerator(seed: Long = 0L) extends IdGenerator[Long] {
  private var currentId: Long = seed

  override def nextId(): Long = {
    synchronized {
      val id = currentId
      currentId += 1
      id
    }
  }

  override def reset(): Unit = {
    currentId = 0L
  }
}
