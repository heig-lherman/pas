package Data

import scala.collection.mutable

trait AccountService:
  /** Retrieve the balance of a given account
    * @param user
    *   the name of the user whose account will be retrieved
    * @return
    *   the current balance of the user
    */
  def getAccountBalance(user: String): Double

  /** Add an account to the existing accounts
    * @param user
    *   the name of the user
    * @param balance
    *   the initial balance value
    */
  def addAccount(user: String, balance: Double): Unit

  /** Indicate is an account exist
    * @param user
    *   the name of the user whose account is checked to exist
    * @return
    *   whether the account exists or not
    */
  def isAccountExisting(user: String): Boolean

  /** Update an account by decreasing its balance.
    * @param user
    *   the name of the user whose account will be updated
    * @param amount
    *   the amount to decrease
    * @return
    *   the new balance if the purchase succeeded or None otherwise
    */
  def purchase(user: String, amount: Double): Option[Double]

  /** Set the current user, creating its account if it doesn't exist and setting
    * the current user for the session.
    *
    * @param user
    *   the new current user
    * @param session
    *   the session on which the new user will be set
    */
  def setCurrent(user: String, session: Session): Unit

class AccountImpl extends AccountService:
  private val accounts: mutable.Map[String, Double] = mutable.Map.empty

  override def getAccountBalance(user: String): Double =
    accounts.getOrElse(user, 0.0)

  override def addAccount(user: String, balance: Double): Unit =
    require(balance >= 0, "Balance must be positive")
    accounts += user -> balance

  override def isAccountExisting(user: String): Boolean =
    accounts.contains(user)

  override def purchase(user: String, amount: Double): Option[Double] =
    val currentBalance = getAccountBalance(user)
    if currentBalance >= amount then
      val newBalance = currentBalance - amount
      accounts += user -> newBalance
      Some(newBalance)
    else None

  override def setCurrent(user: String, session: Session): Unit =
    if !isAccountExisting(user) then addAccount(user, 30)
    session.setCurrentUser(user)
end AccountImpl
