package Data

import munit.FunSuite

class AccountImplTest extends FunSuite {

  def createTestSession(sid: String = "test-session"): Session =
    new Session(sid)

  test("getAccountBalance should return 0.0 for non-existing accounts") {
    val service = new AccountImpl()
    assertEquals(service.getAccountBalance("non-existing-user"), 0.0)
  }

  test(
    "getAccountBalance should return correct balance for existing accounts",
  ) {
    val service = new AccountImpl()
    service.addAccount("test-user", 100.0)
    assertEquals(service.getAccountBalance("test-user"), 100.0)
  }

  test("addAccount should create a new account with the specified balance") {
    val service = new AccountImpl()
    service.addAccount("new-user", 50.0)
    assertEquals(service.getAccountBalance("new-user"), 50.0)
    assert(service.isAccountExisting("new-user"))
  }

  test("addAccount should update balance if account already exists") {
    val service = new AccountImpl()
    service.addAccount("existing-user", 50.0)
    service.addAccount("existing-user", 75.0)
    assertEquals(service.getAccountBalance("existing-user"), 75.0)
  }

  test("addAccount should refuse adding negative balance") {
    val service = new AccountImpl()

    intercept[IllegalArgumentException] {
      service.addAccount("negative-balance", -10.0)
    }
  }

  test("isAccountExisting should return true for existing accounts") {
    val service = new AccountImpl()
    service.addAccount("existing-user", 50.0)
    assert(service.isAccountExisting("existing-user"))
  }

  test("isAccountExisting should return false for non-existing accounts") {
    val service = new AccountImpl()
    assert(!service.isAccountExisting("non-existing-user"))
  }

  test(
    "purchase should decrease balance and return new balance when sufficient funds",
  ) {
    val service = new AccountImpl()
    service.addAccount("rich-user", 100.0)

    val result = service.purchase("rich-user", 30.0)
    assert(result.isDefined)
    assertEquals(result.get, 70.0)
    assertEquals(service.getAccountBalance("rich-user"), 70.0)
  }

  test("purchase should work with exact balance amount") {
    val service = new AccountImpl()
    service.addAccount("exact-user", 50.0)

    val result = service.purchase("exact-user", 50.0)
    assert(result.isDefined)
    assertEquals(result.get, 0.0)
    assertEquals(service.getAccountBalance("exact-user"), 0.0)
  }

  test("purchase should return None when insufficient funds") {
    val service = new AccountImpl()
    service.addAccount("poor-user", 10.0)

    val result = service.purchase("poor-user", 20.0)
    assert(result.isEmpty)
    // Balance should remain unchanged
    assertEquals(service.getAccountBalance("poor-user"), 10.0)
  }

  test("purchase should return None for non-existing accounts") {
    val service = new AccountImpl()
    val result = service.purchase("non-existing-user", 10.0)
    assert(result.isEmpty)
  }

  test("setCurrent should set user in session for existing accounts") {
    val service = new AccountImpl()
    service.addAccount("existing-user", 50.0)
    val session = createTestSession()

    service.setCurrent("existing-user", session)
    assertEquals(session.getCurrentUser, Some("existing-user"))
    // Balance should remain unchanged
    assertEquals(service.getAccountBalance("existing-user"), 50.0)
  }

  test(
    "setCurrent should create account with default balance for non-existing accounts",
  ) {
    val service = new AccountImpl()
    val session = createTestSession()

    service.setCurrent("new-user", session)

    // Should create account with balance 30
    assert(service.isAccountExisting("new-user"))
    assertEquals(service.getAccountBalance("new-user"), 30.0)
    assertEquals(session.getCurrentUser, Some("new-user"))
  }

  test("multiple accounts can exist simultaneously") {
    val service = new AccountImpl()
    service.addAccount("user1", 100.0)
    service.addAccount("user2", 200.0)
    service.addAccount("user3", 300.0)

    assertEquals(service.getAccountBalance("user1"), 100.0)
    assertEquals(service.getAccountBalance("user2"), 200.0)
    assertEquals(service.getAccountBalance("user3"), 300.0)

    // Purchases should only affect the specified account
    service.purchase("user2", 50.0)

    assertEquals(service.getAccountBalance("user1"), 100.0)
    assertEquals(service.getAccountBalance("user2"), 150.0)
    assertEquals(service.getAccountBalance("user3"), 300.0)
  }

  test("account operations maintain consistency through multiple operations") {
    val service = new AccountImpl()
    service.addAccount("sequential-user", 100.0)

    // Sequential purchases
    val result1 = service.purchase("sequential-user", 20.0)
    val result2 = service.purchase("sequential-user", 30.0)
    val result3 = service.purchase("sequential-user", 60.0)

    assert(result1.isDefined)
    assert(result2.isDefined)
    assert(result3.isEmpty) // Should fail due to insufficient funds

    assertEquals(
      service.getAccountBalance("sequential-user"),
      50.0,
    ) // 100 - 20 - 30
  }
}
