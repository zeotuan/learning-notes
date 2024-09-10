package sandbox.C4_Monad


/**
 * Reader Monad is a monad that allows us to sequence operations that depend on some input.
 * instances of Reader are functions that take an input and compute a result providing us with a way to compose them
 * as well as a form of dependency injection
 *
 * readers are most useful in situation where:
 * - we are constructing a program that can easily be represented by a function
 * - injection of dependencies need to be deferred
 * - we need to test part of program in isolation (this is general reason to use dependency injection)
 * */
object ReaderMonad {
  import cats.data.Reader
  import cats.syntax.applicative._
  final case class Cat(name: String, favoriteFood: String)
  val getCatName: Reader[Cat, String] = Reader(cat => cat.name)
  val catName = getCatName.run(Cat("Garfield", "lasagne")) //

  val greetCatName = getCatName.map(name => s"Hello $name")
  val gCatName = greetCatName.run(Cat("Heathcliff", "junk food"))

  val feedCat: Reader[Cat, String] = Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] = for {
    greet <- greetCatName
    feed <- feedCat
  } yield s"$greet. $feed"

  val greetAndFeed1 = greetAndFeed.run(Cat("Heathcliff", "junk food"))


  final case class DB(usernames: Map[Int, String], passwords: Map[String, String])
  type DbReader[A] = Reader[DB, A]

  def findUsername(userId: Int): DbReader[Option[String]] = Reader(oS => oS.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] = Reader(oS => oS.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = for {
    username <- findUsername(userId)
    passwordOk <- username.map(un => checkPassword(un, password)).getOrElse(false.pure[DbReader])
  } yield passwordOk


  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val userDb = DB(users, passwords)

  checkLogin(1, "date").run(userDb)
  checkLogin(4, "test").run(userDb)

}

