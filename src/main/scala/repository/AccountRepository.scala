package repository

import cats.data.NonEmptyList
import cats.effect.IO
import doobie.util.transactor.Transactor
import model.{Account, AccountId}
import doobie._
import doobie.implicits._

class AccountRepository(transactor: Transactor[IO]) {

  implicit val accountIdMeta: Meta[AccountId] = Meta[Long].xmap(AccountId, _.id)

  def createAccount(name: String, description: Option[String]): IO[Account] = {
    sql"INSERT INTO accounts (name, description) VALUES (${name}, ${description})"
      .update
      .withUniqueGeneratedKeys[Long]("id")
      .transact(transactor)
      .map { id => Account(id = AccountId(id), name, description) }
  }

  def listAccounts: IO[List[Account]] = {
    sql"SELECT (id, name, description) FROM accounts"
      .query[Account]
      .to[List]
      .transact(transactor)
  }

  def getAccountById(id: AccountId): IO[Option[Account]] = {
    sql"SELECT (id, name, description) FROM accounts WHERE id = ${id}"
      .query[Account]
      .option
      .transact(transactor)
  }

}
