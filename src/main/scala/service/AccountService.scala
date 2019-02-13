package service

import cats.effect.IO
import org.http4s.HttpService
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import model.{Account, AccountId}
import org.http4s.dsl.impl.Root
import repository.{AccountRepository, EntryRepository}

class AccountService(accountRepository: AccountRepository, entryRepository: EntryRepository) {

  private case class CreateAccountRequest(name: String, description: Option[String])
  private case class CreateAccountResponse(id: Long, name: String, description: Option[String])

  val service = HttpService[IO] {
    case req @ POST -> Root / "accounts" =>
      for {
        r <- req.decodeJson[CreateAccountRequest]
        _ <- accountRepository.createAccount(Account(None, r.name, r.description))
        response <- Created(r.asJson)
      } yield response

    case GET -> Root / "accounts" / LongVar(id) =>
      for {
        accountOption <- accountRepository.getAccountById(AccountId(id))
        response <- accountOption match {
          case Some(account) => Ok(account.asJson)
          case None => NotFound
        }
      } yield response
  }

}
