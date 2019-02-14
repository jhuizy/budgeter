package service

import cats.effect.IO
import cats.data.OptionT
import cats.implicits._
import org.http4s.{HttpService, Response}
import org.http4s.dsl.io._
import org.http4s.circe._
import io.circe.generic.auto._
import io.circe.syntax._
import model.{Account, AccountId}
import org.http4s.dsl.impl.Root
import repository.{AccountRepository, EntryRepository}

class AccountService(accountRepository: AccountRepository, entryRepository: EntryRepository) {

  private case class CreateAccountRequest(name: String, description: Option[String])
  private case class GetAccountResponse(id: Int, name: String, description: Option[String])

  val service = HttpService[IO] {
    case req @ POST -> Root / "accounts" =>
      for {
        r <- req.decodeJson[CreateAccountRequest]
        account <- accountRepository.createAccount(r.name, r.description)
        response <- Created(GetAccountResponse(account.id.id.toInt, account.name, account.description).asJson)
      } yield response
    case GET -> Root / "accounts" =>
      for {
        accounts <- accountRepository.listAccounts
        response <- Ok(accounts.map { acc => GetAccountResponse(acc.id.id.toInt, acc.name, acc.description) }.asJson)
      } yield response
    case GET -> Root / "accounts" / IntVar(id) =>
      for {
        account <- accountRepository.getAccountById(AccountId(id.toLong))
        response <- account.map { a => Ok(a.asJson) }.getOrElse(NotFound())
      } yield response
  }

}
