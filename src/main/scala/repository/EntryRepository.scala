package repository

import java.sql.Timestamp

import cats.data.OptionT
import cats.{Applicative, Traverse}
import cats.effect.IO
import cats.implicits._
import doobie.free.connection.ConnectionIO
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.util.meta.Meta
import doobie.util.update.Update
import model._
import repository._

class EntryRepository(transactor: Transactor[IO]) {

  implicit val entryIdMeta: Meta[EntryId] = Meta[Long].xmap(EntryId, _.value)

  private case class EntryWithoutAttachment(id: EntryId, description: Option[String], amount: Double, date: Timestamp, accountId: AccountId)

  def createEntry(entry: CreateEntry): IO[Entry] = {
    val q = for {
      entryId <- sql"INSERT INTO entries (description, amount, date, account_id) VALUES (${entry.description}, ${entry.amount}, ${entry.date}, ${entry.accountId})".update.withUniqueGeneratedKeys[Long]("id")
      _       <- addAttachmentsToEntry(EntryId(entryId), entry.attachments)
    } yield Entry(EntryId(entryId), entry.accountId, entry.description, entry.amount, entry.date, entry.attachments)

    q.transact(transactor)
  }

  private def addAttachmentsToEntry(entryId: EntryId, attachmentUrls: List[String]): ConnectionIO[List[Long]] = {
    attachmentUrls.map { url => sql"INSERT INTO attachments (url, entry_id) VALUES (${url}, ${entryId})".update.withUniqueGeneratedKeys[Long]("id") }.sequence
  }

  private def getAttachmentsForEntry(entryId: EntryId): ConnectionIO[List[String]] = {
    sql"SELECT url FROM attachments WHERE entry_id = ${entryId}".query[String].to[List]
  }

  def getEntriesForAccount(id: AccountId): ConnectionIO[List[Entry]] = {
    val q = for {
      entriesWithoutAttachments <- sql"SELECT id, description, amount, date, account_id FROM enries WHERE account_id = ${id}".query[EntryWithoutAttachment].to[List]
      entries <- entriesWithoutAttachments.map { e => getAttachmentsForEntry(e.id).map { attachments => Entry(e.id, e.accountId, e.description, e.amount, e.date, attachments) } }
    } yield entries

    q.transact(transactor)
  }

  def getEntry(id: Long): IO[Option[Entry]] = {

    val q = for {
      entryWithoutAttachments <- OptionT(sql"SELECT id, description, amount, date, account_id FROM entries WHERE id = ${id}".query[EntryWithoutAttachment].option)
      attachments             <- OptionT(getAttachmentsForEntry(entryWithoutAttachments.id).map(Some(_)))
    } yield Entry(entryWithoutAttachments.id, entryWithoutAttachments.accountId, entryWithoutAttachments.description, entryWithoutAttachments.amount, entryWithoutAttachments.date, attachments)

    q.value.transact(transactor)
  }
}
