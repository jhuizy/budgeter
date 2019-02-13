package repository

import java.sql.Timestamp

import cats.{Applicative, Traverse}
import cats.effect.IO
import cats.implicits._
import doobie.free.connection.ConnectionIO
import doobie.util.transactor.Transactor
import doobie.implicits._
import doobie.util.meta.Meta
import doobie.util.update.Update
import model._

class EntryRepository(transactor: Transactor[IO]) {

  implicit val entryIdMeta: Meta[EntryId] = Meta[Long].xmap(EntryId, _.id)

  def createEntry(entry: CreateEntry): IO[Entry] = {
    val q = for {
      entryId <- sql"INSERT INTO entries (description, amount, date, account_id) VALUES (${entry.description}, ${entry.amount}, ${entry.date}, ${entry.accountId})".update.withUniqueGeneratedKeys[Long]("id")
      _       <- addAttachmentsToEntry(EntryId(entryId), entry.attachments)
    } yield Entry(EntryId(entryId), entry.accountId, entry.description, entry.amount, entry.date, entry.attachments)

    q.transact(transactor)
  }

  private def addAttachmentsToEntry(entryId: EntryId, attachmentUrls: List[String]): ConnectionIO[List[AttachmentId]] = {
    attachmentUrls.map { url => sql"INSERT INTO attachments (url, entry_id) VALUES (${url}, ${entryId})".update.withUniqueGeneratedKeys[Long]("id").map(AttachmentId) }.sequence
  }

  def getEntry(id: EntryId): IO[Option[Entry]] = {
    val q = for {
      entry <- sql"SELECT description, amount, date, account_id FROM entries WHERE id = ${id}".query[(Long, Option[String], Double, Timestamp, Long)].option
      attachments <- sql"SELECT url FROM attachments WHERE entry_id = ${id}".query[String].to[List]
    } yield entry.map { e => Entry(EntryId(e._1), AccountId(e._5), e._2, e._3, e._4, attachments) }

    q.transact(transactor)
  }
}
