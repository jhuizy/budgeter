import java.sql.Timestamp

package object model {
  abstract sealed class Importance(val value: String)
  case object High extends Importance("high")
  case object Medium extends Importance("medium")
  case object Low extends Importance("low")

  object Importance {
    private def values = Set(High, Medium, Low)

    def unsafeFromString(value: String): Importance = {
      values.find(_.value == value).get
    }
  }

  case class Todo(id: Option[Long], description: String, importance: Importance)

  case object TodoNotFoundError

  case class AccountId(value: Long)
  case class Account(id: AccountId, name: String, description: Option[String])

  case class EntryId(value: Long)
  case class CreateEntry(accountId: AccountId, description: Option[String], amount: Double, date: Timestamp, attachments: List[String])
  case class Entry(id: EntryId, accountId: AccountId, description: Option[String], amount: Double, date: Timestamp, attachments: List[String])

}
