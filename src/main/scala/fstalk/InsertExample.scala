package fstalk

import zio.schema.DeriveSchema

import java.util.UUID
import zio.schema.meta._
import zio.schema._
import zio._
import zio.schema.meta.Migration._
import zio.schema.StandardType._
import java.time._

/** insertInto(users)(id, email, validated) .values(
  * ("deb68414-8575-11ee-b9d1-0242ac120002", "someone@gmail.com", true),
  * ("e1f5598e-8575-11ee-b9d1-0242ac120002", "someoneelse@gmail.com", false) )
  *
  * INSERT INTO users (id, email, validated) VALUES
  * ("deb68414-8575-11ee-b9d1-0242ac120002", "someone@gmail.com", true),
  * ("e1f5598e-8575-11ee-b9d1-0242ac120002", "someoneelse@gmail.com", false);
  */
// + type safe derivation?
object InsertExample {

  sealed trait ColumnType[A]
  object ColumnType {
    implicit case object StringType extends ColumnType[String]
    implicit case object IntType extends ColumnType[Int]
    implicit case object UUIDType extends ColumnType[UUID]
    implicit case object BooleanType extends ColumnType[Boolean]
    implicit case object LocalDateType extends ColumnType[LocalDate]
  }

  sealed trait Column[KeyValue] { self =>
    type Key <: Singleton with String
    type Value

    def columnName: Key

    def paramType: ColumnType[Value]
  }

  object Column {
    private def apply[N <: Singleton with String, T](
        name0: N
    )(implicit paramType0: ColumnType[T]): Column[(N, T)] =
      new Column[(N, T)] {
        type Key = N
        type Value = T
        def columnName: N = name0
        def paramType: ColumnType[T] = paramType0
      }

    def int[N <: Singleton with String](name0: N): Column[(N, Int)] =
      Column[N, Int](name0)
    def string[N <: Singleton with String](name0: N): Column[(N, String)] =
      Column[N, String](name0)
    def uuid[N <: Singleton with String](name0: N): Column[(N, UUID)] =
      Column[N, UUID](name0)
    def boolean[N <: Singleton with String](name0: N): Column[(N, Boolean)] =
      Column[N, Boolean](name0)
    def localDate[N <: Singleton with String](
        name0: N
    ): Column[(N, LocalDate)] = Column[N, LocalDate](name0)

    implicit def columnToColumnSet[N <: Singleton with String, T: ColumnType](
        c: Column[(N, T)]
    ): ColumnSet[(N, T)] =
      new ColumnSet(Chunk.empty) ++ c
  }

  final case class ColumnSet[+Columns](
      private val chunk: Chunk[Column[_]]
  ) { self =>
    def ++[C](
        column: Column[C]
    ): ColumnSet[Columns with C] =
      new ColumnSet(chunk = chunk :+ column)

    def table[TableName0 <: Singleton with String](tableName: TableName0) =
      new Table(tableName, self)
  }

  final case class Table[TableName <: Singleton with String, +Columns](
      tableName: TableName,
      columns: ColumnSet[Columns]
  )

  def insertInto[TableName <: Singleton with String, Columns](
      table: Table[TableName, Columns]
  )(
      columns: ColumnSet[Columns]
  ) = InsertBuilder(table, columns)

  final case class InsertBuilder[TableName <: Singleton with String, Columns](
      table: Table[TableName, Columns],
      sources: ColumnSet[Columns]
  ) {

    def values[A](values: List[A])(implicit
        schema: Schema[A] // Schema[User]
    ): Insert[TableName, Columns, A] =
      Insert(table, sources, values)
  }

  sealed case class Insert[TableType <: Singleton with String, Columns, A](
      table: Table[TableType, Columns],
      sources: ColumnSet[Columns],
      values: List[A]
  )(implicit schema: Schema[A])

  sealed trait SchemaValidity[A]

  object SchemaValidity {
    implicit def valid[A]: SchemaValidity[A] =
      new SchemaValidity[A] {}
  }

  val id = Column.uuid("id")
  val email = Column.string("email")
  val validated = Column.boolean("validated")

  // ColumnSet[("id", UUID) with ("email", String) with ("validated", Boolean)]
  val columnSet = id ++ email ++ validated

  // Table[String("users"),("id", UUID) with ("email", String) with ("validated", Boolean)]
  val users = columnSet.table("users")

  final case class User(id: UUID, email: String, validated: Boolean)
  implicit val userSchema = DeriveSchema.gen[User]

  insertInto(users)(id ++ email ++ validated)
    .values(
      List(
        User(UUID.randomUUID(), "someone@gmail.com", true),
        User(UUID.randomUUID(), "someoneelse@gmail.com", false)
      )
    )
}
