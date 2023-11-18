package fstalk

import zio.schema.DeriveSchema

import java.util.UUID
import zio.schema.meta._
import zio.schema._
import zio._
import zio.schema.meta.Migration._
import zio.schema.StandardType._
import java.time._
import scala.annotation.implicitNotFound

/** Type-safe checks of structure
  */
object InsertExample {

  sealed trait ColumnType[A]
  object ColumnType {
    implicit case object StringType extends ColumnType[String]
    implicit case object IntType extends ColumnType[Int]
    implicit case object UUIDType extends ColumnType[UUID]
    implicit case object BooleanType extends ColumnType[Boolean]
    implicit case object LocalDateType extends ColumnType[LocalDate]
  }

  sealed trait Column[NameType] { self =>
    type Name <: Singleton with String
    type CType

    def columnName: Name

    def paramType: ColumnType[CType]
  }

  object Column {
    private def apply[N <: Singleton with String, T](
        name0: N
    )(implicit paramType0: ColumnType[T]): Column[(N, T)] =
      new Column[(N, T)] {
        type Name = N
        type CType = T
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
      val columns: Chunk[Column[_]]
  ) { self =>
    def ++[C](
        column: Column[C]
    ): ColumnSet[Columns with C] =
      new ColumnSet(columns = columns :+ column)

    def table[TableName0 <: Singleton with String](tableName: TableName0) =
      new Table(tableName, self)
  }

  final case class Table[TableName <: Singleton with String, +Columns](
      tableName: TableName,
      columns: ColumnSet[Columns]
  )

  def insertInto[TableName <: Singleton with String, Columns](
      table: Table[TableName, Columns]
  ) = InsertBuilder(table)

  final case class InsertBuilder[TableName <: Singleton with String, Columns](
      table: Table[TableName, Columns]
  ) {

    def values[A](values: List[A])(implicit
        schema: Schema[A], 
        validInsert: ValidInsert[A, Columns]
    ): Insert[TableName, Columns, A] =
      Insert(table, values)
  }

  final case class Insert[TableType <: Singleton with String, Columns, A](
      table: Table[TableType, Columns],
      values: List[A]
  )(implicit schema: Schema[A])

  val id = Column.uuid("id")
  val email = Column.string("email")
  val validated = Column.boolean("validated")
  val columnSet = id ++ email ++ validated

  val users: Table[
    "users",
    ("id", UUID) with ("email", String) with ("validated", Boolean)
  ] =
    columnSet.table("users")

  final case class User(id: UUID, email: String, validated: Boolean)

  // Schema.CaseClass3.WithFields["id", "email", "validated", UUID, String, Boolean, User]
  implicit val userSchema = DeriveSchema.gen[User]

  // if you add age: Int to user you would get an error:
  // Error Could not validate that case class fields User match columns ("id", java.util.UUID) with ("email", String) with ("validated", Boolean)
  insertInto(users)
    .values(
      List(
        User(UUID.randomUUID(), "someone@gmail.com", true),
        User(UUID.randomUUID(), "someoneelse@gmail.com", false)
      )
    )
}

// Schema[A] ==  Schema.CaseClass3.WithFields["id","email","validated",UUID,String,Boolean,User]
// Columns  == ("id", UUID) with ("email", String) with ("validated", Boolean)
@implicitNotFound(
  "Could not validate that case class fields ${A} match columns ${Columns}"
)
sealed trait ValidInsert[A, Columns]

object ValidInsert {
  implicit def validateCaseClass3[
      A,
      Columns,
      Field1 <: Singleton with String,
      Field2 <: Singleton with String,
      Field3 <: Singleton with String,
      Type1,
      Type2,
      Type3
  ](implicit
      schema: Schema.CaseClass3.WithFields[
        Field1,
        Field2,
        Field3,
        Type1,
        Type2,
        Type3,
        A
      ],
      evidence: Columns =:= (Field1, Type1) with (Field2, Type2) with (
          Field3,
          Type3
      )
  ): ValidInsert[A, Columns] =
    new ValidInsert[A, Columns] {}
}
