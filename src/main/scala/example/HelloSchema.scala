package examples

import zio.schema.DeriveSchema

import java.util.UUID
import zio.schema.meta._
import zio.schema._
import zio._
import zio.schema.meta.Migration._
import zio.schema.StandardType._
import java.time.LocalDateTime

// insert
// translation + default values
object InsertExample {

  final case class User(id: UUID, email: String, validated: Boolean)

  /**
      insertInto(users)(id, email, validated)
         .values(
            ("deb68414-8575-11ee-b9d1-0242ac120002", "someone@gmail.com", true),
            ("e1f5598e-8575-11ee-b9d1-0242ac120002", "someoneelse@gmail.com", false)
          )

      INSERT INTO users (id, email, validated)
         VALUES 
            ("deb68414-8575-11ee-b9d1-0242ac120002", "someone@gmail.com", true),
            ("e1f5598e-8575-11ee-b9d1-0242ac120002", "someoneelse@gmail.com", false);
         
    */

  trait Table[TableType] {
    type Columns
  }

  object Table {
    type WithColumns[TableType, Columns0] = Table[TableType] {
      type Columns = Columns0
    }
  }

  trait Column[TableType, -Field] { self =>
    val columns = List(self)

    def ++[Field2](column2: Column[TableType, Field2]): Column[TableType, Field with Field2] =
      new Column[TableType, Field with Field2] {
        override val columns = column2 :: self.columns
      }
  }

  def insertInto[TableType, Columns](table: Table.WithColumns[TableType, Columns])(
    columns: Column[TableType, Columns]
  ) = InsertBuilder(table, columns)

  final case class InsertBuilder[TableType, Columns](
    table: Table.WithColumns[TableType, Columns], // TableType = User
    sources: Column[TableType, Columns]           // Columns = ("id", UUID) with ("email", String) with ("validated", Boolean)
  ) {

    def values[A](values: List[A])(implicit
      schema: Schema[A],
      schemaValidity: SchemaValidity[A]
    ): Insert[TableType, Columns, A] =
      Insert(table, sources, values)
  }

  sealed case class Insert[TableType, Columns, A](
    table: Table[TableType],
    sources: Column[TableType, Columns],
    values: List[A]
  )(implicit schema: Schema[A])

  // TableType = User
  // Columns = ("id", UUID) with ("email", String) with ("validated", Boolean)
  // A =  (String, String, Boolean)

  /// WithFields[F1 <: Singleton with String,F2 <: Singleton with String, F3 <: Singleton with String, A1, A2, A3, Z]
  sealed trait SchemaValidity[A]

  object SchemaValidity {
    implicit def valid[A]: SchemaValidity[A] =
      new SchemaValidity[A] {}
  }

  val users: Table.WithColumns[User, ("id", UUID) with ("email", String) with ("validated", Boolean)] =
    new Table[User] {
      type Columns = ("id", UUID) with ("email", String) with ("validated", Boolean)
    }

  val id        = new Column[User, ("id", UUID)] {}
  val email     = new Column[User, ("email", String)] {}
  val validated = new Column[User, ("validated", Boolean)] {}

  val x: Column[User, ("id", UUID) with ("email", String) with ("validated", Boolean)] = id ++ email ++ validated

  case class InputUser(id: UUID, email: String, validated: Boolean)

  // schema here does not do much
  // now I need implicit Schema of Input User ???
  // insertInto(users)(id ++ email ++ validated)
  //       .values(
  //         List(
  //           InputUser(UUID.randomUUID(), "someone@gmail.com", true),
  //           InputUser(UUID.randomUUID(), "someoneelse@gmail.com", false)
  //         ))
}

object MySchema {

  case class Person(name: String, age: Int)

  final case class CaseClass2[A1, A2, Z](
    name: String,
    field1: Field[A1, Z],
    field2: Field[A2, Z],
    construct: (A1, A2) => Z,
    deconstruct: Z => Option[(A1, A2)]
  )

  final case class Field[A, Z](
    name: String,
    tpe: PrimitiveType[A],
    get: Z => A,
    set: (Z, A) => Z
  )

  sealed trait PrimitiveType[A]
  object PrimitiveType {
    case object StringType extends PrimitiveType[String]
    case object IntType    extends PrimitiveType[Int]
  }

  CaseClass2[String, Int, Person](
    name = "Person",
    field1 =
      Field[String, Person]("name", PrimitiveType.StringType, _.name, (person, name) => person.copy(name = name)),
    field2 = Field[Int, Person]("age", PrimitiveType.IntType, _.age, (person, age) => person.copy(age = age)),
    construct = Person.apply,
    deconstruct = Person.unapply
  )

  // What can we do with CaseClass2 ?
  /**
    
    Let's derive SQL migration script




    */

}

trait SchemaExamples {

  // ===================

  case class Customer(id: UUID, name: String, age: Int, born: LocalDateTime)
  case class CustomerV2(id: UUID, name: String, born: LocalDateTime, verified: Boolean)

  val customerV1Schema = DeriveSchema.gen[Customer]
  val customerV2Schema = DeriveSchema.gen[CustomerV2]

  def convertType[A](s: StandardType[A], isNotNull: Boolean): String = {
    val typeName = s match {
      case UUIDType          => "UUID"
      case IntType           => "INTEGER"
      case StringType        => "VARCHAR(50)"
      case LocalDateTimeType => "TIMESTAMP"
      case BoolType          => "BOOLEAN"
      case _                 => ""
    }

    if (isNotNull)
      s"${typeName} NOT NULL"
    else
      typeName
  }

  def fieldTypeName[A](schema: Schema[A]): String =
    schema match {
      case Schema.Primitive(standardType, _)                     =>
        convertType(standardType, true)
      case Schema.Optional(Schema.Primitive(standardType, _), _) =>
        convertType(standardType, true)
      case Schema.Lazy(schema)                                   => fieldTypeName(schema())
      case _                                                     =>
        throw new Error("unsupported")
    }

  def createTableScript[A](schema: Schema.Record[A]): String = {
    val body = schema.fields.map { f =>
      val fieldName = f.name.toString()

      val fieldType = fieldTypeName(f.schema)

      s"${fieldName} ${fieldType}"
    }

    s"""
      CREATE TABLE ${schema.id.name.toLowerCase()} {
          ${body.mkString(", \n")}
      }
    """.stripMargin
  }

  def migrateScript[A, B](schema1: Schema.Record[A], schema2: Schema.Record[B]): String = {
    val migrations = Migration.derive(
      MetaSchema.fromSchema(schema1),
      MetaSchema.fromSchema(schema2)
    ) match {
      case Left(value)  => Chunk.empty[Migration]
      case Right(value) => value
    }

    val scripts = migrations.collect {
      case AddNode(path, node) =>
        s"""
          ALTER TABLE ${schema1.id.name.toLowerCase()}
            ADD COLUMN ${path.render} ${fieldTypeName(node.toSchema)}
            DEFAULT ${node.toSchema.defaultValue.getOrElse(null)};
          """.stripMargin

      case DeleteNode(path) =>
        s""" 
          ALTER TABLE ${schema1.id.name.toLowerCase()}
            DROP COLUMN ${path.render};
          """.stripMargin

    }

    scripts.mkString("\n")
  }

}

object RunnableExample extends ZIOAppDefault with SchemaExamples {

  def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed {

      val script = createTableScript(customerV1Schema)

      val migrate = migrateScript(customerV1Schema, customerV2Schema)
      val _       = script
      val _       = migrate

    //  println(s"${script})")
      println(s"${migrate}")
    }
}

object ClientProjectExamples {

  // zio test gen
  // ordering
  // validation

}
