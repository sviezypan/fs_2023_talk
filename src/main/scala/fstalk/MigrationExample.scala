package fstalk

import zio.schema.DeriveSchema

import java.util.UUID
import zio.schema.meta._
import zio.schema._
import zio._
import zio.schema.meta.Migration._
import zio.schema.StandardType._
import java.time._

object MigrationExample {

  case class Customer(id: UUID, name: String, age: Int, born: LocalDateTime)
  case class CustomerV2(
      id: UUID,
      name: String,
      born: LocalDateTime,
      verified: Boolean
  )

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
      case Schema.Primitive(standardType, _) =>
        convertType(standardType, true)
      case Schema.Optional(Schema.Primitive(standardType, _), _) =>
        convertType(standardType, true)
      case Schema.Lazy(schema) => fieldTypeName(schema())
      case _ =>
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

  def migrateScript[A, B](
      schema1: Schema.Record[A],
      schema2: Schema.Record[B]
  ): String = {
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

object RunnableExample extends ZIOAppDefault {

  import MigrationExample._

  def run: ZIO[Environment with ZIOAppArgs with Scope, Any, Any] =
    ZIO.succeed {
      val script = createTableScript(customerV1Schema)
      val migrate = migrateScript(customerV1Schema, customerV2Schema)

      println(s"${script})")
      println(s"${migrate}")
    }
}
