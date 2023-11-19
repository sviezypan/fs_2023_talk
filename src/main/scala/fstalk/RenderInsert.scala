package fstalk

import zio.schema._
import zio._
import java.util.UUID
import zio.schema.DynamicValue.Sequence
import zio.schema.DynamicValue.Dictionary
import zio.schema.DynamicValue.Tuple
import zio.schema.DynamicValue.RightValue
import zio.schema.DynamicValue.SetValue
import zio.schema.DynamicValue.LeftValue
import zio.schema.DynamicValue.DynamicAst
import zio.schema.DynamicValue.NoneValue
import zio.schema.DynamicValue.Primitive
import zio.schema.DynamicValue.SomeValue
import zio.schema.StandardType.UUIDType
import zio.schema.StandardType.LongType
import zio.schema.StandardType.CharType
import zio.schema.StandardType.DoubleType
import zio.schema.StandardType.BigDecimalType
import zio.schema.StandardType.LocalDateType
import zio.schema.StandardType.BigIntegerType
import zio.schema.StandardType.ZoneOffsetType
import zio.schema.StandardType.ZonedDateTimeType
import zio.schema.StandardType.YearMonthType
import zio.schema.StandardType.DayOfWeekType
import zio.schema.StandardType.ByteType
import zio.schema.StandardType.BinaryType
import zio.schema.StandardType.PeriodType
import zio.schema.StandardType.ZoneIdType
import zio.schema.StandardType.StringType
import zio.schema.StandardType.BoolType
import zio.schema.StandardType.YearType
import zio.schema.StandardType.FloatType
import zio.schema.StandardType.DurationType
import zio.schema.StandardType.IntType
import zio.schema.StandardType.OffsetDateTimeType
import zio.schema.StandardType.MonthType
import zio.schema.StandardType.LocalDateTimeType
import zio.schema.StandardType.ShortType
import zio.schema.StandardType.MonthDayType
import zio.schema.StandardType.InstantType
import zio.schema.StandardType.OffsetTimeType
import zio.schema.StandardType.UnitType
import zio.schema.StandardType.LocalTimeType
import java.time.format.DateTimeFormatter

/** 
 * DynamicValues
  
   final case class Insert[TableType <: Singleton with String, Columns, A](
        table: Table[TableType, Columns], sources: ColumnSet[Columns], values:
        List[A] )(implicit schema: Schema[A])
  
        INSERT INTO
            users ( id, email, validated )
        VALUES
            ( 'c5631db3-9f1a-4ad0-8754-a1a1d43b3904', 'someone@gmail.com', true ), 
            ( '8f943c93-6d59-4210-8e61-7cc0819c642a', 'someoneelse@gmail.com', false ) ;
        
  */
object RenderInsert extends ZIOAppDefault {

  import InsertExample._

  def render[TableType <: Singleton with String, Columns, A](
      insert: Insert[TableType, Columns, A]
  )(implicit schema: Schema[A]): String = {

    val columnNames = insert.table.columns.columns
      .map(column => {
        column.columnName.toString()
      })
      .mkString(", ")

    s"""
        INSERT INTO
            ${insert.table.tableName.toString()} ( ${columnNames} )
        VALUES
            ${renderValues(insert.values)} ;
        """.stripMargin
  }

  def renderValuesA[A](values: List[A]): String = {

    // User(c7341ec0-f06d-4a15-be2d-4780f495b2af,someone@gmail.com,true), 
    // User(6c7cbd30-d960-4712-970f-5eef9874b46f,someoneelse@gmail.com,false))
    values.mkString(", \n")
  }

  def renderValues[A](values: List[A])(implicit schema: Schema[A]): String = 
    values.map(a => renderValue(a)).mkString(", \n")

  def renderValue[A](value: A)(implicit schema: Schema[A]): String = {

    def renderDynamic(dynamicValue: DynamicValue): String = 
        dynamicValue match {
            case DynamicValue.Record(_, values) => {
                val rendered = values.map {
                    case (_, v) => renderDynamic(v)
                }.mkString(", ")

                s"( ${rendered} )"
            }
            case DynamicValue.Primitive(v, standardType)    => renderPrimitiveValue(v, standardType)
            case DynamicValue.Tuple(left, right) => s"${renderDynamic(left)}, ${renderDynamic(right)}"
            case DynamicValue.SomeValue(value)   => renderDynamic(value)
            case _ => ""
        }

    val dynamicValue = schema.toDynamic(value)

    renderDynamic(dynamicValue)
  }

  def renderPrimitiveValue[A](v: A, standardType: StandardType[A]): String = 
    standardType match {
       case StringType => s"'${v}'"
       case UUIDType =>  s"'${v.toString()}'"
       case LocalDateType => s"${DateTimeFormatter.ISO_DATE_TIME.format(v)}"
       case BoolType => s"${v}"
       case IntType => s"${v}"
       case _ => s"${v}"
    }

  override def run =
    ZIO.succeed {

      val insert = insertInto(users)
        .values(
          List(
            User(UUID.randomUUID(), "someone@gmail.com", true),
            User(UUID.randomUUID(), "someoneelse@gmail.com", false)
          )
        )

      val rendered = render(insert)

      println(s"${rendered}")
    }

}
