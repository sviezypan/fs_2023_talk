package fstalk

/**
  * Schema basics
  */
object MySchema {

  case class Person(name: String, age: Int)

  final case class CaseClass[A1, A2, Z](
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
    case object IntType extends PrimitiveType[Int]
  }

  CaseClass[String, Int, Person](
    name = "Person",
    field1 = Field[String, Person](
      "name",
      PrimitiveType.StringType,
      _.name,
      (person, name) => person.copy(name = name)
    ),
    field2 = Field[Int, Person](
      "age",
      PrimitiveType.IntType,
      _.age,
      (person, age) => person.copy(age = age)
    ),
    construct = Person.apply,
    deconstruct = Person.unapply
  )

  // What can we do with CaseClass2 ?
  /** Let's derive SQL migration script
    */

}
