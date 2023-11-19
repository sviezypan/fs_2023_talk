package fstalk

import zio.test._
import java.time._
import java.util.UUID
import java.time.Instant
import zio.Scope
import zio.schema.{DeriveGen, DeriveSchema, Schema}
import zio.ZIOAppDefault
import zio.{ZIO, ZIOAppArgs}
import zio.schema.validation._
import zio.schema.validation.Validation._
import zio.schema.validation.PhoneNumberValidation._
import zio.schema.annotation.validate
import zio.schema.codec.JsonCodec
import zio.Chunk
import scala.language.postfixOps

/** Type class derivation like with Magnolia
  *
  * GEN[R, A] describes generator of A values that needs an environment R.
  * Generator generates data for property based tests.
  */
object PropertyBasedTestingExample extends ZIOSpecDefault {

  final case class Event(
      id: UUID,
      baseValue: Option[BaseValue],
      startTime: Instant,
      endTime: Instant,
      notificationTime: Option[Instant],
      buffer: Long,
      entities: List[Entity],
      granularity: Long,
      portfolioId: Option[String],
      interval: Option[Int],
      requestType: Option[String],
      category: Option[String],
      keyId: Option[String],
      definition: Option[Int],
      eventType: Option[EventType]
  )

  final case class BaseValue(id: UUID, details: String)
  final case class Entity(id: UUID, description: String)

  sealed trait EventType
  object EventType {
    case object Primary extends EventType
    case object Secondary extends EventType
  }

  object Event {

    def gen: Gen[Any, Event] =
      for {
        id <- Gen.uuid
        baseValue <- Gen.option {
          (Gen.uuid <*> Gen.stringN(5)(Gen.alphaChar)).map(BaseValue.tupled)
        }
        startTime <- Gen.instant
        endTime <- Gen.instant
        notificationTime <- Gen.option(Gen.instant)
        buffer <- Gen.long
        entities <- Gen.listOf(
          (Gen.uuid <*> Gen.stringN(5)(Gen.alphaChar)).map(Entity.tupled)
        )
        granularity <- Gen.long
        portfolioId <- Gen.option(Gen.string)
        interval <- Gen.option(Gen.int)
        requestType <- Gen.option(Gen.string)
        category <- Gen.option(Gen.string)
        keyId <- Gen.option(Gen.string)
        definition <- Gen.option(Gen.int)
        eventType <- Gen.option(
          Gen.oneOf(
            Gen.const(EventType.Primary),
            Gen.const(EventType.Secondary)
          )
        )
      } yield Event(
        id,
        baseValue,
        startTime,
        endTime,
        notificationTime,
        buffer,
        entities,
        granularity,
        portfolioId,
        interval,
        requestType,
        category,
        keyId,
        definition,
        eventType
      )

    implicit val baseType = DeriveSchema.gen[BaseValue]
    implicit val entityType = DeriveSchema.gen[Entity]
    implicit val eventType = DeriveSchema.gen[EventType]
    implicit val eventSchema = DeriveSchema.gen[Event]

    implicit val eventGen = DeriveGen.gen[Event]

  }

  def spec: Spec[TestEnvironment with Scope, Any] = {
    suite("zio schema zio test gen example")(
      test("event generated test") {
        check(Event.eventGen)(event => {
          val x = event.baseValue.nonEmpty
          assertTrue(x)
        })
      } @@ TestAspect.ignore
    )

  }

}

object OrderingExample {

  import PropertyBasedTestingExample._

  val eventsSorted =
    for {
      events <- Event.eventGen.runCollect
    } yield (events.sorted(Event.eventSchema.ordering))

}

/** You can write your own validation
  *   - date is in the past etc
  */
object ValidationExample extends ZIOSpecDefault {

  final case class UserInvalid(
      uuid: String,
      userName: String,
      phoneNumber: String,
      email: String
  )

  final case class User(
      @validate(Validation.uuidV4) uuid: String,
      @validate(Validation.regex(Regex.letter *)) userName: String,
      @validate(PhoneNumberValidation.phoneNumberSK) phoneNumber: String,
      @validate(Validation.email) email: String
  )

  implicit val userSchema = DeriveSchema.gen[User]

  def spec: Spec[TestEnvironment with Scope, Any] =
    suite("validation suite")(
      test("user is valid") {

        val user = User(
          "7af504b8-135a-4c2f-9737-c2e89cb38648",
          "user",
          "00 421 123456",
          "user@gmail.com"
        )

        val validationErrors = userSchema.validate(user)

        assertTrue(validationErrors.size == 0)
      },
      test("properties of user are valid") {

        val user = User(
          "7af504b8-135a-4c2f-9737-c2e89cb38648",
          "user",
          "00 421 123456",
          "user@gmail.com"
        )

        val uuidIsValid = Validation.uuidV4.validate(user.uuid)
        val userNameIsValid = Validation
          .regex(Regex.letter *)
          .validate(user.userName)
        val phoneNumberIsValid = PhoneNumberValidation.phoneNumberSK
          .validate(user.phoneNumber)
        val emailIsValid = Validation.email.validate(user.email)

        assertTrue(uuidIsValid.isRight) &&
        assertTrue(userNameIsValid.isRight) &&
        assertTrue(phoneNumberIsValid.isRight) &&
        assertTrue(emailIsValid.isRight)
      }
    )

}

object DerivedSerialization extends ZIOAppDefault {

  final case class Person(name: String, age: Int)
  final case class PersonDTO(firstname: String, lastname: String, years: Int)

  val personDTOSchema = DeriveSchema.gen[PersonDTO]

  val personSchema: Schema[Person] =
    personDTOSchema.transform[Person](
      (dto: PersonDTO) => Person(dto.firstname + " " + dto.lastname, dto.years),
      (person: Person) => {
        val name = person.name.split(" ").toSeq
        PersonDTO(name.head, name.tail.mkString(" "), person.age)
      }
    )

  override val run =
    for {
      _ <- ZIO.unit
      json = """{"firstname":"John","lastname":"Doe","years":42}"""

      chunks = Chunk.fromArray(json.getBytes)

      _ <- ZIO.debug("input JSON    : " + json)

      person <- ZIO.fromEither(
        JsonCodec
          .schemaBasedBinaryCodec[Person](personSchema)
          .decode(chunks)
      )

      _ <- ZIO.debug("Person        : " + person)

      //   personDTO <- ZIO.fromEither(
      //     JsonCodec
      //       .schemaBasedBinaryCodec[PersonDTO](personDTOSchema)
      //       .decode(chunks)
      //   )
      //   _ <- ZIO.debug("PersonDTO     : " + personDTO)

    } yield ()

}
