package neoproto

object Main extends App:
  final case class ProtoPerson(
    name: String,
    location: Option[ProtoAddress],
    payment: Option[ProtoPaymentMethod]
  )

  final case class Person(
    name: Name,
    location: Address,
    payment: PaymentMethod
  )

  final case class ProtoAddress(
    street: String
  )

  final case class Address(
    street: String
  )

  final case class Name(value: String)
  object Name:
    given Reader[String, Name] = Reader.valueClass[Name]

  sealed trait ProtoPaymentMethod
  object ProtoPaymentMethod:
    case class Cash(amount: Int)                 extends ProtoPaymentMethod
    case class Card(amount: Int, number: String) extends ProtoPaymentMethod

  enum PaymentMethod:
    case Cash(amount: Int)
    case Card(amount: Int, number: String)

  val reader    = Reader.gen[ProtoPerson, Person]
  val goodProto =
    ProtoPerson("Navid", Some(ProtoAddress("Some street")), Some(ProtoPaymentMethod.Cash(12)))
  val badProto  = ProtoPerson("Navid", None, Some(ProtoPaymentMethod.Cash(12)))

  println(reader.read(goodProto))
  // Success(Person(Name(Navid),Address(Some street),Cash(12)))
  println(reader.read(badProto))
  // Failure(NonEmptyList(Property location on type Person is required but is missing from the input!))
