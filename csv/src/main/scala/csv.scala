final case class Employee(
                           name    : String,
                           number  : Int,
                           manager : Boolean
                         )

final case class IceCream(
                           name        : String,
                           numCherries : Int,
                           inCone      : Boolean
                         )


trait CsvEncoder[A] {
  def encode(value: A): List[String]
}


object CsvEncoder {
  def pure[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

   implicit val stringEnc: CsvEncoder[String] =
     pure(str => List(str))

  implicit val intEnc: CsvEncoder[Int] =
     pure(num => List(num.toString))

  implicit val booleanEnc: CsvEncoder[Boolean] =
    pure(bool => List(if(bool) "yes" else "no"))

  implicit val iceCreamEnc: CsvEncoder[IceCream] =
    pure(iceCream => List(
      iceCream.name,
      iceCream.numCherries.toString,
      iceCream.inCone.toString
    ))
}



object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  println(encodeCsv("Dave"))
  println(encodeCsv(123))
  println(encodeCsv(true))
}


