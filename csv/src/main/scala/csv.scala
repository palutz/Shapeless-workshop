import shapeless._

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

   implicit def stringEnc: CsvEncoder[String] =
     pure(str => List(str))

  implicit def intEnc: CsvEncoder[Int] =
     pure(num => List(num.toString))

  implicit def booleanEnc: CsvEncoder[Boolean] =
    pure(bool => List(if(bool) "yes" else "no"))

  // I have to comment this to use the genericEncoder, cause this is more speciic and Scala will use is if available
//  implicit def iceCreamEnc: CsvEncoder[IceCream] =
//    pure(iceCream => List(
//      iceCream.name,
//      iceCream.numCherries.toString,
//      iceCream.inCone.toString
//    ))

  // adding implicit for shapeless...
  implicit def hnilEnc: CsvEncoder[HNil] =
    pure(hnil => Nil)

  implicit def hconsEnc[H, T <: HList](implicit hEnc: CsvEncoder[H], tEnc: CsvEncoder[T]): CsvEncoder[H :: T] = {
    pure {
      case head :: tail =>
        hEnc.encode(head) ++ tEnc.encode(tail)
    }
  }

  // use dependant types to make a genercic encoder(s)
  // GENERIC and Dependent types (CHECK IT !!!)
  implicit def denericEnc[A, R] (
                                    implicit
                                    generic: Generic[A] { type Repr = R },  // or writing generic: Generic.Aux[A, R]
                                    encoder: CsvEncoder[R]
                                  ) : CsvEncoder[A] =
      pure(a => encoder.encode(generic.to(a)))
}



object Main extends Demo {
  def encodeCsv[A](value: A)(implicit enc: CsvEncoder[A]): List[String] =
    enc.encode(value)

  println(encodeCsv("Dave"))
  println(encodeCsv(123))
  println(encodeCsv(true))
  println(encodeCsv(IceCream("IceCream1", 0, false)))

  val hlist = "Magnum" :: 0 :: false :: HNil
  println(encodeCsv(hlist))
}


