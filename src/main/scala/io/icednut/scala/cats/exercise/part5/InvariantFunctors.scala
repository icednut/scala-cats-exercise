package io.icednut.scala.cats.exercise.part5

import cats.{Contravariant, Monoid}

object InvariantFunctors {

  trait Crypto[A] {
    self =>
    def encrypt(value: A): String

    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B): String = self.encrypt(back(value))

      override def decrypt(encrypted: String): B = forth(self.decrypt(encrypted))
    }

    //    def optionMap[Option[A]](forth: A => Option[A])(implicit mo: Monoid[A]): Crypto[Option[A]] = new Crypto[Option[A]] {
    //      override def encrypt(v: Option[A]): String = self.encrypt(v.getOrElse(mo.empty))
    //
    //      override def decrypt(encrypted: String): Option[A] = mo. self.decrypt(encrypted)
    //    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)

  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String): String = value.map(c => (c + 2).toChar)

    override def decrypt(encrypted: String): String = encrypted.map(c => (c - 2).toChar)
  }

  /*
    How can we support ints, doubles, Option[String]?
   */
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(
    back = _.toString,
    forth = _.toDouble
  )

  // TODO 1 - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(
    back = v => v.getOrElse(""),
    forth = v => v match {
      case "" => None
      case v@_ => Some(v)
    }
  )

  // TODO 2 - if you have a Crypt[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  //  implicit val contraCrypto: Contravariant[Crypto] = new Contravariant[Crypto] {
  //    override def contramap[A, B](fa: Crypto[A])(f: B => A): Crypto[B] = ???
  //  }
  //  val originalCrypto: Crypto[String] = caesarCypher
  //  val optionCrypto: Crypto[Option[String]] = Contravariant[Crypto].contramap(originalCrypto)(_.getOrElse(""))
  implicit def optionCrypto[T](implicit crypto: Crypto[T], mo: Monoid[T]): Crypto[Option[T]] = crypto.imap(
    back = _.getOrElse(mo.empty),
    forth = v => Option(v)
  )

  import cats.Invariant
  import cats.Show
  import cats.instances.string._
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import  cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse(""))

  // TODO 3:
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: A)(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A) =
      contramap[A, B](wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] {
    def map[A, B](wa: W[A])(forth: A => B): W[B]
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)
    val doubleEncrypted = encrypt(Math.PI)
    val doubleDecrypted = decrypt[Double](doubleEncrypted)
    val optionStringEncrypted = encrypt[Option[String]](Some("Let's encrypt"))
    val optionStringDecrypted = decrypt[Option[String]](optionStringEncrypted)
    val optionStringDecrypted3 = decrypt[Option[String]](encrypted)
    val optionStringEncrypted2 = encrypt[Option[String]](None)
    val optionStringDecrypted2 = decrypt[Option[String]](optionStringEncrypted2)

    import cats.instances.double._
    val optionDoubleEncrypted = encrypt[Option[Double]](Option(Math.PI))
    val optionDoubleDecrypted = decrypt[Option[Double]](optionDoubleEncrypted)

    println(encrypted)
    println(decrypted)
    println(doubleEncrypted)
    println(doubleDecrypted)
    println(optionStringEncrypted)
    println(optionStringDecrypted)
    println(optionStringEncrypted2)
    println(optionStringDecrypted2)
    println(optionStringDecrypted3)
    println(optionDoubleEncrypted)
    println(optionDoubleDecrypted)
  }
}
