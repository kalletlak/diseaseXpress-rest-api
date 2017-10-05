package utils

object Implicits {

  implicit class AnythingImplicits[A](a: A) {
    
    // see https://users.scala-lang.org/t/implicit-class-for-any-and-or-generic-type/501)    
    /** thrush combinator (formerly "then", before it was reserved in scala) */
    def zen[B](f: A => B) = f(a)

  }
  
}