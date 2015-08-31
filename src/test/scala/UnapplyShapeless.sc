import shapeless._

trait UnapplyLeft[L <: HList] extends DepFn1[L]

trait LPUnapplyLeft {
  import ops.hlist.{ Init, Last }
  type Aux[L <: HList, Out0] = UnapplyLeft[L] { type Out = Out0 }
  implicit def unapplyHCons[L <: HList, I <: HList, F]
  (implicit
   init: Init.Aux[L, I],
   last: Last.Aux[L, F]): Aux[L, Option[(I, F)]] =
    new UnapplyLeft[L] {
      type Out = Option[(I, F)]
      def apply(l: L): Out = Option((l.init, l.last))
    }
}

object UnapplyLeft extends LPUnapplyLeft {
  implicit def unapplyPair[H1, H2]: Aux[H1 :: H2 :: HNil, Option[(H1, H2)]] =
    new UnapplyLeft[H1 :: H2 :: HNil] {
      type Out = Option[(H1, H2)]
      def apply(l: H1 :: H2 :: HNil): Out = Option((l.head, l.tail.head))
    }
}

object ~ {
  def unapply[L <: HList, Out](l: L)
                              (implicit ua: UnapplyLeft.Aux[L, Out]): Out = ua(l)
}

object Test {

  val l = 23 :: "foo" :: true :: HNil
  //val a ~ b ~ c = l
}


//right associative operators, end with a :