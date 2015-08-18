import org.scalatest._
import shapeless.{HList, HNil}
import shapeless._, nat._, shapeless.ops.hlist.{LeftReducer, Length}, ops.nat.{Mod, Prod, Sum}

object BowlingScoreShapeless {

  type Manche = Int :: Int :: HNil

  type DerniereManche = Int :: Int :: Int :: HNil
  type Game = Manche :: HNil

  def score(game: Game) = {
    import XXXXXXX._
    val a = game.
  }

  object Sum extends LeftReducer[Game, Int] {
    override type Out = Int

    override def apply(t: Game): Int =2
  }
}
  object XXXXXXX extends Poly1 {
    implicit def caseInt = at[Int](x => x)

  //val game: Game = (0 :: 0 :: HNil) :: (0 :: 0 :: 0 :: HNil) :: HNil

}


class BowlingScoreShapelessSpec extends FlatSpec with ShouldMatchers {


  import BowlingScoreShapeless._


  "Une manche 0 + 0" should " doit avoir un score de 0" in {
    score((0::0::HNil)::HNil) shouldEqual 0
  }

  "Une manche 3 + 1" should " doit avoir un score de 4" in {
    score((3::1::HNil)::HNil) shouldEqual 4
  }
}