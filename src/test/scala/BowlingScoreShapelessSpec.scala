import org.scalatest._
import shapeless._
import syntax.std.tuple._
/**
object BowlingScoreShapeless {

  type Manche = Int :: Int :: HNil

  type DerniereManche = Int :: Int :: Int :: HNil
  type Game = Manche :: HNil



  def score(game: Game) = {
    import Add._
    game.head.reduceLeft(Add)
  }


  object Add extends Poly2 {
    implicit def caseInt = at[Int, Int](_ + _)

    implicit def caseManche = at[Int, Manche](_ + _.reduceLeft(Add))
  }

 object MancheScore extends Poly1 {

    import Add._

    implicit def caseManche = at[Manche](_.reduceLeft(Add))

    implicit def caseDerniereManche = at[DerniereManche](_.reduceLeft(Add))
  }

}

class BowlingScoreShapelessSpec extends FlatSpec with ShouldMatchers {


  import BowlingScoreShapeless._


  "Une manche 0 + 0" should " doit avoir un score de 0" in {
    score((0 :: 0 :: HNil) :: HNil) shouldEqual 0
  }

  "Une manche 3 + 1" should " doit avoir un score de 4" in {
    score((3 :: 1 :: HNil) :: HNil) shouldEqual 4
  }
}**/