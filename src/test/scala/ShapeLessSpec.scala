import org.scalatest._
import shapeless._


object ScoreComputer {

  object Add1 extends shapeless.Poly1 {
    implicit def caseInt = at[Int](i=> (i, i))
    implicit def caseManche = at[Manche](m=> (m, m.reduceLeft(Add2)))
    implicit def caseGame = at[Game](g=> (g, g.reduceLeft(Add2)))
  }
  object Add2 extends shapeless.Poly2 {
    implicit def caseInt = at[Int, Int](_ + _)
    implicit def caseManche = at[Int, Manche](_ + _.reduceLeft(Add2))
  }

  type Manche = Int :: Int :: HNil
  type Game = Manche :: Manche :: HNil
  type Game3 = Manche :: Manche :: Manche :: HNil

  def sumGame(l: Game) = {
    l.reduceLeft(Add2)
  }

  def sum(l: Manche) = {
    import Add2._
    l.reduceLeft(Add2)
  }

}

class ShapeLessSpec extends FlatSpec with ShouldMatchers {

  import ScoreComputer.{sum, sumGame}

  "Une manche 0 + 0" should " doit avoir un score de 0" in {
    sum(0 :: 0 :: HNil) shouldEqual 0
  }

  "Une manche 3 + 1" should " doit avoir un score de 4" in {
    sum(3 :: 1 :: HNil) shouldEqual 4
  }

  "Une manche 0 + 9 + 1 + 2" should " doit avoir un score de 0+9 + 1+2" in {
    sumGame((0 :: 9 :: HNil) :: (1 :: 2 :: HNil) :: HNil) shouldEqual 0 + 9 + 1 + 2
  }

/**  "Une manche 0 + 9 + 1 + 2 + 1 + 3 " should " doit avoir un score de 0+9 + 1+2 +1+3" in {
    sumGame3((0 :: 9 :: HNil) :: (1 :: 2 :: HNil) :: (1 :: 3 :: HNil) :: HNil) shouldEqual 0 + 9 + 1 + 2 + 1 + 3
  }*/


  "Une manche 0 + 10 + 1 + 2" should " doit avoir un score de 0+10+1 + 1+2" in {
    sumGame((0 :: 10 :: HNil) :: (1 :: 2 :: HNil) :: HNil) shouldEqual 0 + 10 + 1 + 1 + 2
  }


}
