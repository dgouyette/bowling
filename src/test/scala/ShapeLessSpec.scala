import org.scalatest._
import shapeless._


object Bowling {

type Manche = Int :: Int :: HNil

type DerniereManche = Int :: Int :: Int ::  HNil

type JeuUneManche = Manche :: HNil
type JeuDeuxManches = Manche :: Manche ::  HNil
type JeuTroisManches = Manche :: Manche :: Manche ::  HNil
type JeuMancheMixtes = Manche :: Manche :: DerniereManche :: HNil
type JeuOriginal =  Manche :: Manche :: Manche :: Manche  :: Manche :: Manche  ::Manche :: Manche :: Manche :: DerniereManche :: HNil

  trait Jeu[L <: HList]

  object Score extends Poly1 {
  implicit def caseManche  = at[Manche](manche => manche.head + manche.tail.reduceLeft(Add))
  implicit def caseDerniereManche  = at[DerniereManche](manche => manche.head + Score(manche.tail))
  implicit def caseJeu = at[JeuUneManche](jeu =>  Score(jeu.head))
  implicit def caseJeu2 = at[JeuDeuxManches](jeux2Manches2Score )
  implicit def caseJeu3 = at[JeuTroisManches](jeu3ManchesScore)


  def jeux2Manches2Score: (Manche :: Manche :: HNil) => Int = {
    case (h1 :: h2 :: HNil) :: (t1 :: t2 :: HNil) :: _ if h1 == 10 => h1 + t1 + t1 + t2 + t2
    case (h1 :: h2 :: HNil) :: (t1 :: t2 :: HNil) :: _ if h1 + h2 == 10 => h1 + h2 + t1 + t1 + t2
    case jeu => Score(jeu.head) + jeu.tail.map(Score).reduceLeft(Add)
  }



    def jeu3ManchesScore : (Manche :: Manche :: Manche :: HNil ) => Int  = {
    case (h1 :: h2 :: HNil) :: (t1 :: t2 :: HNil) :: tail if h1 == 10 => h1 + t1 + t1 + t2 + t2 + Score(tail)
    case (h1 :: h2 :: HNil) :: (t1 :: t2 :: HNil) :: tail if h1 + h2 == 10 & t1 + t2 == 10 => h1 + h2 + t1 + t1 + t2 + tail.head.head +  Score(tail)
    case (h1 :: h2 :: HNil) :: (t1 :: t2 :: HNil) :: tail if h1 + h2 == 10 => h1 + h2 + t1 + t1 + t2 + Score(tail)
    case jeu => Score(jeu.head) + Score(jeu.tail)
  }
}




object Add extends Poly2 {
  implicit def caseInt = at[Int, Int]( _ + _ )
}


}
class ShapeLessSpec extends FunSuite with ShouldMatchers {

  import Bowling._

  test("Une manche 0 + 0  doit avoir un score de 0"){
    Score(0  :: 0 :: HNil) shouldEqual 0
  }

  test("Une manche 3 + 1 doit avoir un score de 4") {
    Score(3  :: 1 :: HNil) shouldEqual 4
  }

  test("Une manche 0 + 0 + 0 doit avoir un score de 0" ) {
    Score(0  :: 0 ::  0 ::  HNil) shouldEqual 0
  }

  test("Deux manches 0 + 10 + 1 + 2  doit avoir un score de 0+10+1 + 1+2") {
    Score((0  ::  10  :: HNil) :: ( 1 ::  2 :: HNil)  :: HNil) shouldEqual 0 + 10 + 1 + 1 + 2
  }

  test("Deux manches  1 + 10 + 1 + 2  doit avoir un score de 0+10 + 1+1 + 2") {
    Score((0  ::  10  :: HNil) :: ( 1 ::  2 :: HNil) :: HNil) shouldEqual 0+10 + 1+1 + 2
  }

  test("Trois manches 1 + 2 +  + 1 +  2  +  1 + 2 doit avoir un score de  1+2 + 1+2 + 1+2") {
    Score((1 :: 2 :: HNil) :: (1 :: 2 :: HNil) :: (1 :: 2 :: HNil) :: HNil) shouldEqual 1+2 + 1+2 + 1+2
  }


  test("Trois manche 0+10 + 5+5 +  1+2 doit avoir un score de 0+10+ 5+5+5 + 1+1 + 2") {
    Score((0 :: 10 :: HNil) :: (5 :: 5 :: HNil) :: (1 :: 2 :: HNil) :: HNil) shouldEqual 0+10+ 5+5+5 + 1+1 + 2
  }

  test("Une manche 3+1 + 0+10 + 5+1 doit avoir un score de 3+1 + 0+10+5 + 5+1") {
    Score((3 :: 1 :: HNil) ::  (0 ::  10 :: HNil) :: (5 ::  1 :: HNil ) :: HNil ) shouldEqual 3+1 + 0+10+5 + 5+1
  }

  test("Une manche 10 + 2 + 5  doit avoir un score de 10 + 2+5 + 2+5") {
    Score((10 :: 0 :: HNil)   :: (2 ::  5 :: HNil)  :: HNil ) shouldEqual 10 + 2 +2 + 5+5
  }

  /**test("Une manche 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5+5 doit avoir un score de 5+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5+5"){
    val spair  = 5 :: 5 :: HNil
    val dernierSpair = 5 :: 5 :: 0 :: HNil
    Score(spair :: spair :: spair :: spair :: spair :: spair :: spair :: spair :: spair :: dernierSpair :: HNil) shouldEqual 5+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5+5
  }**/

/**
  "Une manche 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 " should " doit avoir un score de 300" in {
    score(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10) shouldEqual 300
  }
**/

}
