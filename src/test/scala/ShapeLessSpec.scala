import org.scalatest._
import shapeless._

import scala.annotation.tailrec


object Bowling {

  sealed trait MancheT

  case class Manche(a: Int, b: Int) extends MancheT

  case class DerniereManche(a: Int, b: Int, c: Int) extends MancheT

  object MancheT {
    def unapply(m: MancheT): Option[(Int, Int, Option[Int])] = {
      m match {
        case Manche(a, b) => Some(a, b, None)
        case DerniereManche(a, b, c) => Some(a, b, Some(c))
      }
    }

    def sum(m: MancheT): Int = {
      unapply(m).map { p => p._1 + p._2 + p._3.getOrElse(0)
      }.getOrElse(0)
    }
  }


  type JeuUneManche = Manche :: HNil
  type JeuDeuxManches = Manche :: Manche :: HNil
  type JeuTroisManches = Manche :: Manche :: Manche :: HNil
  type JeuMancheMixtes = Manche :: Manche :: DerniereManche :: HNil
  type JeuOriginal = Manche :: Manche :: Manche :: Manche :: Manche :: Manche :: Manche :: Manche :: Manche :: DerniereManche :: HNil

  object Score extends Poly1 {
    implicit def caseMancheT = at[MancheT] {
      case Manche(a, b) => a + b
      case DerniereManche(a, b, c) => a + b + c
    }

    implicit def caseManche = at[Manche](MancheT.sum)
    implicit def caseDerniereManche = at[DerniereManche](MancheT.sum)

    implicit def caseJeu = at[JeuUneManche](jeux2Manches2Score)
    implicit def caseJeu2 = at[JeuDeuxManches](jeux2Manches2Score)
    implicit def caseJeu3 = at[JeuTroisManches](jeux2Manches2Score)
    implicit def caseJeuOriginal = at[JeuOriginal](jeux2Manches2Score)

    def jeux2Manches2Score[L]: (HList) => Int = {
      case DerniereManche(a,b,c) :: _  if a == 10 && b == 10 && c == 10 => a + strike(Manche(b,c)) + b + strike(Manche(c,0)) + c
      case (Manche(h1, h2)) :: (m2: MancheT) :: tail if h1 == 10 => h1 + strike(m2) + jeux2Manches2Score(m2 :: tail)
      case (Manche(h1, h2)) :: (m2: MancheT) :: tail if h1 + h2 == 10 => h1+h2  + spair(m2)  +   jeux2Manches2Score(m2 :: tail)

      case (m1: MancheT) :: t => Score(m1) + jeux2Manches2Score(t)
      case HNil => 0
    }
  }


  def spair[L <: MancheT](m: MancheT): Int = {
    m match {
      case Manche(a, b) => a
      case DerniereManche(a, b, c) => a

    }
  }

  def strike[L <: MancheT](m: MancheT): Int = {
    m match {
      case Manche(a, b) => a+b
      case DerniereManche(a, b, c) => a+b

    }
  }
}
class ShapeLessSpec extends FunSuite with ShouldMatchers {

  import Bowling._

  test("une spair doit retourner la première boule d'un Manche") {
    spair(Manche(1, 2)) shouldEqual 1
    spair(DerniereManche(5, 1, 2)) shouldEqual 5
  }

  test("un strike doit retourner les  deux boules d'une manche Manche"){
    strike(Manche(1,2))  shouldEqual 1+2
    strike(DerniereManche(5,1,2))  shouldEqual 5 + 1
  }

  test("deux strikes de suite égal  30"){
    Score(Manche(10, 0) :: Manche(10, 0) :: HNil) shouldEqual 30
  }

  test("Une manche 0 + 0  doit avoir un score de 0"){
    Score(Manche(0, 0)) shouldEqual 0
  }

  test("Une manche 3 + 1 doit avoir un score de 4") {
    Score(Manche(3, 1)) shouldEqual 4
  }

  test("Une manche 0 + 0 + 0 doit avoir un score de 0" ) {
    Score(DerniereManche(0, 0, 0)) shouldEqual 0
  }

  test("Deux manches 0 + 10 + 1 + 2  doit avoir un score de 0+10+1 + 1+2") {
    Score(Manche(0 ,  10  ) :: Manche(1 ,  2 )  :: HNil) shouldEqual 0 + 10 + 1 + 1 + 2
  }

  test("Deux manches  0+10 + 1+2  doit avoir un score de 0+10 + 1+1 + 2") {
    Score(Manche(0  ,  10  ) :: Manche( 1 ,  2 ) :: HNil) shouldEqual 0+10 + 1+1 + 2
  }

  test("Trois manches 1 + 2 +  + 1 +  2  +  1 + 2 doit avoir un score de  1+2 + 1+2 + 1+2") {
    Score(Manche(1, 2) :: Manche(1, 2) :: Manche(1, 2) :: HNil) shouldEqual 1+2 + 1+2 + 1+2
  }


  test("Trois manche 0+10 + 5+5 +  1+2 doit avoir un score de 0+10+ 5+5+5 + 1+1 + 2") {
    Score(Manche(0 , 10 ) :: Manche(5, 5) :: Manche(1 , 2 ) :: HNil) shouldEqual 0+10 + 5+5+5 + 1+1 + 2
  }

  test("Une manche 3+1 + 0+10 + 5+1 doit avoir un score de 3+1 + 0+10+5 + 5+1") {
    Score(Manche(3 , 1) ::  Manche(0 ,  10 ) :: Manche(5 ,  1 ) :: HNil ) shouldEqual 3+1 + 0+10+5 + 5+1
  }

  test("Une manche 10 + 2 + 5  doit avoir un score de 10 + 2+5 + 2+5") {
    Score(Manche(10 , 0 )   :: Manche(2 ,  5 )  :: HNil ) shouldEqual 10 + 2 +2 + 5+5
  }

  test("Une manche 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5+5 doit avoir un score de 5+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5+5"){
    val spair  = Manche(5 , 5 )
    val dernierSpair = DerniereManche(5 , 5 , 5)
    Score(spair :: spair :: spair :: spair :: spair :: spair :: spair :: spair :: spair :: dernierSpair :: HNil) shouldEqual 5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5 + 5+5+5+5
  }

  test("Une manche 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 doit doit avoir un score de 300") {
    val strike = Manche(10, 0)
    val dernierStrike = DerniereManche(10, 10, 10)
    Score(strike :: strike :: strike :: strike :: strike :: strike :: strike :: strike :: strike :: dernierStrike :: HNil) shouldEqual 300
  }

}
