import org.scalatest._


object BowlingScore {
  def score(i: Int*): Int = {
    score(i.toList)
  }


  def score(i: List[Int]): Int = {
    i match {
      case Nil => 0
      case a :: b :: c :: Nil => a + b + c
      case a :: b :: c :: tail if a == 10 => a + b + c + score(b :: c :: tail)
      case a :: b :: c :: tail if a + b == 10 => a + b + c + score(c :: tail)
      case a :: b :: tail => a + b + score(tail)
    }
  }
}

class BowlingScoreSpec extends FlatSpec with ShouldMatchers {

  import BowlingScore._


  "Une manche 0 + 0" should " doit avoir un score de 0" in {
    score(0, 0) shouldEqual 0
  }

  "Une manche 3 + 1" should " doit avoir un score de 4" in {
    score(3, 1) shouldEqual 4
  }

  "Une manche 0 + 10 + 1 + 2" should " doit avoir un score de 0+10+1 + 1+2" in {
    score(0, 10, 1, 2) shouldEqual 0 + 10 + 1 + 1 + 2
  }


  "Une manche 0 + 10 + 5 + 5 +  1 + 2" should " doit avoir un score de 0+10+ 5*2+5 + 1*2 + 2" in {
    score(0, 10, 5, 5, 1, 2) shouldEqual 0 + 10 + 5 * 2 + 5 + 1 * 2 + 2
  }


  "Une manche 3+1 + 0+10 + 5+1" should " doit avoir un score de 3+1 + 0+10+5 + 5+1" in {
    score(3, 1, 0, 10, 5, 1) shouldEqual 3 + 1 + 0 + 10 + 5 + 5 + 1
  }

  "Une manche 10 + 2 + 5" should " doit avoir un score de 10 + 2+5 + 2+5" in {
    score(10, 2, 5, 0, 0) shouldEqual 10 + 2 + 5 + 2 + 5
  }

  "Une manche 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5 + 5+5+5" should " doit avoir un score de 5+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5 + 5*2+5+5" in {
    score(5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5) shouldEqual 5 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5 * 2 + 5 + 5
  }

  "Une manche 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 + 10 " should " doit avoir un score de 300" in {
    score(10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10) shouldEqual 300
  }
}
