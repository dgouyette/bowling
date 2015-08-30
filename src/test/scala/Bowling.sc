import shapeless._

type Manche = Int :: Int :: HNil
type Heterogene = Int :: String :: Boolean :: List[Int] :: HNil
type DerniereManche = Int :: Int :: Int ::  HNil
type JeuUneManche = Manche :: HNil
type JeuDeuxManches = Manche :: Manche ::  HNil
type JeuMancheMixtes = Manche :: Manche :: DerniereManche :: HNil


object Score extends Poly1 {
  implicit def caseManche  = at[Manche](manche => manche.head + manche.tail.reduceLeft(Add))
  implicit def caseDerniereManche  = at[DerniereManche](manche => manche.head + Score(manche.tail))
  implicit def caseJeu = at[JeuUneManche](jeu =>  Score(jeu.head))
  implicit def caseJeu2 = at[JeuDeuxManches](jeux2Manches2Score )

  def jeux2Manches2Score: (JeuDeuxManches) => Int = {
    jeu =>
      (jeu.head, jeu.tail) match {
        case ((h1 :: h2 :: HNil), (t1 :: t2 :: HNil) :: _) if h1 == 10 => h1 + t1 + t1 + t2 + t2
        case ((h1 :: h2 :: HNil), (t1 :: t2 :: HNil) :: _) if h1 + h2 == 10 => h1 + h2 + t1 + t1 + t2
        case _ => Score(jeu.head) + jeu.tail.map(Score).reduceLeft(Add)
      }
  }

  implicit def caseJeu3 = at[JeuMancheMixtes](jeu =>  Score(jeu.head) + Score(jeu.tail.head)  + Score(jeu.tail.tail.head))
}



object Add extends Poly2 {
  implicit def caseInt = at[Int, Int]( _ + _ )
}
val maVar : Heterogene  = 1 :: "marouane" :: true :: List(1) :: HNil


val manche : Manche = 2 ::  3 :: HNil
val scoreManche= Score(manche)
assert(scoreManche == 5)

val derniereManche : DerniereManche = 5 :: 5 :: 10 :: HNil
val scoreDerniereManche= Score(derniereManche)
assert(scoreDerniereManche == 20)

val gameUneManche : JeuUneManche = (1 :: 2 :: HNil)  :: HNil
val scoreGameUneMAnche = Score(gameUneManche)
assert(scoreGameUneMAnche == 3)

val gameDeuxManche : JeuDeuxManches  = (1 :: 2 :: HNil) :: ( 3 :: 4 :: HNil) :: HNil
val scoreGameDeuxManches = Score(gameDeuxManche)
assert(scoreGameDeuxManches == 10)

val jeuMixte:JeuMancheMixtes  = (1 :: 2 :: HNil) :: ( 5 :: 5 :: HNil) :: ( 10 :: 5 :: 5 ::  HNil) :: HNil
val scoreJeuMixt = Score(jeuMixte)
assert(scoreJeuMixt == 33)

val gameDeuxMancheSpair : JeuDeuxManches  = (5 :: 5 :: HNil) :: ( 2 :: 1 :: HNil) :: HNil
val scoreGameDeuxManchesSpair = Score(gameDeuxMancheSpair)
assert(scoreGameDeuxManchesSpair == 5 + 5+ 2+ 2 +1)

val gameDeuxMancheStrike : JeuDeuxManches  = (10 :: 0 :: HNil) :: ( 3 :: 4:: HNil) :: HNil
val scoreGameDeuxManchesStrike = Score(gameDeuxMancheStrike)
assert(scoreGameDeuxManchesStrike == 10 + 0+ 3+3  + 4+4)