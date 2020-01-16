case class DieResult(die: Int, amount: Int)

object Greed {

  type DieValue = Int
  type Score = Int
  type GeneralRule = DieResult => Score
  type SpecialRule = Map[DieValue, Int] => Score

  def score(dieValues: List[DieValue])(implicit specialRules: List[SpecialRule]): Score = {
    val valueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }
    val specialScores = specialRules.map(rule => rule(valueMap))

    specialScores.max
  }
}

object Rules {

  import Greed.{GeneralRule, SpecialRule}

  private val singleOne: GeneralRule = dieResult => if (dieResult.die == 1 && dieResult.amount == 1) 100 else 0
  private val tripleOne: GeneralRule = dieResult => if (dieResult.die == 1 && dieResult.amount == 3) 1000 else 0
  private val singleFive: GeneralRule = dieResult => if (dieResult.die == 5 && dieResult.amount == 1) 50 else 0

  private val triples: GeneralRule = dieResult => if (dieResult.amount == 3) dieResult.die * 100 else 0
  private val quadruples: GeneralRule = dieResult => if (dieResult.amount == 4) dieResult.die * 100 * 2 else 0
  private val quintuples: GeneralRule = dieResult => if (dieResult.amount == 5) dieResult.die * 100 * 4 else 0
  private val sextuples: GeneralRule = dieResult => if (dieResult.amount == 6) dieResult.die * 100 * 8 else 0

  private val singleSpecialRules = List(singleOne, singleFive, tripleOne)
  private val singleRules = List(triples, quadruples, quintuples, sextuples)
  private val compoundSingleRules = singleRules ++ singleSpecialRules

  private val compoundRules: SpecialRule = valMap => valMap
    .map { case (dieValue, amount) => compoundSingleRules.map(rule => rule(DieResult(dieValue, amount))).max }
    .iterator
    .sum
  private val straight: SpecialRule = valMap => if (valMap.size == 6) 1200 else 0
  private val threePairs: SpecialRule = { valMap =>
    val amounts = valMap.map { case (_, amount) => amount }.toList
    if (amounts.forall(_ == 2) && amounts.size == 3) 800 else 0
  }

  object Implicits {
    implicit val specialRules: List[SpecialRule] = straight :: threePairs :: compoundRules :: Nil
  }

}

object Main extends App {

  import Rules.Implicits._

  val greed1000 = Greed.score(List(1, 1, 1))
  val greed100 = Greed.score(List(1))
  val greed50 = Greed.score(List(5))
  val greed800 = Greed.score(List(2, 2, 3, 3, 5, 5))
  val greed0 = Greed.score(List(1, 2, 3))

}