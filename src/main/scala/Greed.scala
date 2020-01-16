case class DieResult(die: Int, amount: Int)

object Greed {

  type DieValue = Int
  type Score = Int
  type SingleRule = DieResult => Score
  type CompoundRule = Map[DieValue, Int] => Score

  def score(dieValues: List[DieValue])(implicit rules: List[CompoundRule]): Score = {
    val valueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }
    val scores = rules.map(rule => rule(valueMap))

    scores.max
  }
}

object Rules {

  import Greed.{CompoundRule, DieValue, Score, SingleRule}

  private def evalSingleRule(cond: Boolean, score: Score): Score = if (cond) score else 0

  private def evalSpecialRule(
    maybeAmountOfDies: Option[Int],
    numOfDifferentDies: Int,
    amountOfDies: Int,
    score: Score
  ): DieValue = maybeAmountOfDies match {
    case Some(amount) if numOfDifferentDies == 1 => evalSingleRule(cond = amount == amountOfDies, score = score)
    case _ => 0
  }

  private val triples: SingleRule =
    dieResult => evalSingleRule(cond = dieResult.amount == 3, score = dieResult.die * 100)
  private val quadruples: SingleRule =
    dieResult => evalSingleRule(cond = dieResult.amount == 4, score = dieResult.die * 100 * 2)
  private val quintuples: SingleRule =
    dieResult => evalSingleRule(cond = dieResult.amount == 5, score = dieResult.die * 100 * 4)
  private val sextuples: SingleRule =
    dieResult => evalSingleRule(cond = dieResult.amount == 6, score = dieResult.die * 100 * 8)

  private val singleRules = triples :: quadruples :: quintuples :: sextuples :: Nil

  private val compoundOfSingleRules: CompoundRule = valMap =>
    valMap
      .map { case (dieValue, amount) => singleRules.map(rule => rule(DieResult(dieValue, amount))).max }
      .iterator
      .sum

  private val singleOne: CompoundRule =
    valMap => evalSpecialRule(maybeAmountOfDies = valMap.get(1), numOfDifferentDies = valMap.size, amountOfDies = 1, score = 100)
  private val tripleOne: CompoundRule =
    valMap => evalSpecialRule(maybeAmountOfDies = valMap.get(1), numOfDifferentDies = valMap.size, amountOfDies = 3, score = 1000)
  private val singleFive: CompoundRule =
    valMap => evalSpecialRule(maybeAmountOfDies = valMap.get(5), numOfDifferentDies = valMap.size, amountOfDies = 1, score = 50)

  private val straight: CompoundRule = valMap => if (valMap.size == 6) 1200 else 0
  private val threePairs: CompoundRule = { valMap =>
    val amounts = valMap.map { case (_, amount) => amount }.toList
    if (amounts.forall(_ == 2) && amounts.size == 3) 800 else 0
  }

  object Implicits {
    implicit val compoundRules: List[CompoundRule] =
      List(straight, threePairs, compoundOfSingleRules, singleOne, singleFive, tripleOne)
  }
}

object Main extends App {

  import Rules.Implicits._

  val greed1000 = Greed.score(List(1, 1, 1))
  val greed100 = Greed.score(List(1))
  val greed50 = Greed.score(List(5))
  val greed800 = Greed.score(List(2, 2, 3, 3, 5, 5))
  val greed0 = Greed.score(List(1, 2, 3))

  println(greed1000)
  println(greed100)
  println(greed50)
  println(greed800)
  println(greed0)
}