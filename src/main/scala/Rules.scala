import Greed.{DieValue, Score}

object Rules {

  type SingleRule = DieResult => Score
  type CompoundRule = Map[DieValue, Int] => Score

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