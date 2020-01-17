import Greed.{DieValue, Score}

object Rules {

  type SingleDieValueRule = DieResult => Score
  type CompoundRule = Map[DieValue, Int] => Score

  private def evalSingleDieValueRule(predicate: Boolean, score: Score): Score = if (predicate) score else 0

  private def evalCompoundRule(
      maybeAmountOfDies: Option[Int],
      numOfDifferentDies: Int,
      expectedAmountOfDies: Int,
      score: Score
  ): DieValue = maybeAmountOfDies match {
    case Some(amount) if numOfDifferentDies == 1 => evalSingleDieValueRule(predicate = amount == expectedAmountOfDies, score = score)
    case _ => 0
  }

  private val triples: SingleDieValueRule =
    dieResult => evalSingleDieValueRule(predicate = dieResult.amount == 3, score = dieResult.die * 100)
  private val quadruples: SingleDieValueRule =
    dieResult => evalSingleDieValueRule(predicate = dieResult.amount == 4, score = dieResult.die * 100 * 2)
  private val quintuples: SingleDieValueRule =
    dieResult => evalSingleDieValueRule(predicate = dieResult.amount == 5, score = dieResult.die * 100 * 4)
  private val sextuples: SingleDieValueRule =
    dieResult => evalSingleDieValueRule(predicate = dieResult.amount == 6, score = dieResult.die * 100 * 8)

  private val singleRules = triples :: quadruples :: quintuples :: sextuples :: Nil

  private val compoundOfSingleRules: CompoundRule = dieValueMap =>
    dieValueMap.map { case (dieValue, amount) => singleRules.map(rule => rule(DieResult(dieValue, amount))).max }.sum

  private val singleOne: CompoundRule = dieValueMap =>
    evalCompoundRule(maybeAmountOfDies = dieValueMap.get(1), numOfDifferentDies = dieValueMap.size, expectedAmountOfDies = 1, score = 100)

  private val tripleOne: CompoundRule = dieValueMap =>
    evalCompoundRule(maybeAmountOfDies = dieValueMap.get(1), numOfDifferentDies = dieValueMap.size, expectedAmountOfDies = 3, score = 1000)

  private val singleFive: CompoundRule = dieValueMap =>
    evalCompoundRule(maybeAmountOfDies = dieValueMap.get(5), numOfDifferentDies = dieValueMap.size, expectedAmountOfDies = 1, score = 50)

  private val straight: CompoundRule = dieValueMap => if (dieValueMap.size == 6) 1200 else 0

  private val threePairs: CompoundRule = { dieValueMap =>
    val amounts = dieValueMap.map { case (_, amount) => amount }.toList
    if (amounts.forall(_ == 2) && amounts.size == 3) 800 else 0
  }

  object Implicits {
    implicit val compoundRules: List[CompoundRule] =
      List(straight, threePairs, compoundOfSingleRules, singleOne, singleFive, tripleOne)
  }

}