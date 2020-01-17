import Rules.CompoundRule
import cats.data._
import cats.implicits._

object Greed {

  type DieValue = Int
  type Score = Int
  type ValidationResult[A] = ValidatedNel[DieInputValidationError, A]

  def score(dieValues: List[DieValue])(implicit rules: List[CompoundRule]): ValidationResult[Score] =
    (Validator.validateDieValues(dieValues), Validator.validateAmountOfDies(dieValues)).mapN { (_, _) =>
      val dieValueMap = dieValues.groupBy(identity).map { case (dieValue, amount) => (dieValue, amount.size) }
      val allPossibleScores = rules.map(rule => rule(dieValueMap))

      allPossibleScores.max
    }

}
