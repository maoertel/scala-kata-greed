sealed trait DieInputValidationError {
  def errorMessage: String
}

case object AmountOfDiesError extends DieInputValidationError {
  def errorMessage: String = "Too many dies."
}

case object DieNotValid extends DieInputValidationError {
  def errorMessage: String = "Only dies from one to six are allowed."
}
