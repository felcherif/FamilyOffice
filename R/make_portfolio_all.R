#' @title make_portfolio_all
#' @description
#' function that test and find the names for a classes in the portfolio - function input : function(portfolio, class) where portfolio is a list and class is a string e.g.: find_portfolio_class(portfolio = output$Portfolio$portfolio, class = "employment_class")
#' @param user identity of the user.
#' @return this function returns the

make_portfolio_all <- function(user) {

  filepath <- FamilyOffice::user_choices(user)
  output <- list()
  source(file = filepath, local = TRUE)
  ptf <- list()

  for (p in ls()) {

    object <- eval(expr = parse(text = p))

    condition <-
      methods::is(object, class2 = "property_class") +
      methods::is(object, class2 = "debt_class") +
      methods::is(object, class2 = "employment_class") +
      methods::is(object, class2 = "expenses_class") +
      methods::is(object, class2 = "pension_class") +
      methods::is(object, class2 = "annuity_class")

    if (condition) {
      ptf[[p]] <- eval(expr = parse(text = p))
    }

  }

  for (p in ls()) {

    object <- eval(expr = parse(text = p))

    conditon <- methods::is(object, class2 = "cash_account_class")

    if (conditon) {
      ptf[[p]] <- eval(expr = parse(text = p))
    }

  }

  return(ptf)
}
