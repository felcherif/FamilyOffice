#' @title find_portfolio_class
#' @description function that test and find the names for a classes in the portfolio - function input : function(portfolio, class) where portfolio is a list and class is a string e.g.: find_portfolio_class(portfolio = output$Portfolio$portfolio, class = "employment_class")
#' @param portfolio this is the list of all financial information (e.g. asset and liability)
#' @return this function returns the class of 'portfolio'
#' @export

find_portfolio_class <- function(portfolio) {

  class <- list()

  for (p in names(portfolio)) {

    class$all <- c(p, class$all)

    if (methods::is(object = portfolio[[p]], class2 = "property_class")) {
      class$property_class <- c(p, class$property_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "debt_class")) {
      class$debt_class <- c(p, class$debt_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "employment_class")) {
      class$employment_class <- c(p, class$employment_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "expenses_class")) {
      class$expenses_class <- c(p, class$expenses_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "cash_account_class")) {
      class$cash_account_class <- c(p, class$cash_account_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "pension_class")) {
      class$pension_class <- c(p, class$pension_class)
    }

    if (methods::is(object = portfolio[[p]], class2 = "annuity_class")) {
      class$annuity_class <- c(p, class$annuity_class)
    }

  }

  return(class)
}

