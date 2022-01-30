#' @title cash_flow
#' @description this function applies the different financial cash flow projection function depending on the class of 'x'
#' @param x is the object to apply the financial function to
#' @param output is object containing all data
#' @return output with a modified statement
#' @export

cash_flow <- function(x, output) {
  switch(class(x)[1],
         debt_class         = debt(x, output),
         employment_class   = employment(x, output),
         property_class     = property(x, output),
         expenses_class     = expenses(x, output),
         cash_account_class = cash_account(x, output),
         pension_class      = pension(x, output),
         annuity_class      = annuity(x, output),
         warning("cash flow function applied to non-defined class")
  )
}
