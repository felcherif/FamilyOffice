#' @title iterate
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @description iterate over time and portfolio steps
#' @param output is the only parameter needed.
#' @return this function returns a text file 'output'
#' @export

iterate <- function(output) {
  for (reporting_date in as.character(output$reporting$reporting_dates)) {
    for (portfolio_name in names(output$Portfolio$portfolio)) {
      output$reporting$portfolio_name <- portfolio_name
      output$reporting$reporting_date <- reporting_date
      output <- statement_total_program(reporting_date, X = output) # program that sums up all subsection of statements
      output <- cash_flow(x = output$Portfolio$portfolio[[portfolio_name]], output)
      output <- statement_total_program(reporting_date, X = output) # program that sums up all subsection of statements
    }
  }
  return(output)
}
