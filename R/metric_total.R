#' @title metric_total
#' @description function that calculates sum of rows of financial statements to summarise the information and adds the metric to the statement
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param x is the only parameter needed is the part the financial statements to summarise.
#' @return this function returns a text file 'output'
#' @export

metric_total <- function(x) {
  return(rbind(x , "total" = colSums(x)))
}
