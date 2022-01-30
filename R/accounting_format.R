#' @title accounting_format
#' @description this function prints number in a nice accounting format
#' @param x is the object to apply the accounting format to
#' @param digits is the number of digits to use in printing the statement
#' @param zero.print is format to use for nil
#' @return this function the modified x
#' @export

accounting_format <- function(x, digits = 0, zero.print = ".") {
  if (is.list(x)) {
    lapply(X = x, FUN = accounting_format)
  } else {
    formattable::accounting(x = x, digits = 0, zero.print = ".")
  }
}
