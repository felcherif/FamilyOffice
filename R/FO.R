#' @title FO
#' @description family office wrapping function. This function calculate the financial statement projections.
#' @param input is the only parameter 'input' needed is the input files name.
#' @return this function returns a text file 'output.txt'
#' @export

FO <- function(input) {
  process_cash_flow(input)
}
