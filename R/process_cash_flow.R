#' @title process_cash_flow
#' @description This function calls each of the function calls all the other function needed to creates financial statement.
#' @param input is the only parameter 'input' needed is the input files name.
#' @return this function returns a text file 'output.txt'
#' @export

process_cash_flow <- function(input) {
  output                 <- program_initialisation()
   # input                  <- list()
  output                 <- input_function(input, output)
  output$Portfolio$class <- find_portfolio_class(portfolio = output$Portfolio$portfolio) # find class of each element of the portfolio
  output                 <- statement(output = output)                                   # create statement object
  output                 <- fx(output = output)                                          # rebase fx in based reporting currency
  output                 <- iterate(output = output)
  output                 <- global_metrics(output = output)
  output                 <- format_tables(output = output)
  output                 <- rejig_output(output)

  sink("output.txt")
  print(output)
  sink()
  file.show("output.txt")
  file.remove("output.txt")

  return(output)
}


