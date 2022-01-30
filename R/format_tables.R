#' @title format_tables
#' @description format tables with Excel like accounting number format
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns a text file 'output'
#' @export

format_tables <- function(output) {
  output$statement <- accounting_format(x = output$statement)

  for (m in names(output$Portfolio$portfolio)) {
    output$Portfolio$portfolio[[m]]@metrics <- accounting_format(x = output$Portfolio$portfolio[[m]]@metrics)
    output$Portfolio$portfolio[[m]]@statement <- accounting_format(x = output$Portfolio$portfolio[[m]]@statement)
  }

  output$reporting$age <- lapply(X = output$reporting$age, FUN = accounting_format)

  output$metrics <- Map(f = accounting_format, output$metrics)

  output$metrics$loan_to_value <-
    formattable::accounting(
      x = output$metrics$loan_to_value,
      digits = 2,
      zero.print = "."
    )

  # reduce the size of income tax tables to length of 10
  for (curr in names(output$economic_data$tax$income)) {
    temp <- output$economic_data$tax$income[[curr]]
    nrow <- nrow(temp)
    if (nrow > 10) {
      temp <- temp[seq(
        from = 1,
        to = nrow,
        by = nrow / 10
      ), ]
      rownames(temp) <- NULL
    }
    temp[, "threshold"] <- accounting_format(x = temp$threshold)
    temp[, "tax_rate"] <- round(temp$tax_rate, digits = 2)
    output$economic_data$tax$income[[curr]] <- temp
  }

  return(output)
}
