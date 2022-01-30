#' @title formatted_table
#' @description function that format tables in accounting format
#' @param data table to format
#' @param currency three letter currency code (character string) e.g.: "USD", "GBP", "KRW", "CHF", "EUR", "CAD"
#' @param interval number grouping e.g: 1,000,000 (normally 3, Korean 4), default = 3
#' @param mark number group separator (normally ",", French " " i.e.: space), default = ","
#' @param digits number of decimals to keep, default = "0"
#' @return a formatted table
#' @export

formatted_table <- function(data, currency, interval = 3, mark = ",", digits = 0) {
  table <- DT::datatable(data,
                     options = list(scrollX     = TRUE,
                                    pageLength  = 100,
                                    paging      = FALSE,
                                    searching   = FALSE,
                                    info        = FALSE,
                                    sort        = FALSE,
                                    rowCallback = DT::JS(
                                      'function(row,data, index) {
     if($(row)["0"]["_DT_RowIndex"] % 2 <1)
            $(row).css("background", "#8abad6")
     $(this.api().cell(index, 0).node())
     .css("text-align", "left")
     .css("font-weight", "bold");
   }')))

  if(!is.null(currency)) {

    for(columns in colnames(data)) {
      table <- DT::formatCurrency(table,
                              columns = columns,
                              currency = c("$", "$", "\UFFE1", "\U20A3", "\U20A9", "\U20AC")[currency == c("CAD","USD","GBP","CHF","KRW","EUR")],
                              interval,
                              mark,
                              digits,
                              dec.mark = getOption("OutDec"),
                              before = TRUE)
    }
  }

  if(is.null(currency)) {

    for(columns in colnames(data)) {
      table <- DT::formatPercentage(table,
                                columns = columns,
                                digits,
                                interval,
                                mark,
                                dec.mark = getOption("OutDec"))
    }
  }

  return(table)
}
