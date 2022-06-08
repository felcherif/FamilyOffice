#' @title global_metrics
#' @description produces all the global metrics in the financial statements
#' @param output all in memory
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @return output with updated global metrics
#' @export

global_metrics <- function(output) {

  output$metrics$effective_tax_rate <-
    output$statement$income_statement$expenses_taxes$income_tax["total", , drop = FALSE] /
    output$statement$income_statement$income$gross_profit["gross_profit", , drop = FALSE]

  output$metrics$overall_leverage_ratio <-
    output$statement$balance_sheet$liabilities$total["total", , drop = FALSE] /
    output$statement$balance_sheet$equity$total_WO_DTA["total_WO_DTA", , drop = FALSE]

  # equity change
  total_equity_WO_DTA_change <- c(0, diff(x = as.numeric(output$statement$balance_sheet$equity$total_WO_DTA["total_WO_DTA", ])))
  output$statement$balance_sheet$equity$total_WO_DTA <- rbind(output$statement$balance_sheet$equity$total_WO_DTA, total_equity_WO_DTA_change)

  Properties <- NULL

  for (portfolio_name in names(output$Portfolio$portfolio)) {

    if (class(output$Portfolio$portfolio[[portfolio_name]]) == "property_class") {

      Properties <- c(portfolio_name, Properties)

      output$metrics$savings <- rbind(output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$savings["savings", ], output$metrics$savings)

      output$metrics$cash <- rbind(output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash["total", ], output$metrics$cash)

      output$metrics$operating_cash_flows <-
        rbind(output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$operating_cash_flows["total", ], output$metrics$operating_cash_flows)

      output$metrics$financing_cash_flows <-
        rbind(output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$financing_cash_flows["total", ], output$metrics$financing_cash_flows)

      output$metrics$increase_in_cash <-
        rbind(output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$increase_in_cash["increase_in_cash", , drop = FALSE], output$metrics$increase_in_cash)

      # equity change
      output$metrics$equity_WO_DTA_change <- rbind(
        output$metrics$equity_WO_DTA_change,
        matrix(data = c(0, diff(x = as.numeric(output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total_WO_DTA["total_WO_DTA", ]))), nrow = 1, dimnames = list(portfolio_name,
                                                                                                                                                                                        as.character(output$reporting$reporting_dates))))
      # mortgage interest rate
      output$metrics$mortgage_interest_rate <- rbind(output$Portfolio$portfolio[[portfolio_name]]@metrics$metric["mortgage_interest_rate",], output$metrics$mortgage_interest_rate)

      # HELOC interest rate
      output$metrics$HELOC_interest_rate <- rbind(output$Portfolio$portfolio[[portfolio_name]]@metrics$metric["HELOC_interest_rate",], output$metrics$HELOC_interest_rate)

      # equity_available_for_refinancing
      output$metrics$equity_available_for_refinancing <-
        rbind(pmax(output$Portfolio$portfolio[[portfolio_name]]@refinancing_loan_to_value_max -
                     (output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$mortgage[portfolio_name, ] +
                        output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$HELOC_balance[portfolio_name, ] +
                        output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$refinancing_balance[portfolio_name, ]) /
                     output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$property[portfolio_name, ], 0) *
                output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$property[portfolio_name, ], output$metrics$equity_available_for_refinancing)

      # loan to value - I
      output$metrics$loan_to_value <-
        rbind((output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$mortgage[portfolio_name, ] +
                 output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$HELOC_balance[portfolio_name, ] +
                 output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$refinancing_balance[portfolio_name, ]) /
                output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$property[portfolio_name, ], output$metrics$loan_to_value)
    } else {
      next
    }
  }

  if (is.null(Properties)) {
    return(output)
  }

  output$metrics$savings <- metric_total(output$metrics$savings)
  output$metrics$cash <- metric_total(output$metrics$cash)
  output$metrics$operating_cash_flows <- metric_total(output$metrics$operating_cash_flows)
  output$metrics$financing_cash_flows <- metric_total(output$metrics$financing_cash_flows)
  output$metrics$increase_in_cash <- metric_total(output$metrics$increase_in_cash)
  output$metrics$equity_WO_DTA_change <- metric_total(output$metrics$equity_WO_DTA_change)
  output$metrics$equity_available_for_refinancing <- metric_total(output$metrics$equity_available_for_refinancing)

  # loan to value - II
  output$metrics$loan_to_value <- rbind(output$metrics$loan_to_value,
                                        "total" = (output$statement$balance_sheet$liabilities$mortgage["total", ] +
                                                     output$statement$balance_sheet$liabilities$refinancing_balance["total", ] +
                                                     output$statement$balance_sheet$liabilities$HELOC_balance["total", ]) /
                                          output$statement$balance_sheet$assets$property["total", ])

  rownames(output$metrics$savings) <-
    rownames(output$metrics$cash) <-
    rownames(output$metrics$operating_cash_flows) <-
    rownames(output$metrics$financing_cash_flows) <-
    rownames(output$metrics$increase_in_cash) <-
    rownames(output$metrics$equity_available_for_refinancing) <-
    rownames(output$metrics$loan_to_value) <-
    c(Properties, "total")

  rownames(output$metrics$mortgage_interest_rate) <-
    rownames(output$metrics$HELOC_interest_rate) <-
    Properties


  colnames(output$metrics$savings) <-
    colnames(output$metrics$cash) <-
    colnames(output$metrics$financing_cash_flows) <-
    colnames(output$metrics$increase_in_cash) <-
    colnames(output$metrics$loan_to_value) <-
    colnames(output$metrics$equity_available_for_refinancing) <-
    as.character(output$reporting$reporting_dates)

  return(output)

}
