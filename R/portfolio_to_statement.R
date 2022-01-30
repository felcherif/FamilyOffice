#' @title portfolio_to_statement
#' @description function that calculates the impact of a portfolio object 'portfolio_name' on the financial statement for a specifc date
#' @param output is the only parameter 'input' needed is the input files name.
#' @param portfolio_name is the only parameter 'input' needed is the input files name.
#' @param date is the only parameter 'input' needed is the input files name.
#' @return this function returns 'output' with a modified statement
#' @export

portfolio_to_statement <- function(output, portfolio_name, date) {

  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`

  # income statement - income
  output$statement$income_statement$income$salary[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$salary[, date]
  output$statement$income_statement$income$interest_earned[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$interest_earned[, date]
  output$statement$income_statement$income$house_rental_income[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$house_rental_income[, date]
  output$statement$income_statement$income$gross_profit[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$gross_profit[, date]

  # income statement - expenses & taxes
  output$statement$income_statement$expenses_taxes$interest_deductible[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_deductible[, date]
  output$statement$income_statement$expenses_taxes$interest_non_deductible[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_non_deductible[, date]
  output$statement$income_statement$expenses_taxes$income_tax[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[, date]
  output$statement$income_statement$expenses_taxes$depreciation[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$depreciation[, date]
  output$statement$income_statement$expenses_taxes$personal_expenses[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$personal_expenses[, date]
  for (property in output$Portfolio$class$property) {
    output$statement$income_statement$expenses_taxes$property_expenses[[property]][, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$property_expenses[[property]][, date]
  }
  output$statement$income_statement$expenses_taxes$property_expenses$total[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$property_expenses$total[, date]
  output$statement$income_statement$expenses_taxes$total[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$total[, date]
  output$statement$income_statement$expenses_taxes$savings[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$savings[, date]

  # balance sheet - assets
  output$statement$balance_sheet$assets$cash[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[, date]
  output$statement$balance_sheet$assets$property[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$property[, date]
  output$statement$balance_sheet$assets$pension[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$pension[, date]
  output$statement$balance_sheet$assets$deferred_tax_asset[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$deferred_tax_asset[, date]

  # balance sheet - liabilities
  output$statement$balance_sheet$liabilities$short_term_debt[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$short_term_debt[, date]
  output$statement$balance_sheet$liabilities$mortgage[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$mortgage[, date]
  output$statement$balance_sheet$liabilities$HELOC_balance[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$HELOC_balance[, date]
  output$statement$balance_sheet$liabilities$refinancing_balance[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$refinancing_balance[, date]
  output$statement$balance_sheet$liabilities$total[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$total[, date]

  # balance sheet - equity
  output$statement$balance_sheet$equity$total[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total[, date]

  # cash flow statement - operating activities
  output$statement$cash_flow_statement$operating_activities$salary_income[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$salary_income[, date]
  output$statement$cash_flow_statement$operating_activities$rental_income[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$rental_income[, date]
  output$statement$cash_flow_statement$operating_activities$income_taxes_paid[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[, date]
  output$statement$cash_flow_statement$operating_activities$personal_expense[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$personal_expense[, date]
  output$statement$cash_flow_statement$operating_activities$property_expenses[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$property_expenses[, date]
  output$statement$cash_flow_statement$operating_activities$operating_cash_flows[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$operating_cash_flows[, date]

  # cash flow statement - investing activities
  output$statement$cash_flow_statement$investing_activities$dividends_received[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$investing_activities$dividends_received[, date]
  output$statement$cash_flow_statement$investing_activities$interest_earned[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$investing_activities$interest_earned[, date]
  output$statement$cash_flow_statement$investing_activities$investing_cash_flows[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$investing_activities$investing_cash_flows[, date]

  # cash flow statement - financing activities
  output$statement$cash_flow_statement$financing_activities$short_term_debt_repayment[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$short_term_debt_repayment[, date]
  output$statement$cash_flow_statement$financing_activities$mortgage_repayment[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$mortgage_repayment[, date]
  output$statement$cash_flow_statement$financing_activities$HELOC[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$HELOC[, date]
  output$statement$cash_flow_statement$financing_activities$interest_paid_deductible[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_deductible[, date]
  output$statement$cash_flow_statement$financing_activities$interest_paid_non_deductible[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_non_deductible[, date]
  output$statement$cash_flow_statement$financing_activities$equity_refinanced[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$equity_refinanced[, date]
  output$statement$cash_flow_statement$financing_activities$financing_cash_flows[, date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$financing_cash_flows[, date]

  # cash flow statement - total
  output$statement$cash_flow_statement$total$beginning_of_period_cash["beginning_of_period_cash", date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$beginning_of_period_cash["beginning_of_period_cash", date]
  output$statement$cash_flow_statement$total$end_of_period_cash["end_of_period_cash", date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$end_of_period_cash["end_of_period_cash", date]
  output$statement$cash_flow_statement$total$interest_earned["interest_earned", date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$interest_earned["interest_earned", date]
  output$statement$cash_flow_statement$total$increase_in_cash["increase_in_cash", date] %+=% output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$total$increase_in_cash["increase_in_cash", date]

  return(output)
}
