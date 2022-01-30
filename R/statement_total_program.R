#' @title statement_total_program
#' @description program that sums up all subsection of statements for each reporting date; function that calculate the sub total for of a statement say all debt to total debt for a specified reporting date
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param reporting_date date of the projection
#' @param X statement to apply the sub-total
#' @return this function returns X modified
#' @export

statement_total_program <- function(X, reporting_date) {
  sub_total <- function(x, total = "total") {
    x[total, reporting_date] <-
      sum(x[rownames(x) != total, reporting_date])
    return(x)
  }

  if (isS4(X)) {
    Y <- X@statement
  } else {
    if (!isS4(X)) {
      Y <- X$statement
      Z <- X$Portfolio
    }
  }

  # output$statement$income_statement$income$salary
  Y$income_statement$income$salary <-
    sub_total(Y$income_statement$income$salary)

  # output$statement$income_statement$income$interest_earned
  Y$income_statement$income$interest_earned <-
    sub_total(Y$income_statement$income$interest_earned)

  # output$statement$income_statement$income$house_rental_income
  Y$income_statement$income$house_rental_income <-
    sub_total(Y$income_statement$income$house_rental_income)

  # output$statement$income_statement$income$gross_profit
  Y$income_statement$income$gross_profit["gross_profit", ] <-
    Y$income_statement$income$salary["total", ] +
    Y$income_statement$income$interest_earned["total", ] +
    Y$income_statement$income$house_rental_income["total", ]

  # output$statement$income_statement$expenses_taxes$interest_deductible
  Y$income_statement$expenses_taxes$interest_deductible <-
    sub_total(Y$income_statement$expenses_taxes$interest_deductible)

  # output$statement$income_statement$expenses_taxes$interest_non_deductible
  Y$income_statement$expenses_taxes$interest_non_deductible <-
    sub_total(Y$income_statement$expenses_taxes$interest_non_deductible)

  # output$statement$income_statement$expenses_taxes$income_tax
  Y$income_statement$expenses_taxes$income_tax <-
    sub_total(Y$income_statement$expenses_taxes$income_tax)

  # output$statement$income_statement$expenses_taxes$depreciation
  Y$income_statement$expenses_taxes$depreciation <-
    sub_total(Y$income_statement$expenses_taxes$depreciation)

  # output$statement$income_statement$expenses_taxes$personal_expenses
  if (exists("Z")) {
    if (length(Z$class$expenses_class)) {
      Y$income_statement$expenses_taxes$personal_expenses <-
        sub_total(Y$income_statement$expenses_taxes$personal_expenses)
    }
  }

  # output$statement$income_statement$expenses_taxes$property_expenses[[property]]
  if (exists("Z")) {
    for (property in Z$class$property_class)
    {
      Y$income_statement$expenses_taxes$property_expenses[[property]] <-
        sub_total(Y$income_statement$expenses_taxes$property_expenses[[property]])
    }
  }

  # output$statement$income_statement$expenses_taxes$property_expenses
  Y$income_statement$expenses_taxes$property_expenses$total <-
    sub_total(Y$income_statement$expenses_taxes$property_expenses$total)

  # output$statement$income_statement$expenses_taxes$total
  Y$income_statement$expenses_taxes$total["total", ] <-
    Y$income_statement$expenses_taxes$interest_deductible["total", ] +
    Y$income_statement$expenses_taxes$interest_non_deductible["total", ] +
    Y$income_statement$expenses_taxes$income_tax["total", ] +
    Y$income_statement$expenses_taxes$depreciation["total", ] +
    Y$income_statement$expenses_taxes$property_expenses$total["total", ]

  if (exists("Z")) {
    if (length(Z$class$expenses_class)) {
      Y$income_statement$expenses_taxes$total["total", ] <-
        Y$income_statement$expenses_taxes$total["total", ] +
        Y$income_statement$expenses_taxes$personal_expenses["total", ]
    }
  }

  # output$statement$income_statement$expenses_taxes$savings
  Y$income_statement$expenses_taxes$savings["savings", ] <-
    Y$income_statement$income$gross_profit["gross_profit", ] -
    Y$income_statement$expenses_taxes$total["total", ] +
    Y$income_statement$expenses_taxes$depreciation["total", ]

  # output$statement$balance_sheet$assets$cash
  Y$balance_sheet$assets$cash <- sub_total(Y$balance_sheet$assets$cash)

  # output$statement$balance_sheet$assets$property
  Y$balance_sheet$assets$property <- sub_total(Y$balance_sheet$assets$property)

  # output$statement$balance_sheet$assets$pension
  Y$balance_sheet$assets$pension <- sub_total(Y$balance_sheet$assets$pension)

  # output$statement$balance_sheet$assets$deferred_tax_asset
  Y$balance_sheet$assets$deferred_tax_asset <- sub_total(Y$balance_sheet$assets$deferred_tax_asset)

  # output$statement$balance_sheet$assets$total
  Y$balance_sheet$assets$total["total", ] <- Y$balance_sheet$assets$cash["total", ] + Y$balance_sheet$assets$property["total", ] + Y$balance_sheet$assets$pension["total", ] + Y$balance_sheet$assets$deferred_tax_asset["total", ]

  # output$statement$balance_sheet$liabilities$short_term_debt
  Y$balance_sheet$liabilities$short_term_debt <- sub_total(Y$balance_sheet$liabilities$short_term_debt)

  # output$statement$balance_sheet$liabilities$mortgage
  Y$balance_sheet$liabilities$mortgage <- sub_total(Y$balance_sheet$liabilities$mortgage)

  # output$statement$balance_sheet$liabilities$HELOC_balance
  Y$balance_sheet$liabilities$HELOC_balance <- sub_total(Y$balance_sheet$liabilities$HELOC_balance)

  # output$statement$balance_sheet$liabilities$refinancing_balance
  Y$balance_sheet$liabilities$refinancing_balance <- sub_total(Y$balance_sheet$liabilities$refinancing_balance)

  # output$statement$balance_sheet$liabilities$total
  Y$balance_sheet$liabilities$total["total", ] <- Y$balance_sheet$liabilities$short_term_debt["total", ] + Y$balance_sheet$liabilities$mortgage["total", ] + Y$balance_sheet$liabilities$HELOC_balance["total", ] + Y$balance_sheet$liabilities$refinancing_balance["total", ]

  # output$statement$balance_sheet$equity$total
  Y$balance_sheet$equity$total["total", ] <- Y$balance_sheet$assets$total["total", ] - Y$balance_sheet$liabilities$total["total", ]

  # output$statement$balance_sheet$equity$total_WO_DTA
  Y$balance_sheet$equity$total_WO_DTA["total_WO_DTA", ] <- Y$balance_sheet$equity$total["total", ] - Y$balance_sheet$assets$deferred_tax_asset["total", ]

  # output$statement$cash_flow_statement$operating_activities$salary_income
  Y$cash_flow_statement$operating_activities$salary_income <- sub_total(Y$cash_flow_statement$operating_activities$salary_income)

  # output$statement$cash_flow_statement$operating_activities$rental_income
  Y$cash_flow_statement$operating_activities$rental_income <- sub_total(Y$cash_flow_statement$operating_activities$rental_income)

  # output$statement$cash_flow_statement$operating_activities$income_taxes_paid
  Y$cash_flow_statement$operating_activities$income_taxes_paid <- sub_total(Y$cash_flow_statement$operating_activities$income_taxes_paid)

  # output$statement$cash_flow_statement$operating_activities$personal_expense
  if (exists("Z")) {
    if (length(Z$class$expenses_class)) {
      Y$cash_flow_statement$operating_activities$personal_expense <- sub_total(Y$cash_flow_statement$operating_activities$personal_expense)
    }
  }

  # output$statement$cash_flow_statement$operating_activities$property_expenses
  Y$cash_flow_statement$operating_activities$property_expenses <- sub_total(Y$cash_flow_statement$operating_activities$property_expenses)

  # output$statement$cash_flow_statement$operating_activities$operating_cash_flows
  Y$cash_flow_statement$operating_activities$operating_cash_flows["total", ] <- Y$cash_flow_statement$operating_activities$salary_income["total", ] + Y$cash_flow_statement$operating_activities$rental_income["total", ] - Y$cash_flow_statement$operating_activities$income_taxes_paid["total", ] - Y$cash_flow_statement$operating_activities$property_expenses["total", ]

  if (exists("Z")) {
    if (length(Z$class$expenses_class)) {
      Y$cash_flow_statement$operating_activities$operating_cash_flows["total", ] <- Y$cash_flow_statement$operating_activities$operating_cash_flows["total", ] - Y$cash_flow_statement$operating_activities$personal_expense["total", ]
    }
  }

  # output$statement$cash_flow_statement$investing_activities$dividends_received
  Y$cash_flow_statement$investing_activities$dividends_received <- sub_total(Y$cash_flow_statement$investing_activities$dividends_received)

  # output$statement$cash_flow_statement$investing_activities$interest_earned
  Y$cash_flow_statement$investing_activities$interest_earned <- sub_total(Y$cash_flow_statement$investing_activities$interest_earned)

  # output$statement$cash_flow_statement$investing_activities$investing_cash_flows
  Y$cash_flow_statement$investing_activities$investing_cash_flows["total", ] <- Y$cash_flow_statement$investing_activities$dividends_received["total", ] + Y$cash_flow_statement$investing_activities$interest_earned["total", ]

  # output$statement$cash_flow_statement$financing_activities$short_term_debt_repayment
  Y$cash_flow_statement$financing_activities$short_term_debt_repayment <- sub_total(Y$cash_flow_statement$financing_activities$short_term_debt_repayment)

  # output$statement$cash_flow_statement$financing_activities$mortgage_repayment
  Y$cash_flow_statement$financing_activities$mortgage_repayment <- sub_total(Y$cash_flow_statement$financing_activities$mortgage_repayment)

  # output$statement$cash_flow_statement$financing_activities$HELOC
  Y$cash_flow_statement$financing_activities$HELOC <- sub_total(Y$cash_flow_statement$financing_activities$HELOC)

  # output$statement$cash_flow_statement$financing_activities$interest_paid_deductible
  Y$cash_flow_statement$financing_activities$interest_paid_deductible <- sub_total(Y$cash_flow_statement$financing_activities$interest_paid_deductible)

  # output$statement$cash_flow_statement$financing_activities$interest_paid_non_deductible
  Y$cash_flow_statement$financing_activities$interest_paid_non_deductible <- sub_total(Y$cash_flow_statement$financing_activities$interest_paid_non_deductible)

  # output$statement$cash_flow_statement$financing_activities$equity_refinanced
  Y$cash_flow_statement$financing_activities$equity_refinanced <- sub_total(Y$cash_flow_statement$financing_activities$equity_refinanced)

  # output$statement$cash_flow_statement$financing_activities$financing_cash_flows
  Y$cash_flow_statement$financing_activities$financing_cash_flows["total", ] <-
    Y$cash_flow_statement$financing_activities$HELOC["total", ] -
    Y$cash_flow_statement$financing_activities$interest_paid_deductible["total", ] -
    Y$cash_flow_statement$financing_activities$interest_paid_non_deductible["total", ] -
    Y$cash_flow_statement$financing_activities$short_term_debt_repayment["total", ] -
    Y$cash_flow_statement$financing_activities$mortgage_repayment["total", ] +
    Y$cash_flow_statement$financing_activities$equity_refinanced["total", ]

  # output$statement$cash_flow_statement$total$end_of_period_cash
  Y$cash_flow_statement$total$end_of_period_cash["end_of_period_cash", ] <- Y$balance_sheet$assets$cash["total", ]

  # output$statement$cash_flow_statement$total$increase_in_cash
  Y$cash_flow_statement$total$increase_in_cash["increase_in_cash", ] <-
    Y$cash_flow_statement$operating_activities$operating_cash_flows["total", ] +
    Y$cash_flow_statement$investing_activities$investing_cash_flows["total", ] +
    Y$cash_flow_statement$financing_activities$financing_cash_flows["total", ]

  # output$statement$cash_flow_statement$total$beginning_of_period_cash
  Y$cash_flow_statement$total$beginning_of_period_cash["beginning_of_period_cash", ] <-
    Y$cash_flow_statement$total$end_of_period_cash["end_of_period_cash", ] -
    Y$cash_flow_statement$total$increase_in_cash["increase_in_cash", ]

  if (!isS4(X)) {
    X$statement <- Y
  } else {
    if (isS4(X)) {
      X@statement <- Y
    }
  }

  return(X)
}
