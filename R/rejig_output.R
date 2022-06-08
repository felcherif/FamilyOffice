#' @title rejig_output
#' @description funcion that calculates the totals and subtotals for the financial statements
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns the output with modified statements
#' @export

rejig_output <- function(output) {

  personal_expenses <- output$statement$income_statement$expenses_taxes$personal_expenses["total", ]

  if (is.null(personal_expenses)) {
    gross_profit <- output$statement$income_statement$income$gross_profit
    personal_expenses <- rep(x = 0, length(gross_profit))
  }

  salary_income <- output$statement$cash_flow_statement$operating_activities$salary_income["total", ]

  if (is.null(salary_income)) {
    gross_profit <- output$statement$income_statement$income$gross_profit
    salary_income <- rep(x = 0, length(gross_profit))
  }

  personal_expenses_2 <- output$statement$cash_flow_statement$operating_activities$personal_expense["total", ]

  if (is.null(personal_expenses_2)) {
    gross_profit <- output$statement$income_statement$income$gross_profit
    personal_expenses_2 <- rep(x = 0, length(gross_profit))
  }


  income_statement <-
    accounting_format(matrix(
      data = c(
        output$statement$income_statement$income$salary["total", ],
        output$statement$income_statement$income$interest_earned["total", ],
        output$statement$income_statement$income$house_rental_income["total", ],
        output$statement$income_statement$income$gross_profit,
        output$statement$income_statement$expenses_taxes$interest_deductible["total", ],
        output$statement$income_statement$expenses_taxes$interest_non_deductible["total", ],
        output$statement$income_statement$expenses_taxes$income_tax["total", ],
        output$statement$income_statement$expenses_taxes$depreciation["total", ],
        personal_expenses,
        output$statement$income_statement$expenses_taxes$property_expenses$total["total", ],
        output$statement$income_statement$expenses_taxes$total["total", ],
        output$statement$income_statement$expenses_taxes$savings["savings", ]
      ),
      nrow = 12,
      byrow = TRUE,
      dimnames = list(
        c(
          "salary",
          "interest_earned",
          "house_rental_income",
          "gross_profit",
          "interest_deductible",
          "interest_non_deductible",
          "income_tax",
          "depreciation",
          "personal_expenses",
          "property_expenses",
          "total_expense",
          "savings"
        ),
        as.character(output$reporting$reporting_dates)
      )
    ))

  balance_sheet <-
    accounting_format(matrix(
      data = c(
        output$statement$balance_sheet$assets$cash["total", ],
        output$statement$balance_sheet$assets$property["total", ],
        output$statement$balance_sheet$assets$pension["total", ],
        output$statement$balance_sheet$assets$deferred_tax_asset["total", ],
        output$statement$balance_sheet$assets$total["total", ],
        output$statement$balance_sheet$liabilities$short_term_debt["total", ],
        output$statement$balance_sheet$liabilities$mortgage["total", ],
        output$statement$balance_sheet$liabilities$HELOC_balance["total", ],
        output$statement$balance_sheet$liabilities$refinancing_balance["total", ],
        output$statement$balance_sheet$liabilities$total["total", ],
        output$statement$balance_sheet$equity$total["total", ],
        output$statement$balance_sheet$equity$total_WO_DTA["total_WO_DTA", ]
      ),
      nrow = 12,
      byrow = TRUE,
      dimnames = list(
        c(
          "cash",
          "property",
          "pension",
          "deferred_tax_asset",
          "total_asset",
          "short_term_debt",
          "mortgage",
          "HELOC_balance",
          "refinancing_balance",
          "total_liability",
          "equity_total",
          "equity_total_WO_DTA"
        ),
        as.character(output$reporting$reporting_dates)
      )
    ))

  cash_flow <- accounting_format(matrix(
    data = c(
      salary_income,
      output$statement$cash_flow_statement$operating_activities$rental_income["total", ],
      output$statement$cash_flow_statement$operating_activities$income_taxes_paid["total", ],
      personal_expenses_2,
      output$statement$cash_flow_statement$operating_activities$property_expenses["total", ],
      output$statement$cash_flow_statement$operating_activities$operating_cash_flows["total", ],
      output$statement$cash_flow_statement$investing_activities$dividends_received["total", ],
      output$statement$cash_flow_statement$investing_activities$interest_earned["total", ],
      output$statement$cash_flow_statement$investing_activities$investing_cash_flows["total", ],
      output$statement$cash_flow_statement$financing_activities$short_term_debt_repayment["total", ],
      output$statement$cash_flow_statement$financing_activities$mortgage_repayment["total", ],
      output$statement$cash_flow_statement$financing_activities$HELOC["total", ],
      output$statement$cash_flow_statement$financing_activities$interest_paid_deductible["total", ],
      output$statement$cash_flow_statement$financing_activities$interest_paid_non_deductible["total", ],
      output$statement$cash_flow_statement$financing_activities$equity_refinanced["total", ],
      output$statement$cash_flow_statement$financing_activities$financing_cash_flows["total", ],
      output$statement$cash_flow_statement$total$beginning_of_period_cash,
      output$statement$cash_flow_statement$total$end_of_period_cash,
      output$statement$cash_flow_statement$total$interest_earned,
      output$statement$cash_flow_statement$total$increase_in_cash
    ),
    nrow = 20,
    byrow = TRUE,
    dimnames = list(
      c(
        "salary_income",
        "rental_income",
        "income_taxes_paid",
        "personal_expense",
        "property_expenses",
        "operating_cash_flows",
        "dividends_received",
        "interest_earned",
        "investing_cash_flows",
        "short_term_debt_repayment",
        "mortgage_repayment",
        "HELOC",
        "interest_paid_deductible",
        "interest_paid_non_deductible",
        "equity_refinanced",
        "financing_cash_flows",
        "beginning_of_period_cash",
        "end_of_period_cash",
        "interest_earned",
        "increase_in_cash"
      ),
      as.character(output$reporting$reporting_dates)
    )
  ))

  simple <- list(income_statement = income_statement, balance_sheet = balance_sheet, cash_flow = cash_flow)

  statement <- list(simple = simple, detailed = output$statement)

  output <- list(
    statement     = statement,
    metrics       = output$metrics,
    reporting     = output$reporting,
    economic_data = output$economic_data,
    Portfolio     = output$Portfolio,
    input_file    = output$input_file
  )

  return(output)

}
