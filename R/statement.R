#' @title statement
#' @description function that creates the statement
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns a 'output'
#' @export

statement <- function(output) {
  projection_length_in_years <- output$reporting$projection_length_in_years
  birthdate <- output$reporting$birthdate
  frequency <- output$reporting$reporting_frequency
  reporting_dates <- output$reporting$reporting_dates

  for (taxpayer in output$reporting$taxpayer)
  {
    output$reporting$age[[taxpayer]] <-
      matrix(
        data = (as.Date(reporting_dates) - as.Date(birthdate[[taxpayer]])) / 365.25,
        nrow = 1,
        dimnames = list(NULL, as.character(reporting_dates))
      ) # define age at each reporting date
  }

  # define all statements to be produced by the program
  output$statement$income_statement$income$salary <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$employment_class) + length(output$Portfolio$class$pension_class) + length(output$Portfolio$class$annuity_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$employment_class,
          output$Portfolio$class$pension_class,
          output$Portfolio$class$annuity_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )


  output$statement$income_statement$income$interest_earned <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$cash_account_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$cash_account_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$income$house_rental_income <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$income$gross_profit <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("gross_profit"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$expenses_taxes$interest_deductible <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$expenses_taxes$interest_non_deductible <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class) + length(output$Portfolio$class$debt_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$property_class,
          output$Portfolio$class$debt_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$expenses_taxes$income_tax <-
    matrix(
      data = 0,
      nrow = 1 +
        length(output$Portfolio$class$property_class) +
        length(output$Portfolio$class$employment_class) +
        length(output$Portfolio$class$pension_class) +
        length(output$Portfolio$class$annuity_class) +
        length(output$Portfolio$class$cash_account_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$property_class,
          output$Portfolio$class$employment_class,
          output$Portfolio$class$pension_class,
          output$Portfolio$class$annuity_class,
          output$Portfolio$class$cash_account_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$expenses_taxes$depreciation <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  if (length(output$Portfolio$class$expenses_class)) {
    output$statement$income_statement$expenses_taxes$personal_expenses <-
      matrix(
        data = 0,
        nrow = length(
          output$Portfolio$portfolio$personal_expenses@expenses
        ) + 1,
        ncol = length(output$reporting$reporting_dates),
        dimnames = list(
          c(
            names(
              output$Portfolio$portfolio$personal_expenses@expenses
            ),
            "total"
          ),
          as.character(output$reporting$reporting_dates)
        )
      )
  }

  # output$statement$income_statement$expenses_taxes$property_expenses
  output$statement$income_statement$expenses_taxes$property_expenses$total <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  # output$statement$income_statement$expenses_taxes$property_expenses[[property]]
  for (property in output$Portfolio$class$property_class)
  {
    output$statement$income_statement$expenses_taxes$property_expenses[[property]] <-
      matrix(
        data = 0,
        nrow = 20,
        ncol = length(output$reporting$reporting_dates),
        dimnames = list(
          c(
            "inventory_fee",
            "contract_fee",
            "boiler_insurance",
            "ground_rent_incl._building_insurance",
            "insurance_premium",
            "property_maintenance",
            "accountant_fee",
            "solicitor_fee",
            "municipal_and_school_tax",
            "borough_charges",
            "broadband",
            "water_tenant",
            "water_owner",
            "heathing_tenant",
            "heathing_owner",
            "electricity_tenant",
            "electricity_owner",
            "tenant_finders_fee",
            "property_management",
            "total"
          ),
          as.character(output$reporting$reporting_dates)
        )
      )
  }

  output$statement$income_statement$expenses_taxes$total <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$income_statement$expenses_taxes$savings <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("savings"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$assets$cash <- matrix(
    data = 0,
    nrow = 1 + length(output$Portfolio$class$cash_account_class),
    ncol = length(output$reporting$reporting_dates),
    dimnames = list(
      c(output$Portfolio$class$cash_account_class, "total"),
      as.character(output$reporting$reporting_dates)
    )
  )

  output$statement$balance_sheet$assets$property <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$assets$pension <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$pension_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$pension_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$assets$deferred_tax_asset <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$assets$total <- matrix(
    data = 0,
    nrow = 1,
    ncol = length(output$reporting$reporting_dates),
    dimnames = list(
      c("total"),
      as.character(output$reporting$reporting_dates)
    )
  )

  output$statement$balance_sheet$liabilities$short_term_debt <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$debt_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$debt_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$liabilities$mortgage <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$liabilities$HELOC_balance <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$liabilities$refinancing_balance <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$liabilities$total <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$balance_sheet$equity$total <- matrix(
    data = 0,
    nrow = 1 + length(output$Portfolio$class$property_class) + length(output$Portfolio$class$debt_class) + length(output$Portfolio$class$cash_account_class) + length(output$Portfolio$class$pension_class),
    ncol = length(output$reporting$reporting_dates),
    dimnames = list(
      c(
        output$Portfolio$class$property_class,
        output$Portfolio$class$debt_class,
        output$Portfolio$class$cash_account_class,
        output$Portfolio$class$pension_class,
        "total"
      ),
      as.character(output$reporting$reporting_dates)
    )
  )

  output$statement$balance_sheet$equity$total_WO_DTA <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total_WO_DTA"),
        as.character(output$reporting$reporting_dates)
      )
    )


  output$statement$cash_flow_statement$operating_activities$salary_income <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$employment_class) + length(output$Portfolio$class$pension_class) + length(output$Portfolio$class$annuity_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$employment_class,
          output$Portfolio$class$pension_class,
          output$Portfolio$class$annuity_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$operating_activities$rental_income <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$operating_activities$income_taxes_paid <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class) +
        length(output$Portfolio$class$employment_class) +
        length(output$Portfolio$class$annuity_class) +
        length(output$Portfolio$class$cash_account_class) +
        length(output$Portfolio$class$pension_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$property_class,
          output$Portfolio$class$employment_class,
          output$Portfolio$class$annuity_class,
          output$Portfolio$class$cash_account_class,
          output$Portfolio$class$pension_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )

  if (length(output$Portfolio$class$expenses_class)) {
    output$statement$cash_flow_statement$operating_activities$personal_expense <-
      matrix(
        data = 0,
        nrow = 1 + length(
          output$Portfolio$portfolio$personal_expenses@expenses
        ),
        ncol = length(output$reporting$reporting_dates),
        dimnames = list(
          c(
            names(
              output$Portfolio$portfolio$personal_expenses@expenses
            ),
            "total"
          ),
          as.character(output$reporting$reporting_dates)
        )
      )
  }


  output$statement$cash_flow_statement$operating_activities$property_expenses <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$operating_activities$operating_cash_flows <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$investing_activities$dividends_received <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$pension_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$pension_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$investing_activities$interest_earned <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$cash_account_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$cash_account_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$investing_activities$investing_cash_flows <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$short_term_debt_repayment <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$debt_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$debt_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$mortgage_repayment <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$HELOC <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$interest_paid_deductible <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$interest_paid_non_deductible <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$debt_class) +
        length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(
          output$Portfolio$class$debt_class,
          output$Portfolio$class$property_class,
          "total"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$equity_refinanced <-
    matrix(
      data = 0,
      nrow = 1 + length(output$Portfolio$class$property_class),
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c(output$Portfolio$class$property_class, "total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$financing_activities$financing_cash_flows <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("total"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$total$beginning_of_period_cash <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("beginning_of_period_cash"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$total$end_of_period_cash <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("end_of_period_cash"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$total$interest_earned <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("interest_earned"),
        as.character(output$reporting$reporting_dates)
      )
    )

  output$statement$cash_flow_statement$total$increase_in_cash <-
    matrix(
      data = 0,
      nrow = 1,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("increase_in_cash"),
        as.character(output$reporting$reporting_dates)
      )
    )

  # cumulative tax in currency and for taxpayers for each date
  reporting_currencies <- c("GBP", "CAD", "USD", "EUR", "KRW", "CHF")

  output$metrics$tax <- array(
    data = 0,
    dim = c(
      length(reporting_currencies),
      length(output$reporting$reporting_dates),
      length(output$reporting$taxpayer)
    ),
    dimnames = list(
      currency = reporting_currencies,
      date = as.character(output$reporting$reporting_dates),
      taxpayer = output$reporting$taxpayer
    )
  )

  output$metrics$taxable_income <-
    array(
      data = 0,
      dim = c(
        length(reporting_currencies),
        length(output$reporting$reporting_dates),
        length(output$reporting$taxpayer)
      ),
      dimnames = list(
        currency = reporting_currencies,
        date = as.character(output$reporting$reporting_dates),
        taxpayer = output$reporting$taxpayer
      )
    )

  Portfolio <- output$Portfolio$portfolio
  for (portfolio_name in names(Portfolio))
  {
    output$Portfolio$portfolio[[portfolio_name]]@statement <- output$statement
  }

  return(output)
}
