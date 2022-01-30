#' @title property
#' @description Property function
#' @family cash flow function
#' @param x property object
#' @param output all in memory
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @export

property <- function(x, output) {
  
  "%+=%" <- roperators::`%+=%`
  "%-=%" <- roperators::`%-=%`
  "%*=%" <- roperators::`%*=%`
  
  # reporting calculation value needed
  date                      <- output$reporting$reporting_date
  portfolio_name            <- output$reporting$portfolio_name
  frequency                 <- output$reporting$frequency_number
  currency                  <- x@currency
  FX                        <- output$economic_data$exchange_rate[[currency]]
  DTA_future_tax_rate       <- output$reporting$DTA_future_tax_rate
  mortgage_holiday_end_date <- x@mortgage_holiday_end_date
  mortgage_term_in_year     <- (as.numeric(as.Date(x@mortgage_term) - lubridate::today()))/365.25
  
  reporting_dates <- as.character(output$reporting$reporting_dates)
  previous_date   <- which(reporting_dates == date) - 1
  Previous_date   <- as.character(reporting_dates[previous_date])
  
  if (date == as.character(output$reporting$reporting_dates[1])) {
    if (length(x@initial_rent_annual)) {
      if (date < x@initial_rent_annual_end_date) {
        rent_annual <- x@initial_rent_annual
      } else {
        rent_annual <- x@rent_annual
      }
    } else {
      rent_annual <- x@rent_annual
    }
    
    metric <- matrix(
      data = c(0,                               0,                             0,                                  0,                                 0,                                 0,                                 0,                            0,                                       0,                                             0,
               0,                               0,                             0,                                  0,                                 0,                                 0,                                 0,                            0,                                       0,                                             0,
               0,                               0,                             0,                                  0,                                 0,                                 0,                                 0,                            0,                                       0,                                             x@property_value,
               0,                               0,                             x@occupancy,                        x@capital_repayment_mortgage,      x@capital_repayment_proportion,    mortgage_term_in_year,             x@home_equity_line_of_credit, x@mortgage_loan_value,                   x@HELOC_balance,                               x@refinancing_balance,
               0,                               x@refinancing,                 x@mortgage_interest_rate_if_fixed,  x@taxable,                         x@depreciation,                    rent_annual,                       x@inventory_fee_annual,       x@contract_fee_annual,                   x@boiler_insurance_annual,                     x@ground_rent_incl._building_insurance_annual,
               x@insurance_premium_annual,      x@property_maintenance_annual, x@accountant_fee_annual,            x@solicitor_fee_annual,            x@municipal_and_school_tax_annual, x@borough_charges,                 x@broadband_annual,           x@water_annual,                          x@heathing_tenant_annual,                      x@heathing_owner_annual,
               x@electricity_tenant_annual,     x@electricity_owner_annual,    FX,                                 0,                                 0,                                 x@refinancing_loan_to_value_max,   0,                            0,                                       x@tenant_finders_fee_percentage_of_rent,       x@property_management_percentage_of_rent,
               x@refinance_interest_deductable, 0,                             0,                                  0,                                 0,                                 0,                                 0,                            0,                                       0,                                             0,
               0,                               0,                             0,                                  0,                                 0,                                 0,                                 0,                            0),
      nrow = 88,
      ncol = length(output$reporting$reporting_dates),
      dimnames = list(
        c("mortgage_principal_payment_reporting_currency",
          "rent_reporting_currency",
          "rent_after_occupancy_reporting_currency",
          "tenant_finders_fee_reporting_currency",
          "property_management_reporting_currency",
          "inventory_fee_reporting_currency",
          "contract_fee_reporting_currency",
          "boiler_insurance_reporting_currency",
          "ground_rent_incl._building_insurance_reporting_currency",
          "insurance_premium_reporting_currency",
          
          "property_maintenance_reporting_currency",
          "accountant_fee_reporting_currency",
          "solicitor_fee_reporting_currency",
          "municipal_and_school_tax_reporting_currency",
          "borough_charges_reporting_currency",
          "broadband_reporting_currency",
          "water_reporting_currency",
          "heathing_tenant_reporting_currency",
          "heathing_owner_reporting_currency",
          "electricity_tenant_reporting_currency",
          
          "electricity_owner_reporting_currency",
          "HELOC_withdrawal",
          "deductable_expenses",
          "tax_reporting_currency",
          "cumulative_losses",
          "deferred_tax_asset",
          "depreciation_reporting_currency",
          "taxable_income",
          "loan_to_value",
          "property_value",
          
          "property_value_reporting_currency",
          "equity_refinanced_reporting_currency",
          "occupancy",
          "capital_repayment_mortgage",
          "capital_repayment_proportion",
          "mortgage_term_in_year",
          "home_equity_line_of_credit",
          "mortgage_loan_value",
          "HELOC_balance",
          "refinancing_balance",
          
          "refinancing_balance_reporting_currency",
          "refinancing",
          "mortgage_interest_rate_if_fixed",
          "taxable",
          "depreciation",
          "rent_annual",
          "inventory_fee_annual",
          "contract_fee_annual",
          "boiler_insurance_annual",
          "ground_rent_incl._building_insurance_annual",
          
          "insurance_premium_annual",
          "property_maintenance_annual",
          "accountant_fee_annual",
          "solicitor_fee_annual",
          "municipal_and_school_tax_annual",
          "borough_charges",
          "broadband_annual",
          "water_annual",
          "heathing_tenant_annual",
          "heathing_owner_annual",
          
          "electricity_tenant_annual",
          "electricity_owner_annual",
          "FX",
          "rent",
          "rent_after_occupancy",
          "refinancing_loan_to_value_max",
          "mortgage_loan_value_reporting_currency",
          "mortgage_interest_payment_reporting_currency",
          "tenant_finders_fee_percentage_of_rent",
          "property_management_percentage_of_rent",
          
          "refinance_interest_deductable",
          "HELOC_interest_payment_reporting_currency",
          "refinancing_interest_payment",
          "refinancing_interest_payment_reporting_currency",
          "HELOC_withdrawal_reporting_currency",
          "mortgage_interest_payment",
          "HELOC_interest_payment",
          "equity_available_for_refinancing",
          "equity_available_for_refinancing_reporting_currency",
          "refinancing_principal_payment",
          
          "refinancing_principal_payment_reporting_currency",
          "tax",
          "mortgage_principal_payment",
          "HELOC_balance_reporting_currency",
          "mortgage_interest_reporting_currency",
          "mortgage_interest",
          "HELOC_interest_rate",
          "mortgage_interest_rate"
        ),
        as.character(output$reporting$reporting_dates)
      )
    )
    
    output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric
    property_value                                              <- metric["property_value", date]
    mortgage_loan_value                                         <- metric["mortgage_loan_value", date]
    HELOC_balance                                               <- metric["HELOC_balance", date]
    refinancing_balance                                         <- metric["refinancing_balance", date]
    rent_annual                                                 <- metric["rent_annual", date]
    inventory_fee_annual                                        <- metric["inventory_fee_annual", date]
    contract_fee_annual                                         <- metric["contract_fee_annual", date]
    boiler_insurance_annual                                     <- metric["boiler_insurance_annual", date]
    ground_rent_incl._building_insurance_annual                 <- metric["ground_rent_incl._building_insurance_annual", date]
    insurance_premium_annual                                    <- metric["insurance_premium_annual", date]
    property_maintenance_annual                                 <- metric["property_maintenance_annual", date]
    accountant_fee_annual                                       <- metric["accountant_fee_annual", date]
    solicitor_fee_annual                                        <- metric["solicitor_fee_annual", date]
    municipal_and_school_tax_annual                             <- metric["municipal_and_school_tax_annual", date]
    borough_charges                                             <- metric["borough_charges", date]
    broadband_annual                                            <- metric["broadband_annual", date]
    water_annual                                                <- metric["water_annual", date]
    heathing_tenant_annual                                      <- metric["heathing_tenant_annual", date]
    heathing_owner_annual                                       <- metric["heathing_owner_annual", date]
    electricity_tenant_annual                                   <- metric["electricity_tenant_annual", date]
    electricity_owner_annual                                    <- metric["electricity_owner_annual", date]
    cumulative_losses                                           <- metric["cumulative_losses", date]
  } else {
    metric                                                      <- output$Portfolio$portfolio[[portfolio_name]]@metrics$metric
    property_value                                              <- inflation_adjustment(amount = metric["property_value", Previous_date], currency = currency, output = output)
    metric["property_value", date]                              <- property_value                              <- risk_premium_adjustment(amount = property_value, asset_class = "property_class", output = output)
    metric["mortgage_loan_value", date]                         <- mortgage_loan_value                         <- metric["mortgage_loan_value", Previous_date]
    metric["HELOC_balance", date]                               <- HELOC_balance                               <- metric["HELOC_balance", Previous_date]
    metric["refinancing_balance", date]                         <- refinancing_balance                         <- metric["refinancing_balance", Previous_date]
    
    if (length(x@initial_rent_annual)) {
      if (date <= x@initial_rent_annual_end_date) {
        metric["rent_annual", date] <- rent_annual <- x@initial_rent_annual
      } else if (date >= x@initial_rent_annual_end_date & Previous_date <= x@initial_rent_annual_end_date) {
        metric["rent_annual", date] <- rent_annual <- x@rent_annual
      } else {
        metric["rent_annual", date]                                 <- rent_annual                                 <- inflation_adjustment(amount = metric["rent_annual", Previous_date], currency = currency, output = output)
      }
    } else {
      metric["rent_annual", date]                                 <- rent_annual                                 <- inflation_adjustment(amount = metric["rent_annual", Previous_date], currency = currency, output = output)
    }
    
    metric["inventory_fee_annual", date]                        <- inventory_fee_annual                        <- inflation_adjustment(amount = metric["inventory_fee_annual", Previous_date], currency =  currency, output = output)
    metric["contract_fee_annual", date]                         <- contract_fee_annual                         <- inflation_adjustment(amount = metric["contract_fee_annual", Previous_date], currency = currency, output = output)
    metric["boiler_insurance_annual", date]                     <- boiler_insurance_annual                     <- inflation_adjustment(amount = metric["boiler_insurance_annual", Previous_date], currency = currency, output = output)
    metric["ground_rent_incl._building_insurance_annual", date] <- ground_rent_incl._building_insurance_annual <- inflation_adjustment(amount = metric["ground_rent_incl._building_insurance_annual", Previous_date], currency = currency, output = output)
    metric["insurance_premium_annual", date]                    <- insurance_premium_annual                    <- inflation_adjustment(amount = metric["insurance_premium_annual", Previous_date], currency = currency, output = output)
    metric["property_maintenance_annual", date]                 <- property_maintenance_annual                 <- inflation_adjustment(amount = metric["property_maintenance_annual", Previous_date], currency = currency, output = output)
    metric["accountant_fee_annual", date]                       <- accountant_fee_annual                       <- inflation_adjustment(amount = metric["accountant_fee_annual", Previous_date], currency = currency, output = output)
    metric["solicitor_fee_annual", date]                        <- solicitor_fee_annual                        <- inflation_adjustment(amount = metric["solicitor_fee_annual", Previous_date], currency = currency, output = output)
    metric["municipal_and_school_tax_annual", date]             <- municipal_and_school_tax_annual             <- inflation_adjustment(amount = metric["municipal_and_school_tax_annual", Previous_date], currency = currency, output = output)
    metric["borough_charges", date]                             <- borough_charges                             <- inflation_adjustment(amount = metric["borough_charges", Previous_date], currency = currency, output = output)
    metric["broadband_annual", date]                            <- broadband_annual                            <- inflation_adjustment(amount = metric["broadband_annual", Previous_date], currency = currency, output = output)
    metric["water_annual", date]                                <- water_annual                                <- inflation_adjustment(amount = metric["water_annual", Previous_date], currency = currency, output = output)
    metric["heathing_tenant_annual", date]                      <- heathing_tenant_annual                      <- inflation_adjustment(amount = metric["heathing_tenant_annual", Previous_date], currency = currency, output = output)
    metric["heathing_owner_annual", date]                       <- heathing_owner_annual                       <- inflation_adjustment(amount = metric["heathing_owner_annual", Previous_date], currency = currency, output = output)
    metric["electricity_tenant_annual", date]                   <- electricity_tenant_annual                   <- inflation_adjustment(amount = metric["electricity_tenant_annual", Previous_date], currency = currency, output = output)
    metric["electricity_owner_annual", date]                    <- electricity_owner_annual                    <- inflation_adjustment(amount = metric["electricity_owner_annual", Previous_date], currency = currency, output = output)
    metric["cumulative_losses", date]                           <- cumulative_losses                           <- metric["cumulative_losses", Previous_date]
  }
  occupancy                                           <- metric["occupancy", date]
  capital_repayment_mortgage                          <- metric["capital_repayment_mortgage", date]
  capital_repayment_proportion                        <- metric["capital_repayment_proportion", date]
  mortgage_term_in_year                               <- metric["mortgage_term_in_year", date]
  home_equity_line_of_credit                          <- metric["home_equity_line_of_credit", date]
  refinancing                                         <- metric["refinancing", date]
  taxable                                             <- metric["taxable", date]
  depreciation                                        <- metric["depreciation", date]
  refinancing_loan_to_value_max                       <- metric["refinancing_loan_to_value_max", date]
  tenant_finders_fee_percentage_of_rent               <- metric["tenant_finders_fee_percentage_of_rent", date]
  property_management_percentage_of_rent              <- metric["property_management_percentage_of_rent", date]
  refinance_interest_deductable                       <- metric["refinance_interest_deductable", date]
  refinancing_interest_payment                        <- metric["refinancing_interest_payment", date]
  refinancing_interest_payment_reporting_currency     <- metric["refinancing_interest_payment_reporting_currency", date]
  HELOC_withdrawal_reporting_currency                 <- metric["HELOC_withdrawal_reporting_currency", date]
  HELOC_withdrawal                                    <- metric["HELOC_withdrawal", date]
  mortgage_interest_payment                           <- metric["mortgage_interest_payment", date]
  HELOC_interest_payment                              <- metric["HELOC_interest_payment", date]
  equity_available_for_refinancing                    <- metric["equity_available_for_refinancing", date]
  equity_available_for_refinancing_reporting_currency <- metric["equity_available_for_refinancing_reporting_currency", date]
  refinancing_principal_payment_reporting_currency    <- metric["refinancing_principal_payment_reporting_currency", date]
  depreciation_reporting_currency                     <- metric["depreciation_reporting_currency", date]
  taxable_income                                      <- metric["taxable_income", date]
  deductable_expenses                                 <- metric["deductable_expenses", date]
  tax                                                 <- metric["tax", date]
  HELOC_balance_reporting_currency                    <- metric["HELOC_balance_reporting_currency", date]
  
  if (x@mortgage_type == "fixed" & date <= x@mortgage_initial_rate_maturity) {
    mortgage_interest_rate <- x@mortgage_interest_rate_if_fixed
  } else if (x@mortgage_type == "variable" | date > x@mortgage_initial_rate_maturity) {
    mortgage_interest_rate <- output$economic_data$bank_rate[[currency]] + x@mortgage_spread
  }
  
  metric["HELOC_interest_rate", date] <- HELOC_interest_rate <- output$economic_data$bank_rate[[currency]] + x@HELOC_interest_rate_spread
  metric["mortgage_interest_rate", date] <- mortgage_interest_rate
  
  # refinancing capital repayment mortgage  - no mortgage payment holiday
  if (refinancing & capital_repayment_mortgage & (mortgage_holiday_end_date <= date)) {
    mortgage_principal_payment <- mortgage(principal             = mortgage_loan_value,
                                           annual_rate           = mortgage_interest_rate,
                                           frequency             = frequency,
                                           mortgage_term_in_year = mortgage_term_in_year)$principal_payment
  }
  
  # non-refinancing capital repayment mortgage - no mortgage payment holiday
  if (!refinancing & capital_repayment_mortgage & (mortgage_holiday_end_date <= date)) {
    mortgage_principal_payment <- mortgage(principal             = x@mortgage_loan_value,
                                           annual_rate           = mortgage_interest_rate,
                                           frequency             = frequency,
                                           mortgage_term_in_year = mortgage_term_in_year)$principal_payment
  }
  
  # interest calculations
  mortgage_interest <- mortgage(principal             = mortgage_loan_value,
                                annual_rate           = mortgage_interest_rate,
                                frequency             = frequency,
                                mortgage_term_in_year = mortgage_term_in_year)$interest_payment
  
  # interest-only mortgage or mortgage payment holiday
  if ((!capital_repayment_mortgage) | (mortgage_holiday_end_date > date)) {
    mortgage_principal_payment <- 0
  }
  
  mortgage_principal_payment <- mortgage_principal_payment * capital_repayment_proportion
  
  metric["mortgage_interest_reporting_currency", date] <- mortgage_interest_reporting_currency <- mortgage_interest * FX
  
  metric["mortgage_principal_payment", date] <- mortgage_principal_payment
  
  # mortgage payment holiday
  if(mortgage_holiday_end_date > date) {
    mortgage_interest_payment <- 0
  }
  if(mortgage_holiday_end_date <= date) {
    mortgage_interest_payment <- mortgage_interest
  }
  
  metric["mortgage_loan_value", date]                          <- mortgage_loan_value                          <- mortgage_loan_value + mortgage_interest - mortgage_interest_payment - mortgage_principal_payment
  metric["mortgage_interest_payment", date]                    <- mortgage_interest_payment
  metric["mortgage_interest_payment_reporting_currency", date] <- mortgage_interest_payment_reporting_currency <- mortgage_interest_payment * FX
  
  # refinancing mortgage
  if (refinancing) {
    refinancing_mortgage <- mortgage(principal             = refinancing_balance,
                                     annual_rate           = mortgage_interest_rate,
                                     frequency             = frequency,
                                     mortgage_term_in_year = mortgage_term_in_year)
    metric["refinancing_interest_payment", date]                     <- refinancing_interest_payment                        <- refinancing_mortgage$interest_payment
    metric["refinancing_principal_payment", date]                    <- refinancing_principal_payment                       <- refinancing_mortgage$principal_payment
    metric["refinancing_interest_payment_reporting_currency", date]  <- refinancing_interest_payment_reporting_currency     <- refinancing_interest_payment * FX
    metric["refinancing_principal_payment_reporting_currency", date] <- refinancing_principal_payment_reporting_currency    <- refinancing_principal_payment * FX
    metric["refinancing_balance", date]                              <- refinancing_balance - refinancing_principal_payment
  }
  
  # HELOC
  if (home_equity_line_of_credit) {
    metric["HELOC_interest_payment", date] <- HELOC_interest_payment <- HELOC_balance * ((1 + HELOC_interest_rate) ^ (1 / frequency) - 1)
    metric["HELOC_balance", date]          <- HELOC_balance          <- HELOC_balance + HELOC_interest_payment
    metric["loan_to_value", date]          <- loan_to_value          <- (mortgage_loan_value + refinancing_balance + HELOC_balance) / property_value
    if (loan_to_value < x@HELOC_max_loan_to_value) {
      metric["HELOC_withdrawal", date]                    <- HELOC_withdrawal                    <- min(pmax(x@HELOC_max_loan_to_value - loan_to_value, 0) * property_value, mortgage_principal_payment)
      metric["HELOC_withdrawal_reporting_currency", date] <- HELOC_withdrawal_reporting_currency <- HELOC_withdrawal * FX
      metric["HELOC_balance", date]                       <- HELOC_balance                       <- HELOC_balance + HELOC_withdrawal
      metric["loan_to_value", date]                       <- loan_to_value                       <- (mortgage_loan_value + refinancing_balance + HELOC_balance) / property_value
    }
    metric["HELOC_balance_reporting_currency", date] <- HELOC_balance_reporting_currency <- HELOC_balance * FX
  }
  metric["HELOC_interest_payment_reporting_currency", date] <- HELOC_interest_payment_reporting_currency <- HELOC_interest_payment * FX
  
  # refinancing additional
  if (refinancing) {
    metric["equity_available_for_refinancing", date]                    <- equity_available_for_refinancing                    <- pmax(refinancing_loan_to_value_max - (mortgage_loan_value + refinancing_balance + HELOC_balance ) / property_value, 0) * property_value
    metric["equity_available_for_refinancing_reporting_currency", date] <- equity_available_for_refinancing_reporting_currency <- equity_available_for_refinancing * FX
    metric["refinancing_balance", date]                                 <- refinancing_balance                                 <- refinancing_balance + equity_available_for_refinancing
    metric["refinancing_balance_reporting_currency", date]              <- refinancing_balance_reporting_currency              <- refinancing_balance * FX
  }
  
  #rent calculations
  metric["rent", date]                                    <- rent                                    <- rent_annual / frequency
  metric["rent_after_occupancy", date]                    <- rent_after_occupancy                    <- rent * occupancy
  metric["rent_reporting_currency", date]                 <- rent_reporting_currency                 <- rent * FX
  metric["rent_after_occupancy_reporting_currency", date] <- rent_after_occupancy_reporting_currency <- rent_after_occupancy * FX
  
  # adjust for reporting currency and period (frequency)
  metric["mortgage_principal_payment_reporting_currency", date]           <- mortgage_principal_payment_reporting_currency           <- mortgage_principal_payment * FX
  metric["mortgage_loan_value_reporting_currency", date]                  <- mortgage_loan_value_reporting_currency                  <- FX * mortgage_loan_value
  metric["property_value_reporting_currency", date]                       <- property_value_reporting_currency                       <- property_value * FX
  metric["inventory_fee_reporting_currency", date]                        <- inventory_fee_reporting_currency                        <- inventory_fee_annual / frequency * FX
  metric["contract_fee_reporting_currency", date]                         <- contract_fee_reporting_currency                         <- contract_fee_reporting_currency          <- contract_fee_annual / frequency * FX
  metric["boiler_insurance_reporting_currency", date]                     <- boiler_insurance_reporting_currency                     <- boiler_insurance_annual / frequency * FX
  metric["ground_rent_incl._building_insurance_reporting_currency", date] <- ground_rent_incl._building_insurance_reporting_currency <- ground_rent_incl._building_insurance_annual / frequency * FX
  metric["insurance_premium_reporting_currency", date]                    <- insurance_premium_reporting_currency                    <- insurance_premium_annual / frequency * FX
  metric["property_maintenance_reporting_currency", date]                 <- property_maintenance_reporting_currency                 <- property_maintenance_annual / frequency * FX
  metric["accountant_fee_reporting_currency", date]                       <- accountant_fee_reporting_currency                       <- accountant_fee_annual / frequency * FX
  metric["solicitor_fee_reporting_currency", date]                        <- solicitor_fee_reporting_currency                        <- solicitor_fee_annual / frequency * FX
  metric["municipal_and_school_tax_reporting_currency", date]             <- municipal_and_school_tax_reporting_currency             <- municipal_and_school_tax_annual / frequency * FX
  metric["borough_charges_reporting_currency", date]                      <- borough_charges_reporting_currency                      <- borough_charges / frequency * FX
  metric["broadband_reporting_currency", date]                            <- broadband_reporting_currency                            <- broadband_annual / frequency * FX
  metric["water_reporting_currency", date]                                <- water_reporting_currency                                <- water_annual / frequency * FX
  metric["heathing_tenant_reporting_currency", date]                      <- heathing_tenant_reporting_currency                      <- heathing_tenant_annual / frequency * FX
  metric["heathing_owner_reporting_currency", date]                       <- heathing_owner_reporting_currency                       <- heathing_owner_annual / frequency * FX
  metric["electricity_tenant_reporting_currency", date]                   <- electricity_tenant_reporting_currency                   <- electricity_tenant_annual / frequency * FX
  metric["electricity_owner_reporting_currency", date]                    <- electricity_owner_reporting_currency                    <- electricity_owner_annual / frequency * FX
  metric["tenant_finders_fee_reporting_currency", date]                   <- tenant_finders_fee_reporting_currency                   <- rent_after_occupancy_reporting_currency * tenant_finders_fee_percentage_of_rent
  metric["property_management_reporting_currency", date]                  <- property_management_reporting_currency                  <- rent_after_occupancy_reporting_currency * property_management_percentage_of_rent
  
  if (taxable) {
    borough_charges_reporting_currency    <- (1 - occupancy) * borough_charges_reporting_currency
    broadband_reporting_currency          <- (1 - occupancy) * broadband_reporting_currency
    water_reporting_currency              <- (1 - occupancy) * water_reporting_currency
    heathing_tenant_reporting_currency    <- (1 - occupancy) * heathing_tenant_reporting_currency
    electricity_tenant_reporting_currency <- (1 - occupancy) * electricity_tenant_reporting_currency
    
    if (depreciation != 0) {
      depreciation_reporting_currency <- x@building_purchase_price * x@depreciation / frequency * FX
    }
    
    tax_rate <- x@tax_rate
    
    deductable_expenses <-
      mortgage_interest_reporting_currency                    + inventory_fee_reporting_currency            + contract_fee_reporting_currency         + boiler_insurance_reporting_currency   +
      ground_rent_incl._building_insurance_reporting_currency + insurance_premium_reporting_currency        + property_maintenance_reporting_currency + accountant_fee_reporting_currency     +
      solicitor_fee_reporting_currency                        + municipal_and_school_tax_reporting_currency + borough_charges_reporting_currency      + broadband_reporting_currency          +
      water_reporting_currency                                + heathing_tenant_reporting_currency          + heathing_owner_reporting_currency       + electricity_tenant_reporting_currency +
      electricity_owner_reporting_currency                    + tenant_finders_fee_reporting_currency       + property_management_reporting_currency  + depreciation_reporting_currency
    
    if (refinance_interest_deductable) {
      deductable_expenses %+=% (HELOC_interest_payment_reporting_currency + refinancing_interest_payment_reporting_currency)
    }
    metric["deductable_expenses", date] <- deductable_expenses
    
    taxable_income <- rent_after_occupancy_reporting_currency - deductable_expenses
    
    if (taxable_income < 0) {
      cumulative_losses %-=% taxable_income
      tax               <- 0
    }
    if (taxable_income >= 0) {
      if (cumulative_losses <= taxable_income) {
        taxable_income    %-=% cumulative_losses
        cumulative_losses <- 0
      }
      if (cumulative_losses > taxable_income) {
        taxable_income    <- 0
        cumulative_losses %-=% taxable_income
      }
      if (length(x@tax_rate) != 0) {
        tax <- taxable_income * tax_rate
      }
      if (length(x@tax_rate) == 0) {
        output <- tax_calculations(x = x, output = output, income = taxable_income)
        tax    <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax["total", date]
      }
    }
  }
  metric["taxable_income", date]         <- taxable_income
  metric["cumulative_losses", date]      <- output$Portfolio$portfolio[[portfolio_name]]@cumulative_losses <- cumulative_losses
  metric["tax", date]                    <- tax
  metric["tax_reporting_currency", date] <- tax_reporting_currency                                         <- tax * FX
  
  deferred_tax_asset <- DTA_future_tax_rate * cumulative_losses
  
  property_expenses <-
    inventory_fee_reporting_currency            + contract_fee_reporting_currency                         +
    boiler_insurance_reporting_currency         + ground_rent_incl._building_insurance_reporting_currency +
    insurance_premium_reporting_currency        + property_maintenance_reporting_currency                 +
    accountant_fee_reporting_currency           + solicitor_fee_reporting_currency                        +
    municipal_and_school_tax_reporting_currency + borough_charges_reporting_currency                      +
    broadband_reporting_currency                + water_reporting_currency                                +
    heathing_tenant_reporting_currency          + heathing_owner_reporting_currency                       +
    electricity_tenant_reporting_currency       + electricity_owner_reporting_currency                    +
    tenant_finders_fee_reporting_currency       + property_management_reporting_currency
  
  #loan_to_value <- mortgage_loan_value/property_value
  metric["loan_to_value", date] <- loan_to_value <- (mortgage_loan_value + refinancing_balance + HELOC_balance) / property_value
  
  # store metrics
  output$Portfolio$portfolio[[portfolio_name]]@metrics$metric <- metric
  
  # store to income statements
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$income$house_rental_income[portfolio_name, date] <- rent_after_occupancy_reporting_currency
  
  if (taxable) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_deductible[portfolio_name, date] <- mortgage_interest_reporting_currency
  } else {
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_non_deductible[portfolio_name, date] <- mortgage_interest_reporting_currency
  }
  
  if (refinance_interest_deductable & taxable) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_deductible[portfolio_name, date] %+=% (HELOC_interest_payment_reporting_currency + refinancing_interest_payment_reporting_currency)
  } else {
    output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$interest_non_deductible[portfolio_name, date] %+=% (HELOC_interest_payment_reporting_currency + refinancing_interest_payment_reporting_currency)
  }
  
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$property_expenses[[portfolio_name]][c("inventory_fee",
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
                                                                                                                               "water",
                                                                                                                               "heathing_tenant",
                                                                                                                               "heathing_owner",
                                                                                                                               "electricity_tenant",
                                                                                                                               "electricity_owner",
                                                                                                                               "tenant_finders_fee",
                                                                                                                               "property_management"), date] <- c(inventory_fee_reporting_currency,
                                                                                                                                                                  contract_fee_reporting_currency,
                                                                                                                                                                  boiler_insurance_reporting_currency,
                                                                                                                                                                  ground_rent_incl._building_insurance_reporting_currency,
                                                                                                                                                                  insurance_premium_reporting_currency,
                                                                                                                                                                  property_maintenance_reporting_currency,
                                                                                                                                                                  accountant_fee_reporting_currency,
                                                                                                                                                                  solicitor_fee_reporting_currency,
                                                                                                                                                                  municipal_and_school_tax_reporting_currency,
                                                                                                                                                                  borough_charges_reporting_currency,
                                                                                                                                                                  broadband_reporting_currency,
                                                                                                                                                                  water_reporting_currency,
                                                                                                                                                                  heathing_tenant_reporting_currency,
                                                                                                                                                                  heathing_owner_reporting_currency,
                                                                                                                                                                  electricity_tenant_reporting_currency,
                                                                                                                                                                  electricity_owner_reporting_currency,
                                                                                                                                                                  tenant_finders_fee_reporting_currency,
                                                                                                                                                                  property_management_reporting_currency)
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$property_expenses$total[portfolio_name, date] <- property_expenses
  metric["depreciation_reporting_currency", date]                                                                                      <- output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$depreciation[portfolio_name, date] <- depreciation_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$income_statement$expenses_taxes$income_tax[portfolio_name, date]              <- tax_reporting_currency
  
  # store to balance sheet
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$cash[output$Portfolio$class$cash_account_class, date] %+=% (rent_after_occupancy_reporting_currency               + HELOC_withdrawal_reporting_currency              +
                                                                                                                                            equity_available_for_refinancing_reporting_currency - mortgage_interest_payment_reporting_currency     -
                                                                                                                                            HELOC_interest_payment_reporting_currency           - refinancing_interest_payment_reporting_currency  -
                                                                                                                                            mortgage_principal_payment_reporting_currency       - refinancing_principal_payment_reporting_currency -
                                                                                                                                            property_expenses                                   - tax_reporting_currency)
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$property[portfolio_name, date]                  <- property_value_reporting_currency
  metric["deferred_tax_asset", date]                                                                                          <- output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$assets$deferred_tax_asset[portfolio_name, date] <- deferred_tax_asset
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$mortgage[portfolio_name, date]             <- mortgage_loan_value_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$HELOC_balance[portfolio_name, date]        <- HELOC_balance_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$liabilities$refinancing_balance[portfolio_name, date]  <- equity_available_for_refinancing_reporting_currency
  
  output$Portfolio$portfolio[[portfolio_name]]@statement$balance_sheet$equity$total[portfolio_name, date]                     <- property_value_reporting_currency - mortgage_loan_value_reporting_currency - HELOC_balance_reporting_currency + deferred_tax_asset
  
  # store to cash flow statement
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$rental_income[portfolio_name, date]     <- rent_after_occupancy_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$income_taxes_paid[portfolio_name, date] <- tax_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$operating_activities$property_expenses[portfolio_name, date] <- property_expenses
  
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$HELOC[portfolio_name, date]              <- HELOC_withdrawal_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$equity_refinanced[portfolio_name, date]  <- equity_available_for_refinancing_reporting_currency
  output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$mortgage_repayment[portfolio_name, date] <- mortgage_principal_payment_reporting_currency
  
  if (taxable) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_deductible[portfolio_name, date] <- mortgage_interest_payment_reporting_currency
  } else {
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_non_deductible[portfolio_name, date] <- mortgage_interest_payment_reporting_currency
  }
  
  if (refinance_interest_deductable & taxable) {
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_deductible[portfolio_name, date] %+=% (HELOC_interest_payment_reporting_currency + refinancing_interest_payment_reporting_currency)
  } else {
    output$Portfolio$portfolio[[portfolio_name]]@statement$cash_flow_statement$financing_activities$interest_paid_non_deductible[portfolio_name, date] %+=% (HELOC_interest_payment_reporting_currency + refinancing_interest_payment_reporting_currency)
  }
  
  output                                       <- portfolio_to_statement(output = output, portfolio_name = portfolio_name, date = date)
  
  output$Portfolio$portfolio[[portfolio_name]] <- statement_total_program(X = output$Portfolio$portfolio[[portfolio_name]], reporting_date = date)
  
  return(output)
}

