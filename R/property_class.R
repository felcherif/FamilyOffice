#' @title property_class
#' @description function that set the class for property
#' @param property_value "numeric" e.g.: '895000'
#' @param building_purchase_price "numeric" e.g.: '450000'
#' @param currency "character", e.g.: '"CAD"'
#' @param mortgage_type "character" e.g.: 'fixed' or 'variable'
#' @param mortgage_term "character", e.g.: '25'
#' @param mortgage_spread "numeric" e.g.: -0.01
#' @param mortgage_interest_rate_if_fixed "numeric", e.g.: '0.0395' = 3.95\% per annum
#' @param mortgage_initial_rate_maturity "character", e.g.: '"2011-09-24"'
#' @param mortgage_holiday_end_date "character", date at which mortgage holiday ends e.g.: '"2022-01-29"'
#' @param mortgage_loan_value "numeric", e.g.: '600000'
#' @param HELOC_balance "numeric", e.g.: '30000'
#' @param HELOC_interest_rate_spread "numeric", e.g.: '0.0595' = 5.95\% per annum
#' @param HELOC_max_loan_to_value "numeric", e.g. '0.8'
#' @param refinancing_balance "numeric", e.g.: '50000'
#' @param refinance_interest_deductable "logical", e.g.: 'TRUE' or 'FALSE'
#' @param capital_repayment_mortgage "logical", e.g.: if 'TRUE' then capital repayment are calculated
#' @param capital_repayment_proportion "numeric", e.g.: usually 1 for full capital repayment, not used if not capital repayment, 0 for interest only, any real value can be used
#' @param home_equity_line_of_credit "logical", e.g.: if 'TRUE' then when mortgage reaches 80\% loan to value, withdrawal are taken
#' @param refinancing_loan_to_value_max "numeric", e.g.: '0.8' for 80\% loan to value
#' @param refinancing "logical", e.g.: 'TRUE' if true then refinancing happens when hitting 'refinancing_loan_to_value_max'
#' @param taxable "logical", e.g.: 'TRUE' if true then tax applies to rental income
#' @param tax_rate "numeric", e.g.: '0.0595' = 5.95\% per annum
#' @param taxpayer "character", e.g.: 'FREDERIC' tax payer name
#' @param depreciation "numeric", e.g.: '0.04', 4\% per annum
#' @param cumulative_losses "numeric", e.g.; '50000', money spent on property which can be offset against taxable income
#' @param occupancy "numeric", e.g.: '0.95' means the property is empty (free of rent) 5\% of the time
#' @param tenant_finders_fee_percentage_of_rent "numeric", e.g.: '0.03' meaning 3\% of rent in addition to tenant finders fee if applicable
#' @param property_management_percentage_of_rent "numeric"
#' @param inventory_fee_annual "numeric"
#' @param contract_fee_annual "numeric"
#' @param rent_annual "numeric"
#' @param initial_rent_annual_end_date "character"
#' @param initial_rent_annual "numeric"
#' @param boiler_insurance_annual "numeric"
#' @param ground_rent_incl._building_insurance_annual "numeric"
#' @param insurance_premium_annual "numeric"
#' @param property_maintenance_annual "numeric"
#' @param accountant_fee_annual "numeric"
#' @param solicitor_fee_annual "numeric"
#' @param municipal_and_school_tax_annual "numeric"
#' @param borough_charges "numeric"
#' @param broadband_annual "numeric"
#' @param water_tenant_annual "numeric"
#' @param water_owner_annual "numeric"
#' @param heathing_tenant_annual "numeric"
#' @param heathing_owner_annual "numeric"
#' @param electricity_tenant_annual "numeric"
#' @param electricity_owner_annual "numeric"
#' @param metrics "list"
#' @param statement "list"
#' @return this function returns a property class object
#' @export

property_class <- setClass(Class = "property_class",
                           slots = c(
                             property_value                              = "numeric",
                             building_purchase_price                     = "numeric",
                             currency                                    = "character",
                             mortgage_type                               = "character",
                             mortgage_term                               = "character",
                             mortgage_spread                             = "numeric",
                             mortgage_interest_rate_if_fixed             = "numeric",
                             mortgage_initial_rate_maturity              = "character",
                             mortgage_holiday_end_date                   = "character",
                             mortgage_loan_value                         = "numeric",
                             HELOC_balance                               = "numeric",
                             HELOC_interest_rate_spread                  = "numeric",
                             HELOC_max_loan_to_value                     = "numeric",
                             refinancing_balance                         = "numeric",
                             refinance_interest_deductable               = "logical",
                             capital_repayment_mortgage                  = "logical",
                             capital_repayment_proportion                = "numeric",
                             home_equity_line_of_credit                  = "logical",
                             refinancing_loan_to_value_max               = "numeric",
                             refinancing                                 = "logical",
                             taxable                                     = "logical",
                             tax_rate                                    = "numeric",
                             taxpayer                                    = "character",
                             depreciation                                = "numeric",
                             cumulative_losses                           = "numeric",
                             occupancy                                   = "numeric",
                             tenant_finders_fee_percentage_of_rent       = "numeric",
                             property_management_percentage_of_rent      = "numeric",
                             inventory_fee_annual                        = "numeric",
                             contract_fee_annual                         = "numeric",
                             rent_annual                                 = "numeric",
                             initial_rent_annual_end_date                = "character",
                             initial_rent_annual                         = "numeric",
                             boiler_insurance_annual                     = "numeric",
                             ground_rent_incl._building_insurance_annual = "numeric",
                             insurance_premium_annual                    = "numeric",
                             property_maintenance_annual                 = "numeric",
                             accountant_fee_annual                       = "numeric",
                             solicitor_fee_annual                        = "numeric",
                             municipal_and_school_tax_annual             = "numeric",
                             borough_charges                             = "numeric",
                             broadband_annual                            = "numeric",
                             water_tenant_annual                         = "numeric",
                             water_owner_annual                          = "numeric",
                             heathing_tenant_annual                      = "numeric",
                             heathing_owner_annual                       = "numeric",
                             electricity_tenant_annual                   = "numeric",
                             electricity_owner_annual                    = "numeric",
                             metrics                                     = "list",
                             statement                                   = "list"))
