#' @title input_function
#' @description family office wrapping function. This function calculate the financial statement projections.
#' @param input is the only parameter 'input' needed is the input files name.
#' @param output is the only parameter 'input' needed is the input files name.
#' @return this function returns a text file 'output.txt'
#' @export

input_function <- function(input, output) {
  
  token <- FALSE
  
  if (length(input) == 0) {
    input <- list(
      userName = "David",
      input_file_name = "David_&_Marie-Claude.R",
      reporting_frequency = "years",
      reporting_currency = "GBP", # currency in which the balance sheet/income statement is reported
      DTA_future_tax_rate = 20,
      GBP_inflation = 2,
      CAD_inflation = 1,
      USD_inflation = 2,
      EUR_inflation = 3,
      KRW_inflation = 2,
      CHF_inflation = 1,
      property_risk_premium = 2,
      share_risk_premium = 3,
      cash_rate_GBP = c(-5, 15), # checking account interest rate (first rate for positive balance, second rate for negative balance)
      cash_rate_CAD = c(-5, 15),
      cash_rate_USD = c(-5, 15),
      cash_rate_EUR = c(-5, 15),
      cash_rate_KRW = c(-5, 15),
      cash_rate_CHF = c(-5, 15),
      GBP_exchange_rate_base_CAD = 0.8,
      USD_exchange_rate_base_CAD = 0.85,
      EUR_exchange_rate_base_CAD = 0.8,
      KRW_exchange_rate_base_CAD = 0.011,
      CHF_exchange_rate_base_CAD = 0.85,
      bank_rate_CAD = 2.45,
      bank_rate_GBP = 0.25,
      bank_rate_USD = 2,
      bank_rate_KRW = 2,
      bank_rate_CHF = 2,
      bank_rate_EUR = 2)
    token <- TRUE
  }
  
  filepath <- user_choices(user = input$userName)
  source(file = filepath, local = TRUE)
  
  output$Portfolio$portfolio_all <- make_portfolio_all(user = input$userName)
  output$Portfolio$class <- find_portfolio_class(portfolio = output$Portfolio$portfolio_all) # find class of each element of the portfolio
  output$input_file <- utils::capture.output(cat(readChar(con = filepath, file.info(filepath)$size)))
  
  output$reporting$valuation_date <- lubridate::as_date(x = as.character(lubridate::today())) # redefine valuation as date format
  output$reporting$taxpayer <- names(output$reporting$birthdate) # frequency of the reporting (days,weeks,months,quarters,years)
  output$reporting$projection_length_in_years <- switch(input$reporting_frequency, "years" = 12, "quarters" = 3, "months" = 1, "weeks" = 12/52, "days" = 12/365.25)
  output$reporting$reporting_currency <- input$reporting_currency # currency in which the balance sheet/income statement is reported
  output$reporting$DTA_future_tax_rate <- input$DTA_future_tax_rate / 100
  output$reporting$reporting_frequency <- input$reporting_frequency
  output$reporting$frequency_number <- switch(input$reporting_frequency, "days" = 365.25, "weeks" = 52, "months" = 12, "quarters" = 4, "years" =1) # define frequency (numerical)
  output$reporting$reporting_dates <- seq(from = output$reporting$valuation_date, to = output$reporting$valuation_date + (output$reporting$projection_length_in_years * 365.25), by = 365.25 / output$reporting$frequency_number)
  output$economic_data$inflation$GBP <- input$GBP_inflation / 100
  output$economic_data$inflation$CAD <- input$CAD_inflation / 100
  output$economic_data$inflation$USD <- input$USD_inflation / 100
  output$economic_data$inflation$EUR <- input$EUR_inflation / 100
  output$economic_data$inflation$KRW <- input$KRW_inflation / 100
  output$economic_data$inflation$CHF <- input$CHF_inflation / 100
  output$economic_data$risk_premium$property <- input$property_risk_premium / 100
  output$economic_data$risk_premium$share <- input$share_risk_premium / 100
  output$economic_data$cash_rate$GBP <- input$cash_rate_GBP / 100 # checking account interest rate (first rate for positive balance, second rate for negative balance)
  output$economic_data$cash_rate$CAD <- input$cash_rate_CAD / 100
  output$economic_data$cash_rate$USD <- input$cash_rate_USD / 100
  output$economic_data$cash_rate$EUR <- input$cash_rate_EUR / 100
  output$economic_data$cash_rate$KRW <- input$cash_rate_KRW / 100
  output$economic_data$cash_rate$CHF <- input$cash_rate_CHF / 100
  output$economic_data$exchange_rate_base_CAD <- list(GBP = input$GBP_exchange_rate_base_CAD, CAD = 1, USD = input$USD_exchange_rate_base_CAD, EUR = input$EUR_exchange_rate_base_CAD, KRW = input$KRW_exchange_rate_base_CAD, CHF = input$CHF_exchange_rate_base_CAD)
  output$economic_data$bank_rate$GBP <- input$bank_rate_GBP / 100
  output$economic_data$bank_rate$CAD <- input$bank_rate_CAD / 100
  output$economic_data$bank_rate$USD <- input$bank_rate_USD / 100
  output$economic_data$bank_rate$EUR <- input$bank_rate_EUR / 100
  output$economic_data$bank_rate$KRW <- input$bank_rate_KRW / 100
  output$economic_data$bank_rate$CHF <- input$bank_rate_CHF / 100
  
  if (token) {
    output$Portfolio$portfolio <- output$Portfolio$portfolio_all
  }
  
  if (!token) {
    output$Portfolio$portfolio <- list()
    
    for (pn in input$portfolio_selector) {
      output$Portfolio$portfolio[[pn]] <- output$Portfolio$portfolio_all[[pn]]
    }
  }
  
  return(output)
}
