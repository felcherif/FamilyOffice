#' @title debt_class
#' @description class definition for debt object
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param balance amount left to pay
#' @param repayment_cash_annual annual repayment amount (optional if repayment_percentage_annual is used instead)
#' @param repayment_percentage_annual percentage of balance to repay (optional is repayment_percentage_annual is used instead)
#' @param interest_rate annual interest rate e.g.: 1\% is entered as 0.01
#' @param promo_interest_rate_end_date end date of temporary promotional annual interest rate (optional - used in conjunction with promo_interest_rate)
#' @param promo_interest_rate temporary annual interest rate during promotion (optional - used in conjunction with promo_interest_rate_end_date)
#' @param currency currency of the loan
#' @param metrics leave unused
#' @param statement leave unused
#' @return a class for debt object
#' @export

debt_class <- setClass(
  Class = "debt_class",
  slots = c(
    balance = "numeric",
    repayment_cash_annual = "numeric",
    repayment_percentage_annual = "numeric",
    interest_rate = "numeric",
    promo_interest_rate_end_date = "character",
    promo_interest_rate = "numeric",
    currency = "character",
    metrics = "list",
    statement = "list"
  )
)
