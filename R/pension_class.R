#' @title pension_class
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @family class
#' @param value numeric
#' @param minimum_age_withdrawal numeric
#' @param currency character
#' @param cash_withdrawal numeric
#' @param percentage_withdrawal numeric
#' @param tax_rate numeric
#' @param taxpayer character
#' @param metrics list
#' @param statement list
#' @export

pension_class <- setClass(
  Class = "pension_class",
  slots = c(
    value = "numeric",
    minimum_age_withdrawal = "numeric",
    currency = "character",
    cash_withdrawal = "numeric",
    percentage_withdrawal = "numeric",
    tax_rate = "numeric",
    taxpayer = "character",
    metrics = "list",
    statement = "list"))
