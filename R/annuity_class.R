#' @title annuity_class
#' @family class
#' @param annual_notional = numeric
#' @param minimum_age_payment = numeric
#' @param maximum_age_payment = numeric
#' @param currency = character
#' @param inflation_indexation = logical
#' @param tax_rate = numeric
#' @param taxpayer = character
#' @param metrics = list
#' @param statement = "list
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @export

annuity_class <- methods::setClass(
  Class = "annuity_class",
  slots = c(
    annual_notional = "numeric",
    minimum_age_payment = "numeric",
    maximum_age_payment = "numeric",
    currency = "character",
    inflation_indexation = "logical",
    tax_rate = "numeric",
    taxpayer = "character",
    metrics = "list",
    statement = "list"))
