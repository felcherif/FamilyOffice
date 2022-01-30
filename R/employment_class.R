#' @title employment_class
#' @description employment class
#' @param retirement_age = "numeric",
#' @param yearly_salary = "numeric",
#' @param currency = "character",
#' @param taxpayer = "character",
#' @param tax_rate = "numeric",
#' @param metrics = "list",
#' @param statement = "list"
#' @export

employment_class <- setClass(
  Class = "employment_class",
  slots = c(
    retirement_age = "numeric",
    yearly_salary = "numeric",
    currency = "character",
    taxpayer = "character",
    tax_rate = "numeric",
    metrics = "list",
    statement = "list"))

