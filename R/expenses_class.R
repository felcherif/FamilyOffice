#' @title expenses_class
#' @family class
#' @param currency = "character"
#' @param expenses = "list"
#' @param metrics = "list"
#' @param statement = "list"
#' @return an object of expenses class
#' @export

expenses_class <- setClass(
  Class = "expenses_class",
  slots = c(
    currency = "character",
    expenses = "list",
    metrics = "list",
    statement = "list"))

