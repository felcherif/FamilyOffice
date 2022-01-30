#' @title cash_account_class
#' @family class
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param GBP "numeric"
#' @param CAD = "numeric"
#' @param USD = "numeric"
#' @param EUR = "numeric"
#' @param KRW = "numeric"
#' @param CHF = "numeric"
#' @param metrics = "list"
#' @param statement = "list"
#' @export

cash_account_class <-
  setClass(
    Class = "cash_account_class",
    slots = c(
      GBP = "numeric",
      CAD = "numeric",
      USD = "numeric",
      EUR = "numeric",
      KRW = "numeric",
      CHF = "numeric",
      metrics = "list",
      statement = "list"))
