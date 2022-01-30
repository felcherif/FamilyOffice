#' @title mortgage
#' @description mortgage function
#' @param principal principal amount, numeric
#' @param annual_rate numeric e.g.: 1\% is 0.01
#' @param frequency choice of "years", "quarters", "months", "weeks", "days"
#' @param mortgage_term_in_year number of years on mortgage
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @return mortgage payment: amortisation interest and principal
#' @export

mortgage <-
  function(principal,
           annual_rate,
           frequency,
           mortgage_term_in_year) {
    P <- principal
    r <- annual_rate / frequency
    n <- frequency * mortgage_term_in_year
    amortisation_payment = P * (r * (1 + r) ^ n) / ((1 + r) ^ n - 1)
    interest_payment = P * ((1 + annual_rate) ^ (1 / frequency) - 1)
    return(
      list(
        amortisation_payment = amortisation_payment,
        interest_payment = interest_payment,
        principal_payment = amortisation_payment - interest_payment))
  }
