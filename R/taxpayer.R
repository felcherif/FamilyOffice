#' @title taxpayer
#' @description check the input file associated with the user for taxpayer n's name
#' @param user username
#' @param n order of the taxpayer for which to return the name e.g.: '1' or '2'
#' @author person(given = "frederic", given = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @export

taxpayer = function(user, n) {
  filepath <- user_choices(user)
  output   <- list()
  source(file = filepath, local = TRUE)
  return((names(output$reporting$birthdate))[n])
}
