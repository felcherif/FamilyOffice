#' @title processFile2
#' @description return the sub-portfolio for a class for a given user
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param user name of user 'character'
#' @param class name of the class
#' @export

processFile2 = function(user, class) {
  return(find_portfolio_class(make_portfolio_all(user))[[class]])
}
