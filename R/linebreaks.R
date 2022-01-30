#' @title linebreaks
#' @description skip multiple line in HTML
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param n number of lines to skip
#' @return this function returns a text file 'output.txt'
#' @family user interface
#' @export

linebreaks <- function(n){
  shiny::HTML(base::strrep(shiny::br(), n))
}
