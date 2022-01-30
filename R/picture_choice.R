#' @title picture_choice
#' @description randomise the choice of picture for graphical user interface background
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))S
#' @export

picture_choice <- function() {
  pic_list <- list.files(path = "inst", pattern = "*.jpg", full.names = TRUE)
  if (length(pic_list) == 1) {
    return(pic_list)
  } else {
    return(sample(pic_list,1))
  }
}

