#' @title user_choices
#' @description function that returns the files accessible to a user in the app folder
#' @param user this is the name of the user
#' @return this function returns a text file 'output.txt'
#' @export

user_choices <- function(user) {

  if (user %in% c("David", "Marie-Claude")) {
    filepath <-
      list.files(path = "inst", full.names = TRUE) |>
      grep(pattern = "David_&_Marie-Claude", value = TRUE)
  }

  if (user %in% c("Tarek", "Marie")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Marie_&_Tarek", value = TRUE)
  }

  if (user %in% c("Mi Jeong","Frederic")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Frederic_&_Mi_Jeong", value = TRUE)
  }

  if (user %in% c("FredericSP", "KyungA")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "FredericSP_&_KyungA", value = TRUE)
  }

  if (user %in% c("Nico", "Esther")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Nico_&_Esther", value = TRUE)
  }

  if (user %in% c("Bertrand")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Bertrand", value = TRUE)
  }

  if (user %in% c("SylvainP")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "SylvainP", value = TRUE)
  }

  if (user %in% c("Sarah")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Sarah", value = TRUE)
  }

  if (user %in% c("SylvainF", "Amandine")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "SylvainF_&_Amandine", value = TRUE)
  }

  if (user %in% c("Ryan")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "Ryan", value = TRUE)
  }

  if (user %in% c("John Doe","Jane Doe")) {
     filepath <-
       list.files(path = "inst", full.names = TRUE) |>
       grep(pattern = "John_Doe_&_Jane_Doe", value = TRUE)
  }

  return(filepath)

}


