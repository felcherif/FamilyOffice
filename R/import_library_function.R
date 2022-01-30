#' @title import_library_function
#' @description this function applies the different financial cash flow projection function depending on the class of 'x'
#' @return nothing, function are exported to global environment
#' @export

import_library_function <- function() {

  pos <- 1
  envir = as.environment(pos)
  
  # base64enc
  assign(x = "dataURI", value = base64enc::dataURI, envir = envir)

  # datatable
  assign(x = "datatable", value = DT::datatable, envir = envir)
  assign(x = "dataTableOutput", value = DT::dataTableOutput, envir = envir)

  # DT
  assign(x = "formatCurrency", value = DT::formatCurrency, envir = envir)
  assign(x = "formatPercentage", value = DT::formatPercentage, envir = envir)
  assign(x = "JS", value = DT::JS, envir = envir)
  assign(x = "renderDataTable", value = DT::renderDataTable, envir = envir)

  # formattable
  assign(x = "accounting", value = formattable::accounting, envir = envir)

  # ggplot2
  assign(x = "aes", value = ggplot2::aes, envir = envir)
  assign(x = "element_blank", value = ggplot2::element_blank, envir = envir)
  assign(x = "geom_area", value = ggplot2::geom_area, envir = envir)
  assign(x = "geom_hline", value = ggplot2::geom_hline, envir = envir)
  assign(x = "ggplot", value = ggplot2::ggplot, envir = envir)
  assign(x = "ggplotGrob", value = ggplot2::ggplotGrob, envir = envir)
  assign(x = "labs", value = ggplot2::labs, envir = envir)
  assign(x = "scale_color_manual", value = ggplot2::scale_color_manual, envir = envir)
  assign(x = "scale_fill_manual", value = ggplot2::scale_fill_manual, envir = envir)
  assign(x = "theme", value = ggplot2::theme, envir = envir)

  # grid
  assign(x = "grid.draw", value = grid::grid.draw, envir = envir)
  assign(x = "grid.newpage", value = grid::grid.newpage, envir = envir)

  # lubridate
  assign(x = "as_date", value = lubridate::as_date, envir = envir)
  assign(x = "today", value = lubridate::today, envir = envir)
  assign(x = "year", value = lubridate::year, envir = envir)

  # magrittr
  assign(x = "%>%", value = magrittr::`%>%`, envir = envir)

  # methods
  assign(x = "is", value = methods::is, envir = envir)

  # quantmod
  assign(x = "getQuote", value = quantmod::getQuote, envir = envir)

  # reshape2
  assign(x = "melt", value = reshape2::melt, envir = envir)

  # roperators
  assign(x = "%+=%", value = roperators::`%+=%`, envir = envir)
  assign(x = "%-=%", value = roperators::`%-=%`, envir = envir)
  assign(x = "%*=%", value = roperators::`%*=%`, envir = envir)

  # shiny
  assign(x = "div", value = shiny::div, envir = envir)
  assign(x = "shinyApp", value = shiny::shinyApp, envir = envir)

  # shinydashboard
  assign(x = "box", value = shinydashboard::box, envir = envir)
  assign(x = "dashboardBody", value = shinydashboard::dashboardBody, envir = envir)
  assign(x = "dashboardHeader", value = shinydashboard::dashboardHeader, envir = envir)
  assign(x = "dashboardPage", value = shinydashboard::dashboardPage, envir = envir)
  assign(x = "dashboardSidebar", value = shinydashboard::dashboardSidebar, envir = envir)
  assign(x = "menuItem", value = shinydashboard::menuItem, envir = envir)
  assign(x = "sidebarMenu", value = shinydashboard::sidebarMenu, envir = envir)
  assign(x = "tabItem", value = shinydashboard::tabItem, envir = envir)
  assign(x = "tabItems", value = shinydashboard::tabItems, envir = envir)

  # shinydashboardPlus
  assign(x = "box", value = shinydashboardPlus::box, envir = envir)

  # shinyjs
  assign(x = "delay", value = shinyjs::delay, envir = envir)
  assign(x = "hidden", value = shinyjs::hidden, envir = envir)
  assign(x = "toggle", value = shinyjs::toggle, envir = envir)
  assign(x = "useShinyjs", value = shinyjs::useShinyjs, envir = envir)

  # sodium
  assign(x = "password_store", value = sodium::password_store, envir = envir)
  assign(x = "password_verify", value = sodium::password_verify, envir = envir)

  # utils
  assign(x = "capture.output", value = utils::capture.output, envir = envir)
}
