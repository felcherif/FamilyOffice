#' @title plot_FO_2
#' @description plotting function for statements
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @param FO object of family office to plot
#' @param input input parameters to plot
#' @export

plot_FO_2 <- function(FO, input) {

  colors      <- c("#8ad6a6", "#a68ad6", "#8abad6", "#d68aba")
  name        <- paste0("'000", c("$", "$", "\UFFE1", "\U20A3", "\U20A9", "\U20AC")[input$reporting_currency == c("CAD","USD","GBP","CHF","KRW","EUR")])
  variable    <- c("cash")
  df          <- FO$statement$simple[["balance_sheet"]]
  time        <- as.Date(FO$reporting$reporting_dates)
  df          <- data.frame(cash = t(df[variable,]))
  df$time     <- time
  df          <- reshape2::melt(df, id.vars = c("time"),)
  df$value    <- as.numeric(df$value)/1000
  df$variable <- as.character(df$variable)
  plot0 <-
    ggplot2::ggplot(df, ggplot2::aes(x = time, y = value))                                                  +
    ggplot2::geom_area(ggplot2::aes(color = variable, fill = variable), alpha = 0.3, position = "identity") +
    ggplot2::geom_hline(yintercept=c(0), linetype="dotted")                                        +
    ggplot2::scale_color_manual(values = colors)                                                   +
    ggplot2::scale_fill_manual(values = colors)                                                    +
    ggplot2::theme(legend.title = ggplot2::element_blank())                                                 +
    ggplot2::labs(title = "", x = "", y = name)

  variable         <- c("gross_profit", "total_expense", "savings")
  df               <- FO$statement$simple[["income_statement"]]
  time             <- as.Date(FO$reporting$reporting_dates)
  df               <- as.data.frame(t(df[variable,]))
  df$time          <- time
  df$total_expense <- - df$total_expense
  df               <- reshape2::melt(df, id.vars = c("time"),)
  df$value         <- as.numeric(df$value)/1000
  df$variable      <- as.character(df$variable)
  plot1            <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = value)) +
    ggplot2::geom_area(ggplot2::aes(color = variable, fill = variable), alpha = 0.3, position = "identity") +
    ggplot2::geom_hline(yintercept=c(0), linetype="dotted")                                        +
    ggplot2::scale_color_manual(values = colors)                                                   +
    ggplot2::scale_fill_manual(values = colors)                                                    +
    ggplot2::theme(legend.title = ggplot2::element_blank())                                                 +
    ggplot2::labs(title = "", x = "", y = name)


  variable           <- c("total_asset", "total_liability", "equity_total_WO_DTA")
  df                 <- FO$statement$simple[["balance_sheet"]]
  time               <- as.Date(FO$reporting$reporting_dates)
  df                 <- as.data.frame(t(df[variable,]))
  df$time            <- time
  df$total_liability <- - df$total_liability
  df                 <- reshape2::melt(df, id.vars = c("time"),)
  df$value           <- as.numeric(df$value)/1000
  df$variable        <- as.character(df$variable)
  plot2 <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = value))                                           +
    ggplot2::geom_area(ggplot2::aes(color = variable, fill = variable), alpha = 0.3, position = "identity") +
    ggplot2::geom_hline(yintercept=c(0), linetype="dotted")                                        +
    ggplot2::scale_color_manual(values = colors)                                                   +
    ggplot2::scale_fill_manual(values = colors)                                                    +
    ggplot2::theme(legend.title = ggplot2::element_blank())                                                 +
    ggplot2::labs(title = "", x = "", y = name)

  variable    <- c("operating_cash_flows", "investing_cash_flows", "financing_cash_flows", "increase_in_cash")
  df          <- FO$statement$simple[["cash_flow"]]
  time        <- as.Date(FO$reporting$reporting_dates)
  df          <- as.data.frame(t(df[variable,]))
  df$time     <- time
  df          <- reshape2::melt(df, id.vars = c("time"),)
  df$value    <- as.numeric(df$value)/1000
  df$variable <- as.character(df$variable)
  plot3 <- ggplot2::ggplot(df, ggplot2::aes(x = time, y = value))                                           +
    ggplot2::geom_area(ggplot2::aes(color = variable, fill = variable), alpha = 0.3, position = "identity") +
    ggplot2::geom_hline(yintercept=c(0), linetype = "dotted")                                      +
    ggplot2::scale_color_manual(values = colors)                                                   +
    ggplot2::scale_fill_manual(values = colors)                                                    +
    ggplot2::theme(legend.title = ggplot2::element_blank())                                                 +
    ggplot2::labs(title = "", x = "", y = name)

  grid::grid.newpage()
  grid::grid.draw(rbind(
    ggplot2::ggplotGrob(plot0),
    ggplot2::ggplotGrob(plot1),
    ggplot2::ggplotGrob(plot2),
    ggplot2::ggplotGrob(plot3), size = "last"))
}


