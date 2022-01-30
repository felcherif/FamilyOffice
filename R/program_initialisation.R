#' @title program_initialisation
#' @description initialise values like tax rates.
#' @author person(given = "frederic", family = "elcherif", email = "f.elcherif@@gmail.com", role = c("aut", "cre"))
#' @return this function returns a text file 'output.txt'
#' @export

program_initialisation <- function() {

  output <- list()                                                                                          # create output list to store everything from the FO function

  # cumulative (effective) tax rates for each currency
  threshold <- tax_rate <- seq(from = 0, to = 500000, by = 1000)
  tax_rate[threshold <= 12500] <- 0
  tax_rate[threshold > 12500 & threshold <= 50000] <- 0.2 * ( threshold[threshold > 12500 & threshold <= 50000] - 12500)
  tax_rate[threshold > 50000 & threshold <= 100000] <- 0.4 * (threshold[threshold > 50000 & threshold <= 100000] - 50000) + 0.2 * (50000 - 12500)
  tax_rate[threshold > 100000 & threshold <= 125000] <- 0.6 * (threshold[threshold > 100000 & threshold <= 125000] - 100000) + 0.4 * (100000 - 50000) + 0.2 * (50000 - 12500)
  tax_rate[threshold > 125000 & threshold <= 150000] <- 0.4 * (threshold[threshold > 125000 & threshold <= 150000] - 125000) + 0.6 * (125000 - 100000) + 0.4 * (100000 - 50000) + 0.2 * (50000 - 12500)
  tax_rate[threshold > 150000] <- 0.45 * ( threshold[threshold > 150000] - 150000) + 0.4 * (150000 - 125000) + 0.6 * (125000 - 100000) + 0.4 * (100000 - 50000) + 0.2 * (50000 - 12500)
  tax_rate <- tax_rate + 0.04 * threshold
  tax_rate[tax_rate != 0] <- tax_rate[tax_rate != 0] / threshold[tax_rate != 0]
  output$economic_data$tax$income$GBP <- data.frame(threshold = threshold,
                                                    tax_rate =  tax_rate)
  output$economic_data$tax$income$CAD <- data.frame(threshold = c(0, 11000, 12000, 13000,  14000,  15000,  16000,   17000,  18000,  19000,  20000,  21000,  22000,  23000, 24000,  25000,  26000,  27000,  28000,  29000,  30000,  31000,  32000,  33000,  34000,  35000,  36000,  37000,  38000,  39000,  40000,  41000,  42000,  43000,  44000,  45000,  46000,  47000,  48000,  49000,  50000,  51000,  52000,  53000,  54000,  55000,  56000,  57000,  58000,  59000,  60000,  61000,  62000,  63000,  64000,  65000,  66000,  67000,  68000,  69000,  70000,  71000,  72000,  73000,  74000,  75000,  80000,  85000,  90000,  95000,  100000, 105000, 110000, 115000, 120000, 125000, 130000, 140000, 150000, 160000, 170000, 180000, 190000, 200000, 250000, 300000, 350000, 400000),
                                                    tax_rate =  c(0, 0,     0.002, 0.0115, 0.0196, 0.0267, 0.0421,  0.0558, 0.0679, 0.0789, 0.0887, 0.0967, 0.1056, 0.113, 0.1198, 0.1260, 0.1317, 0.1371, 0.1420, 0.1466, 0.1509, 0.1549, 0.1587, 0.1622, 0.1655, 0.1686, 0.1716, 0.1744, 0.1771, 0.1796, 0.1820, 0.1842, 0.1864, 0.1885, 0.1915, 0.1945, 0.1973, 0.2004, 0.2040, 0.2074, 0.2107, 0.2138, 0.2169, 0.2198, 0.2226, 0.2253, 0.2279, 0.2304, 0.2328, 0.2352, 0.2373, 0.2396, 0.2417, 0.2438, 0.2458, 0.2477, 0.2496, 0.2514, 0.2532, 0.2549, 0.2565, 0.2582, 0.2597, 0.2613, 0.2627, 0.2642, 0.2709, 0.2768, 0.2837, 0.2913, 0.2996, 0.3071, 0.3148, 0.3217, 0.3281, 0.3339, 0.3394, 0.3490, 0.3583, 0.3671, 0.3749, 0.3819, 0.3881, 0.3936, 0.4207, 0.4395, 0.4528, 0.4629))
  output$economic_data$tax$income$USD <- data.frame(threshold = c(0,    11000, 40000),
                                                    tax_rate =  c(0.15, 0.25,  0.45))
  output$economic_data$tax$income$EUR <- data.frame(threshold = c(0,   20000, 50000, 150000),
                                                    tax_rate =  c(0.1, 0.25,  0.45,  0.65))
  output$economic_data$tax$income$KRW <- data.frame(threshold = c(0,   20000, 50000, 150000),
                                                    tax_rate =  c(0.1, 0.25,  0.45,  0.65))
  output$economic_data$tax$income$CHF <- data.frame(threshold = c(0,   20000, 50000, 150000),
                                                    tax_rate =  c(0.1, 0.25,  0.45,  0.65))

  return(output)
}
