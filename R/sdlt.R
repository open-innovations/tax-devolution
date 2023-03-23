# stamp duty

# sdlt_rates_2021_07_01 <- data.frame(
#   band = c(250000, 925000, 1.5e6, Inf),
#   rate = c(0, 0.05, 0.1, 0.12)
# )
#
#
#
# sdlt_payable <- function(price, i = 1) {
#
#   taxable_value <- sdlt_rates$band[i] - ifelse(i == 1, 0, sdlt_rates$band[i - 1])
#
#   if (price <= taxable_value) {
#     return(price * sdlt_rates$rate[i])
#   }
#
#   return(
#     taxable_value * sdlt_rates$rate[i] +
#       sdlt_payable(
#         (price - taxable_value), i = i + 1)
#   )
# }
#
# sdlt_payable(250000)
# sdlt_payable(260000)
# sdlt_payable(1e6)
# sdlt_payable(1.6e6)
# sdlt_payable(10e6)

# TODO update with historic rates post 2014 Osborne reforms

sdlt_rates <- data.frame(
  band = c(250000, 925000, 1.5e6, Inf),
  rate = c(0, 0.05, 0.1, 0.12)
)

calc_sdlt_payable <- function(price, i = 1) {

  taxable_value <- sdlt_rates$band[i] - if (i == 1) 0 else sdlt_rates$band[i - 1]
  tax <- min(price, taxable_value) * sdlt_rates$rate[i]

  if (price > taxable_value) {
    tax <- tax + calc_sdlt_payable(price - taxable_value, i = i + 1)
  }

  return(tax)
}

sdlt_payable(250000)
sdlt_payable(300000)
sdlt_payable(1e6)
sdlt_payable(1.5e6)
sdlt_payable(1.6e6)
sdlt_payable(1.7e6)
sdlt_payable(1.8e6)
sdlt_payable(10e6)

ppd_2022$sdlt <- sapply(ppd_2022$Price, calc_sdlt_payable)

ppd_2022 |> dplyr::group_by(oslaua) |> dplyr::summarise(sdlt = sum(sdlt)) |> dplyr::arrange(dplyr::desc(sdlt))

ppd$sdlt <- sapply(ppd$Price, calc_sdlt_payable)

ppd |>
  dplyr::group_by(substr(`Date of Transfer`, 1, 4)) |>
  dplyr::summarise(sum(sdlt))
