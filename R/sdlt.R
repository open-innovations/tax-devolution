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

calc_sdlt_payable(250000)
calc_sdlt_payable(300000)
calc_sdlt_payable(1e6)
calc_sdlt_payable(1.5e6)
calc_sdlt_payable(1.6e6)
calc_sdlt_payable(1.7e6)
calc_sdlt_payable(1.8e6)
calc_sdlt_payable(10e6)

ppd_england_2022$sdlt <- sapply(ppd_england_2022$price, calc_sdlt_payable)

sdlt_2022 <- ppd_england_2022 |>
  dplyr::group_by(oslaua) |>
  dplyr::summarise(sdlt = sum(sdlt))

ctsop_la_rgn_lookup <- readRDS("~/Projects/Tax-Devolution/app/app.data/ctsop_la_rgn_lookup.rds")

sdlt_2022_la <- sdlt_2022 |>
  dplyr::inner_join(ctsop_la_rgn_lookup,
                    by = c("oslaua" = "geography_code")) |>
  dplyr::rename(geography_code = oslaua)

sdlt_2022_rgn <- sdlt_2022_la |>
  dplyr::group_by(region_code, region_name) |>
  dplyr::summarise(sdlt = sum(sdlt)) |>
  dplyr::mutate(geography_type = "REGL") |>
  dplyr::rename(geography_code = region_code,
                geography_name = region_name)

sdlt_2022_final <- dplyr::bind_rows(sdlt_2022_rgn, sdlt_2022_la)

saveRDS(sdlt_2022_final, "app/app.data/sdlt.rds")

# Time series
# ppd$sdlt <- sapply(ppd$Price, calc_sdlt_payable)
#
# ppd |>
#   dplyr::group_by(substr(`Date of Transfer`, 1, 4)) |>
#   dplyr::summarise(sum(sdlt))
