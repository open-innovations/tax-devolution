# council-tax

# data import -------------------------------------------------------------

import_ppd <- function(update = FALSE, url = NULL, path = NULL) {
  ppd_url <- "http://prod.publicdata.landregistry.gov.uk.s3-website-eu-west-1.amazonaws.com/pp-complete.csv"
  ppd_path <- "~/Data/Land Registry/pp-complete.csv"
  if (update) {
    download.file(url = ppd_url,
                  destfile = ppd_path,
                  mode = "wb")
  } else {
    ppd_source <- "~/Data/Land Registry/ppd-complete.csv"
  }

  ppd_cols <- c("id",
                "price",
                "date",
                "postcode",
                "property_type",
                "old_new",
                "duration",
                "paon",
                "saon",
                "street",
                "locality",
                "town_city",
                "district",
                "county",
                "ppd_category_type",
                "record_status"
  )

  ppd <- readr::read_csv(ppd_path,
                         col_names = ppd_cols,
                         col_select = c(date, postcode, price)
  )

  return(ppd)
}

import_onspd <- function(update = FALSE) {
  if (update) {
    stop("Automatic updating not yet enabled.")
  }

  onspd <- readr::read_csv("~/Data/Geodata/ONSPD/ONSPD_FEB_2023_UK/Data/ONSPD_FEB_2023_UK.csv", col_select = c(pcds, oslaua, rgn))

  return(onspd)
}

# ppd <- import_ppd()
# onspd <- import_onspd()

# ppd_onspd <- dplyr::inner_join(ppd, onspd, by = c("postcode" = "pcds"))
# |>
  # dplyr::select(-postcode)
# rm(ppd, onspd)

# saveRDS(ppd_onspd, "data/ppd_onspd.rds")


# TODO import CTSOP, CTB and CTR here

# global variables --------------------------------------------------------

cpi <- readr::read_csv("data-raw/CPI Index Annual.csv") |>
  dplyr::mutate(index_1991 = value / value[dates.date == "1991-01-01"]) |>
  dplyr::mutate(index_1993 = value / value[dates.date == "1993-01-01"]) |>
  dplyr::mutate(index_1995 = value / value[dates.date == "1995-01-01"]) |>
  dplyr::mutate(index_2022 = value / value[dates.date == "2022-01-01"])

# saveRDS(cpi, "app/app.data/cpi.rds")

ct_bands_england_1991 <- data.frame(
  band  = LETTERS[1:8],
  min   = c(0, 40001, 52001, 68001, 88001, 120001, 160001, 320001),
  max   = c(40000, 52000, 68000, 88000, 120000, 160000, 320000, Inf),
  ratio = c(6/9, 7/9, 8/9,  9/9,  11/9, 13/9, 15/9, 18/9)
) # |> dplyr::mutate(cpi_2022 = max * cpi$index_1991[cpi$dates.date == "2022-01-01"])

ct_bands_england_npp_superbands <- data.frame(
  band  = c("X", "Y", "Z"),
  min   = c(2e6 + 1, 10e6 + 1, 20e6 + 1),
  max   = c(10e6, 20e6, Inf),
  ratio = c(8, 16, 32)
)

ct_bands_england_1991_npp <- data.frame(
  band  = c(LETTERS[1:8], LETTERS[24:26]),
  min   = c(0, 40001, 52001, 68001, 88001, 120001, 160001, 320001,
            2e6 + 1, 10e6 + 1, 20e6 + 1),
  max   = c(40000, 52000, 68000, 88000, 120000, 160000, 320000,
            2e6, 10e6, 20e6, Inf),
  ratio = c(6/9, 7/9, 8/9,  9/9,  11/9, 13/9, 15/9, 18/9,
            8, 16, 32)
)

ct_bands_wales_2003 <- data.frame(
  band  = LETTERS[1:9],
  min   = c(0, 44001, 65001, 91001, 123001, 162001, 223001, 324001, 424001),
  max   = c(44000, 65000, 91000, 123000, 162000, 223000, 324000, 424000, Inf),
  ratio = c(6/9, 7/9, 8/9, 9/9, 11/9, 13/9, 15/9, 18/9, 21/9)
)

ct_bands_scotland_1991 <- data.frame(
  band  = LETTERS[1:8],
  min   = c(0, 27001, 35001, 45001, 58001, 80001, 106001, 212001),
  max   = c(27000, 35000, 45000, 58000, 80000, 106000, 212000, Inf),
  ratio = c(6/9, 7/9, 8/9, 9/9, 131/100, 163/100, 49/25, 49/20)
)

ppd_onspd <- readRDS("data/ppd_onspd.rds")

ppd_england_2022 <- ppd_onspd |>
  dplyr::filter(date >= "2022-01-01",
                date <= "2022-12-31",
                substr(oslaua, 1, 1) == "E")

# verify number of local authorities
ppd_england_2022$oslaua |> unique() |> length()
# 309

process_ctsop <- function(x) {
  df <- x |>
    dplyr::select(-all_properties) |>
    tidyr::pivot_longer(cols = band_a:band_i,
                        names_to = "band",
                        values_to = "n_properties",
                        values_drop_na = TRUE
                        ) |>
    dplyr::mutate(n_properties = ifelse(n_properties == "-",
                                        0,
                                        n_properties)) |>
    dplyr::mutate(band = toupper(substr(band, 6, 6))) |>
    dplyr::mutate(n_properties = as.numeric(n_properties)) |>
    dplyr::group_by(area_name) |>
    dplyr::mutate(proportion = n_properties / sum(n_properties)) |>
    dplyr::mutate(cumprop = cumsum(proportion))
  return(df)
}

# Analysis ----------------------------------------------------------------

# .. = not valid (i.e band I in Wales)
# - = no properties in this band - should be 0
# ctsop_2022 <- readr::read_csv('data-raw/Council Tax/CTSOP1-0-1993-2022/CTSOP1_0_2022_03_31.csv', na = c("..", "-")) |>
#   dplyr::mutate(band_i = as.numeric(band_i)) |>
#   process_ctsop()

ctsop_2023 <- readr::read_csv('~/Data/VOA/CTSOP/CTSOP1_0_1993_2023/CTSOP1_0_2023_03_31.csv', na = c(".."), col_types = readr::cols(.default = readr::col_character())) |>
  # dplyr::mutate(band_i = as.numeric(band_i)) |>
  process_ctsop()

ctsop_2023_england <- ctsop_2023 |>
  dplyr::filter(area_name == "ENGLAND")

ctsop_2023_laua <- ctsop_2023 |>
  dplyr::filter(ecode %in% ppd_england_2022$oslaua)

# verify number of local authorities
ctsop_2023_laua$ecode |> unique() |> length()
# 309

# england-wide property bands at 2022 prices
quantile(ppd_england_2022$price, probs = ctsop_2023_england$cumprop)

# test difference in methodologies
test <- list()
for (type in 1:9) {
  test[[type]] <- quantile(ppd_england_2022$price,
                           probs = ctsop_2023_england$cumprop,
                           type = type)
}

# how many sold in each la?
n_transactions <- ppd_england_2022 |>
  dplyr::group_by(oslaua) |>
  dplyr::summarise(n = dplyr::n())

range(n_transactions$n)
hist(n_transactions$n)

# how many properties in each la?
n_properties <- ctsop_2023_laua |>
  dplyr::group_by(ecode) |>
  dplyr::summarise(n = sum(n_properties))

hist(n_properties$n)

# proportion sold in each la in 2022
proportion_sold_la <- dplyr::left_join(n_properties, n_transactions,
                                       by = c("ecode" = "oslaua")) |>
  dplyr::mutate(prop_sold = n.y/n.x)

hist(proportion_sold_la$prop_sold)
summary(proportion_sold_la$prop_sold)

# proportion of properties in the new super bands
ecdf(ppd_england_2022$price)(c(2e6, 10e6, 20e6)) |>
  setNames(c("X", "Y", "Z"))

# calculate new A-H bands for all England LAs based on 2022 prices
# keeping proportion of properties in each band the same
new_bands <- list()
for (la in unique(ctsop_2023_laua$ecode)) {
  la_name <- unique(ctsop_2023_laua$area_name[ctsop_2023_laua$ecode == la])
  new_bands[[la_name]] <- quantile(ppd_england_2022$price[ppd_england_2022$oslaua == la], ctsop_2023_laua$cumprop[ctsop_2023_laua$ecode == la]) |>
    setNames(LETTERS[1:8])
}

new_bands_df <- as.data.frame(do.call(rbind, new_bands))

# need to calculate a cumprop value for each LA which takes into account
# a revised band H and the new NPP bands X, Y, Z

# the new max value for band H is going to be the value which the
# new £2m band X kicks in. We'll need to calculate the percentile for this
# for each local authority

# The national level for this will be
eng_superbands_ptiles <- ecdf(ppd_england_2022$price)(c(2e6, 10e6, 20e6))
# [1] 0.9895456 0.9989964 0.9995767
# These numbers should replace the max values for bands H, X and Y
# (Band Z max value will always be Inf)

# We can only add superbands to the original 1991 bands. If we revalue them
# to modern valuations, then Band X kicks in too low in many places

# We don't have prices for 1991, so we use 1995 instead

ppd_england_1995 <- ppd_onspd |>
  dplyr::filter(date >= "1995-01-01",
                date <= "1995-12-31") |>
  dplyr::filter(substr(oslaua, 1, 1) == "E")


ecdf(ppd_england_1995$price)(ct_bands_england_1991_npp$max)
ecdf(ppd_england_2022$price)(ct_bands_england_1991_npp$max)
ecdf(ppd_england_2022$price)(ct_bands_england_1991$max)



england_bands <- quantile(ppd_england_2022$price, ctsop_2023_england$cumprop) |> setNames(LETTERS[1:8])

new_national_bands_cumsum <- list()
for (la in unique(ctsop_2023_laua$ecode)) {
  la_name <- unique(ctsop_2023_laua$area_name[ctsop_2023_laua$ecode == la])
  new_national_bands_cumsum[[la_name]] <- ecdf(ppd_england_2022$price[ppd_england_2022$oslaua == la])(england_bands) |>
    setNames(LETTERS[1:8])
}

new_national_bands_df <- as.data.frame(do.call(rbind, new_national_bands_cumsum))
new_national_bands_df$area_name <- row.names(new_national_bands_df)
new_national_bands_df1 <- new_national_bands_df |>
  tidyr::pivot_longer(-area_name, names_to = "band", values_to = "cumsum") |>
  dplyr::group_by(area_name) |>
  dplyr::mutate(proportion = diff(c(0, cumsum))) |>
  dplyr::mutate(n_properties = sum(ctsop_2023_laua$n_properties[ctsop_2023_laua$area_name == area_name]) * proportion) |>
  dplyr::mutate(new_band_d_equivalents = n_properties * ct_bands_england_1991$ratio[ct_bands_england_1991$band == band]) |>
  dplyr::summarise(new_band_d_equivalents = sum(new_band_d_equivalents))

current_national_bands_df <- ctsop_2023_laua |>
  dplyr::mutate(current_band_d_equivalents = n_properties * ct_bands_england_1991$ratio[ct_bands_england_1991$band == band]) |>
  dplyr::summarise(current_band_d_equivalents = sum(current_band_d_equivalents))

# combine band_d equivalents

compare_band_d_equivalents <- dplyr::inner_join(new_national_bands_df1,
                                                current_national_bands_df) |>
  dplyr::mutate(difference = (new_band_d_equivalents - current_band_d_equivalents)/ current_band_d_equivalents)

# readr::write_csv(compare_band_d_equivalents, 'data/compare_band_d_equivalents.csv')

# TODO replicate for proportion of properties in each band at system inception




# old stuff ---------------------------------------------------------------






# distribution of prices in the first year UK-wide
# 1995 ppd distribution by decile
quantile(ppd$price[substr(ppd$date, 1, 4) == "1995"], probs = seq(0, 1, 0.1))

# Get distribution of properties by band at inception

ctsop_1993 <- readr::read_csv('data-raw/Council Tax/CTSOP1-0-1993-2022/CTSOP1_0_1993_04_01.csv', na = c("..", "-")) |>
  dplyr::mutate(band_i = as.numeric(band_i))

# saveRDS(ctsop_1993, "app/app.data/ctsop_1993.rds")


# distribution of England properties by band in 1993

ctsop_1993_eng <- ctsop_1993 |>
  dplyr::filter(area_name == "ENGLAND") |>
  process_ctsop()

ggplot2::ggplot(ctsop_1993_eng, ggplot2::aes(x = band, y = proportion)) +
  ggplot2::geom_col() +
  ggplot2::labs(title = "Distribution of properties by CT band (England, 1993)")

# distribution of English regions properties by band in 1993
ctsop_1993_rgn <- ctsop_1993 |>
  dplyr::filter(geography == "REGL") |>
  process_ctsop()

ggplot2::ggplot(ctsop_1993_rgn, ggplot2::aes(x = band, y = proportion)) +
  ggplot2::geom_col(position = "dodge") +
  ggplot2::facet_wrap("area_name", labeller = ggplot2::label_wrap_gen(20)) +
  ggplot2::labs(title = "Distribution of properties by Council Tax band, 1993")

# 2022 analysis - first format to long





ctsop_2022_england <- ctsop_2022 |>
  dplyr::filter(area_name == "ENGLAND") |>
  process_ctsop()

ctsop_2022_rgn <- ctsop_2022 |>
  dplyr::filter(geography == "REGL") |>
  process_ctsop()

ppd_2022 <- ppd |>
  dplyr::filter(`Date of Transfer` >= "2022-01-01",
                `Date of Transfer` <= "2022-12-31")

quantile(ppd_2022$Price, ctsop_2022_england$cumprop)

# distribution by region, 1993 and 2022

ctsop_1993_2022_rgn <- list(`1993` = ctsop_1993_rgn,
                            `2022` = ctsop_2022_rgn) |>
  dplyr::bind_rows(.id = "year")

ggplot(ctsop_1993_2022_rgn, aes(x = band, y = proportion, fill = year)) +
  geom_col(position = "dodge") +
  facet_wrap("area_name", labeller = ggplot2::label_wrap_gen(20)) +
  labs(title = "Distribution of properties by Council Tax band, 1993 & 2022")


# Band D equivalence

regional_band_d_equivalence <- ctsop_1993_2022_rgn |>
  dplyr::left_join(ct_bands_england_1991) |>
  dplyr::mutate(band_d_equivalence = n_properties * ratio) |>
  dplyr::ungroup() |>
  dplyr::group_by(year, area_name) |>
  dplyr::summarise(n_properties = sum(n_properties, na.rm = TRUE),
                   band_d_equivalent = sum(band_d_equivalence, na.rm = TRUE))

ggplot(regional_band_d_equivalence, aes(x = area_name, y = band_d_equivalent, fill = year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Number of Band D equivalent properties",
       x = "Region",
       y = "Number of Band D equivalent properties") +
  scale_y_continuous(labels = scales::label_comma())


# what should the bands be in 1995 based on same distribution as 1993 inception?

ct_bands_1995 <- quantile(ppd_1995$Price, probs = ctsop_1993_eng$cumprop) |>
  setNames(LETTERS[1:8])

# Equivalent bands on 2022 property values by keeping share of properties in each constant

ct_bands_2022_prop_n <- quantile(ppd_2022$Price, probs = ctsop_1993_eng$cumprop[1:8])


# Look at Greater Manchester
ppd_2022_gm <- ppd |>
  dplyr::filter(`Date of Transfer` >= "2022-01-01",
                `Date of Transfer` <= "2022-12-31") |>
  dplyr::filter(oslaua %in% c(paste0("E0800000", 1:9), "E08000010"))

ct_bands_2022_prop_n_gm <- quantile(ppd_2022_gm$Price,
                                    probs = ctsop_eng$cumprop)

# Get 2022 CTSOP for GM
ctsop_2022_gm <- readr::read_csv('data-raw/Council Tax/CTSOP1-0-1993-2022/CTSOP1_0_2022_03_31.csv', na = c("..", "-")) |>
  dplyr::select(-c(band_i, all_properties)) |>
  dplyr::filter(ecode == "E11000001") |>
  tidyr::pivot_longer(cols = -(1:4), names_to = "band", values_to = "n_properties") |>
  dplyr::mutate(proportion = n_properties / sum(n_properties, na.rm = TRUE)) |>
  dplyr::mutate(cumprop = cumsum(proportion))

ggplot(ctsop_2022_gm, aes(x = band, y = n_properties)) +
  geom_col()

dplyr::bind_rows(ctsop_eng, ctsop_2022_gm) |>
  ggplot(aes(x = substr(band, 6, 6), y = proportion, fill = area_name)) +
  geom_col(position = "dodge")

# gm 2022 prices by percentile
quantile(ppd_2022_gm$Price, probs = seq(0, 1, 0.1))

# plot by new England bands
ctsop_2022_gm_new <- ctsop_2022_gm |>
  dplyr::mutate(new_cumprop = ecdf(ppd_2022_gm$Price)(ct_bands_2022_prop_n)) |>
  dplyr::mutate(new_proportion = dplyr::coalesce(new_cumprop - dplyr::lag(new_cumprop), new_cumprop)) |>
  dplyr::mutate(new_n_properties = new_cumprop * sum(n_properties))

ctsop_2022_gm_new |>
  dplyr::select(band, proportion, new_proportion) |>
  tidyr::pivot_longer(cols = c(proportion, new_proportion)) |>
  ggplot(aes(x = band, y = value, fill = name)) +
  geom_col(position = "dodge")

# plot by new GM bands
ctsop_2022_gm |>
  dplyr::mutate(new_cumprop = ecdf(ppd_2022_gm$Price)(ct_bands_2022_prop_n_gm)) |>
  dplyr::mutate(proportion_new_GM_bands = dplyr::coalesce(new_cumprop - dplyr::lag(new_cumprop), new_cumprop)) |>
  dplyr::mutate(new_n_properties = new_cumprop * sum(n_properties)) |>
  dplyr::select(band, proportion, proportion_new_GM_bands) |>
  tidyr::pivot_longer(cols = c(proportion, proportion_new_GM_bands)) |>
  dplyr::mutate(band = substr(band, 6, 6) |> toupper()) |>
  ggplot(aes(x = band, y = value, fill = name)) +
  geom_col(position = "dodge")



# First common date across PPD and CTSOP is 1995, so test against those

ctsop_1995 <- readr::read_csv('data-raw/Council Tax/CTSOP1-0-1993-2022/CTSOP1_0_1995_03_31.csv', na = c("..", "-")) |>
  dplyr::mutate(band_i = as.numeric(band_i))

ctsop_eng_1995 <- ctsop_1995 |> dplyr::filter(area_name == "ENGLAND") |>
  dplyr::select(-all_properties) |>
  tidyr::pivot_longer(cols = -(1:4), names_to = "Band") |>
  dplyr::mutate(proportion = value / sum(value, na.rm = TRUE)) |>
  dplyr::mutate(cumprop = cumsum(proportion))

ppd_1995 <- ppd |>
  dplyr::filter(`Date of Transfer` >= "1995-01-01",
                `Date of Transfer` <= "1995-12-31")

ct_bands_1995_prop_n <- quantile(ppd_1995$Price, ctsop_eng_1995$cumprop[1:8])

# ratio of one bands to previous band

bands_1995 <- substr(names(ct_bands_1995_prop_n), 1, nchar(names(ct_bands_1995_prop_n)) - 1) |> as.numeric()

for (i in 1:length(bands_1995) - 1) {
  print((bands_1995[i + 1] - bands_1995[i]) / bands_1995[i])
}

tibble::tibble(y1995 = ct_bands_1995_prop_n,
               y2022 = ct_bands_2022_prop_n,
               growth = (y2022 - y1995) / y1995)

# how many properties changes hands multiple times?
ppd_by_property <- ppd |>
  dplyr::mutate(address = paste(PAON, SAON, Postcode)) |>
  dplyr::group_by(address) |>
  dplyr::summarise(n = dplyr::n()) |>
  dplyr::arrange(dplyr::desc(n))

# apply Welsh bands to 2003 national prices

ctsop_2022_wales <- readr::read_csv('data-raw/Council Tax/CTSOP1-0-1993-2022/CTSOP1_0_2022_03_31.csv', na = c("..", "-")) |>
  dplyr::select(-all_properties) |>
  dplyr::filter(area_name == "WALES") |>
  tidyr::pivot_longer(cols = -(1:4), names_to = "band", values_to = "n_properties") |>
  dplyr::mutate(proportion = n_properties / sum(n_properties, na.rm = TRUE)) |>
  dplyr::mutate(cumprop = cumsum(proportion))

quantile(ppd_2022$Price, ctsop_2022_wales$cumprop)


ppd_2003 <- ppd |>
  dplyr::filter(`Date of Transfer` >= "2003-01-01",
                `Date of Transfer` <= "2003-12-31")

quantile(ppd_2003$Price, probs = ctsop_2022_wales$cumprop)





# Using 1991 bands CPI uprated --------------------------------------------

ppd_2022_distribution <- quantile(ppd_2022$Price, seq(0, 1, 0.01))
OriginalBandsCpiInflated <- ecdf(ppd_2022_distribution)(ct_bands_england_1991$cpi_2022) |> setNames(LETTERS[1:8])

new_proportions <- diff(c(0, OriginalBandsCpiInflated))

# add to main table

ct_bands_england_1991$cpi_2022_prop <- new_proportions
ct_bands_england_1991$cpi_2022_n_properties <- sum(ctsop_2022_england$n_properties) * ct_bands_england_1991$cpi_2022_prop

ct_bands_england_1991$cpi_2022_band_d_equiv <- ct_bands_england_1991$cpi_2022_n_properties * ct_bands_england_1991$ratio

ct_bands_cpi_inflated_band_d_equivalents <- sum(ct_bands_england_1991$cpi_2022_band_d_equiv)

current_band_d_equivalents <- regional_band_d_equivalence[regional_band_d_equivalence$year == 2022, ]$band_d_equivalent |> sum()

additional_band_d_equivalents <- abs(ct_bands_cpi_inflated_band_d_equivalents - current_band_d_equivalents)

additional_band_d_equivalents * 1898 # 1898 is 2021-22 avg Band D

# build and plot

data.frame(band = LETTERS[1:8],
           `Original bands` = ctsop_2022_england$proportion,
           `Original bands inflated by CPI` = ct_bands_england_1991$cpi_2022_prop) |>
  tidyr::pivot_longer(-band) |>
  ggplot(aes(x = band, y = value, fill = name)) +
  geom_col(position = "dodge") +
  labs(title = "Proportion of houses in each CT band",
       subtitle = "Original bands vs CPI inflated bands to 2022 and revaluing to 2022 prices",
       y = "Proportion",
       fill = "") +
  theme(legend.position = "top")

# REDO the above to account by region -------------------------------------

# store an ecdf function
ppd_2022_ecdf <- ecdf(ppd_2022_distribution)

# test calling it by passing a vector of bands (nominal values for top of each band)
ppd_2022_ecdf(ct_bands_england_1991$cpi_2022)

# write a list which builds an ecdf for each region
ppd_2022_ecdf_rgn <- list()
for (region in unique(ppd_2022$rgn)) {
  ppd_2022_rgn <- dplyr::filter(ppd_2022, rgn == region)
  ppd_2022_rgn_dist <- quantile(ppd_2022_rgn$Price, seq(0, 1, 0.01), type = 1)
  ppd_2022_ecdf_rgn[[region]] <- ecdf(ppd_2022_rgn_dist)
}

object.size(ppd_2022_ecdf_rgn) # 162KB

# and a list for each lad
ppd_2022_ecdf_lad <- list()
for (lad in unique(ppd_2022$oslaua)) {
  ppd_2022_lad <- dplyr::filter(ppd_2022, oslaua == lad)
  ppd_2022_lad_dist <- quantile(ppd_2022_lad$Price, seq(0, 1, 0.01), type = 1)
  ppd_2022_ecdf_lad[[lad]] <- ecdf(ppd_2022_lad_dist)
}

object.size(ppd_2022_ecdf_lad) # 5.4MB

ppd_2022_ecdf <- c(ppd_2022_ecdf_lad, ppd_2022_ecdf_rgn)
saveRDS(ppd_2022_ecdf, "app/app.data/ppd_2022_ecdf.rds")

# sizes of these are small enough to keep the web app light (raw ppd data for 2022 is 26MB)

# pass a set of chosen bands (nominal values for top of each band) into the ecdf. We'll use the CPI uprated version of the original band structure

temp.bands <- ct_bands_england_1991$cpi_2022
names(temp.bands) <- LETTERS[1:length(temp.bands)]


temp.df <- data.frame(band = LETTERS[1:length(temp.bands)])
for (area in names(ppd_2022_ecdf_rgn)[names(ppd_2022_ecdf_rgn) != "W99999999"]) {
   temp.df[[area]] <- ppd_2022_ecdf_rgn[[area]](temp.bands)
   temp.df[[area]] <- diff(c(0, temp.df[[area]]))# cumdist to dist
}

temp.df |>
  tidyr::pivot_longer(cols = -band, names_to = "region", values_to = "n_prop") |>
  ggplot(aes(x = band, y = n_prop)) +
  geom_col() +
  facet_wrap("region")

# temp.list <- list()
# for (region in names(ppd_2022_ecdf_rgn)[names(ppd_2022_ecdf_rgn) != "W99999999"]) {
#   temp.list[["band"]] <- LETTERS[1:length(temp.bands)]
#   temp.list[[region]] <- ppd_2022_ecdf_rgn[[region]](temp.bands)
#   temp.list[[region]] <- diff(c(0, temp.list[[region]])) # cumdist to dist
#   temp.list[[region]] <- round(temp.list[[region]] * ctsop_2022[ctsop_2022$ecode == region, ]$all_properties)
# }

# this now gives us number of properties in each geog by band
# as.data.frame(temp.list)|>
#   tidyr::pivot_longer(cols = -band, names_to = "region", values_to = "n_prop") |>


# geog lookups

ctsop_la_rgn_lookup <- ctsop_1993 |>
  dplyr::select(geography_type = geography,
                ba_code,
                geography_code = ecode,
                geography_name = area_name) |>
  dplyr::mutate(region_code = ifelse(geography_type == "REGL", geography_code, NA),
                region_name = ifelse(geography_type == "REGL", geography_name, NA)) |>
  tidyr::fill(region_code, region_name) |>
  dplyr::mutate(region_code = ifelse(geography_type == "REGL", NA, region_code),
                region_name = ifelse(geography_type == "REGL", NA, region_name))

saveRDS(ctsop_la_rgn_lookup, "app/app.data/ctsop_la_rgn_lookup.rds")



# ppd 1995 ecdf

ppd_1995_ecdf_rgn <- list()
for (region in unique(ppd_1995$rgn)) {
  ppd_1995_rgn <- dplyr::filter(ppd_1995, rgn == region)
  ppd_1995_rgn_dist <- quantile(ppd_1995_rgn$Price, seq(0, 1, 0.01), type = 1)
  ppd_1995_ecdf_rgn[[region]] <- ecdf(ppd_1995_rgn_dist)
}

object.size(ppd_1995_ecdf_rgn) # 162KB

# and a list for each lad
ppd_1995_ecdf_lad <- list()
for (lad in unique(ppd_1995$oslaua)) {
  ppd_1995_lad <- dplyr::filter(ppd_1995, oslaua == lad)
  ppd_1995_lad_dist <- quantile(ppd_1995_lad$Price, seq(0, 1, 0.01), type = 1)
  ppd_1995_ecdf_lad[[lad]] <- ecdf(ppd_1995_lad_dist)
}

object.size(ppd_1995_ecdf_lad) # 5.4MB

ppd_1995_ecdf <- c(ppd_1995_ecdf_lad, ppd_1995_ecdf_rgn)
saveRDS(ppd_1995_ecdf, "app/app.data/ppd_1995_ecdf.rds")

# collect effective number of band d properties allowing for benefits etc

council_tax_base <- readxl::read_excel("data-raw/Council_Taxbase_local_authority_level_data_2022.xlsx", sheet = "Council_Taxbase_Data_CS", skip = 5)

council_tax_base_rgn <- council_tax_base |>
  left_join(ctsop_la_rgn_lookup, by = c("ONS Code" = "geography_code")) |>
  group_by(region_code, region_name) |>
  summarise(adjusted_band_d_equivalents = sum(adjusted_band_d_equivalents)) |>
  filter(!is.na(region_code)) |>
  rename(`ONS Code` = region_code,
         `Local Authority` = region_name)

bind_rows(council_tax_base, council_tax_base_rgn) |>
  saveRDS("app/app.data/council_tax_base.rds")


# rework the above to collect bands in the way of CTSOP
# we're going to make it exactly the same shape as CTSOP so we don't have to rewrite anything else
council_tax_base2 <- readxl::read_excel("data-raw/Council_Taxbase_local_authority_level_data_2022.xlsx", sheet = "Council_Taxbase_Data_CS_Bands", skip = 5) |>
  dplyr::filter(`E-code` != "ENG") |>
  dplyr::mutate(geography = "LAUA", .before = 1) |>
  dplyr::rename(ba_code = `E-code`,
                ecode = `ONS Code`,
                area_name = `Local Authority`,
                band_a = `Band A`,
                band_b = `Band B`,
                band_c = `Band C`,
                band_d = `Band D`,
                band_e = `Band E`,
                band_f = `Band F`,
                band_g = `Band G`,
                band_h = `Band H`) |>
  dplyr::mutate(band_i = NA) |>
  dplyr::mutate(all_properties = Total) |>
  dplyr::select(-c(Region, `Band A entitled to disabled relief reduction`)) |>
  dplyr::select(-c(`Number band D equiv in lieu`, `Adjusted Total`, Total))

council_tax_base_rgn2 <- council_tax_base2 |>
  left_join(ctsop_la_rgn_lookup, by = c("ecode" = "geography_code")) |>
  group_by(region_code, region_name) |>
  summarise(band_a = sum(band_a, na.rm = TRUE),
            band_b = sum(band_b, na.rm = TRUE),
            band_c = sum(band_c, na.rm = TRUE),
            band_d = sum(band_d, na.rm = TRUE),
            band_e = sum(band_e, na.rm = TRUE),
            band_f = sum(band_f, na.rm = TRUE),
            band_g = sum(band_g, na.rm = TRUE),
            band_h = sum(band_h, na.rm = TRUE),
            band_i = sum(band_i, na.rm = TRUE),
            all_properties = sum(all_properties, na.rm = TRUE)) |>
  filter(!is.na(region_code)) |>
  mutate(geography = "REGL") |>
  rename(ecode = region_code,
         area_name = region_name)

bind_rows(council_tax_base_rgn2, council_tax_base2) |>
  saveRDS("app/app.data/council_tax_base.rds")


# Create current baseline object for app

baseline <- ctsop_2022 |>
  process_ctsop() |>
  dplyr::inner_join(ct_bands_england_1991, by = c("band"))

baseline_band_d <- baseline |>
  dplyr::mutate(band_d_equivalents = n_properties * ratio) |>
  dplyr::group_by(ecode, area_name) |>
  dplyr::summarise(n_properties = sum(n_properties),
                   band_d_equivalents = sum(band_d_equivalents))




# band d rates
band_d_rates_path <- "data-raw/CTR_Table__live__-_Band_D_2022-23.xlsx"
band_d_rates_sheets <- readxl::excel_sheets(band_d_rates_path)
band_d_rates_sheets <- band_d_rates_sheets[band_d_rates_sheets == "Area CT"]
band_d_rates <- readxl::read_excel(band_d_rates_path, sheet = "Area CT", skip = 2, na = "-") |>
  dplyr::filter(!is.na(Reg)) |>
  tidyr::pivot_longer(cols = tidyselect:::where(is.numeric),
                      names_to = "date", values_to = "band_d_rate")

saveRDS(band_d_rates, "app/app.data/band_d_rates.rds")
