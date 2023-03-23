nndr_cols <- c("Incrementing Entry Number",
               "Billing Authority Code",
               "Non-Domestic Rating (NDR) Community Code",
               "BA Reference Number",
               "Primary and Secondary Description Code",
               "Primary Description Text",
               "Unique Address Reference Number (UARN)",
               "Full Property Identifier",
               "Firm's Name",
               "Number or Name",
               "Street",
               "Town",
               "Postal District",
               "County",
               "Postcode",
               "Effective Date",
               "Composite Indicator",
               "Rateable Value",
               "Appeal Settlement Code",
               "Assessment Reference",
               "List Alteration Date",
               "SCAT Code and Suffix",
               "Sub-Street Level 3",
               "Sub-Street Level 2",
               "Sub-Street Level 1",
               "Case Number",
               "Current From Date",
               "Current To Date"
)

nndr_list <- readr::read_delim("data-raw/NNDR/uk-englandwales-ndr-2023-draft-listentries-epoch-0001-baseline-csv/uk-englandwales-ndr-2023-draft-listentries-epoch-0001-baseline-csv.csv",
           delim = "*", col_names = nndr_cols)

NNDR_summaryvaluations01_colnames <- c("Assessment Reference",
                                       "Unique Address Reference Number (UARN)",
                                       "Billing Authority Code",
                                       "Firm's Name",
                                       "Number or Name",
                                       "Sub-Street Level 3",
                                       "Sub-Street Level 2",
                                       "Sub-Street Level 1",
                                       "Street",
                                       "Postal District",
                                       "Town",
                                       "County",
                                       "Postcode",
                                       "Scheme Reference",
                                       "Primary Description Text",
                                       "Total Area",
                                       "Sub Total",
                                       "Total Value",
                                       "Adopted RV",
                                       "List Year",
                                       "BA Name",
                                       "BA Reference Number",
                                       "VO Ref",
                                       "From Date",
                                       "To Date",
                                       "SCAT Code Only",
                                       "Unit of Measurement",
                                       "Unadjusted Price"
)

NNDR_summaryvaluations02_colnames <- c("Unique Address Reference Number (UARN)",
                                       "Line",
                                       "Floor",
                                       "Description",
                                       "Area",
                                       "Price",
                                       "Value"
)

NNDR_summaryvaluations03_colnames <- c("Unique Address Reference Number (UARN)",
                                       "Other Addition OA Description",
                                       "OA Size",
                                       "OA Price",
                                       "OA Value"
)

NNDR_summaryvaluations04_colnames <- c("Unique Address Reference Number (UARN)",
                                       "PM Value"
)

NNDR_summaryvaluations05_colnames <- c("Unique Address Reference Number (UARN)",
                                       "CP Spaces",
                                       "CP Spaces Value",
                                       "CP Area",
                                       "CP Area Value",
                                       "CP Total"
)

NNDR_summaryvaluations06_colnames <- c("Unique Address Reference Number (UARN)",
                                       "Adj Desc",
                                       "Adj Percent"
)

NNDR_summaryvaluations07_colnames <- c("Unique Address Reference Number (UARN)",
                                       "Total before Adj",
                                       "Total Adj"
)

nndr_summary_path <- "data-raw/NNDR/uk-englandwales-ndr-2023-draft-summaryvaluations-epoch-0001-baseline-csv/uk-englandwales-ndr-2023-draft-summaryvaluations-epoch-0001-baseline-csv.csv"

build_nndr_summary <- function() {
  nndr_summary <- readr::read_delim(nndr_summary_path, delim = "*",
                                    col_names = FALSE,
                                    col_types = readr::cols(
                                      .default = readr::col_character()
                                    )
  ) |>
    # dplyr::mutate(`Assessment Reference` = ifelse(X1 == "01", X2, NA), .before = 1) |>
    dplyr::mutate(`Unique Address Reference Number (UARN)` = ifelse(X1 == "01", X3, NA), .after = X1) |>
    tidyr::fill(`Unique Address Reference Number (UARN)`)

  nndr_summary <- split(nndr_summary, nndr_summary$X1)

  for (i in names(nndr_summary)) {
    nndr_summary[[i]][["X1"]] <- NULL
    if (i == "01") {
      nndr_summary[[i]][["Unique Address Reference Number (UARN)"]] <- NULL
    }
    names(nndr_summary[[i]]) <- get(paste0("NNDR_summaryvaluations", i, "_colnames"))
    nndr_summary[[i]] <- dplyr::select(nndr_summary[[i]], dplyr::all_of(get(paste0("NNDR_summaryvaluations", i, "_colnames"))))
  }

  return(nndr_summary)
}

nndr_summary <- build_nndr_summary()

# Add geocodes ------------------------------------------------------------

onspd <- readr::read_csv("~/Data/Geodata/ONSPD/ONSPD_NOV_2022_UK/Data/ONSPD_NOV_2022_UK.csv", col_select = c(pcds, oslaua))

la_ua_names <- readr::read_csv("~/Data/Geodata/ONSPD/ONSPD_NOV_2022_UK/Documents/LA_UA names and codes UK as at 04_21.csv")

LAD20_CAUTH20_EN_LU <- readr::read_csv("~/Data/Geodata/Lookups/Local_Authority_District_to_Combined_Authority_(December_2020)_Lookup_in_England.csv")

nndr_list <- nndr_list |>
  dplyr::inner_join(onspd,
                    by = c("Postcode" = "pcds")) |>
  dplyr::inner_join(la_ua_names,
                    by = c("oslaua" = "LAD21CD")) |>
  dplyr::left_join(LAD20_CAUTH20_EN_LU,
                    by = c("oslaua" = "LAD20CD"))

nndr_summary$`01` <- nndr_summary$`01` |>
  dplyr::inner_join(onspd,
                    by = c("Postcode" = "pcds")) |>
  dplyr::inner_join(la_ua_names,
                    by = c("oslaua" = "LAD21CD")) |>
  dplyr::left_join(LAD20_CAUTH20_EN_LU,
                    by = c("oslaua" = "LAD20CD"))

# Car parking -------------------------------------------------------------

carparking <- dplyr::inner_join(nndr_summary$`01`, nndr_summary$`05`)

nndr_carparking <- carparking |>
  dplyr::filter(grepl("E", oslaua),
                `CP Spaces` > 10) |>
  dplyr::group_by(Rateable_spaces = ifelse(`CP Spaces Value` != "0",
                                           "Rateable",
                                           "Non-rateable"),
                  LAD21NM, oslaua) |>
  dplyr::summarise(total_spaces = sum(as.integer(`CP Spaces`), na.rm = TRUE),
                   total_revenue = total_spaces * 475) |>
  tidyr::pivot_wider(names_from = Rateable_spaces, values_from = c(total_spaces, total_revenue)) |>
  # TODO need to change NAs to zero here
  tidyr::replace_na(list(total_spaces_Rateable = 0,
                         total_revenue_Rateable = 0)
                    ) |>
  dplyr::mutate(total_spaces = `total_spaces_Non-rateable` + total_spaces_Rateable,
                total_revenue = `total_revenue_Non-rateable` + total_revenue_Rateable) |>
  dplyr::inner_join(readRDS("app/app.data/ctsop_la_rgn_lookup.rds"),
                   by = c("oslaua" = "geography_code")) |>
  dplyr::select(-geography_name) |>
  dplyr::rename(geography_code = oslaua,
                geography_name = LAD21NM)

# calculate the regional totals and bind to the above data frame
nndr_carparking |>
  dplyr::group_by(region_code, region_name) |>
  dplyr::summarise(`total_spaces_Non-rateable` = sum(`total_spaces_Non-rateable`, na.rm = TRUE),
                   total_spaces_Rateable = sum(total_spaces_Rateable, na.rm = TRUE),
                   `total_revenue_Non-rateable` = sum(`total_revenue_Non-rateable`, na.rm = TRUE),
                   total_revenue_Rateable = sum(total_revenue_Rateable, na.rm = TRUE),
                   total_spaces = sum(total_spaces, na.rm = TRUE),
                   total_revenue = sum(total_revenue, na.rm = TRUE)
  ) |>
  dplyr::rename(geography_code = region_code,
                geography_name = region_name) |>
  dplyr::mutate(geography_type = "REGL") |>
  dplyr::bind_rows(nndr_carparking) |>
  saveRDS("app/app.data/nndr_carparking.rds")

# TODO add flexibility for dynamic value per space (default = 475)
# TODO add minimum number of spaces per property threshold (default = 10)


# Hotels ------------------------------------------------------------------

hotels_list <- nndr_list |>
  dplyr::filter(grepl("CH", `Primary and Secondary Description Code`))

hotels_01 <- nndr_summary$`01` |>
  dplyr::filter(`SCAT Code Only` %in% substr(hotels_list$`SCAT Code and Suffix`, 1, 3))

hotels_02 <- dplyr::inner_join(hotels_01, nndr_summary$`02`)
hotels_03 <- dplyr::inner_join(hotels_01, nndr_summary$`03`)
hotels_04 <- dplyr::inner_join(hotels_01, nndr_summary$`04`)
hotels_05 <- dplyr::inner_join(hotels_01, nndr_summary$`05`)
hotels_06 <- dplyr::inner_join(hotels_01, nndr_summary$`06`)
hotels_07 <- dplyr::inner_join(hotels_01, nndr_summary$`07`)

hotels_01 |>
  dplyr::group_by(LAD21NM) |>
  dplyr::summarise(total_rooms = sum(as.integer(`Total Area`), na.rm = TRUE)) |>
  View()

nndr_hotels <- hotels_02 |>
  dplyr::inner_join(readRDS("app/app.data/ctsop_la_rgn_lookup.rds"),
                   by = c("oslaua" = "geography_code")) |>
  dplyr::filter(grepl("E", oslaua)) |>
  dplyr::group_by(geography_code = oslaua, geography_name, geography_type,
                  region_code, region_name) |>
  dplyr::summarise(total_rooms = sum(as.integer(Area), na.rm = TRUE))

# calculate the regional totals and bind to the above data frame

nndr_hotels |>
  dplyr::group_by(region_code, region_name) |>
  dplyr::summarise(total_rooms = sum(total_rooms, na.rm = TRUE)) |>
  dplyr::rename(geography_code = region_code,
                geography_name = region_name) |>
  dplyr::mutate(geography_type = "REGL") |>
  dplyr::bind_rows(nndr_hotels) |>
  saveRDS("app/app.data/nndr_hotels.rds")

# This is calculating just off 4* and 5* per NNDR manual

######

# nndr - rateable values overall

nndr_data <- nndr_list |>
  dplyr::filter(grepl("E", oslaua)) |>
  dplyr::select(`oslaua`, `Rateable Value`)
saveRDS(nndr_data, "app/app.data/nndr_data.rds")

nndr_data |>
  dplyr::mutate(nndr_payable = dplyr::case_when(
    `Rateable Value` <= 12000 ~ 0,
    `Rateable Value` > 12000 & `Rateable Value` < 51000 ~ `Rateable Value` * 0.499,
    `Rateable Value` >= 51000 ~ `Rateable Value` * 0.512
  )) |>
  dplyr::inner_join(ctsop_la_rgn_lookup,
                    by = c("oslaua" = "geography_code")) |>
  dplyr::group_by(geography_code = oslaua, geography_name, geography_type,
                  region_code, region_name) |>
  dplyr::summarise(nndr_payable = sum(nndr_payable)) |> View()

