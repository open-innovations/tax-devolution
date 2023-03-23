# Tourism Tax

tt <- readxl::read_excel("~/Projects/English Local Taxation Reform/Tourism Tax/final_districts_in_england_2016_final WITH CODES.xlsx", sheet = "Rooms", skip = 2) |>
  dplyr::left_join(ctsop_la_rgn_lookup, by = c("...2" = "geography_code")) |>
  dplyr::select(geography_code = "...2",
                geography_name,
                n_rooms = `Total Serviced and Non-serviced establishments`,
                geography_type,
                region_code,
                region_name) |>
  dplyr::filter(!is.na(region_code))

tt_rgn <- tt |>
  dplyr::group_by(region_code, region_name) |>
  dplyr::summarise(n_rooms = sum(n_rooms, na.rm = TRUE)) |>
  dplyr::mutate(geography_type = "REGL") |>
  dplyr::rename(geography_code = region_code,
                geography_name = region_name)

tt_final <- dplyr::bind_rows(tt_rgn, tt)

saveRDS(tt_final, "app/app.data/tt_final.rds")
