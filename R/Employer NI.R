# p = c(0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9)
# q = c(9600, 15571, 18224, 20244, 23946, 27756, 32258, 37753, 40989, 44680, 56981)
# q.na = c(9600, 15571, 18224, 20244, 23946, NA, 32258, 37753, 40989, 44680, 56981)
#
#
#
# rriskDistributions::fit.perc(
#   p, q
# )
#
#
# rriskDistributions::fit.perc(
#   p = c(0.1, 0.2, 0.25, 0.3, 0.4, 0.5, 0.6, 0.7, 0.75, 0.8, 0.9),
#   q = c(9600, 15571, 18224, 20244, 23946, 27756, 32258, 37753, 40989, 44680, 56981)
# )
#
#
# rriskDistributions::fit.perc(
#   p = c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
#   q = c(9600, 15571, 20244, 23946, 27756, 32258, 37753, 44680, 56981)
# )
#
# rriskDistributions::fit.perc(
#   p = c(0.25, 0.5, 0.75),
#   q = c(1, 5, 9)
# )
#
#
# quantile(q, seq(0, 1, 0.1))

# Employers' NI 1pp share (versement)

# Prepare extract from ASHE survey: total annual pay by LA, MetA, region
# filtered to England only
ashe_path <- "data-raw/ashetable72022provisional/PROV - Work Geography Table 7.7a   Annual pay - Gross 2022.xls"
readxl::excel_sheets(ashe_path)
ashe <- readxl::read_excel(ashe_path, sheet = "All", skip = 4, na = "x") |>
  dplyr::filter(grepl("E", Code)) |>
  dplyr::rename(geography_name = Description,
                geography_code = Code,
                n_employees = `(thousand)`) |>
  dplyr::select(-c(change...5, change...7, ...18, ...19, ...20)) |>
  dplyr::left_join(ctsop_la_rgn_lookup, by = c("geography_code")) |>
  dplyr::select(geography_code,
                geography_name = geography_name.x,
                geography_type,
                region_code,
                region_name,
                n_employees,
                mean = Mean,
                p10 = `10`,
                p20 = `20`,
                p25 = `25`,
                p30 = `30`,
                p40 = `40`,
                p50 = Median,
                p60 = `60`,
                p70 = `70`,
                p75 = `75`,
                p80 = `80`,
                p90 = `90`) |>
  # calculate total pay as a simple check
  dplyr::mutate(total_pay_thousands = as.numeric(n_employees) * mean)

# # p is the probabilities - must have any equivalent values where q is NA removed before passing to the function
#
# q.1 <- (as.numeric(ashe[4, 8:18]))
# q <- q.1[!is.na(q.1)]
# p <- (as.numeric(substr(names(ashe)[8:18], 2, 3)) / 100)[!is.na(q.1)]
#
#
# mod <- rriskDistributions::get.lnorm.par(p, q)
#
# # not needed
# # result <- data.frame(geography_code = ashe$geography_code)
# meanlog <- mod[1]
# sdlog <- mod[2]

# Employers' NI is payable on employed earnings above this annual level
secondary_threshold <- 9100
# Employers' NI rate (13.8%)
employer_ni_rate <- 0.138

# p is a vector of the percentiles in the ASHE data
p <- as.numeric(substr(names(ashe)[8:18], 2, 3)) / 100

# prepare vectors of the appropriate size to hold log-norm values
meanlog <- vector(length = nrow(ashe))
sdlog <- vector(length = nrow(ashe))

# iterate through the ASHE data row-by-row
for (r in 1:nrow(ashe)) {

  # capture the pay data at each percentile (the quartiles)
  q <- as.numeric(ashe[r, 8:18])

  # filter to remove NAs
  q1 <- q[!is.na(q)]

  # filter the percentiles to match the NAs in the quartiles
  p1 <- p[!is.na(q)]

  # only try to fit the model if we have at least 2 quartile values
  if (length(p1) >= 2) {
    # fitted model
    m <- rriskDistributions::get.lnorm.par(p1, q1, show.output = FALSE, plot = FALSE)
    # extract parameters of model and store in vectors
    meanlog[r] <- m[1]
    sdlog[r] <- m[2]
  } else {
    # or store NA if model cannot be fitted (i.e. fewer than 2 quartiles)
    meanlog[r] <- NA
    sdlog[r] <- NA
  }
}

# transfer the model parameters to the main table
ashe$meanlog <- meanlog
ashe$sdlog <- sdlog

# calculate the percentile at which the secondary_threshold occurs
n <- vector(length = nrow(ashe))
for (r in 1:nrow(ashe)) {
  if (!is.na(ashe[r, "meanlog"]) & !is.na(ashe[r, "sdlog"])) {
    n[r] <- plnorm(secondary_threshold, ashe$meanlog[r], ashe$sdlog[r])
  } else {
    n[r] <- NA
  }
}
# and transfer it back into the main table
ashe$p.ni <- n

# duplicate the main table, and convert the values in the percentile columns to
# contain the employers' NI payable on those incomes.
ashe2 <- ashe |>
  dplyr::mutate(dplyr::across(p10:p90, ~ ifelse((.x - secondary_threshold) * 0.01 >= 0, (.x - secondary_threshold) * 0.01, 0)))

# set-up a vector to store the total employer NI paid for each area
ni_paid <- vector(length = nrow(ashe2))

# iterate through the ashe2 df, calculating the percentiles for each model
for (r in 1:nrow(ashe2)) {
  # if we have both parameters of the model ...
  if (!is.na(ashe2[r, "meanlog"]) & !is.na(ashe2[r, "sdlog"])) {
    # ... calculate the percentile values of 1pp of employers' NI paid
    percentiles <- (qlnorm(seq(0.01, 0.99, 0.01), ashe2$meanlog[r], ashe2$sdlog[r]) - secondary_threshold) * 0.01
    # adjust these to remove negative values
    percentiles <- ifelse(percentiles <= 0, 0, percentiles)
    # multiply by 1/99th of the population of each area
    percentiles <- percentiles * (as.numeric(ashe2$n_employees[r]) * 1000) / 99
    # prepare the components for an area under the curve calculation,
    # approximated via trapezium method
    x <- seq_along(percentiles)
    y <- percentiles
    id <- order(x)
    # and calculate ...
    ni_paid[r] <- sum(diff(x[id]) * zoo::rollmean(y[id], 2))
    # replace any calculated zeros with NA
    ni_paid <- ifelse(ni_paid == 0, NA, ni_paid)
  }
}

# add this total back into the main ashe2 df
ashe2$ni_paid <- ni_paid

# TODO create summaries for combined authority level data
# import LA-CA lookup
LAD21_CAUTH21_EN_LU_v2 <- readxl::read_excel("~/Data/Geodata/Lookups/LAD21_CAUTH21_EN_LU_v2.xlsx")

ashe2_ca <- ashe2 |>
  dplyr::left_join(LAD21_CAUTH21_EN_LU_v2, by = c("geography_code" = "LAD21CD")) |>
  dplyr::filter(!is.na(CAUTH21CD))

# Middlesbrough LA (part of Tees Valley CA) has no model as insufficient data in ASHE. As it's the only one from the North East that's missing, we can impute its [ni_paid] as the difference between the sum of the NE LAs that we have minus the modelled total for the NE as a whole.

middlesbrough <- ashe2$ni_paid[ashe2$geography_name == "North East"] - sum(ashe2$ni_paid[ashe2$region_name == "NORTH EAST" & ashe2$geography_type == "LAUA"], na.rm = TRUE)

# write this back to the ashe2 table
ashe2$ni_paid[ashe2$geography_name == "Middlesbrough UA"] <- middlesbrough

# rerun the join and filter, and then calculate the groups
ashe2_ca <- ashe2 |>
  dplyr::left_join(LAD21_CAUTH21_EN_LU_v2, by = c("geography_code" = "LAD21CD")) |>
  dplyr::filter(!is.na(CAUTH21CD)) |>
  dplyr::group_by(ca_code = CAUTH21CD, ca_name = CAUTH21NM) |>
  dplyr::summarise(n_employees = sum(as.numeric(n_employees)),
                   ni_paid = sum(ni_paid)) |>
  dplyr::rename(geography_code = ca_code,
                geography_name = ca_name) |>
  dplyr::mutate(geography_type = "CA",
                n_employees = as.character(n_employees))

# bind the two dfs together

ashe_final <- dplyr::bind_rows(ashe2_ca, ashe2)


# write this out to an rds for the app
ashe_final |>
  dplyr::select(geography_code, geography_name, geography_type,
                region_code, region_name,
                n_employees, mean, ni_paid) |>
  saveRDS("app/app.data/ni_data.rds")
