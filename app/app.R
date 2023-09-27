library(shiny)
library(tidyverse)
library(scales)
library(DT)

ui <- navbarPage(
  title = "Tax Devolution Tool",
  id = "navbar",
  windowTitle = "Tax Devolution Tool",
  header = list(
    div(style = "display:inline-block; margin-left: 20px", uiOutput("choose_geography")),
    div(style = "display:inline-block", uiOutput("rgn_select"))
  ),

  tabPanel(
    id = "Council Tax",
    title = "Council Tax",

    titlePanel("Council Tax"),

    sidebarLayout(
      sidebarPanel(width = 2,
        tabsetPanel(
          id = "sidetab1",
          tabPanel(
            title = "Band values",
            p("Select values for top of each band (last band should be blank)"),
            uiOutput("bands"),
            uiOutput("reset_bands"),
            uiOutput("cpi_index_bands")
          ),
          tabPanel(
            title = "Band ratios",
            p("Select ratio to band D"),
            uiOutput("band_ratios"),
            uiOutput("reset_ratios")
          )
        )
      ),
      mainPanel(width = 10,
        # div(style = "display:inline-block", uiOutput("choose_geography")),
        div(style = "display:inline-block", uiOutput("choose_valuation")),
        # uiOutput("rgn_select"),
        tabsetPanel(
          id = "maintab1",
          tabPanel(
            title = "Analyse by properties",
            uiOutput("plot_variable"),
            plotOutput("plot", height = "600")
          ),
          tabPanel(
            title = "Analyse by revenue",
            plotOutput("plot_by_revenue", height = "600")
          ),
          tabPanel(
            title = "Analyse by difference over baseline",
            plotOutput("plot_by_difference_over_baseline", height = "600")
          )
        )
      )
    )
  ),

  tabPanel(
    id = "Business Rates",
    title = "Business Rates",

    titlePanel("Business Rates"),
    sidebarLayout(
      sidebarPanel(width = 2,
                   numericInput("nndr_sbrr", "Small business rate relief threshold",
                                value = 12000, step = 1000),
                   numericInput("sb_threshold", "Small business upper threshold",
                                value = 51000, step = 1000),
                   numericInput("sb_multiplier", "Small business multiplier",
                                value = 0.499, step = 0.001),
                   numericInput("lb_multiplier", "Large business multiplier",
                                value = 0.512, step = 0.001)
                   ),
      mainPanel(width = 10,
                plotOutput("nndr_plot", height = "600"))
    )
  ),

  tabPanel(
    id = "Stamp Duty",
    title = "Stamp Duty",

    titlePanel("Stamp Duty"),
    sidebarLayout(
      sidebarPanel(width = 2,
        #tabsetPanel(
          id = "sdltsidetab1",
          p("Using current (September 2022) rates:"),
          p("<£250,000 - 0%"),
          p("£250,001-£925,000 - 5%"),
          p("£925,001-£1,500,000 - 10%"),
          p(">£1,500,000 - 15%"),
          # tabPanel(
          #   title = "Band values",
          #   p("Select values for top of each band (last band should be blank)"),
          #   uiOutput("sdlt_bands"),
          #   uiOutput("reset_sdlt_bands")
          # ),
          # tabPanel(
          #   title = "Band rates",
          #   p("Select tax rates"),
          #   uiOutput("sdlt_band_rates"),
          #   uiOutput("sdlt_reset_rates")
          # )
      #  )
      ),
      mainPanel(width = 10,
        div(style = "display:inline-block", uiOutput("choose_sdlt_geography")),
        uiOutput("sdlt_rgn_select"),
        tabsetPanel(
          id = "sdltmaintab1",
          # tabPanel(
          #   title = "Analyse by properties",
          #   plotOutput("sdlt_plot", height = "600")
          # ),
          tabPanel(
            title = "Analyse by revenue",
            plotOutput("sdlt_plot_by_revenue", height = "600")
          ),
          tabPanel(
            title = "Explore the data",
            DT::DTOutput("sdlt_data")
          )
        )
      )
    )

  ),

  tabPanel(
    id = "Workplace Parking Levy",
    title = "Workplace Parking Levy",

    titlePanel("Workplace Parking Levy"),
    sidebarLayout(
      sidebarPanel(width = 2,
        numericInput("wpl_levy", "Levy per parking space (£)",
                     value = 458, step = 10)
      ),
      mainPanel(width = 10,
        plotOutput("wpl_revenue", height = "600"),
      )
    )
  ),

  tabPanel(
    id = "Tourism Tax",
    title = "Tourism Tax",

    titlePanel("Tourism Tax"),
    sidebarLayout(
      sidebarPanel(width = 2,
        numericInput("tt_levy", "Levy per room (£)",
                     value = 1),
        numericInput("tt_occupancy", "Occupancy rate (%)",
                     value = 65)
      ),
      mainPanel(width = 10,
                tabsetPanel(
                  id = "tourism_tax_main_panel",
                  tabPanel("Analyse by revenue",
                           plotOutput("tt_revenue", height = "600"),
                  ),
                  tabPanel("Explore the data",
                           DT::DTOutput("tourism_tax_data")
                           )
                )
      )
    )
  ),

  tabPanel(
    id = "Employer NI retention",
    title = "Employer NI retention",

    titlePanel("Employers' NI retention"),
    sidebarLayout(
      sidebarPanel(width = 2),
      mainPanel(width = 10,
                tabsetPanel(
                  id = "employer_ni_main_panel",
                  tabPanel("Analyse by revenue",
                           plotOutput("ni_revenue", height = "600"),
                  ),
                  tabPanel("Explore the data",
                           DT::DTOutput("ni_data")
                  )
                )
      )
    )
  ),

  tabPanel(
    id = "debug",
    title = "debug",

    titlePanel("Debug outputs"),
    h2("temp.df()"),
    DT::DTOutput("debug1"),
    h2("nndr_plot_data()"),
    DT::DTOutput("debug2")
  )
)

server <- function(input, output) {

  ppd_2022 <- readRDS("app.data/ppd_2022.rds")
  ctsop_1993 <- readRDS("app.data/ctsop_1993.rds")
  ctsop_2022 <- readRDS("app.data/ctsop_2022.rds")
  cpi <- readRDS("app.data/cpi.rds")
  ppd_1995_ecdf <- readRDS("app.data/ppd_1995_ecdf.rds")
  ppd_2022_ecdf <- readRDS("app.data/ppd_2022_ecdf.rds")
  ctsop_la_rgn_lookup <- readRDS("app.data/ctsop_la_rgn_lookup.rds")
  band_d_rates <- readRDS("app.data/band_d_rates.rds")
  council_tax_base <- readRDS("app.data/council_tax_base.rds")
  nndr_carparking <- readRDS("app.data/nndr_carparking.rds")
  # nndr_hotels <- readRDS("app.data/nndr_hotels.rds")
  tt_final <- readRDS("app.data/tt_final.rds")
  nndr_data <- readRDS("app.data/nndr_data.rds")
  ni_data <- readRDS("app.data/ni_data.rds")

  ct_bands_england_1991 <- data.frame(
    band  = LETTERS[1:8],
    #min   = c(0, 40001, 52001, 68001, 88001, 120001, 160001, 320001),
    max   = c(40000, 52000, 68000, 88000, 120000, 160000, 320000, Inf),
    ratio = c(6/9, 7/9, 8/9,  9/9,  11/9, 13/9, 15/9, 18/9)
  ) |>
    dplyr::mutate(cpi_2022 = max * cpi$index_1991[cpi$dates.date == "2022-01-01"])

  process_ctsop <- function(x) {
    df <- x |>
      dplyr::select(-all_properties) |>
      tidyr::pivot_longer(cols = band_a:band_i,
                          names_to = "band",
                          values_to = "n_properties",
                          values_drop_na = TRUE) |>
      dplyr::mutate(band = toupper(substr(band, 6, 6))) |>
      dplyr::group_by(area_name) |>
      dplyr::mutate(proportion = n_properties / sum(n_properties)) |>
      dplyr::mutate(cumprop = cumsum(proportion))
    return(df)
  }

  baseline <- council_tax_base |>
    process_ctsop() |>
    dplyr::inner_join(ct_bands_england_1991, by = c("band"))

  plot.theme <- theme(panel.background = element_blank(),
                      panel.grid = element_blank(),
                      panel.grid.major.y = element_line(linetype = "dotted"),
                      axis.line.y.right = NULL,
                      axis.line = element_line(),
                      text = element_text(size = 18),
                      strip.background = element_blank(),
                      legend.position = "top"
  )

  # ppd_2022_ecdf_rgn <- list()
  # for (region in unique(ppd_2022$rgn)) {
  #   ppd_2022_rgn <- dplyr::filter(ppd_2022, rgn == region)
  #   ppd_2022_rgn_dist <- quantile(ppd_2022_rgn$Price, seq(0, 1, 0.01), type = 1)
  #   ppd_2022_ecdf_rgn[[region]] <- ecdf(ppd_2022_rgn_dist)
  # }
  #
  # ppd_2022_ecdf_lad <- list()
  # for (lad in unique(ppd_2022$oslaua)) {
  #   ppd_2022_lad <- dplyr::filter(ppd_2022, oslaua == lad)
  #   ppd_2022_lad_dist <- quantile(ppd_2022_lad$Price, seq(0, 1, 0.01), type = 1)
  #   ppd_2022_ecdf_lad[[lad]] <- ecdf(ppd_2022_lad_dist)
  # }
  #
  # ppd_2022_ecdf <- c(ppd_2022_ecdf_rgn, ppd_2022_ecdf_lad)

  observe({
    if (input$maintab1 == "Analyse by revenue") {
      updateTabsetPanel(inputId = "sidetab1",
                        selected = "Band ratios")
      updateRadioButtons(inputId = "plot_variable",
                         selected = "Number of properties")
      updateRadioButtons(inputId = "choose_geography",
                         selected  = "Local Authority")
    }

    if (input$maintab1 == "Analyse by properties") {
      updateTabsetPanel(inputId = "sidetab1",
                        selected = "Band values")
    }

    if (input$navbar == "Employer NI retention") {
      updateRadioButtons(inputId = "choose_geography",
                         choices = c("Region", "Local Authority",
                                     "Combined Authority"),
                         inline = TRUE)
    } else {
      updateRadioButtons(inputId = "choose_geography",
                         choices = c("Region", "Local Authority"),
                         inline = TRUE
      )
    }

    # TODO remove when difference over baseline available at regional level
    if (input$maintab1 == "Analyse by difference over baseline") {
      updateRadioButtons(inputId = "choose_geography",
                         selected = "Local Authority",
                         inline = TRUE)
    }
  })

  output$debug1 <- DT::renderDT({
    temp.df()
  })

  output$debug2 <- DT::renderDT({
    nndr_calc()
  })

  output$bands <- renderUI({
    lapply(seq_along(1:nrow(ct_bands_england_1991)), function(n) {
      numericInput(inputId = paste0("band", ct_bands_england_1991$band[n]),
                   label = paste("Band", ct_bands_england_1991$band[n]),
                   value = ct_bands_england_1991$max[n],
                   step = 10000)
    })
  })

  output$reset_bands <- renderUI({
    actionButton("reset_bands_button",
                 "Reset to original 1991 values")
  })

  observeEvent(input$reset_bands_button, {
    output$bands <- renderUI({
      lapply(seq_along(1:nrow(ct_bands_england_1991)), function(n) {
        numericInput(inputId = paste0("band", ct_bands_england_1991$band[n]),
                     label = paste("Band", ct_bands_england_1991$band[n]),
                     value = ct_bands_england_1991$max[n],
                     step = 10000)
      })
    })
  })

  output$cpi_index_bands <- renderUI({
    actionButton("cpi_index_bands_button",
                 "Uprate 1991 bands by CPI")
  })

  observeEvent(input$cpi_index_bands_button, {
    output$bands <- renderUI({
      lapply(seq_along(1:nrow(ct_bands_england_1991)), function(n) {
        numericInput(inputId = paste0("band", ct_bands_england_1991$band[n]),
                     label = paste("Band", ct_bands_england_1991$band[n]),
                     value = round(ct_bands_england_1991$cpi_2022[n]),
                     step = 10000)
      })
    })
  })

  output$band_ratios <- renderUI({
    lapply(seq_along(1:nrow(ct_bands_england_1991)), function(n) {
      numericInput(inputId = paste0("ratio", ct_bands_england_1991$band[n]),
                label = paste("Band", ct_bands_england_1991$band[n]),
                value = ct_bands_england_1991$ratio[n])
    })
  })

  output$reset_ratios <- renderUI({
    actionButton("reset_ratios_button",
                 "Reset to original 1991 ratios")
  })

  observeEvent(input$reset_ratios_button, {
    output$band_ratios <- renderUI({
      lapply(seq_along(1:nrow(ct_bands_england_1991)), function(n) {
        numericInput(inputId = paste0("ratio", ct_bands_england_1991$band[n]),
                  label = paste("Band", ct_bands_england_1991$band[n]),
                  value = ct_bands_england_1991$ratio[n])
      })
    })
  })

  output$plot_variable <- renderUI({
    radioButtons("plot_variable", "Variable",
                 choices = c("Share of properties",
                             "Number of properties"
                 ),
                 inline = TRUE
    )
  })

  output$choose_geography <- renderUI({
    radioButtons("choose_geography", "Choose geography",
                 choices = c("Region",
                             "Local Authority"
                 ),
                 inline = TRUE)
  })

  output$choose_valuation <- renderUI({
    radioButtons("choose_valuation", "Choose valuation year",
                 choices = c("1995", "2022"),
                 selected = "2022",
                 inline = TRUE)
  })

  output$rgn_select <- renderUI({
    req(input$choose_geography)
    if (input$choose_geography == "Local Authority") {
      regions <- unique(ctsop_la_rgn_lookup$region_name[!is.na(ctsop_la_rgn_lookup$region_name)])
      selectInput("rgn_select", "Choose region",
                  choices = regions,
                  selected = regions[1]
      )
    }
  })

  temp.bands <- reactive({
    c(input$bandA, input$bandB, input$bandC, input$bandD,
      input$bandE, input$bandF, input$bandG, Inf)
  })

  temp.df <- reactive({
    # select which year's valuation models to use
    req(input$choose_valuation)
    ecdf_object <- get(paste0("ppd_", input$choose_valuation, "_ecdf"))

    # set up the data frame with the number of bands in col1 in use
    df_share <- data.frame(band = LETTERS[1:length(temp.bands())])
    df_number <- df_share

    baseline_object <- baseline |>
      dplyr::select(ecode, band,
                    baseline_share_properties = proportion,
                    baseline_number_properties = n_properties)


    areas_to_use <- names(ecdf_object)[grepl("E", names(ecdf_object))]

    # run the model and bring the data into df
    for (area in areas_to_use) {
      df_share[[area]] <- ecdf_object[[area]](temp.bands())
      df_share[[area]] <- diff(c(0, df_share[[area]])) # cumdist to dist
      # df_number[[area]] <- round(df_share[[area]] * ctsop_2022[ctsop_2022$ecode == area, ]$all_properties)
      df_number[[area]] <- round(df_share[[area]] * council_tax_base[council_tax_base$ecode == area, ]$all_properties)
    }

    df_share <- df_share |>
      tidyr::pivot_longer(cols = -band,
                          names_to = "geography_code",
                          values_to = "share_properties")

    df_number <- df_number |>
      tidyr::pivot_longer(cols = -band,
                          names_to = "geography_code",
                          values_to = "number_properties")

    df <- dplyr::inner_join(df_share, df_number) |>
      dplyr::left_join(ctsop_la_rgn_lookup,
                       by = c("geography_code")) |>
      tidyr::pivot_longer(cols = c(number_properties,
                                   share_properties),
                          values_to = "model_values") |>
    # # bring in the baseline data
      dplyr::left_join(baseline |>
                         dplyr::select(geography_code = ecode,
                                       band,
                                       number_properties = n_properties,
                                       share_properties = proportion) |>
                         tidyr::pivot_longer(cols = c(number_properties,
                                                      share_properties),
                                             values_to = "baseline_values"),
                       by = c("geography_code", "band", "name")) |>
      tidyr::pivot_longer(cols = c(model_values, baseline_values),
                          names_to = "model_baseline") |>
      tidyr::pivot_wider(names_from = name, values_from = value)

  })


  plot.df <- reactive({
    if (input$choose_geography == "Region") {
      df <- temp.df() |>
        dplyr::filter(geography_type == "REGL")
    } else if (input$choose_geography == "Local Authority") {
      req(input$rgn_select)
      df <- temp.df() |>
        dplyr::filter(geography_type == "LAUA",
                      region_name == input$rgn_select)
    }

    # if (input$plot_variable == "Number of properties") {
    #   df <- df |>
    #     dplyr::filter(name == "number_properties")
    # } else if (input$plot_variable == "Share of properties") {
    #   df <- df |>
    #     dplyr::filter(name == "share_properties")
    # }

    return(df)
  })

    output$plot <- renderPlot({
      req(temp.df())
      variable <- if (input$plot_variable == "Number of properties") {
        "number_properties"
      } else if (input$plot_variable == "Share of properties") {
        "share_properties"
      }

      plot.df() |>
        ggplot(aes_string(x = "band", y = variable, fill = "model_baseline")) +
        geom_col(position = "dodge") +
        facet_wrap("geography_name", labeller = ggplot2::label_wrap_gen()) +
        labs(subtitle = paste("Using", input$choose_valuation, "property values"),
             x = "Band",
             y = input$plot_variable,
             fill = "") +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme
    })

    # here we handle revenues. We'll start by calculating band D equivalent properties for each area

    temp.ratios <- reactive({
      req(input$ratioH)
      data.frame(band = LETTERS[1:nrow(ct_bands_england_1991)],
                 ratio = c(
                   input$ratioA, input$ratioB, input$ratioC, input$ratioD,
                   input$ratioE, input$ratioF, input$ratioG, input$ratioH
                 )
      )
    })

    band.d.equiv.df <- reactive({
      band.d.equiv.la <- temp.df() |>
        # tidyr::pivot_wider(names_from = name, values_from = value) |>
        dplyr::left_join(temp.ratios(), by = c("band")) |>
        dplyr::mutate(band_d_equivalent = number_properties * ratio) |>
        # TODO insert adjusted band D equivalent data here into band_d_equivalent
        dplyr::group_by(geography_code, geography_name,
                        region_code, region_name,
                        geography_type, model_baseline) |>
        dplyr::summarise(number_properties = sum(number_properties),
                         n_band_d_equivalents = sum(band_d_equivalent)) |>       # this runs band d values against band d equivs to calc revenue
      dplyr::left_join(dplyr::filter(band_d_rates, date == "2022-23"),
                       by = c("geography_code" = "ONS Code")) |>
        dplyr::mutate(revenue = n_band_d_equivalents * band_d_rate)

      band.d.equiv.rgn <- band.d.equiv.la |>
        dplyr::filter(!is.na(region_code)) |>
        dplyr::group_by(region_code, region_name, model_baseline) |>
        dplyr::summarise(number_properties = sum(number_properties),
                         n_band_d_equivalents = sum(n_band_d_equivalents),
                         revenue = sum(revenue)) |>
        dplyr::mutate(geography_type = "REGL") |>
        dplyr::rename(geography_code = region_code,
                      geography_name = region_name)

      band.d.equiv.final <- dplyr::bind_rows(band.d.equiv.rgn, band.d.equiv.la)
      return(band.d.equiv.final)
    })

    band.d.plot.df <- reactive({
      req(band.d.equiv.df())
      if (input$choose_geography == "Region") {
        df <- band.d.equiv.df() |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- band.d.equiv.df() |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      }
      return(df)
    })

    output$plot_by_revenue <- renderPlot({
      req(band.d.plot.df(), temp.ratios())
      band.d.plot.df() |>

      ggplot(aes(x = geography_name, y = revenue, fill = model_baseline)) +
        geom_col(position = "dodge") +
        labs(title = "Number of Band D equivalents by area") +
        scale_y_continuous(labels = scales::label_comma()) +
        coord_flip() +
        labs(title = "Potential Council Tax Revenue by Local Authority",
             subtitle = "Does not deduct exemptions, e.g. student relief",
             x = input$choose_geography,
             y = "Revenue (£)",
             fill = ""
             ) +
        plot.theme
    })

    revenue_difference <- reactive({
      band.d.plot.df() |>
        dplyr::select(geography_code,
                      geography_name,
                      region_code,
                      region_name,
                      geography_type,
                      model_baseline,
                      revenue) |>
        tidyr::pivot_wider(names_from = model_baseline,
                           values_from = revenue) |>
        dplyr::mutate(change_in_revenue_over_baseline = model_values - baseline_values)
    })

    output$plot_by_difference_over_baseline <- renderPlot({
      revenue_difference() |>
        ggplot(aes(x = geography_name, y = change_in_revenue_over_baseline)) +
        geom_col() +
        scale_y_continuous(labels = scales::label_comma()) +
        coord_flip() +
        labs(title = "Difference of modelled revenue over baseline revenue",
             subtitle = "NB: not currently available at regional level",
             x = input$choose_geography,
             y = "Modelled revenue difference over baseline (£)") +
        plot.theme
    })

# SDLT --------------------------------------------------------------------

    sdlt_rates_df <- data.frame(
      band = c(250000, 925000, 1.5e6, Inf),
      rate = c(0, 0.05, 0.1, 0.12)
    )

    output$sdlt_bands <- renderUI({
      lapply(seq_along(1:nrow(sdlt_rates_df)), function(n) {
        numericInput(inputId = paste0("sdlt_band", n),
                     label = paste("Band", n),
                     value = sdlt_rates_df$band[n],
                     step = 10000)
      })
    })

    output$sdlt_band_rates <- renderUI({
      lapply(seq_along(1:nrow(sdlt_rates_df)), function(n) {
        numericInput(inputId = paste0("sdlt_rate", n),
                     label = paste("Band", n),
                     value = sdlt_rates_df$rate[n])
      })
    })

    temp.sdlt.bands <- reactive({
      c(input$sdlt_band1, input$sdlt_band2, input$sdlt_band3, Inf)
    })

    temp.sdlt.rates <- reactive({
      c(input$sdlt_rate1, input$sdlt_rate2, input$sdlt_rate3, input$sdlt_rate4)
    })

    calc_sdlt_payable <- function(price, i = 1) {
      taxable_value <- temp.sdlt.bands()[i] - if (i == 1) 0 else temp.sdlt.bands()[i - 1]
      tax <- min(price, taxable_value) * temp.sdlt.rates()[i]
      if (price > taxable_value) {
        tax <- tax + calc_sdlt_payable(price - taxable_value, i = i + 1)
      }
      return(tax)
    }

    # sdlt_df <- reactive({
    #   req(temp.sdlt.bands(), temp.sdlt.rates())
    #   if (identical(temp.sdlt.bands(), sdlt_rates_df$band) &
    #       identical(temp.sdlt.rates(), sdlt_rates_df$rate)) {
    #     ppd_la <- ppd_2022 |>
    #       dplyr::filter(grepl("E", oslaua)) |>
    #       dplyr::inner_join(ctsop_la_rgn_lookup,
    #                         by = c("oslaua" = "geography_code")) |>
    #       dplyr::rename(geography_code = oslaua) |>
    #       dplyr::mutate(geography_type = "LAUA") |>
    #       dplyr::group_by(geography_type, geography_code, geography_name,
    #                       region_code, region_name) |>
    #       dplyr::summarise(sdlt = sum(sdlt))
    #
    #     ppd_rgn <- ppd_la |>
    #       dplyr::group_by(region_code, region_name) |>
    #       dplyr::summarise(sdlt = sum(sdlt)) |>
    #       dplyr::mutate(geography_type = "REGL") |>
    #       dplyr::rename(geography_code = region_code,
    #                     geography_name = region_name)
    #
    #     ppd_final <- dplyr::bind_rows(ppd_rgn, ppd_la)
    #     return(ppd_final)
    #   } else {
    #     ppd_la <- ppd_2022 |>
    #       dplyr::filter(grepl("E", oslaua)) |>
    #       dplyr::mutate(sdlt = sapply(Price, calc_sdlt_payable)) |>
    #       dplyr::inner_join(ctsop_la_rgn_lookup,
    #                         by = c("oslaua" = "geography_code")) |>
    #       dplyr::rename(geography_code = oslaua) |>
    #       dplyr::mutate(geography_type = "LAUA") |>
    #       dplyr::group_by(geography_type, geography_code, geography_name,
    #                       region_code, region_name) |>
    #       dplyr::summarise(sdlt = sum(sdlt))
    #
    #     ppd_rgn <- ppd_la |>
    #       dplyr::group_by(region_code, region_name) |>
    #       dplyr::summarise(sdlt = sum(sdlt)) |>
    #       dplyr::mutate(geography_type = "REGL") |>
    #       dplyr::rename(geography_code = region_code,
    #                     geography_name = region_name)
    #
    #     ppd_final <- dplyr::bind_rows(ppd_rgn, ppd_la)
    #     return(ppd_final)
    #   }
    # })

    sdlt_df <- readRDS("app.data/sdlt.rds")

    sdlt_df_plot <- reactive({
      if (input$choose_geography == "Region") {
        df <- sdlt_df |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- sdlt_df |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      }
      return(df)
    })

    output$sdlt_plot_by_revenue <- renderPlot({
      sdlt_df_plot() |>
        ggplot(aes(x = geography_name, y = sdlt)) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Residential Stamp Duty Land Tax Receipts",
             subtitle = "NB: this does not include commercial SDLT receipts",
             caption = "Based on 2022 Land Registry Price Paid Data\nand stamp duty thresholds and rates from 23 September 2022\nDoes not account for first time buyer reliefs or second home/ATED surcharges",
             x = input$choose_geography,
             y = "Revenue (£)")
    })

    output$sdlt_data <- DT::renderDT({
      sdlt_df_plot()
    })

# Workplace Parking Levy --------------------------------------------------

    wpl_calc <- reactive({
      if (input$choose_geography == "Region") {
        df <- nndr_carparking |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- nndr_carparking |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      }

      df <- df |>
        dplyr::mutate(total_revenue = total_spaces * input$wpl_levy)
      return(df)
    })

    output$wpl_revenue <- renderPlot({
      ggplot(wpl_calc(), aes(x = geography_name, y = total_revenue)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Annual Workplace Parking Levy revenue",
             subtitle = "Excludes premises with fewer than 10 spaces, but includes other possible exemptions, e.g. disabled spaces, etc.",
             x = input$choose_geography,
             y = "Revenue (£)")
    })


    # Tourism Tax -------------------------------------------------------------
    tt_calc <- reactive({

      if (input$choose_geography == "Region") {
        df <- tt_final |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- tt_final |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      }

      df <- df |>
        mutate(revenue = n_rooms * 365 * input$tt_levy * (input$tt_occupancy / 100))
    })

    output$tt_revenue <- renderPlot({
      ggplot(tt_calc(), aes(x = geography_name, y = revenue)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Annual tourist tax revenue",
             subtitle = "Includes hotels and similar establishments, holiday dwellings, tourist campsites and other collective accommodation",
             caption = "Source: Visit England Accommodation Stock Audit 2016",
             x = input$choose_geography,
             y = "Revenue (£)")
    })

    output$tourism_tax_data <- DT::renderDT({
      tt_calc()
    })


# NNDR Business Rates -----------------------------------------------------

    nndr_calc <- reactive({
      nndr_la <- nndr_data |>
        dplyr::mutate(nndr_payable =
                        dplyr::case_when(
          `Rateable Value` <= input$nndr_sbrr ~ 0,
          `Rateable Value` > input$nndr_sbrr & `Rateable Value` < input$sb_threshold ~ `Rateable Value` * input$sb_multiplier,
          `Rateable Value` >= input$sb_threshold ~ `Rateable Value` * input$lb_multiplier
        )) |>
        dplyr::inner_join(ctsop_la_rgn_lookup,
                          by = c("oslaua" = "geography_code")) |>
        dplyr::group_by(geography_code = oslaua, geography_name, geography_type,
                        region_code, region_name) |>
        dplyr::summarise(nndr_payable = sum(nndr_payable))

      nndr_rgn <- nndr_la |>
        dplyr::group_by(region_code, region_name) |>
        dplyr::summarise(nndr_payable = sum(nndr_payable)) |>
        dplyr::mutate(geography_type = "REGL") |>
        dplyr::rename(geography_code = region_code,
                      geography_name = region_name)

      nndr_final <- dplyr::bind_rows(nndr_rgn, nndr_la)
      return(nndr_final)
    })

    nndr_plot_data <- reactive({
      if (input$choose_geography == "Region") {
        df <- nndr_calc() |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- nndr_calc() |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      }
      return(df)
    })

    output$nndr_plot <- renderPlot({
      ggplot(nndr_plot_data(), aes(x = geography_name, y = nndr_payable)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Business Rates revenue",
             subtitle = "Includes SBRR but not transitional adjustments, empty property rate, \nmandatory and discretionary reliefs and accounting adjustments",
             x = input$choose_geography,
             y = "Revenue (£)")
    })

# Employer NI retention ---------------------------------------------------

    ni_plot_data <- reactive({
      if (input$choose_geography == "Region") {
        df <- ni_data |>
          dplyr::filter(geography_type == "REGL")
      } else if (input$choose_geography == "Local Authority") {
        req(input$rgn_select)
        df <- ni_data |>
          dplyr::filter(geography_type == "LAUA",
                        region_name == input$rgn_select)
      } else if (input$choose_geography == "Combined Authority") {
        df <- ni_data |>
          dplyr::filter(geography_type == "CA")
      }
      return(df)
    })

    output$ni_revenue <- renderPlot({
      ggplot(ni_plot_data(), aes(x = geography_name, y = ni_paid)) +
        geom_col(position = "dodge") +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Employers' NI retention",
             subtitle = "Assumes 1pp of Employers' NI is retained\nNB: data not available for all individual LAs, but is included in their regional totals",
             caption = "Source: ASHE pay data, OI modelled calculations",
             x = input$choose_geography,
             y = "Revenue")
    })

    output$ni_data <- DT::renderDT({
      ni_plot_data()
    })

}

shinyApp(ui = ui, server = server)
