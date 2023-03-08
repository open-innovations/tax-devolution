library(shiny)
library(tidyverse)
library(scales)
library(DT)

ui <- navbarPage(
  title = "Tax Devolution Tool",
  id = "navbar",
  windowTitle = "Tax Devolution Tool",

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
            p("Select values for top of each band"),
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
        div(style = "display:inline-block", uiOutput("choose_geography")),
        div(style = "display:inline-block", uiOutput("choose_valuation")),
        uiOutput("rgn_select"),
        tabsetPanel(
          id = "maintab1",
          tabPanel(
            title = "Analyse by properties",
            uiOutput("plot_variable"),
            plotOutput("plot", height = "600")
          ),
          tabPanel(
            title = "Analyse by revenue",
            strong("NB: Still under development"),
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
    id = "Stamp Duty",
    title = "Stamp Duty",

    titlePanel("Stamp Duty: under development (working but running slowly)"),
    sidebarLayout(
      sidebarPanel(width = 3,
        tabsetPanel(
          id = "sdltsidetab1",
          tabPanel(
            title = "SDLT Band values",
            p("Select values for top of each band"),
            uiOutput("sdlt_bands"),
            uiOutput("reset_sdlt_bands")
          ),
          tabPanel(
            title = "SDLT Band rates",
            p("Select tax rates"),
            uiOutput("sdlt_band_rates"),
            uiOutput("sdlt_reset_rates")
          )
        )
      ),
      mainPanel(width = 9,
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
    h2("band.d.equiv.df()"),
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

  ct_bands_england_1991 <- data.frame(
    band  = LETTERS[1:8],
    min   = c(0, 40001, 52001, 68001, 88001, 120001, 160001, 320001),
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

  baseline <- ctsop_2022 |>
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
  })

  output$debug1 <- DT::renderDT({
    temp.df()
  })

  output$debug2 <- DT::renderDT({
    revenue_difference()
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
    radioButtons("choose_valuation", "Choose valuation",
                 choices = c("1995", "2022"),
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


    areas_to_use <- names(ecdf_object)[names(ecdf_object) != "W99999999"]

    # run the model and bring the data into df
    for (area in areas_to_use) {
      df_share[[area]] <- ecdf_object[[area]](temp.bands())
      df_share[[area]] <- diff(c(0, df_share[[area]])) # cumdist to dist
      df_number[[area]] <- round(df_share[[area]] * ctsop_2022[ctsop_2022$ecode == area, ]$all_properties)
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
      temp.df() |>
        # tidyr::pivot_wider(names_from = name, values_from = value) |>
        dplyr::left_join(temp.ratios(), by = c("band")) |>
        dplyr::mutate(band_d_equivalent = number_properties * ratio) |>
        dplyr::group_by(geography_code, geography_name, region_code, region_name, geography_type, model_baseline) |>
        dplyr::summarise(number_properties = sum(number_properties),
                         n_band_d_equivalents = sum(band_d_equivalent)) |>       # this runs band d values against band d equivs to calc revenue
      dplyr::left_join(dplyr::filter(band_d_rates, date == "2022-23"),
                       by = c("geography_code" = "ONS Code")) |>
        dplyr::mutate(revenue = n_band_d_equivalents * band_d_rate)
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
             subtitle = "NB: currently takes no account of single-occupier and other exemptions and CT Benefit provision\nTHIS ANALYSIS NOT YET AVAILABLE AT REGION LEVEL",
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

    sdlt_df <- reactive({
      req(temp.sdlt.bands(), temp.sdlt.rates())
      if (identical(temp.sdlt.bands(), sdlt_rates_df$band) &
          identical(temp.sdlt.rates(), sdlt_rates_df$rate)) {
        ppd_2022 |>
          dplyr::group_by(rgn) |>
          dplyr::summarise(sdlt = sum(sdlt))
      } else {
        ppd_2022 |>
          dplyr::mutate(sdlt = sapply(Price, calc_sdlt_payable)) |>
          dplyr::group_by(rgn) |>
          dplyr::summarise(sdlt = sum(sdlt))
      }
    })

    output$sdlt_plot_by_revenue <- renderPlot({
      sdlt_df() |>
        ggplot(aes(x = rgn, y = sdlt)) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = scales::label_comma()) +
        plot.theme +
        labs(title = "Residential Stamp Duty Land Tax Receipts",
             subtitle = "Based on 2022 Land Registry Price Paid Data")
    })



}

shinyApp(ui = ui, server = server)
