library(shiny)
library(bslib)
library(bsicons)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# ---------------------------------------------------------------------------
# 1. LOAD + CLEAN DATA
# ---------------------------------------------------------------------------

load_psa_data <- function(path = "2M4AFN10.csv") {

  raw <- read_delim(
    path,
    delim = ";",
    skip = 2,
    col_types = cols(.default = col_character()),
    locale = locale(encoding = "UTF-8"),
    trim_ws = TRUE
  )

  month_cols <- setdiff(names(raw), c("Geolocation", "Type"))

  long <- raw |>
    pivot_longer(
      cols = all_of(month_cols),
      names_to = "period",
      values_to = "price_raw"
    ) |>
    mutate(
      # strip leading dots PSA uses to indicate province-level rows,
      # e.g. "....Davao de Oro" -> "Davao de Oro"
      province = str_remove(Geolocation, "^\\.+"),
      commodity = Type,
      date  = parse_date_time(period, orders = "Y B"),
      # any non-numeric footnote code (.., */, 2/, etc.) becomes NA
      price = suppressWarnings(as.numeric(price_raw))
    ) |>
    filter(!is.na(date)) |>
    select(province, commodity, date, price) |>
    arrange(province, commodity, date)

  long
}

psa_data <- load_psa_data()

provinces   <- sort(unique(psa_data$province))
commodities <- sort(unique(psa_data$commodity))
date_min <- min(psa_data$date, na.rm = TRUE)
date_max <- max(psa_data$date, na.rm = TRUE)

fmt_php <- label_currency(prefix = "\u20b1", accuracy = 0.01)

# A ggplot theme shared by every chart so the dashboard reads as one voice
theme_psa <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Inter") %+replace%
    theme(
      plot.title       = element_text(face = "bold", size = base_size * 1.05, hjust = 0),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position  = "bottom",
      legend.title     = element_blank(),
      axis.title       = element_text(color = "#5B6660", size = base_size * 0.85),
      axis.text        = element_text(color = "#5B6660")
    )
}


# ---------------------------------------------------------------------------
# 2. UI
# ---------------------------------------------------------------------------

psa_theme <- bs_theme(
  version      = 5,
  bg           = "#F6F5F1",
  fg           = "#1C2321",
  primary      = "#C98A1F",   # egg-yolk gold - headline numbers
  secondary    = "#1F5C6B",   # deep teal - lines / secondary emphasis
  success      = "#3F6B4F",   # rice-leaf green - "good"/latest indicators
  info         = "#1F5C6B",
  base_font    = font_google("Inter"),
  heading_font = font_google("Fraunces", wght = c(500, 600)),
  code_font    = font_google("IBM Plex Mono"),
  "border-radius" = "0.6rem",
  "card-border-color" = "#E7E3D8"
)

psa_css <- tags$style(HTML("
  body { letter-spacing: 0.1px; }
  .navbar-brand { font-family: 'Fraunces', serif; font-weight: 600; font-size: 1.15rem; }
  .navbar { border-bottom: 3px solid #C98A1F; }

  .bslib-sidebar-layout > .sidebar {
    background-color: #FBFAF6;
    border-right: 1px solid #E7E3D8;
  }
  .sidebar .form-label, .sidebar label { font-weight: 600; font-size: 0.85rem; color: #1C2321; }
  .sidebar hr { border-color: #E7E3D8; }

  .card { box-shadow: 0 1px 3px rgba(28,35,33,0.06); }
  .card-header { font-family: 'Fraunces', serif; font-weight: 600; background-color: transparent; border-bottom: 1px solid #E7E3D8; }

  /* egg-shaped showcase behind value-box icons: our one signature flourish */
  .bslib-value-box .value-box-showcase {
    border-radius: 0% 0% 55% 55% / 0% 0% 100% 100%;
  }
  .bslib-value-box .value-box-title { font-size: 0.8rem; letter-spacing: 0.3px; text-transform: uppercase; opacity: 0.85; }
  .bslib-value-box .value-box-value { font-family: 'IBM Plex Mono', monospace; font-weight: 600; }

  #reset_filters { width: 100%; }
"))

ui <- page_navbar(
  title = tagList(bs_icon("egg-fill"), "Region XI Egg Farmgate Prices"),
  theme = psa_theme,
  header = psa_css,
  fillable = TRUE,
  sidebar = sidebar(
    title = "Filters",
    width = 300,
    pickerInput(
      "province", "Province / City",
      choices = provinces, selected = provinces, multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, liveSearch = TRUE,
                               selectedTextFormat = "count > 3", size = 10)
    ),
    pickerInput(
      "commodity", "Commodity",
      choices = commodities, selected = commodities, multiple = TRUE,
      options = pickerOptions(actionsBox = TRUE, selectedTextFormat = "count > 3")
    ),
    sliderInput(
      "date_range", "Date range",
      min = date_min, max = date_max,
      value = c(date_min, date_max),
      timeFormat = "%b %Y"
    ),
    hr(),
    actionButton("reset_filters", "Reset filters", icon = icon("rotate-left")),
    br(), br(),
    downloadButton("download_csv", "Download filtered data (.csv)", class = "btn-outline-secondary w-100")
  ),

  nav_panel(
    title = "Overview",
    icon = bs_icon("speedometer2"),
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title = "Average price",
        value = textOutput("avg_price"),
        showcase = bs_icon("currency-exchange"),
        theme = "primary"
      ),
      value_box(
        title = "Latest observed price",
        value = textOutput("latest_price"),
        p(textOutput("latest_change"), class = "small"),
        showcase = bs_icon("clock-history"),
        theme = "success"
      ),
      value_box(
        title = "Observations",
        value = textOutput("n_obs"),
        p(textOutput("date_span"), class = "small"),
        showcase = bs_icon("table"),
        theme = "secondary"
      )
    ),
    card(
      card_header("Price trend"),
      plotlyOutput("trend_plot", height = "420px")
    )
  ),

  nav_panel(
    title = "Compare",
    icon = bs_icon("bar-chart-steps"),
    layout_columns(
      col_widths = c(6, 6),
      card(
        card_header("Average price by province"),
        plotlyOutput("bar_province", height = "420px")
      ),
      card(
        card_header("Average price by commodity"),
        plotlyOutput("bar_commodity", height = "420px")
      )
    )
  ),

  nav_panel(
    title = "Data",
    icon = bs_icon("table"),
    card(
      card_header("Filtered data"),
      DTOutput("table")
    )
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text small", "Source: Philippine Statistics Authority"))
)


# ---------------------------------------------------------------------------
# 3. SERVER
# ---------------------------------------------------------------------------

server <- function(input, output, session) {

  observeEvent(input$reset_filters, {
    updatePickerInput(session, "province", selected = provinces)
    updatePickerInput(session, "commodity", selected = commodities)
    updateSliderInput(session, "date_range", value = c(date_min, date_max))
  })

  filtered <- reactive({
    req(input$province, input$commodity)
    psa_data |>
      filter(
        province %in% input$province,
        commodity %in% input$commodity,
        date >= input$date_range[1],
        date <= input$date_range[2],
        !is.na(price)
      )
  })

  output$avg_price <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("\u2014")
    fmt_php(mean(d$price, na.rm = TRUE))
  })

  output$latest_price <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("\u2014")
    latest <- d |> filter(date == max(date)) |> pull(price) |> mean(na.rm = TRUE)
    fmt_php(latest)
  })

  output$latest_change <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("")
    dates <- sort(unique(d$date))
    if (length(dates) < 2) return("first observation on record")
    latest_avg   <- d |> filter(date == dates[length(dates)]) |> pull(price) |> mean(na.rm = TRUE)
    previous_avg <- d |> filter(date == dates[length(dates) - 1]) |> pull(price) |> mean(na.rm = TRUE)
    pct <- (latest_avg - previous_avg) / previous_avg * 100
    arrow <- if (pct >= 0) "\u25b2" else "\u25bc"
    paste0(arrow, " ", number(abs(pct), accuracy = 0.1), "% vs. previous period")
  })

  output$n_obs <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })

  output$date_span <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("")
    paste0(format(min(d$date), "%b %Y"), " \u2013 ", format(max(d$date), "%b %Y"))
  })

  output$trend_plot <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data for the current filters."))

    p <- ggplot(d, aes(x = date, y = price,
                        color = interaction(province, commodity, sep = " \u2013 "),
                        group = interaction(province, commodity, sep = " \u2013 "))) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 0.8, alpha = 0.6) +
      scale_color_viridis_d(option = "cividis", end = 0.85) +
      scale_x_datetime(date_labels = "%Y") +
      labs(x = NULL, y = "Price (PHP)") +
      theme_psa()

    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
  })

  output$bar_province <- renderPlotly({
    d <- filtered() |>
      group_by(province) |>
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data."))

    p <- ggplot(d, aes(x = reorder(province, avg_price), y = avg_price, fill = avg_price)) +
      geom_col() +
      scale_fill_viridis_c(option = "cividis", end = 0.85, guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "Average price (PHP)") +
      theme_psa()
    ggplotly(p)
  })

  output$bar_commodity <- renderPlotly({
    d <- filtered() |>
      group_by(commodity) |>
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data."))

    p <- ggplot(d, aes(x = reorder(commodity, avg_price), y = avg_price, fill = avg_price)) +
      geom_col() +
      scale_fill_viridis_c(option = "cividis", end = 0.85, guide = "none") +
      coord_flip() +
      labs(x = NULL, y = "Average price (PHP)") +
      theme_psa()
    ggplotly(p)
  })

  output$table <- renderDT({
    filtered() |>
      arrange(desc(date)) |>
      mutate(date = format(date, "%Y-%m")) |>
      datatable(
        options = list(pageLength = 10, dom = "ftip"),
        rownames = FALSE,
        colnames = c("Province / City", "Commodity", "Period", "Price")
      ) |>
      formatCurrency("price", currency = "\u20b1", digits = 2)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("psa_egg_prices_filtered_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered(), file)
  )
}


shinyApp(ui, server)