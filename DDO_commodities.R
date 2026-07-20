
library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)
library(scales)

# 1. LOAD + CLEAN DATA


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

provinces  <- sort(unique(psa_data$province))
commodities <- sort(unique(psa_data$commodity))
date_min <- min(psa_data$date, na.rm = TRUE)
date_max <- max(psa_data$date, na.rm = TRUE)


# 2. UI

ui <- page_sidebar(
  title = "Region XI Egg Farmgate Prices (PSA, 2010\u20132026)",
  theme = bs_theme(version = 5, bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,
    checkboxGroupInput(
      "province", "Province / City",
      choices = provinces,
      selected = provinces
    ),
    checkboxGroupInput(
      "commodity", "Commodity",
      choices = commodities,
      selected = commodities
    ),
    sliderInput(
      "date_range", "Date range",
      min = date_min, max = date_max,
      value = c(date_min, date_max),
      timeFormat = "%b %Y"
    ),
    hr(),
    downloadButton("download_csv", "Download filtered data (.csv)")
  ),

  layout_columns(
    col_widths = c(4, 4, 4),
    value_box(title = "Average price (PHP/kg)", value = textOutput("avg_price"), showcase = bsicons::bs_icon("currency-exchange")),
    value_box(title = "Latest observed price", value = textOutput("latest_price"), showcase = bsicons::bs_icon("clock-history")),
    value_box(title = "Observations", value = textOutput("n_obs"), showcase = bsicons::bs_icon("table"))
  ),

  card(
    card_header("Price trend"),
    plotlyOutput("trend_plot", height = "420px")
  ),

  layout_columns(
    col_widths = c(6, 6),
    card(
      card_header("Average price by province"),
      plotlyOutput("bar_province", height = "350px")
    ),
    card(
      card_header("Average price by commodity"),
      plotlyOutput("bar_commodity", height = "350px")
    )
  ),

  card(
    card_header("Filtered data"),
    DTOutput("table")
  )
)


# 3. SERVER

server <- function(input, output, session) {

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
    paste0("\u20b1", number(mean(d$price, na.rm = TRUE), accuracy = 0.01))
  })

  output$latest_price <- renderText({
    d <- filtered()
    if (nrow(d) == 0) return("\u2014")
    latest <- d |> filter(date == max(date)) |> pull(price) |> mean(na.rm = TRUE)
    paste0("\u20b1", number(latest, accuracy = 0.01))
  })

  output$n_obs <- renderText({
    format(nrow(filtered()), big.mark = ",")
  })

  output$trend_plot <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No data for the current filters."))

    p <- ggplot(d, aes(x = date, y = price, color = interaction(province, commodity, sep = " \u2013 "))) +
      geom_line(linewidth = 0.6) +
      geom_point(size = 0.8, alpha = 0.6) +
      labs(x = NULL, y = "Price (PHP/kg or unit)", color = NULL) +
      scale_x_datetime(date_labels = "%Y") +
      theme_minimal(base_size = 12) +
      theme(legend.position = "bottom")

    ggplotly(p) |> layout(legend = list(orientation = "h", y = -0.25))
  })

  output$bar_province <- renderPlotly({
    d <- filtered() |>
      group_by(province) |>
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data."))

    p <- ggplot(d, aes(x = reorder(province, avg_price), y = avg_price)) +
      geom_col(fill = "#2C3E50") +
      coord_flip() +
      labs(x = NULL, y = "Average price") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$bar_commodity <- renderPlotly({
    d <- filtered() |>
      group_by(commodity) |>
      summarise(avg_price = mean(price, na.rm = TRUE), .groups = "drop")
    validate(need(nrow(d) > 0, "No data."))

    p <- ggplot(d, aes(x = reorder(commodity, avg_price), y = avg_price)) +
      geom_col(fill = "#18BC9C") +
      coord_flip() +
      labs(x = NULL, y = "Average price") +
      theme_minimal(base_size = 12)
    ggplotly(p)
  })

  output$table <- renderDT({
    filtered() |>
      arrange(desc(date)) |>
      mutate(date = format(date, "%Y-%m"), price = round(price, 2)) |>
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$download_csv <- downloadHandler(
    filename = function() paste0("psa_egg_prices_filtered_", Sys.Date(), ".csv"),
    content = function(file) write_csv(filtered(), file)
  )
}


shinyApp(ui, server)