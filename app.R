library(shiny)
library(tidyverse)
library(lubridate)
library(showtext)
library(sf)

# Load Jost from Google Fonts
font_add_google("Jost", "Jost")
font_add_google("Jost", "Jost Medium", regular.wt = 500)
showtext_auto()
showtext_opts(dpi = 96)

# --- Configuration ---
PARTY_COLORS <- c(
  "PiS" = "blue",
  "KO" = "orange",
  "Polska 2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "KKP" = "brown",
  "Lewica" = "red",
  "Razem" = "purple",
  "MN" = "khaki",
  "Other" = "gray50"
)

PARTY_ORDER <- c(
  "PiS",
  "KO",
  "Polska 2050",
  "PSL",
  "Lewica",
  "Razem",
  "Konfederacja",
  "KKP"
)

theme_plots <- function(base_size = 16, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      panel.border = element_rect(
        color = "grey50",
        fill = NA,
        linewidth = 0.15
      ),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_text(size = 18),
      axis.title.x = element_text(size = 18, margin = margin(t = 10)),
      axis.title.y = element_text(
        size = 18,
        hjust = 1,
        margin = margin(r = 10)
      ),
      legend.position = "none",
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0), "cm")
    )
}

my_date_format <- function() {
  function(x) {
    m <- format(x, "%b")
    y <- format(x, "\n%Y")
    ifelse(duplicated(y), m, paste(m, y))
  }
}

# --- Load pre-computed data ---
trend_lines <- readRDS("trend_lines.rds")
date_summaries <- readRDS("date_summaries.rds") %>%
  mutate(party = factor(party, levels = PARTY_ORDER)) %>%
  filter(!is.na(party))
weekly_summaries <- readRDS("weekly_summaries.rds") %>%
  mutate(party = factor(party, levels = c(PARTY_ORDER, "MN"))) %>%
  filter(!is.na(party))
point_dta <- readRDS("point_dta.rds") %>%
  mutate(midDate_num = as.numeric(midDate))
constituency_seats <- readRDS("constituency_seats.rds")
const_map <- readRDS("const_map.rds")

# Fix Polish diacritics in constituency names
polish_names <- c(
  "1" = "Białystok", "2" = "Bielsko-Biała", "3" = "Bydgoszcz",
  "4" = "Chełm", "5" = "Częstochowa", "6" = "Elbląg",
  "7" = "Gdańsk", "8" = "Gdynia", "9" = "Gliwice",
  "10" = "Kalisz", "11" = "Katowice", "12" = "Kielce",
  "13" = "Konin", "14" = "Koszalin", "15" = "Kraków I (południe)",
  "16" = "Kraków II (północ)", "17" = "Krosno", "18" = "Legnica",
  "19" = "Lublin", "20" = "Nowy Sącz", "21" = "Olsztyn",
  "22" = "Opole", "23" = "Piotrków Trybunalski", "24" = "Piła",
  "25" = "Poznań", "26" = "Płock", "27" = "Radom",
  "28" = "Rybnik", "29" = "Rzeszów", "30" = "Siedlce",
  "31" = "Sieradz", "32" = "Sosnowiec", "33" = "Szczecin",
  "34" = "Tarnów", "35" = "Toruń", "36" = "Warszawa I (miasto)",
  "37" = "Warszawa II (okręg)", "38" = "Wałbrzych", "39" = "Wrocław",
  "40" = "Zielona Góra", "41" = "Łódź"
)
const_map$cst_n <- polish_names[as.character(const_map$cst)]

available_dates <- sort(unique(weekly_summaries$date))

# --- UI ---
ui <- fluidPage(
  tags$head(tags$style(HTML(
    "
    @import url('https://fonts.googleapis.com/css2?family=Jost:wght@400;500&display=swap');
    body { font-family: 'Jost', sans-serif; }
    .fixed-container {
      max-width: 900px;
      margin: 0 auto;
    }
    .popup-container {
      min-height: 320px;
      margin-top: 20px;
      display: flex;
      justify-content: center;
    }
    .popup-placeholder {
      text-align: center;
      color: #999;
      padding-top: 40px;
    }
    .popup-layout {
      display: flex;
      gap: 16px;
    }
    .popup-grid {
      display: grid;
      grid-template-columns: auto auto;
      gap: 0 16px;
    }
    .popup-coalitions {
      border-left: 1px solid #ddd;
      padding-left: 16px;
      min-width: 160px;
      font-size: 0.85em;
      align-self: center;
    }
    .popup-coalitions h5 {
      margin: 0 0 8px 0;
      font-size: 1em;
    }
    .coalition-entry {
      padding: 4px 0;
      border-bottom: 1px solid #eee;
    }
    .coalition-name {
      color: #555;
    }
    .coalition-seats {
      font-weight: bold;
    }
    .coalition-majority {
      font-size: 0.9em;
      margin-left: 4px;
    }
    .popup-entry {
      padding: 6px 0;
      border-bottom: 1px solid #eee;
    }
    .popup-party-name {
      display: flex;
      align-items: baseline;
    }
    .popup-row {
      display: flex;
      align-items: baseline;
      font-size: 0.85em;
    }
    .popup-label {
      min-width: 44px;
      color: #888;
    }
    .popup-est {
      font-weight: bold;
    }
    .popup-ci {
      margin-left: 6px;
      color: #666;
      white-space: nowrap;
    }
    .color-dot {
      display: inline-block; width: 10px; height: 10px;
      border-radius: 50%; margin-right: 6px; vertical-align: middle;
    }
    .point-tooltip {
      position: absolute;
      background: white;
      border: 1px solid #ccc;
      border-radius: 6px;
      padding: 8px 12px;
      font-size: 0.9em;
      pointer-events: none;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      z-index: 100;
      white-space: nowrap;
    }
    .popup-map {
      border-left: 1px solid #ddd;
      padding-left: 16px;
    }
    .map-popup {
      position: absolute;
      background: white;
      border: 1px solid #ccc;
      border-radius: 6px;
      padding: 10px 14px;
      font-size: 0.9em;
      pointer-events: none;
      box-shadow: 0 2px 6px rgba(0,0,0,0.15);
      z-index: 100;
    }
    .map-popup-entry {
      display: flex;
      align-items: baseline;
      padding: 2px 0;
    }
    .map-popup-party {
      min-width: 110px;
    }
    .map-popup-seats {
      font-weight: bold;
      min-width: 30px;
      text-align: right;
    }
  "
  ))),
  div(
    class = "fixed-container",
    titlePanel("Pooled polls of vote intention in Poland"),
    div(
      style = "position: relative;",
      plotOutput(
        "trend_plot",
        click = "plot_click",
        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
        width = "810px",
        height = "540px"
      ),
      uiOutput("point_tooltip")
    ),
    div(class = "popup-container", uiOutput("click_info"))
  )
)

# --- Helper ---
build_popup_html <- function(snapped_date) {
  day_data <- weekly_summaries %>%
    filter(date == snapped_date) %>%
    arrange(desc(median_pct))

  if (nrow(day_data) == 0) {
    return(NULL)
  }

  entries <- day_data %>%
    rowwise() %>%
    mutate(
      color_hex = PARTY_COLORS[as.character(party)],
      entry_html = paste0(
        "<div class='popup-entry'>",
        "<div class='popup-party-name'><span class='color-dot' style='background:",
        color_hex,
        ";'></span>",
        party,
        "</div>",
        "<div class='popup-row'>",
        "<span class='popup-label'>votes</span>",
        "<span class='popup-est'>",
        median_pct,
        "%</span>",
        "<span class='popup-ci'>(",
        lower_pct,
        "% \u2013 ",
        upper_pct,
        "%)</span>",
        "</div>",
        "<div class='popup-row'>",
        "<span class='popup-label'>seats</span>",
        "<span class='popup-est'>",
        median_seats,
        "</span>",
        "<span class='popup-ci'>(",
        lower_seats,
        " \u2013 ",
        upper_seats,
        ")</span>",
        "</div>",
        "</div>"
      )
    ) %>%
    pull(entry_html)

  # Coalition seat calculations
  get_seats <- function(p) {
    val <- day_data$median_seats[as.character(day_data$party) == p]
    if (length(val) == 0) 0L else val
  }
  coalitions <- list(
    list(
      name = "KO + P2050 + Lewica + PSL",
      seats = get_seats("KO") + get_seats("Polska 2050") +
        get_seats("Lewica") + get_seats("PSL")
    ),
    list(
      name = "PiS + Konf. + KKP",
      seats = get_seats("PiS") + get_seats("Konfederacja") +
        get_seats("KKP")
    ),
    list(
      name = "PiS + Konf.",
      seats = get_seats("PiS") + get_seats("Konfederacja")
    ),
    list(
      name = "KO + Konf.",
      seats = get_seats("KO") + get_seats("Konfederacja")
    )
  )

  coalition_entries <- sapply(coalitions, function(c) {
    majority_icon <- if (c$seats >= 231) {
      "<span class='coalition-majority' style='color:green;'>&#10003;</span>"
    } else {
      "<span class='coalition-majority' style='color:red;'>&#10007;</span>"
    }
    paste0(
      "<div class='coalition-entry'>",
      "<div class='coalition-name'>", c$name, "</div>",
      "<div><span class='coalition-seats'>", c$seats, " seats</span>",
      majority_icon,
      "</div>",
      "</div>"
    )
  })

  list(
    grid_html = paste0(
      "<div class='popup-grid'>",
      paste(entries, collapse = ""),
      "</div>"
    ),
    coalition_html = paste0(
      "<div class='popup-coalitions'>",
      "<h5>Coalitions</h5>",
      paste(coalition_entries, collapse = ""),
      "<div style='margin-top:8px; color:#999; font-size:0.85em;'>",
      "&#10003; = \u2265 231 (majority)</div>",
      "</div>"
    ),
    notes_html = paste0(
      "<p style='margin-bottom:0; margin-top:12px; color:#999; font-size:0.85em;'>",
      "Click anywhere on the plot to see vote and seat estimates for the nearest week. ",
      "Hover over any of the points to see particular polling house estimates. ",
      "80% credible intervals are shown in brackets. Estimated seat shares may not sum to 460.</p>"
    )
  )
}

# --- Server ---
server <- function(input, output, session) {
  selected_date <- reactiveVal(max(available_dates))
  selected_constituency <- reactiveVal(NULL)

  observeEvent(input$plot_click, {
    clicked_date <- as.Date(input$plot_click$x, origin = "1970-01-01")
    idx <- which.min(abs(available_dates - clicked_date))
    selected_date(available_dates[idx])
    selected_constituency(NULL)
  })

  output$trend_plot <- renderPlot(
    {
      showtext_opts(dpi = 96)
      ggplot() +
        geom_point(
          data = point_dta,
          aes(x = midDate, y = est, colour = party),
          size = 2,
          alpha = 0.3,
          show.legend = FALSE
        ) +
        geom_line(
          data = trend_lines,
          aes(x = date, y = median_epred, colour = party),
          linewidth = 1
        ) +
        scale_color_manual(values = PARTY_COLORS) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_date(date_breaks = "1 month", labels = my_date_format()) +
        coord_cartesian(ylim = c(0, NA)) +
        labs(y = "", x = "", color = "") +
        theme_plots()
    },
    res = 96,
    execOnResize = TRUE
  )

  output$point_tooltip <- renderUI({
    hover <- input$plot_hover
    if (is.null(hover)) {
      return(NULL)
    }

    point <- nearPoints(
      point_dta,
      hover,
      xvar = "midDate_num",
      yvar = "est",
      threshold = 10,
      maxpoints = 1
    )

    if (nrow(point) == 0) {
      return(NULL)
    }

    color <- PARTY_COLORS[as.character(point$party[1])]
    left_px <- hover$coords_css$x
    top_px <- hover$coords_css$y

    style <- paste0(
      "left:",
      left_px + 12,
      "px; top:",
      top_px + 12,
      "px;"
    )

    div(
      class = "point-tooltip",
      style = style,
      HTML(paste0(
        "<span class='color-dot' style='background:",
        color,
        ";'></span>",
        "<b>",
        point$party[1],
        "</b> ",
        round(point$est[1] * 100, 1),
        "%<br>",
        as.character(point$org[1]),
        " &middot; ",
        format(point$midDate[1], "%e %B %Y")
      ))
    )
  })

  output$click_info <- renderUI({
    popup_html <- build_popup_html(selected_date())
    if (is.null(popup_html)) return(NULL)

    div(
      style = "padding:16px; border:1px solid #ddd; border-radius:8px; width:fit-content;",
      tags$h4(
        style = "margin-top:0; font-weight:normal;",
        format(selected_date(), "%e %B %Y")
      ),
      div(
        class = "popup-layout",
        HTML(popup_html$grid_html),
        HTML(popup_html$coalition_html),
        div(
          class = "popup-map",
          div(
            style = "position: relative;",
            plotOutput("seat_map", click = "map_click",
                       width = "250px", height = "280px"),
            uiOutput("map_popup")
          ),
          tags$p(
            style = "margin:4px 0 0 0; color:#999; font-size:0.75em; max-width:250px;",
            "Click on the map for seat shares in specific constituencies."
          )
        )
      ),
      HTML(popup_html$notes_html)
    )
  })

  # --- Map ---
  map_data <- reactive({
    cs <- constituency_seats %>%
      filter(date == selected_date())
    if (nrow(cs) == 0) return(NULL)

    # Get total seats and winning party per constituency
    totals <- cs %>%
      group_by(okreg) %>%
      summarise(total_seats = sum(median_seats), .groups = "drop")

    winners <- cs %>%
      group_by(okreg) %>%
      slice_max(median_seats, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(okreg, winning_party = party, winning_seats = median_seats) %>%
      left_join(totals, by = "okreg") %>%
      mutate(
        dominance = ifelse(total_seats > 0, winning_seats / total_seats, 0),
        winning_party = ifelse(winning_seats == 0, NA_character_, winning_party)
      )

    md <- merge(const_map, winners, by.x = "id", by.y = "okreg", all.x = TRUE)

    # Compute blended fill colour (party colour -> white based on dominance)
    md$fill_color <- mapply(function(party, dom) {
      if (is.na(party)) return("grey80")
      base <- col2rgb(PARTY_COLORS[party]) / 255
      blended <- base + (1 - base) * (1 - dom)
      rgb(blended[1], blended[2], blended[3])
    }, md$winning_party, md$dominance)

    md
  })

  output$seat_map <- renderPlot(
    {
      showtext_opts(dpi = 96)
      md <- map_data()
      if (is.null(md)) return(NULL)

      fill_vals <- setNames(md$fill_color, md$id)
      ggplot(md) +
        geom_sf(aes(fill = as.character(id)), color = "white", linewidth = 0.2) +
        scale_fill_manual(values = fill_vals, na.value = "grey80") +
        theme_void(base_family = "Jost") +
        theme(
          legend.position = "none",
          plot.margin = unit(c(0, 0, 0, 0), "cm")
        )
    },
    res = 96
  )

  observeEvent(input$map_click, {
    md <- map_data()
    if (is.null(md)) return()

    click_point <- sf::st_point(c(input$map_click$x, input$map_click$y))
    click_sfc <- sf::st_sfc(click_point, crs = sf::st_crs(md))
    hit <- sf::st_intersects(click_sfc, md)

    if (length(hit[[1]]) > 0) {
      selected_constituency(md$id[hit[[1]][1]])
    } else {
      selected_constituency(NULL)
    }
  })

  output$map_popup <- renderUI({
    const_id <- selected_constituency()
    if (is.null(const_id)) return(NULL)

    cs <- constituency_seats %>%
      filter(date == selected_date(), okreg == const_id, median_seats > 0) %>%
      arrange(desc(median_seats))

    if (nrow(cs) == 0) return(NULL)

    # Get constituency name
    const_name <- const_map$cst_n[const_map$id == const_id]
    if (length(const_name) == 0) const_name <- paste("Constituency", const_id)

    entries <- cs %>%
      rowwise() %>%
      mutate(
        color_hex = PARTY_COLORS[party],
        html = paste0(
          "<div class='map-popup-entry'>",
          "<span class='map-popup-party'><span class='color-dot' style='background:",
          color_hex, ";'></span>",
          party, "</span>",
          "<span class='map-popup-seats'>", median_seats, "</span>",
          "&nbsp;seats</div>"
        )
      ) %>%
      pull(html)

    # Position near the click
    left_px <- input$map_click$coords_css$x
    top_px <- input$map_click$coords_css$y

    div(
      class = "map-popup",
      style = paste0("left:", left_px + 15, "px; top:", top_px + 15, "px;"),
      HTML(paste0(
        "<b>", const_name, "</b><br>",
        paste(entries, collapse = "")
      ))
    )
  })
}

shinyApp(ui, server)
