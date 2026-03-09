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

# --- Simulator data ---
weights <- readRDS("sim_weights.rds")

# Parties for the simulator (order for sliders and D'Hondt)
SIM_PARTIES <- c("PiS", "KO", "Polska 2050", "PSL", "Lewica", "Razem",
                 "Konfederacja", "KKP", "MN", "Other")

# Get latest estimates as starting slider values
latest_data <- weekly_summaries %>%
  filter(date == max(date)) %>%
  select(party, median_pct)
sim_defaults <- setNames(
  sapply(SIM_PARTIES, function(p) {
    val <- latest_data$median_pct[as.character(latest_data$party) == p]
    if (length(val) == 0) {
      if (p == "MN") return(0.8)
      if (p == "Other") return(0)
      return(0)
    }
    val
  }),
  SIM_PARTIES
)
# Ensure they sum to 100 by putting remainder in Other
sim_defaults["Other"] <- round(100 - sum(sim_defaults[SIM_PARTIES != "Other"]), 1)
# Reorder SIM_PARTIES by descending default support (keep Other last)
non_other <- setdiff(SIM_PARTIES, "Other")
non_other <- non_other[order(-sim_defaults[non_other])]
SIM_PARTIES <- c(non_other, "Other")

# D'Hondt seat allocation function
dhondt <- function(votes, n_seats) {
  # votes: named numeric vector of votes per party
  # n_seats: number of seats to allocate
  # Returns named integer vector of seats per party
  parties <- names(votes)
  seats <- setNames(rep(0L, length(parties)), parties)
  # Only consider parties with positive votes
  active <- votes > 0
  if (sum(active) == 0 || n_seats == 0) return(seats)

  for (s in seq_len(n_seats)) {
    quotients <- ifelse(active, votes / (seats + 1), 0)
    winner <- which.max(quotients)
    seats[winner] <- seats[winner] + 1L
  }
  seats
}

allocate_seats <- function(vote_shares_pct) {
  # vote_shares_pct: named vector with percentages for each party
  # Returns a dataframe with okreg, party, seats

  # Parties that participate in seat allocation (exclude Other)
  seat_parties <- c("KO", "Konfederacja", "KKP", "Lewica", "Razem",
                    "MN", "PiS", "Polska 2050", "PSL")

  # Convert percentages to proportions
  shares <- vote_shares_pct[seat_parties] / 100

  # Apply 5% threshold (MN exempt)
  for (p in seat_parties) {
    if (p != "MN" && shares[p] < 0.05) {
      shares[p] <- 0
    }
  }

  # Map parties to their constituency weight coefficients
  coef_map <- c(
    "PiS" = "PiScoef", "KO" = "KOcoef",
    "Lewica" = "Lewicacoef", "Razem" = "Lewicacoef",
    "Konfederacja" = "Konfcoef", "KKP" = "Konfcoef",
    "Polska 2050" = "TDcoef", "PSL" = "TDcoef",
    "MN" = NA_character_
  )

  results <- list()

  for (i in seq_len(nrow(weights))) {
    w <- weights[i, ]
    okreg_id <- w$okreg
    mag <- w$magnitude
    vv <- w$validvotes

    # Calculate weighted votes per party in this constituency
    votes <- sapply(seat_parties, function(p) {
      if (p == "MN") {
        if (okreg_id == 21) return(vv * shares[p])
        else return(0)
      }
      coef_col <- coef_map[p]
      vv * shares[p] * w[[coef_col]]
    })

    seat_result <- dhondt(votes, mag)

    for (j in seq_along(seat_parties)) {
      results[[length(results) + 1]] <- data.frame(
        okreg = okreg_id,
        party = seat_parties[j],
        seats = seat_result[j],
        stringsAsFactors = FALSE
      )
    }
  }

  bind_rows(results)
}

# Initial seat data: all parties with 0 seats (before simulation is run)
sim_initial_seats <- data.frame(
  party = setdiff(SIM_PARTIES, "Other"),
  seats = 0L,
  stringsAsFactors = FALSE
)

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML(
    "
    @import url('https://fonts.googleapis.com/css2?family=Jost:wght@400;500&display=swap');
    body { font-family: 'Jost', sans-serif; }
    html, body, .container-fluid { height: auto !important; min-height: 0 !important; }
    .fixed-container {
      max-width: 900px;
      margin: 0 auto;
      padding: 0 10px;
      box-sizing: border-box;
    }
    .plot-wrapper {
      position: relative;
      width: 100%;
      max-width: 810px;
    }
    .plot-wrapper .shiny-plot-output {
      width: 100% !important;
      height: auto !important;
      aspect-ratio: 810 / 540;
    }
    .section-divider {
      margin-top: 20px;
      padding-top: 20px;
      border-top: 2px solid #ddd;
    }
    .popup-container {
      min-height: 320px;
      margin-top: 12px;
    }
    .popup-placeholder {
      text-align: center;
      color: #999;
      padding-top: 40px;
    }
    .popup-layout {
      display: flex;
      gap: 16px;
      align-items: flex-start;
    }
    .popup-layout h5 {
      margin: 0 0 8px 0;
      font-size: 1.4rem;
      font-weight: bold;
      font-family: 'Jost', sans-serif;
      line-height: 1.2;
    }
    .popup-grid {
      display: grid;
      grid-template-columns: auto auto;
      gap: 0 16px;
    }
    .popup-layout > div:first-child {
      flex: 1 1 auto;
    }
    .popup-coalitions {
      border-left: 1px solid #ddd;
      padding-left: 16px;
      min-width: 160px;
      font-size: 0.85em;
      align-self: flex-start;
      flex: 0 0 auto;
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
      flex: 0 0 auto;
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
      min-width: 90px;
      white-space: nowrap;
    }
    .map-popup-seats {
      font-weight: bold;
      min-width: 30px;
      text-align: right;
    }

    /* --- Simulator styles --- */
    .sim-section {
      margin-top: 40px;
      padding-top: 20px;
      border-top: 2px solid #ddd;
    }
    .sim-inputs {
      flex: 0 0 auto;
    }
    .sim-input-row {
      display: flex;
      align-items: center;
      gap: 8px;
      margin-bottom: 4px;
    }
    .sim-input-label {
      min-width: 100px;
      white-space: nowrap;
      font-size: 0.9em;
    }
    .sim-input-box {
      width: 80px;
    }
    .sim-input-box .form-group {
      margin-bottom: 0;
    }
    .sim-input-box input[type=number] {
      font-size: 0.85em;
      padding: 4px 6px;
    }
    .sim-input-pct {
      font-size: 0.9em;
      color: #888;
    }
    .sim-total {
      font-size: 0.95em;
      margin: 8px 0;
      padding: 6px 10px;
      border-radius: 4px;
    }
    .sim-total-ok { background: #e8f5e9; }
    .sim-total-bad { background: #ffebee; }
    .sim-seat-list {
      min-width: 180px;
    }
    .sim-seat-entry {
      display: flex;
      align-items: center;
      padding: 4px 0;
      border-bottom: 1px solid #eee;
      font-size: 0.9em;
    }
    .sim-seat-party {
      min-width: 100px;
      white-space: nowrap;
    }
    .sim-seat-count {
      font-weight: bold;
      min-width: 30px;
      text-align: right;
    }

    /* --- Mobile responsive styles --- */
    @media (max-width: 768px) {
      .popup-layout {
        flex-direction: column;
        flex-wrap: wrap;
      }
      .popup-coalitions {
        border-left: none;
        border-top: 1px solid #ddd;
        padding-left: 0;
        padding-top: 12px;
      }
      .popup-map {
        border-left: none;
        border-top: 1px solid #ddd;
        padding-left: 0;
        padding-top: 12px;
      }
      .popup-container {
        min-height: auto;
      }
      .click-info-box {
        width: 100% !important;
        box-sizing: border-box;
      }
    }
  "
  )),
  tags$script(HTML("
    $(document).on('change', '.sim-input-box input[type=number]', function() {
      var val = parseFloat(this.value);
      if (!isNaN(val)) {
        this.value = val.toFixed(2);
        $(this).trigger('change');
      }
    });
  "))),
  div(
    class = "fixed-container",
    tags$h4(style = "margin-top:10px;", "Polling trends"),
    tags$p(
      style = "color:#999; font-size:0.85em; max-width:810px; margin-bottom:8px;",
      "Click anywhere on the plot to see vote and seat estimates for the nearest week.",
      "Hover over any of the points to see particular polling house estimates."
    ),
    div(
      class = "plot-wrapper",
      plotOutput(
        "trend_plot",
        click = "plot_click",
        hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
        width = "100%",
        height = "540px"
      ),
      uiOutput("point_tooltip")
    ),
    div(
      style = "margin-top: 20px;",
      uiOutput("click_info")
    ),

    # --- Simulator ---
    div(
      class = "sim-section",
      tags$h4(style = "margin-top:0;", "Seat simulator"),
      tags$p(
        style = "color:#999; font-size:0.85em; margin-bottom:16px;",
        "Adjust vote shares to see how seat allocation would change.",
        "Vote shares must sum to 100% before running the simulation."
      ),
      div(
        class = "popup-layout",
        div(
          style = "display:flex; gap:32px;",
          div(
            class = "sim-inputs",
            lapply(SIM_PARTIES, function(p) {
              col <- PARTY_COLORS[p]
              div(
                class = "sim-input-row",
                div(
                  class = "sim-input-label",
                  HTML(paste0(
                    "<span class='color-dot' style='background:", col, ";'></span>",
                    p
                  ))
                ),
                div(
                  class = "sim-input-box",
                  numericInput(
                    inputId = paste0("sim_", gsub(" ", "_", p)),
                    label = NULL,
                    value = 0,
                    min = 0, max = 100,
                    step = 0.1
                  )
                ),
                span(class = "sim-input-pct", "%")
              )
            }),
            uiOutput("sim_total_display")
          ),
          div(
            class = "sim-seat-list",
            uiOutput("sim_results_ui")
          )
        ),
        uiOutput("sim_coalitions_ui"),
        uiOutput("sim_map_ui")
      )
    )
  )
)

# --- Helpers ---
# Build governing majorities based on compatibility rules
build_coalitions <- function(get_seats_fn) {
  short_names <- c(
    "KO" = "KO", "Polska 2050" = "P2050", "Lewica" = "Lewica",
    "PSL" = "PSL", "PiS" = "PiS", "Konfederacja" = "Konf.",
    "KKP" = "KKP", "Razem" = "Razem"
  )

  all_parties <- names(short_names)
  # Only consider parties that won seats, sorted by seats descending
  active_parties <- all_parties[sapply(all_parties, function(p) get_seats_fn(p) > 0)]
  active_parties <- active_parties[order(-sapply(active_parties, get_seats_fn))]

  # Forbidden pairs (incompatible coalition partners)
  forbidden <- list(
    c("Konfederacja", "Lewica"),
    c("Konfederacja", "Razem"),
    c("KKP", "Lewica"),
    c("KKP", "Razem"),
    c("KKP", "KO"),
    c("PiS", "KO"),
    c("PiS", "Lewica")
  )

  is_compatible <- function(parties) {
    for (fp in forbidden) {
      if (all(fp %in% parties)) return(FALSE)
    }
    TRUE
  }

  # Identify parties with a single-party majority
  majority_parties <- active_parties[sapply(active_parties, function(p)
    get_seats_fn(p) >= 231)]

  coalitions <- list()

  # Add single-party governments
  for (p in majority_parties) {
    s <- get_seats_fn(p)
    coalitions[[length(coalitions) + 1]] <- list(
      name = short_names[p],
      seats = s,
      parties = p
    )
  }

  # Enumerate multi-party coalitions (2+ parties)
  if (length(active_parties) >= 2) {
    for (size in 2:length(active_parties)) {
      combos <- combn(active_parties, size, simplify = FALSE)
      for (combo in combos) {
        # Skip coalitions that are supersets of a single-party majority
        if (any(majority_parties %in% combo)) next
        if (!is_compatible(combo)) next
        total_seats <- sum(sapply(combo, get_seats_fn))
        if (total_seats >= 231) {
          coalitions[[length(coalitions) + 1]] <- list(
            name = paste(short_names[combo], collapse = " + "),
            seats = total_seats,
            parties = combo
          )
        }
      }
    }
  }

  # Sort by seats descending
  coalitions[order(-sapply(coalitions, function(x) x$seats))]
}

build_popup_html <- function(snapped_date, date_label = NULL) {
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
  coalitions <- build_coalitions(get_seats)

  coalition_entries <- sapply(coalitions, function(c) {
    label <- if (c$seats >= 307) {
      "<span class='coalition-majority' style='color:green;'>constitutional majority</span>"
    } else if (c$seats >= 276) {
      "<span class='coalition-majority' style='color:green;'>veto override</span>"
    } else {
      ""
    }
    paste0(
      "<div class='coalition-entry'>",
      "<div class='coalition-name'>", c$name, "</div>",
      "<div><span class='coalition-seats'>", c$seats, " seats</span>",
      label,
      "</div>",
      "</div>"
    )
  })

  date_heading <- if (!is.null(date_label)) {
    paste0("<h5>", date_label, "</h5>")
  } else {
    ""
  }

  list(
    grid_html = paste0(
      "<div>",
      date_heading,
      "<div class='popup-grid'>",
      paste(entries, collapse = ""),
      "</div>",
      "<p style='margin-bottom:0; margin-top:8px; color:#999; font-size:0.75em;'>",
      "80% credible intervals are shown in brackets. ",
      "Seat shares are median estimates and may not sum to 460.</p>",
      "</div>"
    ),
    coalition_html = paste0(
      "<div class='popup-coalitions'>",
      "<h5>Governing majorities</h5>",
      paste(coalition_entries, collapse = ""),
      "</div>"
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
    popup_html <- build_popup_html(selected_date(),
                                    date_label = format(selected_date(), "%e %B %Y"))
    if (is.null(popup_html)) return(NULL)

    div(
      div(
        class = "popup-layout",
        HTML(popup_html$grid_html),
        HTML(popup_html$coalition_html),
        div(
          class = "popup-map",
          tags$h5("Constituency seat shares"),
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
      )
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

  # --- Simulator ---
  sim_slider_values <- reactive({
    vals <- sapply(SIM_PARTIES, function(p) {
      input[[paste0("sim_", gsub(" ", "_", p))]]
    })
    setNames(vals, SIM_PARTIES)
  })

  output$sim_total_display <- renderUI({
    vals <- sim_slider_values()
    if (any(sapply(vals, is.null))) return(NULL)
    total <- sum(unlist(vals))
    css_class <- if (abs(total - 100) < 0.15) "sim-total sim-total-ok" else "sim-total sim-total-bad"
    div(
      class = css_class,
      paste0("Total: ", format(round(total, 1), nsmall = 1), "%")
    )
  })

  sim_seat_data <- reactiveVal(sim_initial_seats)
  sim_has_run <- reactiveVal(FALSE)
  sim_selected_const <- reactiveVal(NULL)

  observeEvent(input$sim_run, {
    vals <- sim_slider_values()
    if (any(sapply(vals, is.null))) return()
    total <- sum(unlist(vals))
    if (abs(total - 100) >= 0.15) return()

    vote_shares <- setNames(as.numeric(vals), SIM_PARTIES)
    result <- allocate_seats(vote_shares)
    sim_seat_data(result)
    sim_has_run(TRUE)
    sim_selected_const(NULL)
  })

  sim_map_data <- reactive({
    result <- sim_seat_data()
    if (is.null(result) || !("okreg" %in% names(result))) return(NULL)

    totals <- result %>%
      group_by(okreg) %>%
      summarise(total_seats = sum(seats), .groups = "drop")

    winners <- result %>%
      group_by(okreg) %>%
      slice_max(seats, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      select(okreg, winning_party = party, winning_seats = seats) %>%
      left_join(totals, by = "okreg") %>%
      mutate(
        dominance = ifelse(total_seats > 0, winning_seats / total_seats, 0),
        winning_party = ifelse(winning_seats == 0, NA_character_, winning_party)
      )

    md <- merge(const_map, winners, by.x = "id", by.y = "okreg", all.x = TRUE)

    md$fill_color <- mapply(function(party, dom) {
      if (is.na(party)) return("grey80")
      base <- col2rgb(PARTY_COLORS[party]) / 255
      blended <- base + (1 - base) * (1 - dom)
      rgb(blended[1], blended[2], blended[3])
    }, md$winning_party, md$dominance)

    md
  })

  sim_national <- reactive({
    result <- sim_seat_data()
    if (is.null(result)) return(NULL)
    national <- result %>%
      group_by(party) %>%
      summarise(seats = sum(seats), .groups = "drop")
    if (sim_has_run()) {
      national <- national %>% filter(seats > 0)
    }
    national %>% arrange(desc(seats))
  })

  output$sim_results_ui <- renderUI({
    national <- sim_national()
    if (is.null(national)) return(NULL)

    seat_entries <- lapply(seq_len(nrow(national)), function(i) {
      p <- national$party[i]
      col <- PARTY_COLORS[p]
      HTML(paste0(
        "<div class='sim-seat-entry'>",
        "<span class='sim-seat-party'><span class='color-dot' style='background:",
        col, ";'></span>", p, "</span>",
        "<span class='sim-seat-count'>", national$seats[i], "</span>",
        "</div>"
      ))
    })

    div(
      tags$h5("Seats"),
      actionButton("sim_run", "Calculate seats",
                   style = "margin-bottom:8px;"),
      tagList(seat_entries)
    )
  })

  output$sim_coalitions_ui <- renderUI({
    national <- sim_national()
    if (is.null(national)) return(NULL)

    coalition_content <- NULL
    if (sim_has_run()) {
      get_s <- function(p) {
        val <- national$seats[national$party == p]
        if (length(val) == 0) 0L else val
      }
      coalitions <- build_coalitions(get_s)

      coalition_content <- lapply(coalitions, function(co) {
        label <- if (co$seats >= 307) {
          "<span class='coalition-majority' style='color:green;'>constitutional majority</span>"
        } else if (co$seats >= 276) {
          "<span class='coalition-majority' style='color:green;'>veto override</span>"
        } else {
          ""
        }
        HTML(paste0(
          "<div class='coalition-entry'>",
          "<div class='coalition-name'>", co$name, "</div>",
          "<div><span class='coalition-seats'>", co$seats, " seats</span>",
          label, "</div></div>"
        ))
      })
    }

    div(
      class = "popup-coalitions",
      tags$h5("Governing majorities"),
      tagList(coalition_content)
    )
  })

  output$sim_map_ui <- renderUI({
    sim_seat_data()  # trigger reactivity
    div(
      class = "popup-map",
      tags$h5("Constituency seat shares"),
      div(
        style = "position: relative;",
        plotOutput("sim_map", click = "sim_map_click",
                   width = "250px", height = "280px"),
        uiOutput("sim_map_popup")
      ),
      tags$p(
        style = "margin:4px 0 0 0; color:#999; font-size:0.75em; max-width:250px;",
        "Click on the map for constituency seat shares."
      )
    )
  })

  output$sim_map <- renderPlot(
    {
      showtext_opts(dpi = 96)
      md <- sim_map_data()
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

  observeEvent(input$sim_map_click, {
    md <- sim_map_data()
    if (is.null(md)) return()

    click_point <- sf::st_point(c(input$sim_map_click$x, input$sim_map_click$y))
    click_sfc <- sf::st_sfc(click_point, crs = sf::st_crs(md))
    hit <- sf::st_intersects(click_sfc, md)

    if (length(hit[[1]]) > 0) {
      sim_selected_const(md$id[hit[[1]][1]])
    } else {
      sim_selected_const(NULL)
    }
  })

  output$sim_map_popup <- renderUI({
    const_id <- sim_selected_const()
    if (is.null(const_id)) return(NULL)

    result <- sim_seat_data()
    if (is.null(result)) return(NULL)

    cs <- result %>%
      filter(okreg == const_id, seats > 0) %>%
      arrange(desc(seats))

    if (nrow(cs) == 0) return(NULL)

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
          "<span class='map-popup-seats'>", seats, "</span>",
          "&nbsp;seats</div>"
        )
      ) %>%
      pull(html)

    left_px <- input$sim_map_click$coords_css$x
    top_px <- input$sim_map_click$coords_css$y

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
