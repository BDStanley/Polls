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
  "1" = "Białystok",
  "2" = "Bielsko-Biała",
  "3" = "Bydgoszcz",
  "4" = "Chełm",
  "5" = "Częstochowa",
  "6" = "Elbląg",
  "7" = "Gdańsk",
  "8" = "Gdynia",
  "9" = "Gliwice",
  "10" = "Kalisz",
  "11" = "Katowice",
  "12" = "Kielce",
  "13" = "Konin",
  "14" = "Koszalin",
  "15" = "Kraków I (południe)",
  "16" = "Kraków II (północ)",
  "17" = "Krosno",
  "18" = "Legnica",
  "19" = "Lublin",
  "20" = "Nowy Sącz",
  "21" = "Olsztyn",
  "22" = "Opole",
  "23" = "Piotrków Trybunalski",
  "24" = "Piła",
  "25" = "Poznań",
  "26" = "Płock",
  "27" = "Radom",
  "28" = "Rybnik",
  "29" = "Rzeszów",
  "30" = "Siedlce",
  "31" = "Sieradz",
  "32" = "Sosnowiec",
  "33" = "Szczecin",
  "34" = "Tarnów",
  "35" = "Toruń",
  "36" = "Warszawa I (miasto)",
  "37" = "Warszawa II (okręg)",
  "38" = "Wałbrzych",
  "39" = "Wrocław",
  "40" = "Zielona Góra",
  "41" = "Łódź"
)
const_map$cst_n <- polish_names[as.character(const_map$cst)]

available_dates <- sort(unique(weekly_summaries$date))

ALL_AGENCIES <- sort(unique(point_dta$org))

# --- Simulator data ---
weights <- readRDS("sim_weights.rds")

# Parties for the simulator (order for sliders and D'Hondt)
SIM_PARTIES <- c(
  "PiS",
  "KO",
  "Polska 2050",
  "PSL",
  "Lewica",
  "Razem",
  "Konfederacja",
  "KKP",
  "MN",
  "Other"
)

# Get latest estimates as starting slider values
latest_data <- weekly_summaries %>%
  filter(date == max(date)) %>%
  select(party, median_pct)
sim_defaults <- setNames(
  sapply(SIM_PARTIES, function(p) {
    val <- latest_data$median_pct[as.character(latest_data$party) == p]
    if (length(val) == 0) {
      if (p == "MN") {
        return(0.8)
      }
      if (p == "Other") {
        return(0)
      }
      return(0)
    }
    val
  }),
  SIM_PARTIES
)
# Ensure they sum to 100 by putting remainder in Other
sim_defaults["Other"] <- round(
  100 - sum(sim_defaults[SIM_PARTIES != "Other"]),
  1
)
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
  if (sum(active) == 0 || n_seats == 0) {
    return(seats)
  }

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
  seat_parties <- c(
    "KO",
    "Konfederacja",
    "KKP",
    "Lewica",
    "Razem",
    "MN",
    "PiS",
    "Polska 2050",
    "PSL"
  )

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
    "PiS" = "PiScoef",
    "KO" = "KOcoef",
    "Lewica" = "Lewicacoef",
    "Razem" = "Lewicacoef",
    "Konfederacja" = "Konfcoef",
    "KKP" = "Konfcoef",
    "Polska 2050" = "TDcoef",
    "PSL" = "TDcoef",
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
        if (okreg_id == 21) {
          return(vv * shares[p])
        } else {
          return(0)
        }
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

allocate_seats_with_coalitions <- function(
  vote_shares_pct,
  coalitions = list()
) {
  # coalitions: list of character vectors, each a set of party names to merge
  # Returns a dataframe with okreg, party, seats (coalition names for merged parties)

  if (length(coalitions) == 0) {
    return(allocate_seats(vote_shares_pct))
  }

  # Map parties to their constituency weight coefficient columns
  coef_map <- c(
    "PiS" = "PiScoef",
    "KO" = "KOcoef",
    "Lewica" = "Lewicacoef",
    "Razem" = "Lewicacoef",
    "Konfederacja" = "Konfcoef",
    "KKP" = "Konfcoef",
    "Polska 2050" = "TDcoef",
    "PSL" = "TDcoef",
    "MN" = NA_character_
  )

  short_names <- c(
    "KO" = "KO",
    "Polska 2050" = "P2050",
    "Lewica" = "Lewica",
    "PSL" = "PSL",
    "PiS" = "PiS",
    "Konfederacja" = "Konf.",
    "KKP" = "KKP",
    "Razem" = "Razem",
    "MN" = "MN"
  )

  # Build entities: either coalitions or solo parties
  coalition_members <- unlist(coalitions)
  seat_parties <- c(
    "KO",
    "Konfederacja",
    "KKP",
    "Lewica",
    "Razem",
    "MN",
    "PiS",
    "Polska 2050",
    "PSL"
  )

  entities <- list()

  # Add coalitions
  for (coal in coalitions) {
    name <- paste(short_names[coal], collapse = " + ")
    entities[[length(entities) + 1]] <- list(
      name = name,
      members = coal,
      is_coalition = TRUE
    )
  }

  # Add solo parties (not in any coalition)
  for (p in seat_parties) {
    if (!(p %in% coalition_members)) {
      entities[[length(entities) + 1]] <- list(
        name = p,
        members = p,
        is_coalition = FALSE
      )
    }
  }

  # Apply thresholds
  entity_shares <- sapply(entities, function(e) {
    sum(vote_shares_pct[e$members], na.rm = TRUE) / 100
  })

  for (j in seq_along(entities)) {
    e <- entities[[j]]
    share <- entity_shares[j]
    # MN exempt from threshold
    if (length(e$members) == 1 && e$members == "MN") {
      next
    }
    # 8% for coalitions, 5% for solo
    thresh <- if (e$is_coalition) 0.08 else 0.05
    if (share < thresh) entity_shares[j] <- 0
  }

  results <- list()

  for (i in seq_len(nrow(weights))) {
    w <- weights[i, ]
    okreg_id <- w$okreg
    mag <- w$magnitude
    vv <- w$validvotes

    votes <- sapply(seq_along(entities), function(j) {
      e <- entities[[j]]
      share <- entity_shares[j]
      if (share == 0) {
        return(0)
      }

      members <- e$members

      # MN special case
      if (length(members) == 1 && members == "MN") {
        if (okreg_id == 21) {
          return(vv * share)
        } else {
          return(0)
        }
      }

      # If any member is MN in a coalition, handle it
      non_mn <- members[members != "MN"]
      if (length(non_mn) == 0) {
        return(0)
      }

      # Vote-share weighted average of coefficients
      member_shares <- vote_shares_pct[non_mn]
      member_coefs <- sapply(non_mn, function(p) {
        cc <- coef_map[p]
        if (is.na(cc)) {
          return(1)
        }
        w[[cc]]
      })

      total_share <- sum(member_shares, na.rm = TRUE)
      if (total_share == 0) {
        return(0)
      }

      weighted_coef <- sum(member_shares * member_coefs, na.rm = TRUE) /
        total_share
      vv * share * weighted_coef
    })

    names(votes) <- sapply(entities, function(e) e$name)
    seat_result <- dhondt(votes, mag)

    for (j in seq_along(entities)) {
      results[[length(results) + 1]] <- data.frame(
        okreg = okreg_id,
        party = entities[[j]]$name,
        seats = seat_result[j],
        stringsAsFactors = FALSE
      )
    }
  }

  bind_rows(results)
}

# Generate a display color for a coalition (use largest member's color)
coalition_color <- function(members, vote_shares_pct = NULL) {
  if (length(members) == 1) {
    return(PARTY_COLORS[members])
  }
  if (!is.null(vote_shares_pct)) {
    shares <- vote_shares_pct[members]
    largest <- names(which.max(shares))
    return(PARTY_COLORS[largest])
  }
  PARTY_COLORS[members[1]]
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
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
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

    /* --- Agency selector styles --- */
    .agency-selector {
      margin-bottom: 10px;
      max-width: 810px;
    }
    .agency-selector .checkbox-inline {
      margin-right: 4px;
      margin-left: 0;
      margin-bottom: 4px;
    }
    .agency-selector .checkbox-inline input[type='checkbox'] {
      display: none;
    }
    .agency-selector .checkbox-inline {
      display: inline-block;
      padding: 3px 10px;
      border-radius: 14px;
      border: 1.5px solid #bbb;
      background: white;
      font-size: 0.8em;
      cursor: pointer;
      transition: background 0.15s, border-color 0.15s, color 0.15s;
      color: #888;
      user-select: none;
    }
    .agency-selector .checkbox-inline.checked {
      background: #333;
      border-color: #333;
      color: white;
    }
    .agency-selector .shiny-input-container { margin-bottom: 0; }
    .agency-selector label.control-label { display: none; }
    .agency-approx-note {
      font-size: 0.8em;
      color: #b07020;
      background: #fff8f0;
      border: 1px solid #f0d8b0;
      border-radius: 6px;
      padding: 8px 12px;
      margin-top: 8px;
      max-width: 810px;
    }

    /* --- Coalition builder styles --- */
    .coalition-modal-overlay {
      display: none;
      position: fixed;
      top: 0; left: 0; right: 0; bottom: 0;
      background: rgba(0,0,0,0.4);
      z-index: 1000;
      justify-content: center;
      align-items: center;
    }
    .coalition-modal-overlay.active { display: flex; }
    .coalition-modal {
      background: white;
      border-radius: 10px;
      padding: 24px;
      max-width: 500px;
      width: 90%;
      max-height: 80vh;
      overflow-y: auto;
      box-shadow: 0 8px 32px rgba(0,0,0,0.2);
    }
    .coalition-modal h5 { margin: 0 0 16px 0; }
    .coalition-group {
      background: #f8f8f8;
      border-radius: 6px;
      padding: 10px 14px;
      margin-bottom: 8px;
      display: flex;
      align-items: center;
      justify-content: space-between;
    }
    .coalition-group-members { display: flex; flex-wrap: wrap; gap: 4px; align-items: center; }
    .coalition-chip {
      display: inline-flex;
      align-items: center;
      gap: 4px;
      padding: 2px 8px;
      border-radius: 12px;
      font-size: 0.85em;
      background: white;
      border: 1px solid #ddd;
    }
    .coalition-remove-btn {
      background: none; border: none; color: #999;
      cursor: pointer; font-size: 1.1em; padding: 4px 8px;
    }
    .coalition-remove-btn:hover { color: #c00; }
    .coalition-party-picker {
      display: flex; flex-wrap: wrap; gap: 6px;
      margin: 12px 0;
    }
    .coalition-party-option {
      display: inline-flex; align-items: center; gap: 4px;
      padding: 4px 10px; border-radius: 16px;
      border: 2px solid #ddd; background: white;
      cursor: pointer; font-size: 0.85em;
      transition: border-color 0.15s, background 0.15s;
    }
    .coalition-party-option.selected {
      border-color: #333; background: #f0f0f0;
    }
    .coalition-party-option.disabled {
      opacity: 0.35; pointer-events: none;
    }
    .coalition-btn {
      padding: 6px 16px; border-radius: 6px;
      border: 1px solid #ccc; background: white;
      cursor: pointer; font-family: 'Jost', sans-serif;
      font-size: 0.9em;
    }
    .coalition-btn:hover { background: #f5f5f5; }
    .coalition-btn-primary {
      background: #333; color: white; border-color: #333;
    }
    .coalition-btn-primary:hover { background: #555; }
    .coalition-active-indicator {
      font-size: 0.8em; color: #666; margin-top: 4px;
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
    tags$script(HTML(
      "
    $(document).on('change', '.sim-input-box input[type=number]', function() {
      var val = parseFloat(this.value);
      if (!isNaN(val)) {
        this.value = val.toFixed(2);
        $(this).trigger('change');
      }
    });

    // Coalition modal open/close
    function openCoalitionModal() {
      var el = document.getElementById('coalition-modal');
      el.classList.add('active');
      // Bind Shiny inputs that were rendered while the modal was hidden
      setTimeout(function() {
        try { Shiny.unbindAll(el); } catch(e) {}
        Shiny.bindAll(el);
      }, 100);
    }
    function closeCoalitionModal() {
      document.getElementById('coalition-modal').classList.remove('active');
    }
    function removeCoalition(idx) {
      Shiny.setInputValue('coalition_remove', {
        index: idx, nonce: Math.random()
      }, {priority: 'event'});
    }

    // Agency selector chip styling
    function updateAgencyChips() {
      $('.agency-selector .checkbox-inline').each(function() {
        var cb = $(this).find('input[type=checkbox]');
        if (cb.prop('checked')) {
          $(this).addClass('checked');
        } else {
          $(this).removeClass('checked');
        }
      });
    }
    $(document).on('change', '.agency-selector input[type=checkbox]', updateAgencyChips);
    $(document).on('shiny:inputchanged', function(e) {
      if (e.name === 'agency_selector') setTimeout(updateAgencyChips, 50);
    });
    $(function() { setTimeout(updateAgencyChips, 200); });
  "
    ))
  ),
  div(
    class = "fixed-container",
    tags$h4(style = "margin-top:10px;", "Polling trends"),
    uiOutput("trend_description"),
    div(
      class = "agency-selector",
      checkboxGroupInput(
        "agency_selector",
        label = NULL,
        choices = setNames(ALL_AGENCIES, ALL_AGENCIES),
        selected = character(0),
        inline = TRUE
      ),
      div(
        style = "display:flex; gap:8px; margin-top:6px; align-items:center;",
        actionButton(
          "agency_generate",
          "Generate estimates",
          class = "coalition-btn coalition-btn-primary"
        ),
        actionButton("agency_reset", "Show all", class = "coalition-btn")
      )
    ),
    uiOutput("agency_approx_note"),
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

    # Coalition builder modal (shared between trend popup and simulator)
    div(
      id = "coalition-modal",
      class = "coalition-modal-overlay",
      onclick = "if(event.target===this) closeCoalitionModal();",
      div(
        class = "coalition-modal",
        tags$h5("Build electoral coalitions"),
        tags$p(
          style = "color:#999; font-size:0.85em; margin-bottom:12px;",
          "Select 2 or more parties to form an electoral coalition.",
          "Coalitions face an 8% threshold (vs 5% for individual parties)."
        ),
        uiOutput("coalition_current_list"),
        tags$h6(style = "margin: 16px 0 8px 0;", "Add new coalition"),
        uiOutput("coalition_party_picker"),
        div(
          style = "display:flex; gap:8px; margin-top:12px;",
          actionButton(
            "coalition_add_btn",
            "Add coalition",
            class = "coalition-btn coalition-btn-primary"
          ),
          tags$button(
            class = "coalition-btn",
            onclick = "closeCoalitionModal();",
            "Close"
          )
        )
      )
    ),

    # --- Simulator ---
    div(
      class = "sim-section",
      tags$h4(style = "margin-top:0;", "Seat simulator"),
      uiOutput("sim_description"),
      div(
        class = "popup-layout",
        div(
          style = "display:flex; gap:32px;",
          div(
            class = "sim-inputs",
            uiOutput("sim_inputs_ui"),
            uiOutput("sim_total_display")
          ),
          div(
            class = "sim-seat-list",
            uiOutput("sim_results_ui")
          )
        ),
        uiOutput("sim_coalitions_ui"),
        uiOutput("sim_map_ui")
      ),
      uiOutput("sim_weighting_note")
    )
  )
)

# --- Helpers ---
# Build governing majorities based on compatibility rules
build_coalitions <- function(get_seats_fn) {
  short_names <- c(
    "KO" = "KO",
    "Polska 2050" = "P2050",
    "Lewica" = "Lewica",
    "PSL" = "PSL",
    "PiS" = "PiS",
    "Konfederacja" = "Konf.",
    "KKP" = "KKP",
    "Razem" = "Razem"
  )

  all_parties <- names(short_names)
  # Only consider parties that won seats, sorted by seats descending
  active_parties <- all_parties[sapply(all_parties, function(p) {
    get_seats_fn(p) > 0
  })]
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
  majority_parties <- active_parties[sapply(active_parties, function(p) {
    get_seats_fn(p) >= 231
  })]

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
        if (any(majority_parties %in% combo)) {
          next
        }
        if (!is_compatible(combo)) {
          next
        }
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

build_popup_html <- function(
  snapped_date,
  date_label = NULL,
  weekly_data = weekly_summaries,
  show_ci = TRUE
) {
  day_data <- weekly_data %>%
    filter(date == snapped_date) %>%
    arrange(desc(median_pct))

  if (nrow(day_data) == 0) {
    return(NULL)
  }

  if (show_ci) {
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
          "</div></div>"
        )
      ) %>%
      pull(entry_html)
  } else {
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
          "</div>",
          "<div class='popup-row'>",
          "<span class='popup-label'>seats</span>",
          "<span class='popup-est'>",
          median_seats,
          "</span>",
          "</div></div>"
        )
      ) %>%
      pull(entry_html)
  }

  get_seats <- function(p) {
    val <- day_data$median_seats[as.character(day_data$party) == p]
    if (length(val) == 0) 0L else val
  }
  gov_coalitions <- build_coalitions(get_seats)

  coalition_entries <- sapply(gov_coalitions, function(c) {
    label <- if (c$seats >= 307) {
      "<span class='coalition-majority' style='color:green;'>constitutional majority</span>"
    } else if (c$seats >= 276) {
      "<span class='coalition-majority' style='color:green;'>veto override</span>"
    } else {
      ""
    }
    paste0(
      "<div class='coalition-entry'>",
      "<div class='coalition-name'>",
      c$name,
      "</div>",
      "<div><span class='coalition-seats'>",
      c$seats,
      " seats</span>",
      label,
      "</div></div>"
    )
  })

  date_heading <- if (!is.null(date_label)) {
    paste0("<h5>", date_label, "</h5>")
  } else {
    ""
  }

  ci_note <- if (show_ci) {
    "80% credible intervals are shown in brackets. Seat shares are median estimates and may not sum to 460."
  } else {
    "LOESS approximation based on selected agencies. Seat shares may not sum to 460."
  }

  list(
    grid_html = paste0(
      "<div>",
      date_heading,
      "<div class='popup-grid'>",
      paste(entries, collapse = ""),
      "</div>",
      "<p style='margin-bottom:0; margin-top:8px; color:#999; font-size:0.75em;'>",
      ci_note,
      "</p>",
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

  # --- Agency selector reactives ---
  # State: committed agencies (only updated on button click)
  committed_agencies <- reactiveVal(ALL_AGENCIES) # start in Bayesian mode

  observeEvent(input$agency_generate, {
    sel <- input$agency_selector
    if (is.null(sel) || length(sel) == 0) {
      return()
    } # need at least 1
    committed_agencies(sel)
    selected_constituency(NULL)
  })

  observeEvent(input$agency_reset, {
    committed_agencies(ALL_AGENCIES)
    updateCheckboxGroupInput(
      session,
      "agency_selector",
      selected = character(0)
    )
    selected_constituency(NULL)
  })

  selected_agencies <- reactive({
    committed_agencies()
  })

  using_bayesian <- reactive({
    setequal(committed_agencies(), ALL_AGENCIES)
  })

  # LOESS trend lines (replaces trend_lines when subset selected)
  active_trend_lines <- reactive({
    if (using_bayesian()) {
      return(trend_lines)
    }

    sel <- selected_agencies()
    sub_data <- point_dta %>% filter(org %in% sel)
    if (nrow(sub_data) < 5) {
      return(trend_lines)
    } # fallback if too little data

    # Prediction grid: same dates as trend_lines
    pred_dates <- sort(unique(trend_lines$date))
    pred_num <- as.numeric(pred_dates)

    parties <- levels(sub_data$party)
    if (is.null(parties)) {
      parties <- unique(as.character(sub_data$party))
    }

    # Helper: fill NAs by carrying forward then backward
    fill_na <- function(x) {
      for (i in seq_along(x)) {
        if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
      }
      for (i in rev(seq_along(x))) {
        if (i < length(x) && is.na(x[i])) x[i] <- x[i + 1]
      }
      x
    }

    results <- lapply(parties, function(p) {
      pd <- sub_data %>% filter(as.character(party) == p)
      if (nrow(pd) < 4) {
        return(NULL)
      }
      fit <- tryCatch(
        loess(est ~ as.numeric(midDate), data = pd, span = 0.3),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(NULL)
      }
      pred <- predict(fit, newdata = data.frame(midDate = pred_num))
      pred <- fill_na(pred)
      pred <- pmax(0, pmin(1, pred))
      data.frame(
        date = pred_dates,
        party = factor(p, levels = PARTY_ORDER),
        median_epred = pred,
        stringsAsFactors = FALSE
      )
    })
    bind_rows(results)
  })

  # LOESS weekly summaries (replaces weekly_summaries when subset selected)
  active_weekly_summaries <- reactive({
    if (using_bayesian()) {
      return(weekly_summaries)
    }

    sel <- selected_agencies()
    sub_data <- point_dta %>% filter(org %in% sel)
    if (nrow(sub_data) < 5) {
      return(weekly_summaries)
    }

    parties <- PARTY_ORDER
    target_num <- as.numeric(available_dates)

    # Helper: fill NAs by carrying forward then backward
    fill_na <- function(x) {
      for (i in seq_along(x)) {
        if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
      }
      for (i in rev(seq_along(x))) {
        if (i < length(x) && is.na(x[i])) x[i] <- x[i + 1]
      }
      x
    }

    # Fit LOESS per party and predict at available_dates
    party_preds <- lapply(parties, function(p) {
      pd <- sub_data %>% filter(as.character(party) == p)
      if (nrow(pd) < 4) {
        return(setNames(rep(0, length(available_dates)), available_dates))
      }
      fit <- tryCatch(
        loess(est ~ as.numeric(midDate), data = pd, span = 0.3),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(setNames(rep(0, length(available_dates)), available_dates))
      }
      pred <- predict(fit, newdata = data.frame(midDate = target_num))
      pred <- fill_na(pred)
      pred <- pmax(0, pmin(1, pred))
      setNames(pred, as.character(available_dates))
    })
    names(party_preds) <- parties

    # For each date: compute vote shares, allocate seats
    results <- lapply(available_dates, function(d) {
      d_chr <- as.character(d)
      pcts <- setNames(
        sapply(
          parties,
          function(p) unname(party_preds[[p]][d_chr]) * 100,
          USE.NAMES = FALSE
        ),
        parties
      )
      pcts[is.na(pcts)] <- 0

      # Build full vote shares vector for allocate_seats
      vote_shares <- setNames(rep(0, length(SIM_PARTIES)), SIM_PARTIES)
      for (p in parties) {
        vote_shares[p] <- pcts[p]
      }
      vote_shares["MN"] <- sim_defaults["MN"]
      vote_shares["Other"] <- max(
        0,
        100 - sum(vote_shares[SIM_PARTIES != "Other"])
      )

      seat_result <- allocate_seats(vote_shares)
      national <- seat_result %>%
        group_by(party) %>%
        summarise(seats = sum(seats), .groups = "drop")

      seat_counts <- sapply(
        parties,
        function(p) {
          val <- national$seats[national$party == p]
          if (length(val) == 0) 0L else val
        },
        USE.NAMES = FALSE
      )

      data.frame(
        party = factor(parties, levels = PARTY_ORDER),
        median_pct = round(unname(pcts[parties]), 1),
        lower_pct = NA_real_,
        upper_pct = NA_real_,
        median_seats = seat_counts,
        lower_seats = NA_real_,
        upper_seats = NA_real_,
        date = d,
        stringsAsFactors = FALSE
      )
    })
    bind_rows(results)
  })

  # LOESS constituency seats (replaces constituency_seats when subset selected)
  active_constituency_seats <- reactive({
    if (using_bayesian()) {
      return(constituency_seats)
    }

    sel <- selected_agencies()
    sub_data <- point_dta %>% filter(org %in% sel)
    if (nrow(sub_data) < 5) {
      return(constituency_seats)
    }

    parties <- PARTY_ORDER
    target_num <- as.numeric(available_dates)

    fill_na <- function(x) {
      for (i in seq_along(x)) {
        if (i > 1 && is.na(x[i])) x[i] <- x[i - 1]
      }
      for (i in rev(seq_along(x))) {
        if (i < length(x) && is.na(x[i])) x[i] <- x[i + 1]
      }
      x
    }

    # Pre-compute predictions for all dates per party
    party_pred_all <- lapply(parties, function(p) {
      pd <- sub_data %>% filter(as.character(party) == p)
      if (nrow(pd) < 4) {
        return(rep(0, length(target_num)))
      }
      fit <- tryCatch(
        loess(est ~ as.numeric(midDate), data = pd, span = 0.3),
        error = function(e) NULL
      )
      if (is.null(fit)) {
        return(rep(0, length(target_num)))
      }
      pred <- predict(fit, newdata = data.frame(midDate = target_num))
      pred <- fill_na(pred)
      pmax(0, pmin(1, pred)) * 100
    })
    names(party_pred_all) <- parties

    results <- lapply(seq_along(available_dates), function(di) {
      d <- available_dates[di]
      pcts <- setNames(
        sapply(parties, function(p) party_pred_all[[p]][di], USE.NAMES = FALSE),
        parties
      )

      vote_shares <- setNames(rep(0, length(SIM_PARTIES)), SIM_PARTIES)
      for (p in parties) {
        vote_shares[p] <- pcts[p]
      }
      vote_shares["MN"] <- sim_defaults["MN"]
      vote_shares["Other"] <- max(
        0,
        100 - sum(vote_shares[SIM_PARTIES != "Other"])
      )

      seat_result <- allocate_seats(vote_shares)
      seat_result$median_seats <- seat_result$seats
      seat_result$date <- d
      seat_result[, c("okreg", "party", "median_seats", "date")]
    })
    bind_rows(results)
  })

  # Description text (changes based on mode)
  output$trend_description <- renderUI({
    if (using_bayesian()) {
      tags$p(
        style = "color:#999; font-size:0.85em; max-width:810px; margin-bottom:8px;",
        "Click anywhere on the plot to see vote and seat estimates for the nearest week.",
        "Hover over any of the points to see particular polling house estimates.",
        "Select one or more polling agencies below and click 'Generate estimates' to see",
        "approximate trends based on those agencies only."
      )
    } else {
      tags$p(
        style = "color:#999; font-size:0.85em; max-width:810px; margin-bottom:8px;",
        "Showing LOESS approximation based on selected agencies.",
        "Click anywhere on the plot to see vote and seat estimates for the nearest week.",
        "Click 'Show all (Bayesian)' to return to the full model estimates."
      )
    }
  })

  # Approximation note
  output$agency_approx_note <- renderUI({
    if (using_bayesian()) {
      return(NULL)
    }
    n <- length(selected_agencies())
    div(
      class = "agency-approx-note",
      paste0(
        "Estimates based on a LOESS approximation using polls from ",
        n,
        " selected agenc",
        ifelse(n == 1, "y", "ies"),
        ". These are simplified trend estimates without the full ",
        "Bayesian model's credible intervals or house-effect corrections."
      )
    )
  })

  # Coalition state (shared between trend popup and simulator)
  coalition_defs <- reactiveVal(list()) # list of character vectors

  observeEvent(input$coalition_add_btn, {
    members <- input$coalition_picker
    if (is.null(members) || length(members) < 2) {
      return()
    }
    current <- coalition_defs()
    # Don't allow a party in multiple coalitions
    already_used <- unlist(current)
    if (any(members %in% already_used)) {
      return()
    }
    current[[length(current) + 1]] <- as.character(members)
    coalition_defs(current)
    # Reset checkbox selection
    updateCheckboxGroupInput(
      session,
      "coalition_picker",
      selected = character(0)
    )
    # Reset simulator state when coalitions change
    sim_has_run(FALSE)
    selected_constituency(NULL)
  })

  observeEvent(input$coalition_remove, {
    idx <- input$coalition_remove$index
    current <- coalition_defs()
    if (idx >= 1 && idx <= length(current)) {
      current[[idx]] <- NULL
      coalition_defs(current)
      sim_has_run(FALSE)
      selected_constituency(NULL)
    }
  })

  # Render current coalitions list in modal
  output$coalition_current_list <- renderUI({
    coals <- coalition_defs()
    if (length(coals) == 0) {
      return(div(
        style = "color:#999; font-size:0.85em; padding:8px 0;",
        "No coalitions defined."
      ))
    }
    short_names <- c(
      "KO" = "KO",
      "Polska 2050" = "P2050",
      "Lewica" = "Lewica",
      "PSL" = "PSL",
      "PiS" = "PiS",
      "Konfederacja" = "Konf.",
      "KKP" = "KKP",
      "Razem" = "Razem",
      "MN" = "MN"
    )
    tagList(lapply(seq_along(coals), function(ci) {
      members <- coals[[ci]]
      chips <- lapply(members, function(p) {
        col <- PARTY_COLORS[p]
        HTML(paste0(
          "<span class='coalition-chip'>",
          "<span class='color-dot' style='background:",
          col,
          ";'></span>",
          short_names[p],
          "</span>"
        ))
      })
      div(
        class = "coalition-group",
        div(class = "coalition-group-members", tagList(chips)),
        tags$button(
          class = "coalition-remove-btn",
          onclick = paste0("removeCoalition(", ci, ");"),
          HTML("&times;")
        )
      )
    }))
  })

  # Render party picker (exclude parties already in coalitions)
  output$coalition_party_picker <- renderUI({
    coals <- coalition_defs()
    used <- unlist(coals)
    pickable <- c(
      "PiS",
      "KO",
      "Polska 2050",
      "PSL",
      "Lewica",
      "Razem",
      "Konfederacja",
      "KKP"
    )
    available <- setdiff(pickable, used)
    if (length(available) < 2) {
      return(div(
        style = "color:#999; font-size:0.85em;",
        "All parties are already in coalitions."
      ))
    }
    checkboxGroupInput(
      "coalition_picker",
      label = NULL,
      choices = setNames(available, available),
      inline = TRUE
    )
  })

  observeEvent(input$plot_click, {
    clicked_date <- as.Date(input$plot_click$x, origin = "1970-01-01")
    idx <- which.min(abs(available_dates - clicked_date))
    selected_date(available_dates[idx])
    selected_constituency(NULL)
  })

  output$trend_plot <- renderPlot(
    {
      showtext_opts(dpi = 96)
      sel <- selected_agencies()
      tl <- active_trend_lines()

      # Split points into selected and deselected agencies
      pts_selected <- point_dta %>% filter(org %in% sel)
      pts_deselected <- point_dta %>% filter(!(org %in% sel))

      p <- ggplot()

      # Deselected agencies: grey, very faint
      if (nrow(pts_deselected) > 0) {
        p <- p +
          geom_point(
            data = pts_deselected,
            aes(x = midDate, y = est),
            colour = "grey80",
            size = 2,
            alpha = 0.15,
            show.legend = FALSE
          )
      }

      # Selected agencies: coloured
      if (nrow(pts_selected) > 0) {
        p <- p +
          geom_point(
            data = pts_selected,
            aes(x = midDate, y = est, colour = party),
            size = 2,
            alpha = 0.3,
            show.legend = FALSE
          )
      }

      p <- p +
        geom_line(
          data = tl,
          aes(x = date, y = median_epred, colour = party),
          linewidth = 1
        ) +
        scale_color_manual(values = PARTY_COLORS) +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        scale_x_date(date_breaks = "1 month", labels = my_date_format()) +
        coord_cartesian(ylim = c(0, NA)) +
        labs(y = "", x = "", color = "") +
        theme_plots()

      p
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
    popup_html <- build_popup_html(
      selected_date(),
      date_label = format(selected_date(), "%e %B %Y"),
      weekly_data = active_weekly_summaries(),
      show_ci = using_bayesian()
    )
    if (is.null(popup_html)) {
      return(NULL)
    }

    div(
      class = "popup-layout",
      HTML(popup_html$grid_html),
      HTML(popup_html$coalition_html),
      div(
        class = "popup-map",
        tags$h5("Constituency seat shares"),
        div(
          style = "position: relative;",
          plotOutput(
            "seat_map",
            click = "map_click",
            width = "250px",
            height = "280px"
          ),
          uiOutput("map_popup")
        ),
        tags$p(
          style = "margin:4px 0 0 0; color:#999; font-size:0.75em; max-width:250px;",
          "Click on the map for seat shares in specific constituencies."
        )
      )
    )
  })

  # --- Map ---
  trend_const_seats <- reactive({
    active_constituency_seats() %>% filter(date == selected_date())
  })

  map_data <- reactive({
    cs <- trend_const_seats()
    if (is.null(cs) || nrow(cs) == 0) {
      return(NULL)
    }

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
    md$fill_color <- mapply(
      function(party, dom) {
        if (is.na(party)) {
          return("grey80")
        }
        col <- PARTY_COLORS[party]
        if (is.na(col)) {
          col <- "gray50"
        }
        base <- col2rgb(col) / 255
        blended <- base + (1 - base) * (1 - dom)
        rgb(blended[1], blended[2], blended[3])
      },
      md$winning_party,
      md$dominance
    )

    md
  })

  output$seat_map <- renderPlot(
    {
      showtext_opts(dpi = 96)
      md <- map_data()
      if (is.null(md)) {
        return(NULL)
      }

      fill_vals <- setNames(md$fill_color, md$id)
      ggplot(md) +
        geom_sf(
          aes(fill = as.character(id)),
          color = "white",
          linewidth = 0.2
        ) +
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
    if (is.null(md)) {
      return()
    }

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
    if (is.null(const_id)) {
      return(NULL)
    }

    cs <- trend_const_seats()
    if (is.null(cs)) {
      return(NULL)
    }

    cs <- cs %>%
      filter(okreg == const_id, median_seats > 0) %>%
      arrange(desc(median_seats))

    if (nrow(cs) == 0) {
      return(NULL)
    }

    # Get constituency name
    const_name <- const_map$cst_n[const_map$id == const_id]
    if (length(const_name) == 0) {
      const_name <- paste("Constituency", const_id)
    }

    entries <- cs %>%
      rowwise() %>%
      mutate(
        color_hex = {
          col <- PARTY_COLORS[party]
          if (is.na(col)) "gray50" else col
        },
        html = paste0(
          "<div class='map-popup-entry'>",
          "<span class='map-popup-party'><span class='color-dot' style='background:",
          color_hex,
          ";'></span>",
          party,
          "</span>",
          "<span class='map-popup-seats'>",
          median_seats,
          "</span>",
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
        "<b>",
        const_name,
        "</b><br>",
        paste(entries, collapse = "")
      ))
    )
  })

  # --- Simulator ---
  # Store the proportional split of party shares within each coalition
  # (set when coalitions are created, used to distribute coalition totals)
  coal_member_ratios <- reactiveVal(list())

  # When coalition_defs change, compute initial values & ratios
  observeEvent(
    coalition_defs(),
    {
      coals <- coalition_defs()
      if (length(coals) == 0) {
        coal_member_ratios(list())
        return()
      }
      # Compute ratios from sim_defaults (latest estimates)
      ratios <- lapply(coals, function(members) {
        shares <- sim_defaults[members]
        total <- sum(shares, na.rm = TRUE)
        if (total == 0) {
          setNames(rep(1 / length(members), length(members)), members)
        } else {
          setNames(shares / total, members)
        }
      })
      coal_member_ratios(ratios)
    },
    ignoreNULL = FALSE
  )

  output$sim_description <- renderUI({
    coals <- coalition_defs()
    if (length(coals) > 0) {
      tags$p(
        style = "color:#999; font-size:0.85em; margin-bottom:16px;",
        "Adjust vote shares for coalitions and individual parties, then calculate seats.",
        "Coalition vote shares are distributed across constituencies using weighted",
        "regional coefficients. Vote shares must sum to 100%."
      )
    } else {
      tags$p(
        style = "color:#999; font-size:0.85em; margin-bottom:16px;",
        "Adjust vote shares to see how seat allocation would change.",
        "Use 'Build coalition' to group parties into electoral coalitions.",
        "Vote shares must sum to 100% before running the simulation."
      )
    }
  })

  output$sim_inputs_ui <- renderUI({
    coals <- coalition_defs()
    coal_members_all <- unlist(coals)

    short_names <- c(
      "KO" = "KO",
      "Polska 2050" = "P2050",
      "Lewica" = "Lewica",
      "PSL" = "PSL",
      "PiS" = "PiS",
      "Konfederacja" = "Konf.",
      "KKP" = "KKP",
      "Razem" = "Razem",
      "MN" = "MN"
    )

    rows <- list()

    if (length(coals) > 0) {
      # Coalition rows
      for (ci in seq_along(coals)) {
        members <- coals[[ci]]
        coal_name <- paste(short_names[members], collapse = " + ")
        col <- coalition_color(members, sim_defaults[members])
        coal_total <- round(sum(sim_defaults[members], na.rm = TRUE), 1)
        input_id <- paste0("sim_coal_", ci)
        rows[[length(rows) + 1]] <- div(
          class = "sim-input-row",
          div(
            class = "sim-input-label",
            style = "min-width:140px;",
            HTML(paste0(
              "<span class='color-dot' style='background:",
              col,
              ";'></span>",
              coal_name,
              " <span style='font-size:0.7em;color:#999;'>(coalition)</span>"
            ))
          ),
          div(
            class = "sim-input-box",
            numericInput(
              inputId = input_id,
              label = NULL,
              value = coal_total,
              min = 0,
              max = 100,
              step = 0.1
            )
          ),
          span(class = "sim-input-pct", "%")
        )
      }

      # Solo party rows (not in any coalition)
      solo_parties <- setdiff(SIM_PARTIES, coal_members_all)
      for (p in solo_parties) {
        col <- PARTY_COLORS[p]
        rows[[length(rows) + 1]] <- div(
          class = "sim-input-row",
          div(
            class = "sim-input-label",
            style = "min-width:140px;",
            HTML(paste0(
              "<span class='color-dot' style='background:",
              col,
              ";'></span>",
              p
            ))
          ),
          div(
            class = "sim-input-box",
            numericInput(
              inputId = paste0("sim_", gsub(" ", "_", p)),
              label = NULL,
              value = round(sim_defaults[p], 1),
              min = 0,
              max = 100,
              step = 0.1
            )
          ),
          span(class = "sim-input-pct", "%")
        )
      }
    } else {
      # No coalitions — show all individual party inputs
      for (p in SIM_PARTIES) {
        col <- PARTY_COLORS[p]
        rows[[length(rows) + 1]] <- div(
          class = "sim-input-row",
          div(
            class = "sim-input-label",
            HTML(paste0(
              "<span class='color-dot' style='background:",
              col,
              ";'></span>",
              p
            ))
          ),
          div(
            class = "sim-input-box",
            numericInput(
              inputId = paste0("sim_", gsub(" ", "_", p)),
              label = NULL,
              value = round(sim_defaults[p], 1),
              min = 0,
              max = 100,
              step = 0.1
            )
          ),
          span(class = "sim-input-pct", "%")
        )
      }
    }

    tagList(rows)
  })

  # Read input values — handles both coalition and non-coalition modes
  # Returns a named vector of per-PARTY vote shares (distributing coalition totals)
  sim_slider_values <- reactive({
    coals <- coalition_defs()
    coal_members_all <- unlist(coals)

    if (length(coals) > 0) {
      vals <- setNames(rep(0, length(SIM_PARTIES)), SIM_PARTIES)
      ratios <- coal_member_ratios()

      # Coalition inputs
      for (ci in seq_along(coals)) {
        members <- coals[[ci]]
        coal_val <- input[[paste0("sim_coal_", ci)]]
        if (is.null(coal_val)) {
          return(NULL)
        }
        # Distribute proportionally
        r <- ratios[[ci]]
        for (m in members) {
          vals[m] <- coal_val * r[m]
        }
      }

      # Solo party inputs
      solo_parties <- setdiff(SIM_PARTIES, coal_members_all)
      for (p in solo_parties) {
        v <- input[[paste0("sim_", gsub(" ", "_", p))]]
        if (is.null(v)) {
          return(NULL)
        }
        vals[p] <- v
      }

      vals
    } else {
      vals <- sapply(SIM_PARTIES, function(p) {
        input[[paste0("sim_", gsub(" ", "_", p))]]
      })
      setNames(vals, SIM_PARTIES)
    }
  })

  # Also compute input total for display (reads coalition or party inputs directly)
  sim_input_total <- reactive({
    coals <- coalition_defs()
    coal_members_all <- unlist(coals)
    total <- 0

    if (length(coals) > 0) {
      for (ci in seq_along(coals)) {
        v <- input[[paste0("sim_coal_", ci)]]
        if (is.null(v)) {
          return(NULL)
        }
        total <- total + v
      }
      solo_parties <- setdiff(SIM_PARTIES, coal_members_all)
      for (p in solo_parties) {
        v <- input[[paste0("sim_", gsub(" ", "_", p))]]
        if (is.null(v)) {
          return(NULL)
        }
        total <- total + v
      }
    } else {
      for (p in SIM_PARTIES) {
        v <- input[[paste0("sim_", gsub(" ", "_", p))]]
        if (is.null(v)) {
          return(NULL)
        }
        total <- total + v
      }
    }
    total
  })

  output$sim_total_display <- renderUI({
    total <- sim_input_total()
    if (is.null(total)) {
      return(NULL)
    }
    css_class <- if (abs(total - 100) < 0.15) {
      "sim-total sim-total-ok"
    } else {
      "sim-total sim-total-bad"
    }
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
    if (is.null(vals) || any(sapply(vals, is.null))) {
      return()
    }
    total <- sum(unlist(vals))
    if (abs(total - 100) >= 0.15) {
      return()
    }

    vote_shares <- setNames(as.numeric(vals), SIM_PARTIES)
    coals <- coalition_defs()
    result <- allocate_seats_with_coalitions(vote_shares, coals)
    sim_seat_data(result)
    sim_has_run(TRUE)
    sim_selected_const(NULL)
  })

  sim_map_data <- reactive({
    result <- sim_seat_data()
    if (is.null(result) || !("okreg" %in% names(result))) {
      return(NULL)
    }

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

    # Build color lookup including coalition colors
    coals <- coalition_defs()
    sim_colors <- PARTY_COLORS
    if (length(coals) > 0) {
      vals <- sim_slider_values()
      if (!is.null(vals)) {
        short_names <- c(
          "KO" = "KO",
          "Polska 2050" = "P2050",
          "Lewica" = "Lewica",
          "PSL" = "PSL",
          "PiS" = "PiS",
          "Konfederacja" = "Konf.",
          "KKP" = "KKP",
          "Razem" = "Razem",
          "MN" = "MN"
        )
        for (coal in coals) {
          coal_name <- paste(short_names[coal], collapse = " + ")
          vote_pcts <- setNames(
            as.numeric(vals[coal]),
            coal
          )
          sim_colors[coal_name] <- coalition_color(coal, vote_pcts)
        }
      }
    }

    md$fill_color <- mapply(
      function(party, dom) {
        if (is.na(party)) {
          return("grey80")
        }
        col <- sim_colors[party]
        if (is.na(col)) {
          col <- "gray50"
        }
        base <- col2rgb(col) / 255
        blended <- base + (1 - base) * (1 - dom)
        rgb(blended[1], blended[2], blended[3])
      },
      md$winning_party,
      md$dominance
    )

    md
  })

  sim_national <- reactive({
    result <- sim_seat_data()
    if (is.null(result)) {
      return(NULL)
    }
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
    if (is.null(national)) {
      return(NULL)
    }

    coals <- coalition_defs()
    sim_colors <- PARTY_COLORS
    if (length(coals) > 0) {
      vals <- sim_slider_values()
      if (!is.null(vals)) {
        short_names <- c(
          "KO" = "KO",
          "Polska 2050" = "P2050",
          "Lewica" = "Lewica",
          "PSL" = "PSL",
          "PiS" = "PiS",
          "Konfederacja" = "Konf.",
          "KKP" = "KKP",
          "Razem" = "Razem",
          "MN" = "MN"
        )
        for (coal in coals) {
          coal_name <- paste(short_names[coal], collapse = " + ")
          vote_pcts <- setNames(as.numeric(vals[coal]), coal)
          sim_colors[coal_name] <- coalition_color(coal, vote_pcts)
        }
      }
    }

    seat_entries <- lapply(seq_len(nrow(national)), function(i) {
      p <- national$party[i]
      col <- sim_colors[p]
      if (is.na(col)) {
        col <- "gray50"
      }
      HTML(paste0(
        "<div class='sim-seat-entry'>",
        "<span class='sim-seat-party'><span class='color-dot' style='background:",
        col,
        ";'></span>",
        p,
        "</span>",
        "<span class='sim-seat-count'>",
        national$seats[i],
        "</span>",
        "</div>"
      ))
    })

    div(
      tags$h5("Seats"),
      div(
        style = "display:flex; gap:8px; margin-bottom:8px; align-items:center;",
        actionButton("sim_run", "Calculate seats"),
        tags$button(
          class = "coalition-btn",
          onclick = "openCoalitionModal();",
          "Build coalition"
        )
      ),
      tagList(seat_entries)
    )
  })

  output$sim_coalitions_ui <- renderUI({
    national <- sim_national()
    if (is.null(national)) {
      return(NULL)
    }

    coalition_content <- NULL
    if (sim_has_run()) {
      # build_coalitions works with the party names in national (which may include
      # coalition names like "KO + P2050"). We need to adapt it.
      coals <- coalition_defs()
      if (length(coals) > 0) {
        # Use entity names from national directly
        get_s <- function(p) {
          val <- national$seats[national$party == p]
          if (length(val) == 0) 0L else val
        }
        # Build governing majorities from the entities in national
        entity_names <- national$party[national$seats > 0]
        gov_combos <- list()

        for (size in 1:length(entity_names)) {
          combos <- combn(entity_names, size, simplify = FALSE)
          for (combo in combos) {
            total <- sum(sapply(combo, get_s))
            if (total >= 231) {
              gov_combos[[length(gov_combos) + 1]] <- list(
                name = paste(combo, collapse = " + "),
                seats = total,
                parties = combo
              )
            }
          }
        }
        # Remove supersets of single-entity majorities
        single_maj <- entity_names[sapply(entity_names, function(p) {
          get_s(p) >= 231
        })]
        gov_combos <- Filter(
          function(x) {
            if (length(x$parties) == 1) {
              return(TRUE)
            }
            !any(single_maj %in% x$parties)
          },
          gov_combos
        )

        gov_combos <- gov_combos[order(
          -sapply(gov_combos, function(x) x$seats)
        )]
        coalition_content <- lapply(gov_combos, function(co) {
          label <- if (co$seats >= 307) {
            "<span class='coalition-majority' style='color:green;'>constitutional majority</span>"
          } else if (co$seats >= 276) {
            "<span class='coalition-majority' style='color:green;'>veto override</span>"
          } else {
            ""
          }
          HTML(paste0(
            "<div class='coalition-entry'>",
            "<div class='coalition-name'>",
            co$name,
            "</div>",
            "<div><span class='coalition-seats'>",
            co$seats,
            " seats</span>",
            label,
            "</div></div>"
          ))
        })
      } else {
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
            "<div class='coalition-name'>",
            co$name,
            "</div>",
            "<div><span class='coalition-seats'>",
            co$seats,
            " seats</span>",
            label,
            "</div></div>"
          ))
        })
      }
    }

    div(
      class = "popup-coalitions",
      tags$h5("Governing majorities"),
      tagList(coalition_content)
    )
  })

  output$sim_map_ui <- renderUI({
    sim_seat_data() # trigger reactivity
    div(
      class = "popup-map",
      tags$h5("Constituency seat shares"),
      div(
        style = "position: relative;",
        plotOutput(
          "sim_map",
          click = "sim_map_click",
          width = "250px",
          height = "280px"
        ),
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
      if (is.null(md)) {
        return(NULL)
      }

      fill_vals <- setNames(md$fill_color, md$id)
      ggplot(md) +
        geom_sf(
          aes(fill = as.character(id)),
          color = "white",
          linewidth = 0.2
        ) +
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
    if (is.null(md)) {
      return()
    }

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
    if (is.null(const_id)) {
      return(NULL)
    }

    result <- sim_seat_data()
    if (is.null(result)) {
      return(NULL)
    }

    cs <- result %>%
      filter(okreg == const_id, seats > 0) %>%
      arrange(desc(seats))

    if (nrow(cs) == 0) {
      return(NULL)
    }

    const_name <- const_map$cst_n[const_map$id == const_id]
    if (length(const_name) == 0) {
      const_name <- paste("Constituency", const_id)
    }

    # Build color lookup including coalition colors
    coals <- coalition_defs()
    sim_colors <- PARTY_COLORS
    if (length(coals) > 0) {
      vals <- sim_slider_values()
      if (!is.null(vals)) {
        short_names <- c(
          "KO" = "KO",
          "Polska 2050" = "P2050",
          "Lewica" = "Lewica",
          "PSL" = "PSL",
          "PiS" = "PiS",
          "Konfederacja" = "Konf.",
          "KKP" = "KKP",
          "Razem" = "Razem",
          "MN" = "MN"
        )
        for (coal in coals) {
          coal_name <- paste(short_names[coal], collapse = " + ")
          vote_pcts <- setNames(as.numeric(vals[coal]), coal)
          sim_colors[coal_name] <- coalition_color(coal, vote_pcts)
        }
      }
    }

    entries <- cs %>%
      rowwise() %>%
      mutate(
        color_hex = {
          col <- sim_colors[party]
          if (is.na(col)) "gray50" else col
        },
        html = paste0(
          "<div class='map-popup-entry'>",
          "<span class='map-popup-party'><span class='color-dot' style='background:",
          color_hex,
          ";'></span>",
          party,
          "</span>",
          "<span class='map-popup-seats'>",
          seats,
          "</span>",
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
        "<b>",
        const_name,
        "</b><br>",
        paste(entries, collapse = "")
      ))
    )
  })

  # Weighting explanation shown below the simulator when coalitions are defined
  output$sim_weighting_note <- renderUI({
    coals <- coalition_defs()
    if (length(coals) == 0) {
      return(NULL)
    }

    short_names <- c(
      "KO" = "KO",
      "Polska 2050" = "P2050",
      "Lewica" = "Lewica",
      "PSL" = "PSL",
      "PiS" = "PiS",
      "Konfederacja" = "Konf.",
      "KKP" = "KKP",
      "Razem" = "Razem",
      "MN" = "MN"
    )

    coef_labels <- c(
      "PiS" = "PiS coefficient",
      "KO" = "KO coefficient",
      "Lewica" = "Lewica coefficient",
      "Razem" = "Lewica coefficient",
      "Konfederacja" = "Konfederacja coefficient",
      "KKP" = "Konfederacja coefficient",
      "Polska 2050" = "TD coefficient",
      "PSL" = "TD coefficient"
    )

    ratios <- coal_member_ratios()
    explanations <- lapply(seq_along(coals), function(ci) {
      members <- coals[[ci]]
      coal_name <- paste(short_names[members], collapse = " + ")
      r <- ratios[[ci]]
      member_desc <- paste(
        sapply(members, function(m) {
          pct <- round(r[m] * 100, 0)
          paste0(
            short_names[m],
            " (",
            pct,
            "% weight, using ",
            coef_labels[m],
            ")"
          )
        }),
        collapse = ", "
      )

      HTML(paste0(
        "<p style='margin:4px 0;'><b>",
        coal_name,
        ":</b> ",
        "Constituency vote shares are calculated using a weighted average of ",
        "regional coefficients based on each member party's share of the coalition total. ",
        "Members: ",
        member_desc,
        ".",
        " An 8% national threshold applies to this coalition.</p>"
      ))
    })

    div(
      style = "margin-top:16px; padding:12px 16px; background:#f8f9fa; border-radius:6px; font-size:0.85em; color:#555; max-width:810px;",
      tags$h6(style = "margin:0 0 8px 0; color:#333;", "Coalition weighting"),
      tagList(explanations),
      tags$p(
        style = "margin:8px 0 0 0; color:#888; font-size:0.9em;",
        "Each party has a regional coefficient derived from the 2023 election results that adjusts ",
        "its vote share up or down in each constituency. For coalitions, these coefficients are ",
        "combined using a weighted average based on each member's share of the coalition's total vote."
      )
    )
  })
}

shinyApp(ui, server)
