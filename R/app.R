library(shiny)
library(dplyr)
library(ggplot2)
library(showtext)
library(sf)
library(here)
# curl is required by font_add_google() to download the font. It is declared
# explicitly so the deploy bundles it (it used to arrive transitively via
# tidyverse). Without it, font_add_google() errors and the app fails to start.
library(curl)

# Load Jost from Google Fonts. Wrapped in tryCatch so that a missing network
# or download failure on the deploy host falls back to the default font
# instead of crashing the app at startup. PLOT_FONT is used as the plot
# font family everywhere, so it degrades to "sans" if Jost is unavailable.
PLOT_FONT <- tryCatch(
  {
    font_add_google("Jost", "Jost")
    font_add_google("Jost", "Jost Medium", regular.wt = 500)
    "Jost"
  },
  error = function(e) {
    message(
      "Could not load Jost from Google Fonts; using default font: ",
      conditionMessage(e)
    )
    "sans"
  }
)
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

# Ideological left -> right ordering for the hemicycle (left side to right side)
HEMICYCLE_ORDER <- c(
  "Razem",
  "Lewica",
  "KO",
  "Polska 2050",
  "PSL",
  "MN",
  "PiS",
  "Konfederacja",
  "KKP"
)

theme_plots <- function(base_size = 16, base_family = PLOT_FONT) {
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
      axis.text = element_text(size = rel(1)),
      axis.title.x = element_text(size = rel(1), margin = margin(t = 10)),
      axis.title.y = element_text(
        size = rel(1),
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
trend_lines <- readRDS(here("data", "trend_lines.rds"))
date_summaries <- readRDS(here("data", "date_summaries.rds")) %>%
  mutate(party = factor(party, levels = PARTY_ORDER)) %>%
  filter(!is.na(party))
weekly_summaries <- readRDS(here("data", "weekly_summaries.rds")) %>%
  mutate(party = factor(party, levels = c(PARTY_ORDER, "MN"))) %>%
  filter(!is.na(party))
point_dta <- readRDS(here("data", "point_dta.rds")) %>%
  mutate(midDate_num = as.numeric(midDate))
constituency_seats <- readRDS(here("data", "constituency_seats.rds"))
const_map <- readRDS(here("data", "const_map_cartogram.rds"))

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
weights <- readRDS(here("data", "sim_weights.rds"))

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
    name <- paste(short_names[coal], collapse = "-")
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

# --- Ideological "punishment" model (Realistic seat projection) ---
# Each party sits on a 0-100 left -> right ideology scale. When parties form an
# electoral coalition with ideologically distant partners, some of their own
# voters abstain ("punishment"). The loss scales with the squared distance to
# the FURTHEST coalition partner, so extreme pairings (e.g. Razem + KKP) are
# penalised heavily while adjacent ones (e.g. PSL + Polska 2050) barely at all.
IDEOLOGY_POSITIONS <- c(
  "Razem" = 0,
  "Lewica" = 14,
  "KO" = 32,
  "Polska 2050" = 44,
  "PSL" = 52,
  "PiS" = 66,
  "Konfederacja" = 84,
  "KKP" = 100
)
# Fraction of a party's own vote lost at maximum ideological distance (dist=100)
# and maximum coalitional-dynamics strength (slider = 1): three quarters.
PUNISH_MAX_LOSS <- 0.75

# Given national vote shares (named %, incl. MN/Other), the coalition
# definitions, and a strength in [0, 1] (the "Coalitional dynamics" slider),
# return adjusted shares where coalition members lose a share of their own vote
# to abstention (added to "Other"). Solo parties are unchanged.
# Loss fraction = strength * PUNISH_MAX_LOSS * (distance_to_furthest / 100)^2.
# strength = 0 reproduces the unpunished ("naive") shares exactly.
punish_vote_shares <- function(vote_shares_pct, coalitions, strength = 1) {
  if (length(coalitions) == 0 || strength <= 0) {
    return(vote_shares_pct)
  }
  adjusted <- vote_shares_pct
  total_lost <- 0
  for (coal in coalitions) {
    # Parties with a defined ideological position (MN has none -> not punished)
    positioned <- coal[coal %in% names(IDEOLOGY_POSITIONS)]
    if (length(positioned) < 2) {
      next
    }
    pos <- IDEOLOGY_POSITIONS[positioned]
    for (p in positioned) {
      furthest <- max(abs(pos - pos[p]))
      loss_frac <- strength * PUNISH_MAX_LOSS * (furthest / 100)^2
      orig <- adjusted[p]
      if (is.na(orig) || orig <= 0) {
        next
      }
      lost <- orig * loss_frac
      adjusted[p] <- orig - lost
      total_lost <- total_lost + lost
    }
  }
  if ("Other" %in% names(adjusted)) {
    adjusted["Other"] <- adjusted["Other"] + total_lost
  }
  adjusted
}

# Incompatible coalition partners (cannot sit in the same government).
FORBIDDEN_PAIRS <- list(
  c("Konfederacja", "Lewica"),
  c("Konfederacja", "Razem"),
  c("KKP", "Lewica"),
  c("KKP", "Razem"),
  c("KKP", "KO"),
  c("PiS", "KO"),
  c("PiS", "Lewica")
)

# Are a set of parties mutually compatible (no forbidden pair present)?
parties_compatible <- function(parties) {
  for (fp in FORBIDDEN_PAIRS) {
    if (all(fp %in% parties)) {
      return(FALSE)
    }
  }
  TRUE
}

# Vote-weighted mean ideological position of a set of parties.
ideology_of <- function(parties, vals) {
  pos <- IDEOLOGY_POSITIONS[parties]
  pos <- pos[!is.na(pos)]
  if (length(pos) == 0) {
    return(NA_real_)
  }
  w <- vals[names(pos)]
  w[is.na(w) | w < 0] <- 0
  if (sum(w) == 0) {
    return(mean(pos))
  }
  sum(pos * w) / sum(w)
}

# Compute hemicycle seat coordinates for a vector of seats (one element per
# seat, in left -> right order). Returns a data frame with x, y per seat.
# Seats are arranged in concentric rows of a half-annulus.
hemicycle_layout <- function(n_seats, n_rows = 11) {
  if (n_seats <= 0) {
    return(data.frame(x = numeric(0), y = numeric(0), seat = integer(0)))
  }
  # Distribute seats across rows proportionally to each row's radius (outer
  # rows are longer, so they hold more seats).
  r_inner <- 1
  r_outer <- 2
  radii <- seq(r_inner, r_outer, length.out = n_rows)
  row_weights <- radii
  row_counts <- floor(n_seats * row_weights / sum(row_weights))
  # Distribute any remainder to the outer rows
  rem <- n_seats - sum(row_counts)
  if (rem > 0) {
    add_order <- order(-radii)
    for (k in seq_len(rem)) {
      row_counts[add_order[((k - 1) %% n_rows) + 1]] <-
        row_counts[add_order[((k - 1) %% n_rows) + 1]] + 1
    }
  }

  coords <- list()
  for (ri in seq_len(n_rows)) {
    nc <- row_counts[ri]
    if (nc == 0) {
      next
    }
    r <- radii[ri]
    # Angles from pi (left) to 0 (right)
    if (nc == 1) {
      angs <- pi / 2
    } else {
      angs <- seq(pi, 0, length.out = nc)
    }
    coords[[ri]] <- data.frame(
      x = r * cos(angs),
      y = r * sin(angs),
      ang = angs,
      r = r
    )
  }
  pts <- do.call(rbind, coords)
  # Order seats left -> right: by decreasing angle (pi = left, 0 = right),
  # then by radius so adjacent seats cluster.
  pts <- pts[order(-pts$ang, pts$r), ]
  pts$seat <- seq_len(nrow(pts))
  rownames(pts) <- NULL
  pts
}

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

    /* --- Estimates + coalition builder layout --- */
    .estimates-layout {
      display: flex;
      gap: 32px;
      align-items: flex-start;
      flex-wrap: wrap;
    }
    .estimates-block { flex: 3 1 480px; }
    .estimates-coalitions {
      flex: 1 1 200px;
      border-left: 1px solid #ddd;
      padding-left: 24px;
    }
    .est-analysis {
      margin-top: 14px;
      max-width: 640px;
    }
    .est-analysis .coalition-entry {
      padding: 4px 0;
      border-bottom: 1px solid #eee;
      font-size: 0.92em;
    }
    .est-analysis .coalition-entry:last-child { border-bottom: none; }
    .est-analysis .coalition-name { color: #333; }
    .est-analysis .coalition-seats { font-weight: bold; }
    /* Coalitional dynamics vertical slider */
    .coalition-dynamics { margin-top: 18px; }
    .coalition-dynamics-title {
      font-size: 1.3rem;
      font-weight: bold;
      margin-bottom: 4px;
    }
    .coalition-dynamics-help {
      color: #999;
      font-size: 0.78em;
      margin-bottom: 8px;
    }
    /* Native vertical range input (0 at the bottom, 100 at the top).
       Native inputs track the trackpad smoothly, unlike a rotated slider. */
    .coalition-dynamics-slider {
      display: flex;
      align-items: center;
      justify-content: flex-start;
      gap: 8px;
      margin-top: 8px;
      margin-left: 8px;
      width: fit-content;
    }
    .vertical-range {
      writing-mode: vertical-lr;
      direction: rtl;            /* low at bottom, high at top */
      -webkit-appearance: slider-vertical;
      appearance: slider-vertical;
      flex: 0 0 24px;
      width: 24px;
      height: 200px;
      margin: 0;
      cursor: pointer;
      accent-color: #333;
    }
    .vertical-range-value {
      font-size: 0.95em;
      font-weight: normal;
      color: #333;
      flex: 0 0 auto;
    }
    /* Columns of editable party estimates */
    .est-columns {
      display: flex;
      gap: 24px;
      flex-wrap: wrap;
      align-items: flex-start;
    }
    .est-col {
      display: flex;
      flex-direction: column;
      gap: 4px;
    }
    .est-coal-col, .est-solo-col {
      border: 1px solid #eee;
      border-radius: 6px;
      padding: 8px 10px;
      background: #fafafa;
    }
    .est-coal-header {
      display: flex;
      align-items: center;
      font-weight: bold;
      font-size: 0.9em;
      margin-bottom: 4px;
    }
    .est-solo-header { color: #777; }
    .est-coal-total {
      display: flex;
      align-items: baseline;
      gap: 8px;
      margin-top: 6px;
      padding-top: 6px;
      border-top: 1px solid #ddd;
      font-size: 0.9em;
    }
    .est-coal-total-label { font-weight: bold; min-width: 70px; }
    .est-coal-total-vote { color: #888; }
    .est-coal-total-seats { font-weight: bold; }
    .est-entry {
      display: flex;
      align-items: center;
      gap: 8px;
      padding: 2px 0;
    }
    .est-party {
      display: flex;
      align-items: center;
      min-width: 96px;
      white-space: nowrap;
      font-size: 0.9em;
    }
    .est-vote-box { width: 64px; }
    .est-vote-box .form-group { margin-bottom: 0; }
    .est-vote-box input[type=number] {
      font-size: 0.82em;
      padding: 3px 5px;
    }
    .est-pct { font-size: 0.85em; color: #888; }
    .est-seats {
      font-weight: bold;
      min-width: 34px;
      text-align: right;
      font-size: 0.9em;
    }
    .est-seats-label { font-size: 0.78em; color: #999; }

    /* --- Seat projection layout --- */
    .projection-layout {
      display: flex;
      gap: 24px;
      align-items: flex-start;
      flex-wrap: wrap;
    }
    .hemicycle-wrapper {
      flex: 1 1 420px;
      max-width: 520px;
    }
    /* Label sits below the hemicycle, clear of the arch */
    .hemicycle-label-wrap {
      display: flex;
      justify-content: center;
      margin-top: 8px;
    }
    .hemicycle-label {
      text-align: center;
      font-size: 0.95em;
      padding: 2px 8px;
    }
    .hemicycle-label-title {
      font-size: 0.78em;
      color: #999;
      text-transform: uppercase;
      letter-spacing: 0.04em;
      margin-bottom: 2px;
    }
    .hemicycle-label-none { color: #999; padding-top: 4px; }
    .hemicycle-win-line { padding: 1px 0; }
    .hemicycle-win-name { font-weight: bold; }
    .hemicycle-win-seats { color: #333; }
    .hemicycle-win-extra { color: green; font-size: 0.85em; }

    @media (max-width: 768px) {
      .estimates-coalitions {
        border-left: none;
        border-top: 1px solid #ddd;
        padding-left: 0;
        padding-top: 16px;
      }
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
      display: inline-block;
      font-size: 0.95em;
      margin: 8px 0;
      padding: 6px 10px;
      border-radius: 4px;
    }
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

    // Coalitional dynamics: update the value label as the range moves
    $(document).on('input change', '#coalition_dynamics', function() {
      $('.vertical-range-value').text(this.value + '%');
    });
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
      )
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

    # --- Editable estimates + coalition buttons (side by side) ---
    div(
      class = "estimates-layout",
      style = "margin-top: 20px;",
      div(
        class = "estimates-block",
        uiOutput("estimates_heading"),
        tags$p(
          style = "color:#999; font-size:0.75em; margin:6px 0 12px 0;",
          "Edit any vote share to override the polling-derived estimate.",
          "Seats recompute automatically. Click the trend plot to reset to a date."
        ),
        uiOutput("est_inputs_ui"),
        uiOutput("est_total_display"),
        uiOutput("est_analysis")
      ),
      div(
        class = "estimates-coalitions",
        tags$div(class = "coalition-dynamics-title", "Coalition formation"),
        div(
          style = "display:flex; gap:8px; flex-wrap:wrap; margin-top:4px;",
          tags$button(
            class = "coalition-btn",
            onclick = "openCoalitionModal();",
            "Build coalition"
          ),
          actionButton(
            "coalition_reset_btn",
            "Reset",
            class = "coalition-btn"
          )
        ),
        div(
          class = "coalition-dynamics",
          tags$div(class = "coalition-dynamics-title", "Coalition dynamics"),
          tags$p(
            class = "coalition-dynamics-help",
            "How strongly voters punish their party for allying with",
            "ideologically distant partners. At 0% there is no effect. At 100%,",
            "parties furthest apart ideologically lose three quarters of their",
            "electorate; parties closer together are punished too, but less so."
          ),
          div(
            class = "coalition-dynamics-slider",
            tags$input(
              id = "coalition_dynamics",
              type = "range",
              min = "0",
              max = "100",
              value = "0",
              step = "1",
              class = "vertical-range",
              oninput = "Shiny.setInputValue('coalition_dynamics', parseInt(this.value));",
              onchange = "Shiny.setInputValue('coalition_dynamics', parseInt(this.value));"
            ),
            tags$div(class = "vertical-range-value", "0%")
          )
        )
      )
    ),

    # --- Seat projection (hemicycle + constituency map), slider-driven ---
    div(
      style = "margin-top: 24px;",
      div(
        class = "projection-layout",
        div(
          class = "hemicycle-wrapper",
          div(
            class = "hemicycle-plot-area",
            style = "position: relative;",
            plotOutput(
              "proj_hemicycle_plot",
              click = "proj_hemicycle_click",
              width = "100%",
              height = "300px"
            ),
            uiOutput("proj_hemicycle_popup")
          ),
          div(
            class = "hemicycle-label-wrap",
            uiOutput("proj_hemicycle_label")
          )
        ),
        uiOutput("proj_map_ui")
      )
    ),

    # Coalition builder modal
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

  # Enumerate multi-party coalitions (2+ parties), but never every party at
  # once -- a coalition must leave at least one party in opposition.
  if (length(active_parties) >= 3) {
    for (size in 2:(length(active_parties) - 1)) {
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
            name = paste(short_names[combo], collapse = " & "),
            seats = total_seats,
            parties = combo
          )
        }
      }
    }
  }

  # Sort by seats descending
  if (length(coalitions) == 0) {
    return(coalitions)
  }
  coalitions[order(-sapply(coalitions, function(x) x$seats))]
}

# Short display names for parties (used in coalition labels)
PARTY_SHORT <- c(
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

# Build governing majorities from a national seat table, accounting for any
# electoral coalitions. `national` is a data frame with columns party, seats
# (where party may be a coalition entity name like "KO + P2050"). `coals` is
# the list of coalition member-vectors. Returns a list of governing
# majorities, each with $name, $seats, $parties (entity names).
build_coalitions_entities <- function(national, coals) {
  get_s <- function(p) {
    val <- national$seats[national$party == p]
    if (length(val) == 0) 0L else val
  }

  if (length(coals) == 0) {
    return(build_coalitions(get_s))
  }

  # Once the user has formed electoral coalitions, the ideological
  # incompatibility rules no longer apply: any blocs (coalitions or leftover
  # solo parties) may combine into a governing majority.

  entity_names <- national$party[national$seats > 0]
  gov_combos <- list()
  if (length(entity_names) >= 1) {
    # A coalition must leave at least one seat-winning bloc in opposition, so
    # never include every entity (max size is one fewer than the total).
    max_size <- max(1, length(entity_names) - 1)
    for (size in 1:max_size) {
      combos <- combn(entity_names, size, simplify = FALSE)
      for (combo in combos) {
        total <- sum(sapply(combo, get_s))
        if (total >= 231) {
          gov_combos[[length(gov_combos) + 1]] <- list(
            name = paste(combo, collapse = " & "),
            seats = total,
            parties = combo
          )
        }
      }
    }
  }

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

  if (length(gov_combos) == 0) {
    return(gov_combos)
  }
  gov_combos[order(-sapply(gov_combos, function(x) x$seats))]
}

# Among governing majorities (output of build_coalitions* ), pick the minimal
# winning coalition: fewest parties, breaking ties by fewest seats. Returns
# NULL if there are no governing majorities.
minimal_winning_coalition <- function(gov_coalitions) {
  if (length(gov_coalitions) == 0) {
    return(NULL)
  }
  n_parties <- sapply(gov_coalitions, function(x) length(x$parties))
  seats <- sapply(gov_coalitions, function(x) x$seats)
  # fewest parties first, then fewest seats
  idx <- order(n_parties, seats)[1]
  gov_coalitions[[idx]]
}

# --- Server ---
server <- function(input, output, session) {
  selected_date <- reactiveVal(max(available_dates))
  selected_constituency <- reactiveVal(NULL)

  # --- Editable vote/seat estimates ---
  # The numeric vote inputs are the single source of truth for vote shares.
  # Clicking a new date on the trend plot re-seeds them (see plot_click), so
  # user edits persist until the next date click.

  # Editable parties (everything except Other, which is a residual)
  EST_PARTIES <- setdiff(SIM_PARTIES, "Other")

  # Median-derived default vote shares for a given date, with MN hardcoded
  # to 0.8% and Other taking the remainder (mirrors sim_defaults logic).
  est_default_shares <- function(date) {
    d <- weekly_summaries %>%
      filter(date == !!date) %>%
      select(party, median_pct)
    shares <- setNames(rep(0, length(SIM_PARTIES)), SIM_PARTIES)
    for (p in PARTY_ORDER) {
      v <- d$median_pct[as.character(d$party) == p]
      if (length(v) > 0) {
        shares[p] <- v
      }
    }
    shares["MN"] <- 0.8
    shares["Other"] <- round(
      100 - sum(shares[setdiff(SIM_PARTIES, "Other")]),
      1
    )
    shares
  }

  # Read the live numeric inputs; fall back to the selected date's defaults
  # for any input that hasn't rendered yet.
  est_input_values <- reactive({
    vals <- est_default_shares(selected_date())
    for (p in EST_PARTIES) {
      v <- input[[paste0("est_", gsub(" ", "_", p))]]
      if (!is.null(v) && !is.na(v)) {
        vals[p] <- v
      }
    }
    vals["Other"] <- round(100 - sum(vals[setdiff(SIM_PARTIES, "Other")]), 1)
    vals
  })

  # Raw vote shares as entered
  est_vote_shares <- reactive({
    vals <- est_input_values()
    setNames(as.numeric(vals[SIM_PARTIES]), SIM_PARTIES)
  })

  # Coalitional-dynamics strength from the slider (0 = no punishment, 1 = max).
  # Debounced so dragging the slider doesn't trigger a recalculation on every
  # intermediate value; the projection updates once the slider settles. The
  # visual % label still tracks live via the oninput JS handler above.
  punish_strength <- reactive({
    s <- input$coalition_dynamics
    if (is.null(s)) 0 else s / 100
  }) %>% debounce(300)

  # Effective vote shares after the coalitional-dynamics punishment. At
  # strength 0 these equal the raw entered shares (so the projection is naive).
  effective_vote_shares <- reactive({
    punish_vote_shares(est_vote_shares(), coalition_defs(), punish_strength())
  })

  # The named shares (8 parties + MN) plus the "Other" residual always sum to
  # 100 by construction, so validity is really about "Other" staying sane:
  # non-negative (named shares not inflated past 100) and not implausibly large.
  # Recency weighting legitimately lifts Other to ~3%, so allow up to 6 pp.
  vote_total_ok <- reactive({
    other <- 100 - sum(est_input_values()[EST_PARTIES])
    other >= -1 && other <= 6
  })

  # Seat allocation. While the vote total is out of tolerance we "do not
  # proceed": the projection holds its last valid state until the user fixes it.
  est_seat_data_cache <- reactiveVal(NULL)
  est_seat_data <- reactive({
    if (vote_total_ok()) {
      result <- allocate_seats_with_coalitions(
        effective_vote_shares(),
        coalition_defs()
      )
      est_seat_data_cache(result)
      result
    } else {
      est_seat_data_cache()
    }
  })

  # --- Agency selector ---
  # Which agencies are highlighted (empty = show all)
  selected_agencies <- reactive({
    sel <- input$agency_selector
    if (is.null(sel) || length(sel) == 0) ALL_AGENCIES else sel
  })

  # Description text
  output$trend_description <- renderUI({
    tags$p(
      style = "color:#999; font-size:0.85em; max-width:810px; margin-bottom:8px;",
      "Click anywhere on the plot to load vote and seat estimates for the nearest week",
      "into the editable block below.",
      "Hover over any of the points to see particular polling house estimates.",
      "Click on one or more polling agencies below to highlight their polls on the chart."
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
    selected_constituency(NULL)
  })

  observeEvent(input$coalition_remove, {
    idx <- input$coalition_remove$index
    current <- coalition_defs()
    if (idx >= 1 && idx <= length(current)) {
      current[[idx]] <- NULL
      coalition_defs(current)
      selected_constituency(NULL)
    }
  })

  # Reset all coalitions: return to the default of all parties separate
  observeEvent(input$coalition_reset_btn, {
    coalition_defs(list())
    selected_constituency(NULL)
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

  # Debounced so a rapid double-click (or a click the browser reports twice)
  # coalesces into a single date selection + reseed, rather than firing the
  # recalculation more than once.
  plot_click_d <- reactive(input$plot_click) %>% debounce(300)

  observeEvent(plot_click_d(), {
    clicked_date <- as.Date(plot_click_d()$x, origin = "1970-01-01")
    idx <- which.min(abs(available_dates - clicked_date))
    new_date <- available_dates[idx]
    selected_date(new_date)
    selected_constituency(NULL)
    # Re-seed the editable vote inputs from the new date's medians,
    # overriding any prior user edits.
    defaults <- est_default_shares(new_date)
    for (p in EST_PARTIES) {
      updateNumericInput(
        session,
        paste0("est_", gsub(" ", "_", p)),
        value = round(defaults[p], 1)
      )
    }
  })

  output$trend_plot <- renderPlot(
    {
      showtext_opts(dpi = 96)
      sel <- selected_agencies()
      tl <- trend_lines

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

  # --- National seat summary (entity-aware) ---
  # --- Projection helpers (shared by the naive and realistic projections) ---
  # These are plain functions of (seat_data, coals, vals) so both projections
  # can reuse them without duplicating reactive logic.

  national_from <- function(seat_data) {
    if (is.null(seat_data)) {
      return(NULL)
    }
    seat_data %>%
      group_by(party) %>%
      summarise(seats = sum(seats), .groups = "drop") %>%
      arrange(desc(seats))
  }

  # Central projection summary used by the estimates block, hemicycle and maps.
  # Returns a list with:
  #   entities    list of {name, members, is_coalition, vote, seats,
  #                         member_seats (named vector), color}
  #   colors      named vector mapping entity name -> display colour
  #               (coalitions take the colour of their largest member BY SEATS)
  #   national    entity -> seats data frame
  compute_projection <- function(national, coals, vals) {
    if (is.null(national)) {
      return(NULL)
    }
    coal_members_all <- unlist(coals)

    seat_of <- function(entity) {
      v <- national$seats[national$party == entity]
      if (length(v) == 0) 0L else v
    }

    # Split a coalition's seats back to members by vote share (largest remainder)
    split_seats <- function(members, coal_seats) {
      shares <- vals[members]
      tot <- sum(shares, na.rm = TRUE)
      if (tot == 0 || coal_seats == 0) {
        return(setNames(rep(0L, length(members)), members))
      }
      raw <- coal_seats * shares / tot
      base_n <- floor(raw)
      rem <- coal_seats - sum(base_n)
      if (rem > 0) {
        add <- order(-(raw - base_n))[seq_len(rem)]
        base_n[add] <- base_n[add] + 1
      }
      setNames(as.integer(base_n), members)
    }

    entities <- list()
    cols <- PARTY_COLORS

    # Coalition entities
    for (coal in coals) {
      name <- paste(PARTY_SHORT[coal], collapse = "-")
      seats <- seat_of(name)
      member_seats <- split_seats(coal, seats)
      # Colour by largest member BY SEATS (tie -> by vote share)
      ms <- member_seats
      ord <- order(-ms, -vals[coal])
      largest <- coal[ord[1]]
      col <- PARTY_COLORS[largest]
      cols[name] <- col
      entities[[length(entities) + 1]] <- list(
        name = name,
        members = coal,
        is_coalition = TRUE,
        vote = sum(vals[coal], na.rm = TRUE),
        seats = seats,
        member_seats = member_seats,
        color = col
      )
    }

    # Solo party entities (incl. MN), not in any coalition
    solo <- setdiff(EST_PARTIES, coal_members_all)
    for (p in solo) {
      seats <- seat_of(p)
      entities[[length(entities) + 1]] <- list(
        name = p,
        members = p,
        is_coalition = FALSE,
        vote = as.numeric(vals[p]),
        seats = seats,
        member_seats = setNames(seats, p),
        color = PARTY_COLORS[p]
      )
    }

    list(entities = entities, colors = cols, national = national)
  }

  # The "winning" coalition for the hemicycle label, matching the highlight:
  #  - no user coalitions: the minimal winning coalition
  #  - user coalitions defined: the largest governing majority
  # Returns list(winners = list of gov-coalition objects, tie = logical).
  # Pick the "winning coalition" to highlight. Always returns a coalition when
  # one is possible, regardless of whether electoral coalitions exist.
  #   national : data frame party -> seats (entity names when coalitions exist)
  #   vals     : named vote shares (for vote-weighted ideological positions)
  #   coals    : the electoral-coalition definitions
  #   gov      : enumerated compatible governing majorities (>= 231)
  compute_winner <- function(gov, coals, national, vals) {
    if (is.null(national) || nrow(national) == 0) {
      return(list(winners = list(), tie = FALSE))
    }
    seat_of <- function(p) {
      v <- national$seats[national$party == p]
      if (length(v) == 0) 0L else v
    }

    if (length(coals) == 0) {
      # --- Single parties: build outward from the largest party by adjacency ---
      parties <- intersect(names(IDEOLOGY_POSITIONS), national$party)
      parties <- parties[sapply(parties, seat_of) > 0]
      if (length(parties) == 0) {
        return(list(winners = list(), tie = FALSE))
      }
      # Largest party by seats
      start <- parties[order(-sapply(parties, seat_of))][1]
      bloc <- start
      total <- seat_of(start)
      repeat {
        if (total >= 231) {
          break
        }
        # Candidates: compatible, not yet in bloc, with seats
        cand <- setdiff(parties, bloc)
        cand <- cand[sapply(cand, function(p) parties_compatible(c(bloc, p)))]
        if (length(cand) == 0) {
          break
        }
        # Most ideologically adjacent to the current bloc (vote-weighted centre)
        centre <- ideology_of(bloc, vals)
        dist <- abs(IDEOLOGY_POSITIONS[cand] - centre)
        nearest <- cand[order(dist, -sapply(cand, seat_of))][1]
        bloc <- c(bloc, nearest)
        total <- total + seat_of(nearest)
      }

      if (total < 231) {
        # Greedy got stuck (a nearby addition blocked a later partner). Fall
        # back to the enumerated winning coalition that includes the largest
        # party, choosing fewest parties then smallest ideological spread.
        with_start <- Filter(function(g) start %in% g$parties, gov)
        if (length(with_start) == 0) {
          return(list(winners = list(), tie = FALSE))
        }
        spread <- function(g) {
          pos <- IDEOLOGY_POSITIONS[g$parties]
          pos <- pos[!is.na(pos)]
          if (length(pos) <= 1) 0 else max(pos) - min(pos)
        }
        np <- sapply(with_start, function(g) length(g$parties))
        sp <- sapply(with_start, spread)
        primary <- with_start[[order(np, sp)[1]]]
        return(list(winners = list(primary), tie = FALSE))
      }

      # Order bloc by ideology for the display name
      bloc <- bloc[order(IDEOLOGY_POSITIONS[bloc])]
      primary <- list(
        name = paste(PARTY_SHORT[bloc], collapse = " & "),
        seats = total,
        parties = bloc
      )
      return(list(winners = list(primary), tie = FALSE))
    }

    # --- Electoral coalitions present ---
    if (length(gov) == 0) {
      return(list(winners = list(), tie = FALSE))
    }
    # Map entity name -> member parties (coalition entities or solo parties)
    entity_members <- list()
    for (coal in coals) {
      entity_members[[paste(PARTY_SHORT[coal], collapse = "-")]] <- coal
    }
    members_of <- function(ent) {
      m <- entity_members[[ent]]
      if (is.null(m)) ent else m
    }

    # Priority 1: a single electoral coalition that holds a majority alone
    single_blocs <- Filter(function(g) length(g$parties) == 1, gov)
    if (length(single_blocs) > 0) {
      single_blocs <- single_blocs[order(
        -sapply(single_blocs, function(g) g$seats)
      )]
      return(list(winners = list(single_blocs[[1]]), tie = FALSE))
    }

    # Priority 2: combine blocs with the smallest combined ideological spread,
    # breaking ties by fewest blocs.
    spread_of <- function(g) {
      pos <- sapply(g$parties, function(ent) ideology_of(members_of(ent), vals))
      pos <- pos[!is.na(pos)]
      if (length(pos) <= 1) 0 else max(pos) - min(pos)
    }
    spreads <- sapply(gov, spread_of)
    nblocs <- sapply(gov, function(g) length(g$parties))
    primary <- gov[[order(spreads, nblocs)[1]]]
    list(winners = list(primary), tie = FALSE)
  }

  # --- Naive projection reactives (vote shares as entered) ---
  # Effective per-party vals (named, incl. Other) after the slider punishment,
  # used for vote-total display and seat-splitting in the projection.
  effective_vals <- reactive({
    vals <- est_input_values()
    full <- setNames(as.numeric(vals[names(vals)]), names(vals))
    punish_vote_shares(full, coalition_defs(), punish_strength())
  })

  est_national <- reactive(national_from(est_seat_data()))
  est_projection <- reactive(
    compute_projection(est_national(), coalition_defs(), effective_vals())
  )
  est_colors <- reactive({
    proj <- est_projection()
    if (is.null(proj)) PARTY_COLORS else proj$colors
  })
  est_gov_coalitions <- reactive({
    national <- est_national()
    if (is.null(national)) {
      list()
    } else {
      build_coalitions_entities(national, coalition_defs())
    }
  })
  est_mwc <- reactive(minimal_winning_coalition(est_gov_coalitions()))
  est_winner <- reactive(
    compute_winner(
      est_gov_coalitions(),
      coalition_defs(),
      est_national(),
      effective_vals()
    )
  )

  # --- Editable vote/seat block (3-row grid) ---
  output$estimates_heading <- renderUI({
    tags$h4(
      style = "margin-top:0; font-size:1.3rem; font-weight:bold;",
      format(selected_date(), "%e %B %Y")
    )
  })

  # A single editable party row: colour dot + name, vote input, seats
  # show_seats = FALSE for coalition members, where an individual party's seat
  # share within the joint list is unknowable (only the coalition total is shown).
  est_party_row <- function(p, col, seats_val, show_seats = TRUE) {
    if (is.na(col)) {
      col <- "gray50"
    }
    vals <- est_input_values()
    seat_tags <- if (show_seats) {
      list(
        span(class = "est-seats", seats_val),
        span(class = "est-seats-label", "seats")
      )
    } else {
      list()
    }
    div(
      class = "est-entry",
      div(
        class = "est-party",
        HTML(paste0(
          "<span class='color-dot' style='background:",
          col,
          ";'></span>",
          p
        ))
      ),
      div(
        class = "est-vote-box",
        numericInput(
          inputId = paste0("est_", gsub(" ", "_", p)),
          label = NULL,
          value = round(vals[p], 1),
          min = 0,
          max = 100,
          step = 0.1
        )
      ),
      span(class = "est-pct", "%"),
      tagList(seat_tags)
    )
  }

  output$est_inputs_ui <- renderUI({
    proj <- est_projection()
    if (is.null(proj)) {
      return(NULL)
    }
    coals <- coalition_defs()
    cols <- proj$colors
    entities <- proj$entities

    if (length(coals) == 0) {
      # --- No coalitions: two columns sorted by vote share descending ---
      vals <- est_input_values()
      national <- proj$national
      seat_of <- function(p) {
        v <- national$seats[national$party == p]
        if (length(v) == 0) 0L else v
      }
      ordered <- EST_PARTIES[order(-vals[EST_PARTIES])]
      rows <- lapply(ordered, function(p) {
        est_party_row(p, cols[p], seat_of(p))
      })
      # Three columns, filled column-major so reading down col 1, then col 2,
      # then col 3 follows descending vote order (top-left -> bottom-right).
      n <- length(rows)
      per_col <- ceiling(n / 3)
      cols_list <- lapply(0:2, function(k) {
        idx <- (k * per_col + 1):min((k + 1) * per_col, n)
        idx <- idx[idx >= 1 & idx <= n]
        if (length(idx) == 0) {
          return(NULL)
        }
        div(class = "est-col", tagList(rows[idx]))
      })
      div(class = "est-columns", tagList(Filter(Negate(is.null), cols_list)))
    } else {
      # --- Coalitions: one column per coalition + a shared solo column ---
      coal_entities <- Filter(function(e) e$is_coalition, entities)
      solo_entities <- Filter(function(e) !e$is_coalition, entities)

      coal_cols <- lapply(coal_entities, function(e) {
        # Members show vote share only; individual seats within a joint list
        # are unknowable, so only the coalition total (below) is given.
        member_rows <- lapply(e$members, function(p) {
          est_party_row(p, PARTY_COLORS[p], NULL, show_seats = FALSE)
        })
        div(
          class = "est-col est-coal-col",
          div(
            class = "est-coal-header",
            HTML(paste0(
              "<span class='color-dot' style='background:",
              e$color,
              ";'></span>",
              e$name
            ))
          ),
          tagList(member_rows),
          div(
            class = "est-coal-total",
            span(class = "est-coal-total-label", "Coalition"),
            span(
              class = "est-coal-total-vote",
              paste0(format(round(e$vote, 1), nsmall = 1), "%")
            ),
            span(class = "est-coal-total-seats", e$seats),
            span(class = "est-seats-label", "seats")
          )
        )
      })

      solo_col <- if (length(solo_entities) > 0) {
        # Sort solo parties by vote descending
        ord <- order(-sapply(solo_entities, function(e) e$vote))
        solo_rows <- lapply(solo_entities[ord], function(e) {
          est_party_row(e$name, e$color, e$seats)
        })
        list(div(
          class = "est-col est-solo-col",
          div(class = "est-coal-header est-solo-header", "Other parties"),
          tagList(solo_rows)
        ))
      } else {
        list()
      }

      div(class = "est-columns", tagList(c(coal_cols, solo_col)))
    }
  })

  output$est_total_display <- renderUI({
    vals <- est_input_values()
    total <- sum(vals[EST_PARTIES])
    other <- vals["Other"]
    # "Other" is the residual after the named shares; it may legitimately be a
    # few percent (minor parties + undecided). Flag only when it goes negative
    # (named shares exceed 100) or grows implausibly large.
    ok <- other >= -1 && other <= 6
    css_class <- if (ok) "sim-total" else "sim-total sim-total-bad"
    div(
      style = "margin-top:10px; display:flex; align-items:center; gap:12px; flex-wrap:wrap;",
      div(
        class = css_class,
        paste0(
          "Parties: ",
          format(round(total, 1), nsmall = 1),
          "%  ·  Remainder: ",
          format(round(other, 1), nsmall = 1),
          "%"
        )
      ),
      if (!ok) {
        tags$span(
          style = "color:#c0392b; font-size:0.85em;",
          "The named shares must leave an Other remainder between 0 and 6 pp – adjust the vote shares before continuing."
        )
      }
    )
  })

  # --- Coalition analysis shown below the party columns ---
  # A "majority class" badge: veto override (>=276) or constitutional (>=307).
  majority_badge <- function(seats) {
    if (seats >= 307) {
      " <span class='coalition-majority' style='color:green;'>(constitutional majority – can amend the constitution)</span>"
    } else if (seats >= 276) {
      " <span class='coalition-majority' style='color:green;'>(can override a presidential veto)</span>"
    } else {
      ""
    }
  }

  coalition_line <- function(label, co) {
    paste0(
      "<div class='coalition-entry'>",
      "<div class='coalition-name'>",
      label,
      ": <b>",
      co$name,
      "</b></div>",
      "<div><span class='coalition-seats'>",
      co$seats,
      " seats</span>",
      majority_badge(co$seats),
      "</div></div>"
    )
  }

  output$est_analysis <- renderUI({
    gov <- est_gov_coalitions()
    coals <- coalition_defs()
    header <- "Coalition possibilities"

    if (length(gov) == 0) {
      return(div(
        class = "est-analysis",
        tags$h6(
          style = "margin:14px 0 6px 0; font-size:1.3rem; font-weight:bold;",
          header
        ),
        div(
          style = "color:#999; font-size:0.9em;",
          "No combination of parties reaches a governing majority (231 seats)."
        )
      ))
    }

    seats_of <- sapply(gov, function(x) x$seats)

    if (length(coals) == 0) {
      # (a) minimum winning coalition; (b) the smallest coalition that reaches
      # a constitutional majority (>= 307 seats), if one is possible.
      mwc <- minimal_winning_coalition(gov)

      lines <- list(coalition_line("Minimum winning coalition", mwc))

      const_majorities <- Filter(function(co) co$seats >= 307, gov)
      if (length(const_majorities) > 0) {
        cm_seats <- sapply(const_majorities, function(co) co$seats)
        smallest_const <- const_majorities[[which.min(cm_seats)[1]]]
        if (!identical(sort(smallest_const$parties), sort(mwc$parties))) {
          lines[[length(lines) + 1]] <- coalition_line(
            "Minimum constitutional majority",
            smallest_const
          )
        }
      }
      body <- tagList(lapply(lines, HTML))
    } else {
      # List the possible governing coalitions among the user-defined blocs
      gov_sorted <- gov[order(-seats_of)]
      lines <- lapply(gov_sorted, function(co) coalition_line("Majority", co))
      body <- tagList(HTML(paste(lines, collapse = "")))
    }

    div(
      class = "est-analysis",
      tags$h6(
        style = "margin:14px 0 6px 0; font-size:1.3rem; font-weight:bold;",
        header
      ),
      body
    )
  })

  # Weighting note (shown below when coalitions are defined)
  # --- Seat projection: hemicycle ---

  # Build the ordered seat-block layout for a hemicycle from a projection and
  # its winning coalition. Returns a list with $layout (one row per seat) and
  # $blocks. Winning bloc on the left, largest opposition on the right, others
  # in between (by seats, largest nearest the winner). All full colour.
  compute_hemicycle_data <- function(proj, winner) {
    if (is.null(proj)) {
      return(NULL)
    }
    entities <- proj$entities
    won <- Filter(function(e) !is.na(e$seats) && e$seats > 0, entities)
    if (length(won) == 0) {
      return(NULL)
    }

    winner_parties <- if (length(winner$winners) > 0) {
      winner$winners[[1]]$parties
    } else {
      character(0)
    }

    is_winner_entity <- function(e) {
      e$name %in% winner_parties || any(e$members %in% winner_parties)
    }
    winner_entities <- Filter(is_winner_entity, won)
    rest <- Filter(function(e) !is_winner_entity(e), won)

    ordered <- winner_entities
    if (length(rest) > 0) {
      rest <- rest[order(-sapply(rest, function(e) e$seats))]
      far_right <- rest[[1]]
      middle <- if (length(rest) > 1) rest[-1] else list()
      ordered <- c(winner_entities, middle, list(far_right))
    }

    order_entities_by_ideology <- function(ents) {
      if (length(ents) <= 1) {
        return(ents)
      }
      key <- sapply(ents, function(e) {
        pos <- match(e$members, HEMICYCLE_ORDER)
        min(pos, na.rm = TRUE)
      })
      ents[order(key)]
    }
    if (length(winner_entities) > 1) {
      ordered <- c(
        order_entities_by_ideology(winner_entities),
        ordered[(length(winner_entities) + 1):length(ordered)]
      )
    }

    seat_rows <- list()
    for (e in ordered) {
      members_ord <- intersect(HEMICYCLE_ORDER, e$members)
      for (p in members_ord) {
        n <- e$member_seats[p]
        if (is.na(n) || n <= 0) {
          next
        }
        seat_rows[[length(seat_rows) + 1]] <- data.frame(
          party = p,
          entity = e$name,
          n = n,
          color = unname(e$color),
          stringsAsFactors = FALSE
        )
      }
    }

    if (length(seat_rows) == 0) {
      return(NULL)
    }
    blocks <- bind_rows(seat_rows)
    blocks$color[is.na(blocks$color)] <- "gray50"
    total_seats <- sum(blocks$n)

    layout <- hemicycle_layout(total_seats)
    layout$party <- rep(blocks$party, blocks$n)
    layout$entity <- rep(blocks$entity, blocks$n)
    layout$fill <- rep(blocks$color, blocks$n)

    list(layout = layout, blocks = blocks)
  }

  # Build the winning-coalition label UI from a winner object.
  build_hemicycle_label <- function(w) {
    if (length(w$winners) == 0) {
      return(div(
        class = "hemicycle-label hemicycle-label-none",
        "No governing majority"
      ))
    }
    seat_badge <- function(co) {
      extra <- if (co$seats >= 307) {
        " · constitutional majority"
      } else if (co$seats >= 276) {
        " · veto override"
      } else {
        ""
      }
      paste0(
        "<span class='hemicycle-win-name'>",
        co$name,
        "</span> <span class='hemicycle-win-seats'>",
        co$seats,
        " seats</span><span class='hemicycle-win-extra'>",
        extra,
        "</span>"
      )
    }
    if (w$tie) {
      blocks <- paste(
        sapply(w$winners, function(co) {
          paste0("<div class='hemicycle-win-line'>", seat_badge(co), "</div>")
        }),
        collapse = ""
      )
      div(
        class = "hemicycle-label",
        HTML(paste0(
          "<div class='hemicycle-label-title'>Tie – two possible winning coalitions</div>",
          blocks
        ))
      )
    } else {
      co <- w$winners[[1]]
      div(
        class = "hemicycle-label",
        HTML(paste0(
          "<div class='hemicycle-label-title'>Winning coalition</div>",
          "<div class='hemicycle-win-line'>",
          seat_badge(co),
          "</div>"
        ))
      )
    }
  }

  # Build the constituency-map sf data frame from a seat-data result + colours.
  compute_map_data <- function(result, colors) {
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
    md$fill_color <- mapply(
      function(party, dom) {
        if (is.na(party)) {
          return("grey80")
        }
        col <- colors[party]
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
  }

  # Register all outputs/observers for one seat-projection section, keyed by a
  # prefix ("naive" / "realistic"). Input/output IDs are <prefix>_<name>.
  register_projection_section <- function(
    prefix,
    proj_r,
    colors_r,
    winner_r,
    seatdata_r
  ) {
    id <- function(name) paste0(prefix, "_", name)
    hemi_selected <- reactiveVal(NULL)
    map_selected <- reactiveVal(NULL)

    hemi_data <- reactive(compute_hemicycle_data(proj_r(), winner_r()))
    map_data <- reactive(compute_map_data(seatdata_r(), colors_r()))

    output[[id("hemicycle_plot")]] <- renderPlot(
      {
        showtext_opts(dpi = 96)
        hd <- hemi_data()
        if (is.null(hd)) {
          return(NULL)
        }
        ggplot(hd$layout, aes(x = x, y = y)) +
          geom_point(
            aes(fill = fill),
            shape = 21,
            color = "white",
            size = 4,
            stroke = 0.3
          ) +
          scale_fill_identity() +
          coord_fixed(xlim = c(-2.1, 2.1), ylim = c(0, 2.15)) +
          theme_void(base_family = PLOT_FONT) +
          theme(
            legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "cm")
          )
      },
      res = 96,
      execOnResize = TRUE
    )

    output[[id(
      "hemicycle_label"
    )]] <- renderUI(build_hemicycle_label(winner_r()))

    observeEvent(list(coalition_defs(), selected_date()), {
      hemi_selected(NULL)
    })

    observeEvent(input[[id("hemicycle_click")]], {
      hd <- hemi_data()
      if (is.null(hd)) {
        return()
      }
      layout <- hd$layout
      click <- input[[id("hemicycle_click")]]
      d2 <- (layout$x - click$x)^2 + (layout$y - click$y)^2
      if (min(d2) > 0.05) {
        hemi_selected(NULL)
        return()
      }
      hemi_selected(layout$entity[which.min(d2)])
    })

    output[[id("hemicycle_popup")]] <- renderUI({
      ent_name <- hemi_selected()
      if (is.null(ent_name)) {
        return(NULL)
      }
      proj <- proj_r()
      if (is.null(proj)) {
        return(NULL)
      }
      e <- Filter(function(x) x$name == ent_name, proj$entities)
      if (length(e) == 0) {
        return(NULL)
      }
      e <- e[[1]]
      if (e$is_coalition) {
        member_html <- sapply(e$members, function(p) {
          paste0(
            "<div class='map-popup-entry'>",
            "<span class='map-popup-party'><span class='color-dot' style='background:",
            PARTY_COLORS[p],
            ";'></span>",
            p,
            "</span>",
            "<span class='map-popup-seats'>",
            e$member_seats[p],
            "</span>&nbsp;seats</div>"
          )
        })
        body <- paste0(
          "<div style='font-size:0.8em; color:#888; margin-bottom:4px;'>Electoral coalition</div>",
          paste(member_html, collapse = ""),
          "<div class='map-popup-entry' style='border-top:1px solid #ddd; margin-top:4px; padding-top:4px; font-weight:bold;'>",
          "<span class='map-popup-party'>Total</span>",
          "<span class='map-popup-seats'>",
          e$seats,
          "</span>&nbsp;seats</div>"
        )
      } else {
        body <- paste0(
          "<div class='map-popup-entry'>",
          "<span class='map-popup-party'><span class='color-dot' style='background:",
          e$color,
          ";'></span>",
          e$name,
          "</span>",
          "<span class='map-popup-seats'>",
          e$seats,
          "</span>&nbsp;seats</div>"
        )
      }
      click <- input[[id("hemicycle_click")]]
      div(
        class = "map-popup",
        style = paste0(
          "left:",
          click$coords_css$x + 15,
          "px; top:",
          click$coords_css$y + 15,
          "px;"
        ),
        HTML(paste0("<b>", ent_name, "</b><br>", body))
      )
    })

    output[[id("map_ui")]] <- renderUI({
      div(
        class = "popup-map",
        style = "border-left:none; padding-left:0;",
        tags$h5("Constituency seat shares"),
        div(
          style = "position: relative;",
          plotOutput(
            id("map"),
            click = id("map_click"),
            width = "250px",
            height = "280px"
          ),
          uiOutput(id("map_popup"))
        ),
        tags$p(
          style = "margin:4px 0 0 0; color:#999; font-size:0.75em; max-width:250px;",
          "Click on the map for constituency seat shares."
        )
      )
    })

    output[[id("map")]] <- renderPlot(
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
          theme_void(base_family = PLOT_FONT) +
          theme(
            legend.position = "none",
            plot.margin = unit(c(0, 0, 0, 0), "cm")
          )
      },
      res = 96
    )

    observeEvent(input[[id("map_click")]], {
      md <- map_data()
      if (is.null(md)) {
        return()
      }
      click <- input[[id("map_click")]]
      click_point <- sf::st_point(c(click$x, click$y))
      click_sfc <- sf::st_sfc(click_point, crs = sf::st_crs(md))
      hit <- sf::st_intersects(click_sfc, md)
      if (length(hit[[1]]) > 0) {
        map_selected(md$id[hit[[1]][1]])
      } else {
        map_selected(NULL)
      }
    })

    output[[id("map_popup")]] <- renderUI({
      const_id <- map_selected()
      if (is.null(const_id)) {
        return(NULL)
      }
      result <- seatdata_r()
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
      colors <- colors_r()
      entries <- cs %>%
        rowwise() %>%
        mutate(
          color_hex = {
            col <- colors[party]
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
      click <- input[[id("map_click")]]
      div(
        class = "map-popup",
        style = paste0(
          "left:",
          click$coords_css$x + 15,
          "px; top:",
          click$coords_css$y + 15,
          "px;"
        ),
        HTML(paste0(
          "<b>",
          const_name,
          "</b><br>",
          paste(entries, collapse = "")
        ))
      )
    })
  }

  # Wire up the single (slider-driven) projection section
  register_projection_section(
    "proj",
    est_projection,
    est_colors,
    est_winner,
    est_seat_data
  )
}

shinyApp(ui, server)
