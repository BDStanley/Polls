#####################################################################
# Electoral consequences of a centre-right splinter from PiS
#
# Counterfactual analysis of the July 2026 split, in which around 40
# more moderate PiS MPs led by Mateusz Morawiecki were ejected from the
# party. The question: what happens if they form a party that competes
# for centre-right voters, with the rest of the party system unchanged?
#
# Approach:
#   1. The current poll-of-polls Dirichlet model (as in PTP.R) supplies
#      the posterior distribution of national vote shares for the
#      existing parties.
#   2. Survey evidence from the Writing repo (PGSW 2023, BDCS December
#      2025) is used to estimate the *pool* of voters in each existing
#      electorate that a moderate, non-PiS centre-right party could
#      plausibly reach.
#   3. Each posterior draw is transformed by transferring a sampled
#      fraction of each donor party's constituency vote to the new
#      party, so the splinter inherits a regional profile that is a
#      mixture of its donors' profiles.
#   4. Seats are allocated by D'Hondt under the 5% national threshold,
#      which the splinter — as a party, not a coalition — must clear.
#   5. A scenario grid varies how much of the available pool the new
#      party actually converts, giving the "cost of the split" curve.
#
# The script is read-only with respect to the rest of the project: it
# fits (and caches) its own copy of the poll model, writes its own
# figures and data objects, and does not deploy or commit anything.
#####################################################################

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.r-project.org")
}

pkgs <- c(
  "tidyverse",
  "readxl",
  "sf",
  "glue",
  "sjlabelled",
  "haven",
  "lubridate",
  "brms",
  "tidybayes",
  "ggdist",
  "ggblend",
  "seatdist",
  "here"
)
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  pak::pkg_install(missing_pkgs, ask = FALSE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(780045)

#####Constants#####
PARTY_COLS <- c(
  "PiS",
  "KO",
  "Lewica",
  "Razem",
  "Polska2050",
  "PSL",
  "Konfederacja",
  "KKP",
  "Other"
)

# Internal (model) names are used throughout; display names only for
# plots, which are labelled in Polish.
DISPLAY_NAMES <- c(
  PiS = "PiS",
  Splinter = "Rozłamowcy z PiS",
  KO = "KO",
  Polska2050 = "Polska 2050",
  PSL = "PSL",
  Konfederacja = "Konfederacja",
  KKP = "KKP",
  Lewica = "Lewica",
  Razem = "Razem",
  MN = "MN",
  Other = "Inne"
)

PARTY_COLORS <- c(
  "PiS" = "blue",
  "Rozłamowcy z PiS" = "steelblue",
  "KO" = "orange",
  "Polska 2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "KKP" = "brown",
  "Lewica" = "red",
  "Razem" = "purple",
  "MN" = "yellow",
  "Inne" = "gray50"
)

TINY_CONSTANT <- 0.0005

# Donor parties (everything the splinter can take votes from) and the
# constituency-level coefficient each one's regional profile follows.
# "Other" is carried through so national shares stay on a 0-1 scale; it
# is given a flat regional profile and never wins seats.
DONORS <- c(
  "PiS",
  "KO",
  "Lewica",
  "Razem",
  "Polska2050",
  "PSL",
  "Konfederacja",
  "KKP",
  "Other"
)

COEF_MAP <- c(
  PiS = "PiScoef",
  KO = "KOcoef",
  Lewica = "Lewicacoef",
  Razem = "Lewicacoef",
  Polska2050 = "TDcoef",
  PSL = "TDcoef",
  Konfederacja = "Konfcoef",
  KKP = "Konfcoef"
)

# Parties that can win seats, in the order used by the D'Hondt call.
SEAT_PARTIES <- c(
  "PiS",
  "Splinter",
  "KO",
  "Lewica",
  "Razem",
  "Polska2050",
  "PSL",
  "Konfederacja",
  "KKP",
  "MN"
)

SEATS_2023 <- c(
  PiS = 194,
  Splinter = 0,
  KO = 157,
  Lewica = 19,
  Razem = 7,
  Polska2050 = 33,
  PSL = 32,
  Konfederacja = 18,
  KKP = 0,
  MN = 0
)

MAJORITY <- 231
VETO_MAJORITY <- 276
CONSTITUTIONAL_MAJORITY <- 307

#####Scenario parameters#####
# The analysis is built around three scenarios, defined by the share of
# the survey-measured available pool that the new party actually
# converts: a half, three quarters, all of it. They are not equally
# likely. calibrate_realisation() below measures how much attitudinal
# availability turned into votes in the NCN panel between May and
# October 2023, and the answer is 23%, so even the lowest of the three
# is an optimistic reading of the base rate. The two published polls of
# a hypothetical Morawiecki party, both fielded 16-17 July 2026, put it
# at 2.33% (Instytut Badań Pollster) and 6.9% (United Surveys/IBRiS) —
# the range these scenarios have to speak to.
SCENARIO_LEVELS <- c(0.50, 0.75, 1.00)
SCENARIO_LABELS <- c(
  "Połowa puli",
  "Trzy czwarte puli",
  "Cała pula"
)

# Beta concentration for the per-draw transfer rates: kappa = 40 gives a
# posterior sd of roughly 5pp on a transfer rate of 0.2.
TRANSFER_KAPPA <- 40

# Extra national vote share won from voters outside the current party
# system (2023 non-voters, don't-knows). PGSW 2023 puts 18% of
# non-voters at 6+ on the Morawiecki like scale; converting a small
# fraction of a ~40% non-voting electorate is worth roughly a point.
MOBILISATION_MEAN <- 0.010
MOBILISATION_SD <- 0.006

# The splinter's regional profile is a mixture of its donors' profiles.
# URBAN_TILT would shift it towards the KO profile. It is set to zero
# because the data contradict the judgement it encoded: among PiS voters
# in PGSW 2023, availability to a Morawiecki-led party runs at 23.8% in
# villages, 16.0% in towns under 50,000, 18.6% in towns of 50-200,000 and
# 21.6% in cities above 200,000 — flat, and if anything rural. In BDCS
# the same holds for education (higher 9.7%, secondary 7.5%, basic 8.3%,
# n.s.) and sex (8.5% vs 8.6%). The only demographic that does predict
# availability is age (17.9% at 18-24 falling to 2.9% at 65+), which
# varies too little across constituencies to be worth post-stratifying.
URBAN_TILT <- 0

# The splinter would contest as a party, not a coalition, so 5%.
THRESHOLD <- 0.05

# No party is assumed to lose more than this share of its voters to the
# breakaway, however successful the breakaway is.
TRANSFER_CAP <- 0.6

# Threshold applied per draw rather than to the posterior median: for a
# party sitting near 5% the risk of falling short is the substantive
# question, not a nuisance.
N_DRAWS_MAIN <- 1000
N_DRAWS_GRID <- 400
REALISATION_GRID <- seq(0, 1, by = 0.1)

MN_SHARE <- 0.079
MN_OKREG <- 21

# Survey evidence lives in the sibling Writing repo.
WRITING_DATA <- path.expand("~/Positron/Writing/data")

#####Theme functions#####
# Copied verbatim from PTP.R so the figures match the rest of the project.
theme_plots <- function(base_size = 11, base_family = "Jost") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(
        size = rel(1),
        family = "Jost Medium",
        face = "plain"
      ),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
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
      axis.title = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      axis.title.x = element_text(margin = margin(t = 10)),
      axis.title.y = element_text(hjust = 1, margin = margin(r = 10)),
      legend.position = "bottom",
      legend.title = element_text(
        size = rel(0.8),
        vjust = 0.5,
        family = "Jost Medium",
        face = "plain"
      ),
      legend.key.size = unit(0.7, "line"),
      legend.key = element_blank(),
      legend.spacing = unit(0.1, "lines"),
      legend.justification = "left",
      legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(
        size = rel(0.9),
        hjust = 0,
        family = "Jost",
        face = "plain"
      ),
      strip.background = element_rect(fill = "white", colour = NA),
      plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")
    )
}

theme_plots_map <- function(base_size = 11, base_family = "Jost") {
  theme_minimal(base_size, base_family) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      strip.text.x = element_text(size = 10, family = "Jost", face = "plain"),
      legend.text = element_text(size = 9, family = "Jost", face = "plain"),
      title = element_text(
        size = rel(1),
        family = "Jost Medium",
        face = "plain"
      ),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Jost",
        face = "plain"
      ),
      plot.caption = element_text(
        margin = margin(t = 10),
        size = rel(0.6),
        family = "Jost",
        face = "plain"
      ),
      legend.title = element_text(family = "Jost", face = "plain"),
      plot.title = element_text(family = "Jost Medium", face = "plain"),
      aspect.ratio = 1,
      legend.position = "none"
    )
}

options(mc.cores = parallel::detectCores())
total_cores <- parallel::detectCores()
n_chains <- 4
threads_per_chain <- max(1, floor(total_cores / n_chains))

#####Helper functions#####
apply_threshold_and_normalize <- function(df, party_cols, threshold = 0.05) {
  outcome_matrix <- as.matrix(df[, party_cols])
  outcome_matrix_fixed <- outcome_matrix + TINY_CONSTANT
  outcome_matrix_fixed <- outcome_matrix_fixed / rowSums(outcome_matrix_fixed)

  for (i in seq_along(party_cols)) {
    df[[party_cols[i]]] <- outcome_matrix_fixed[, i]
  }

  df$outcome <- outcome_matrix_fixed
  df
}

create_const_id_mapping <- function() {
  tibble(
    cst = 1:41,
    id = c(
      24, 27, 4, 7, 28, 34, 25, 26, 29, 36, 31, 33, 37, 40, 13, 12, 22, 1,
      6, 14, 35, 21, 10, 38, 39, 16, 17, 30, 23, 18, 11, 32, 41, 15, 5, 19,
      20, 2, 3, 8, 9
    )
  )
}

# D'Hondt allocation for a single constituency. Equivalent to
# seatdist::giveseats(..., method = "dh", thresh = 0) but fast enough to
# run inside the draw loop; the equivalence is asserted below.
dhondt <- function(v, ns) {
  k <- length(v)
  if (ns <= 0 || all(v <= 0)) {
    return(integer(k))
  }
  quot <- as.vector(outer(v, seq_len(ns), "/"))
  party <- rep.int(seq_len(k), ns)
  idx <- order(quot, decreasing = TRUE)[seq_len(ns)]
  tabulate(party[idx], nbins = k)
}

# Figures are labelled in Polish, so numbers take a decimal comma.
pl_num <- function(x, digits = 1) {
  formatC(x, format = "f", digits = digits, decimal.mark = ",")
}

pl_pct <- function(x, accuracy = 1) {
  scales::percent(x, accuracy = accuracy, decimal.mark = ",")
}

pl_pct_axis <- function(accuracy = 1) {
  scales::percent_format(accuracy = accuracy, decimal.mark = ",")
}

# Axis breaks: decimal comma, no trailing zeros ("2,5" not "2,50").
pl_axis_num <- function(x) {
  sub("\\.", ",", format(x, trim = TRUE, drop0trailing = TRUE))
}

# Beta draws parameterised by mean and concentration; mu may be a vector
# (one mean per draw), and a mean of zero returns exact zeros.
rbeta_mean <- function(n, mu, kappa = TRANSFER_KAPPA) {
  mu <- rep_len(mu, n)
  out <- numeric(n)
  ok <- mu > 0 & mu < 1
  if (any(ok)) {
    out[ok] <- rbeta(sum(ok), mu[ok] * kappa, (1 - mu[ok]) * kappa)
  }
  out[mu >= 1] <- 1
  out
}

#####Calibrating the splinter's potential from survey data#####
# Two independent readings of how much of each electorate a moderate
# centre-right breakaway could reach.
#
# PGSW 2023 (post-election study, n = 1,500) is leader-specific: it
# carries 0-10 like scales for Morawiecki, Kaczynski and the parties.
# The strict availability measure is "likes Morawiecki (6+) and prefers
# him to Kaczynski"; the broad one is "likes Morawiecki (6+) and rates
# him at least as highly as PiS itself".
#
# BDCS (fieldwork 10-19 December 2025, n = 1,550) is more recent but has
# no leader items, so it measures the structural market instead: voters
# who are economically right of centre (5+ on the 1-7 economic
# self-placement scale) and not cold towards *either* PiS supporters or
# supporters of the centrist parties (30+ on the 0-100 thermometers).
# These "bridgers" sit between the two camps on the right, which is
# exactly the space a non-PiS centre-right party would contest.

# Fallback values, computed from the two files on 2026-07-24, used only
# if the Writing repo is not on this machine.
FALLBACK_POOLS <- tibble(
  party = c(
    "PiS",
    "KO",
    "Polska2050",
    "PSL",
    "Konfederacja",
    "KKP",
    "Lewica",
    "Razem"
  ),
  pool = c(0.162, 0.034, 0.103, 0.103, 0.129, 0.036, 0.000, 0.044),
  source = "fallback constants (2026-07-24)"
)

calibrate_pgsw <- function(dir = WRITING_DATA) {
  f <- file.path(dir, "PGSW", "PGSW master file.Rdata")
  if (!file.exists(f)) {
    return(NULL)
  }
  e <- new.env()
  load(f, envir = e)
  pgsw <- get(ls(e)[1], envir = e)

  pgsw %>%
    filter(year == 2023) %>%
    mutate(
      across(
        c(likeMora, likeKacz, likePiS, likePO, likeKonf, likeP2050, leftrt),
        ~ suppressWarnings(as.numeric(.x))
      ),
      group = as.character(votefor)
    ) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      # Availability to a Morawiecki-led party, strict and broad.
      mora_strict = weighted.mean(
        likeMora >= 6 & likeMora > likeKacz,
        weight,
        na.rm = TRUE
      ),
      mora_broad = weighted.mean(
        likeMora >= 6 & likeMora >= likePiS,
        weight,
        na.rm = TRUE
      ),
      # 0 = left, 10 = right: Lewica voters average 1.6, PiS voters 8.3.
      centre_right = weighted.mean(
        leftrt >= 6 & leftrt <= 8,
        weight,
        na.rm = TRUE
      ),
      .groups = "drop"
    )
}

calibrate_bdcs <- function(dir = WRITING_DATA) {
  f <- file.path(dir, "BDCS survey", "BDCS survey PL data cleaned.sav")
  if (!file.exists(f)) {
    return(NULL)
  }
  bdcs <- read_sav(f)

  bdcs %>%
    mutate(
      vote = as.character(as_label(q3_8)),
      w = as.numeric(age_pol_weights_trimmed),
      # 999 = don't know on the 0-100 thermometers.
      across(starts_with("q3_5_"), ~ ifelse(as.numeric(.x) > 100, NA, as.numeric(.x))),
      econ_lr = as.numeric(q3_3_r1)
    ) %>%
    mutate(
      centrist_warmth = rowMeans(cbind(q3_5_2, q3_5_3, q3_5_4), na.rm = TRUE),
      pis_warmth = q3_5_1,
      bridger = econ_lr >= 5 & pis_warmth >= 30 & centrist_warmth >= 30,
      group = case_when(
        str_detect(vote, "^Prawo") ~ "PiS",
        str_detect(vote, "^Koalicja") ~ "KO",
        # PSL (n = 25) and Polska 2050 (n = 63) are pooled: neither
        # subsample supports a separate estimate.
        str_detect(vote, "^Polska 2050|^Polskie Stronnictwo") ~ "TD",
        str_detect(vote, "^Nowa Lewica") ~ "Lewica",
        str_detect(vote, "^Razem") ~ "Razem",
        str_detect(vote, "Wolność") ~ "Konfederacja",
        str_detect(vote, "Korony") ~ "KKP",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>%
    summarise(
      n = n(),
      bridger = weighted.mean(bridger, w, na.rm = TRUE),
      econ_right = weighted.mean(econ_lr >= 5, w, na.rm = TRUE),
      warm_to_pis = weighted.mean(pis_warmth >= 40, w, na.rm = TRUE),
      .groups = "drop"
    )
}

# How much attitudinal availability actually turns into votes. The NCN
# Opus panel rates the parties on 0-10 scales and records vote intention
# in May 2023 (wave 1), then records the actual October 2023 vote in
# wave 3. For every ordered pair of parties it is therefore possible to
# ask: of the people who supported A but were open to B, how many ended
# up voting B five months later? That ratio is the yardstick against
# which the three conversion scenarios are read.
#
#   warm   = rates B at 6+ on the 0-10 scale
#   strict = rates B at 6+ *and* above their own party — the direct
#            analogue of the PGSW availability measure used for PiS
calibrate_realisation <- function(dir = WRITING_DATA) {
  w1_path <- file.path(dir, "NCN Opus panel surveys", "Panel survey wave 1.rds")
  w3_path <- file.path(dir, "NCN Opus panel surveys", "Panel survey wave 3.rds")
  if (!file.exists(w1_path) || !file.exists(w3_path)) {
    return(NULL)
  }

  w1 <- readRDS(w1_path)
  w3 <- readRDS(w3_path)

  panel <- w1 %>%
    select(
      ID,
      prefpis,
      prefpo,
      preflew,
      prefpsl,
      prefkonf,
      elecvote_1
    ) %>%
    inner_join(
      w3 %>% select(ID, votedfor2023, waga_3_trimmed),
      by = "ID"
    ) %>%
    mutate(
      across(starts_with("pref"), ~ suppressWarnings(as.numeric(.x))),
      from = as.character(as_label(elecvote_1)),
      to = as.character(as_label(votedfor2023)),
      w = as.numeric(waga_3_trimmed)
    ) %>%
    filter(!is.na(from), !is.na(to), !is.na(w))

  # PSL's rating stands in for Trzecia Droga, the list it contested on.
  pref_of <- c(
    PiS = "prefpis",
    KO = "prefpo",
    Lewica = "preflew",
    `Trzecia Droga` = "prefpsl",
    Konfederacja = "prefkonf"
  )
  panel <- panel %>% filter(from %in% names(pref_of), to %in% names(pref_of))

  pairs <- expand_grid(A = names(pref_of), B = names(pref_of)) %>%
    filter(A != B)

  rates <- pmap_dfr(pairs, function(A, B) {
    d <- panel %>% filter(from == A)
    pa <- d[[pref_of[[A]]]]
    pb <- d[[pref_of[[B]]]]
    warm <- !is.na(pb) & pb >= 6
    strict <- warm & !is.na(pa) & pb > pa
    moved <- d$to == B

    tibble(
      from = A,
      to = B,
      n_warm = sum(warm),
      moved_warm = sum(d$w[warm & moved]),
      avail_warm = sum(d$w[warm]),
      n_strict = sum(strict),
      moved_strict = sum(d$w[strict & moved]),
      avail_strict = sum(d$w[strict])
    )
  })

  # Pooled over pairs with enough cases to be worth reading, which is
  # also the only level at which the strict measure has any precision.
  usable <- rates %>% filter(n_warm >= 15)
  pooled_warm <- sum(usable$moved_warm) / sum(usable$avail_warm)
  pooled_strict <- sum(usable$moved_strict) / sum(usable$avail_strict)

  list(
    rates = rates,
    pooled_warm = pooled_warm,
    pooled_strict = pooled_strict,
    n_pairs = nrow(usable),
    n_respondents = nrow(panel)
  )
}

pgsw_pools <- calibrate_pgsw()
bdcs_pools <- calibrate_bdcs()
realisation_cal <- calibrate_realisation()

# The panel rate does not set any scenario; it is the yardstick against
# which the three scenarios are read.
REALISATION_OBSERVED <- if (is.null(realisation_cal)) {
  NA_real_
} else {
  realisation_cal$pooled_strict
}

if (is.null(pgsw_pools) || is.null(bdcs_pools)) {
  warning(
    "Writing repo survey data not found at ",
    WRITING_DATA,
    " — using fallback pool constants."
  )
  splinter_pools <- FALLBACK_POOLS
} else {
  bdcs_lookup <- setNames(bdcs_pools$bridger, bdcs_pools$group)
  pgsw_lookup <- setNames(pgsw_pools$mora_strict, pgsw_pools$group)

  splinter_pools <- tibble(
    party = c(
      "PiS",
      "KO",
      "Polska2050",
      "PSL",
      "Konfederacja",
      "KKP",
      "Lewica",
      "Razem"
    ),
    bdcs = c(
      bdcs_lookup[["PiS"]],
      bdcs_lookup[["KO"]],
      bdcs_lookup[["TD"]],
      bdcs_lookup[["TD"]],
      bdcs_lookup[["Konfederacja"]],
      bdcs_lookup[["KKP"]],
      bdcs_lookup[["Lewica"]],
      bdcs_lookup[["Razem"]]
    ),
    # Morawiecki's 2023 ratings outside the PiS electorate are
    # contaminated by his role as PiS prime minister, so the
    # leader-specific reading is used only for PiS itself.
    pgsw = c(pgsw_lookup[["PiS"]], rep(NA_real_, 7))
  ) %>%
    mutate(
      pool = if_else(is.na(pgsw), bdcs, (bdcs + pgsw) / 2),
      source = "PGSW 2023 + BDCS Dec 2025"
    )
}

# Roughly 40 of PiS's 190 MPs left, i.e. 21% of the parliamentary party
# — an organisational anchor close to the 21% of PiS voters who in 2023
# both liked Morawiecki and preferred him to Kaczynski.
MP_DEFECTION_SHARE <- 40 / 190

saveRDS(splinter_pools, here("data", "splinter_pools.rds"))

#####Read in, adjust and subset data#####
source(here("R", "poll_data_scraper.R"))

polls <- polls_cleaned %>%
  select(startDate, endDate, org, all_of(PARTY_COLS[1:8]), Other, DK) %>%
  mutate(
    org = as.factor(org),
    startDate = as.Date(startDate),
    endDate = as.Date(endDate),
    midDate = as.Date(
      startDate + (difftime(endDate, startDate, units = "days") / 2)
    ),
    midDate_int = as.integer(midDate)
  ) %>%
  filter(midDate >= as.Date('2023-10-15'))

polls <- polls %>%
  mutate(across(all_of(PARTY_COLS[1:8]), ~ 100 / ((100 - DK)) * .x))

polls <- polls %>%
  mutate(
    time = as.integer(difftime(midDate, min(midDate), units = "days")),
    pollster = as.integer(factor(org)),
    time = interval(min(midDate), midDate) / years(1)
  )

polls <- polls %>%
  mutate(across(
    all_of(PARTY_COLS[1:8]),
    ~ as.numeric(str_remove(as.character(.x), "%")) / 100
  ))

polls <- polls %>%
  mutate(
    Other = pmax(1 - rowSums(across(all_of(PARTY_COLS[1:8]))), TINY_CONSTANT),
    check = rowSums(across(all_of(PARTY_COLS)))
  )

polls <- apply_threshold_and_normalize(polls, PARTY_COLS)

weights <- read_excel(here("data-raw", "2023_elec_percentages.xlsx"))
weights_c <- weights %>% filter(okreg != 0) %>% arrange(okreg)

#####Run model#####
# Same specification as PTP.R, cached so repeated scenario work does not
# refit; brms refits automatically when the poll data change.
dir.create(here("data", "models"), showWarnings = FALSE, recursive = TRUE)
options(brms.file_refit = "on_change")

m1 <- brm(
  formula = bf(
    outcome ~ 1 + s(time, k = 8, bs = "cs", m = 2) + (1 | pollster)
  ),
  family = dirichlet(link = "logit", refcat = "Other"),
  prior = prior(normal(0, 1.5), class = "Intercept", dpar = "muPiS") +
    prior(exponential(3), class = "sd", dpar = "muPiS") +
    prior(exponential(3), class = "sds", dpar = "muPiS") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKO") +
    prior(exponential(3), class = "sd", dpar = "muKO") +
    prior(exponential(3), class = "sds", dpar = "muKO") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muLewica") +
    prior(exponential(3), class = "sd", dpar = "muLewica") +
    prior(exponential(3), class = "sds", dpar = "muLewica") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muRazem") +
    prior(exponential(3), class = "sd", dpar = "muRazem") +
    prior(exponential(3), class = "sds", dpar = "muRazem") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muPolska2050") +
    prior(exponential(3), class = "sd", dpar = "muPolska2050") +
    prior(exponential(3), class = "sds", dpar = "muPolska2050") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muPSL") +
    prior(exponential(3), class = "sd", dpar = "muPSL") +
    prior(exponential(3), class = "sds", dpar = "muPSL") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
    prior(exponential(3), class = "sd", dpar = "muKonfederacja") +
    prior(exponential(3), class = "sds", dpar = "muKonfederacja") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKKP") +
    prior(exponential(3), class = "sd", dpar = "muKKP") +
    prior(exponential(3), class = "sds", dpar = "muKKP") +
    prior(gamma(2, 0.1), class = "phi"),
  data = polls,
  seed = 780045,
  iter = 3000,
  warmup = 2000,
  backend = "cmdstanr",
  threads = threading(threads_per_chain),
  chains = n_chains,
  cores = n_chains,
  refresh = 5,
  control = list(adapt_delta = .99, max_treedepth = 17),
  file = here("data", "models", "splinter_base_model")
)

# Consensus prediction: the trend averaged over the observed pollster
# houses rather than the population hyper-mean (see PTP.R for why).
consensus_epred <- function(object, newdata, ...) {
  pollsters <- sort(unique(polls$pollster))
  newdata <- newdata %>% mutate(.obs = row_number())
  crossing(newdata, pollster = pollsters) %>%
    add_epred_draws(object = object, newdata = ., re_formula = NULL, ...) %>%
    ungroup() %>%
    group_by(across(c(
      all_of(setdiff(names(newdata), ".obs")),
      ".obs",
      ".category",
      ".draw"
    ))) %>%
    summarise(.epred = mean(.epred), .groups = "drop") %>%
    select(-.obs) %>%
    group_by(.category)
}

today <- interval(min(polls$midDate), Sys.Date()) / years(1)

# Posterior draws of the national vote shares, one row per draw.
national_draws <- consensus_epred(
  object = m1,
  newdata = tibble(time = today),
  ndraws = N_DRAWS_MAIN
) %>%
  ungroup() %>%
  select(.draw, .category, .epred) %>%
  pivot_wider(names_from = .category, values_from = .epred) %>%
  arrange(.draw)

shares_mat <- as.matrix(national_draws[, DONORS])

#####Counterfactual machinery#####
# Constituency-level votes for each donor: valid votes x national share x
# the party's 2023 regional coefficient. The coefficients are normalised
# so their vote-weighted mean is 1, so aggregating back up reproduces the
# national share exactly.
coef_mat <- sapply(DONORS, function(p) {
  if (p == "Other") rep(1, nrow(weights_c)) else weights_c[[COEF_MAP[[p]]]]
})
base_c <- weights_c$validvotes * coef_mat
total_valid <- sum(weights_c$validvotes)
ko_profile <- weights_c$validvotes * weights_c$KOcoef
ko_profile <- ko_profile / sum(ko_profile)
# The German minority list contests Opole only and is exempt from the
# threshold. As in PTP.R its vote is added on top of the modelled
# shares, which double counts it very slightly inside "Other".
mn_votes <- ifelse(
  weights_c$okreg == MN_OKREG,
  weights_c$validvotes * MN_SHARE,
  0
)

# One counterfactual election per posterior draw.
#   shares:       draws x donors matrix of national vote shares
#   transfers:    draws x donors matrix of vote shares lost to the splinter
#   mobilisation: extra national share won from outside the party system
simulate_election <- function(
  shares,
  transfers,
  mobilisation = rep(0, nrow(shares)),
  urban_tilt = URBAN_TILT,
  threshold = THRESHOLD
) {
  n_draws <- nrow(shares)
  seats <- matrix(
    0L,
    nrow = n_draws,
    ncol = length(SEAT_PARTIES),
    dimnames = list(NULL, SEAT_PARTIES)
  )
  votes <- matrix(
    0,
    nrow = n_draws,
    ncol = length(SEAT_PARTIES),
    dimnames = list(NULL, SEAT_PARTIES)
  )
  # Where the new party's vote comes from: national vote share drawn
  # from each donor, plus the share won from outside the party system.
  sources <- matrix(
    0,
    nrow = n_draws,
    ncol = length(DONORS) + 1,
    dimnames = list(NULL, c(DONORS, "Mobilised"))
  )

  for (d in seq_len(n_draws)) {
    s <- shares[d, ]
    tr <- transfers[d, ]

    donor_c <- sweep(base_c, 2, s * (1 - tr), "*")
    splinter_c <- rowSums(sweep(base_c, 2, s * tr, "*"))

    # Mobilised voters follow the splinter's own regional profile.
    if (sum(splinter_c) > 0 && mobilisation[d] > 0) {
      splinter_c <- splinter_c *
        (1 + mobilisation[d] * total_valid / sum(splinter_c))
    }

    # Tilt the profile towards KO's: a moderate breakaway should travel
    # better than PiS in the cities and the west.
    if (sum(splinter_c) > 0 && urban_tilt > 0) {
      splinter_c <- (1 - urban_tilt) * splinter_c +
        urban_tilt * sum(splinter_c) * ko_profile
    }

    v_c <- cbind(
      donor_c[, setdiff(DONORS, "Other"), drop = FALSE],
      Splinter = splinter_c,
      MN = mn_votes
    )

    # National shares are computed on the enlarged electorate, so
    # mobilisation dilutes the existing parties rather than being free.
    denom <- total_valid * (1 + mobilisation[d])
    nat <- colSums(v_c) / denom
    votes[d, ] <- nat[SEAT_PARTIES]
    sources[d, ] <- c(s * tr, mobilisation[d]) / (1 + mobilisation[d])

    # 5% national threshold; the German minority list (MN) is exempt.
    below <- names(nat)[nat < threshold & names(nat) != "MN"]
    if (length(below) > 0) {
      v_c[, below] <- 0
    }

    v_c <- v_c[, SEAT_PARTIES, drop = FALSE]
    alloc <- integer(length(SEAT_PARTIES))
    for (k in seq_len(nrow(v_c))) {
      alloc <- alloc + dhondt(v_c[k, ], weights_c$magnitude[k])
    }
    seats[d, ] <- alloc
  }

  list(seats = seats, votes = votes, sources = sources)
}

# The fast D'Hondt must agree with seatdist on a real allocation.
local({
  v_check <- (colMeans(shares_mat) * 100)[setdiff(DONORS, "Other")]
  v_check <- weights_c$validvotes[1] * v_check
  stopifnot(identical(
    as.integer(dhondt(v_check, weights_c$magnitude[1])),
    as.integer(giveseats(
      v = v_check,
      ns = weights_c$magnitude[1],
      method = "dh",
      thresh = 0
    )$seats)
  ))
})

# Transfer-rate draws for a given conversion rate. Rates above one mean
# the party outgrows the pool the surveys measure, which is what any
# breakthrough scenario requires; TRANSFER_CAP keeps that from implying
# that a party loses most of its electorate.
make_transfers <- function(
  n_draws,
  realisation,
  pools = splinter_pools,
  cap = TRANSFER_CAP
) {
  tr <- matrix(
    0,
    nrow = n_draws,
    ncol = length(DONORS),
    dimnames = list(NULL, DONORS)
  )
  for (p in pools$party) {
    mu <- pmin(cap, realisation * pools$pool[pools$party == p])
    tr[, p] <- rbeta_mean(n_draws, mu)
  }
  tr
}

#####Baseline: the party system as it stands#####
baseline <- simulate_election(
  shares = shares_mat,
  transfers = matrix(
    0,
    nrow = nrow(shares_mat),
    ncol = length(DONORS),
    dimnames = list(NULL, DONORS)
  ),
  mobilisation = rep(0, nrow(shares_mat)),
  urban_tilt = 0
)

#####The three scenarios#####
# Each scenario fixes the conversion rate and lets the poll posterior and
# the transfer draws supply the uncertainty, so the three are directly
# comparable: the only thing that differs between them is how much of
# the available pool the new party converts.
run_conversion <- function(realisation) {
  n <- nrow(shares_mat)
  simulate_election(
    shares = shares_mat,
    transfers = make_transfers(n, realisation),
    mobilisation = pmax(
      0,
      rnorm(
        n,
        MOBILISATION_MEAN * realisation,
        MOBILISATION_SD * realisation
      )
    )
  )
}

scenario_runs <- set_names(
  map(SCENARIO_LEVELS, run_conversion),
  SCENARIO_LABELS
)

summarise_run <- function(run, label) {
  votes_long <- as_tibble(run$votes) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(-.draw, names_to = "party", values_to = "vote")
  seats_long <- as_tibble(run$seats) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(-.draw, names_to = "party", values_to = "seats")

  votes_long %>%
    left_join(seats_long, by = c(".draw", "party")) %>%
    group_by(party) %>%
    summarise(
      vote_med = median(vote) * 100,
      vote_lo = quantile(vote, 0.10) * 100,
      vote_hi = quantile(vote, 0.90) * 100,
      seats_med = round(median(seats)),
      seats_lo = round(quantile(seats, 0.10)),
      seats_hi = round(quantile(seats, 0.90)),
      prob_threshold = mean(vote >= THRESHOLD),
      .groups = "drop"
    ) %>%
    mutate(scenario = label)
}

baseline_summary <- summarise_run(baseline, "No split")
scenario_summaries <- imap_dfr(scenario_runs, ~ summarise_run(.x, .y))

# Blocs, for the coalition arithmetic that follows.
bloc_seats <- function(run, parties) {
  rowSums(run$seats[, parties, drop = FALSE])
}

GOVT_BLOC <- c("KO", "Polska2050", "PSL", "Lewica")
RIGHT_BLOC <- c("PiS", "Konfederacja", "KKP")

# Per-draw bloc arithmetic for each scenario, against the no-split
# baseline: what the split costs the right, and whether it leaves the
# new party holding the balance.
scenario_blocs <- imap_dfr(scenario_runs, function(run, label) {
  tibble(
    scenario = label,
    .draw = seq_len(nrow(shares_mat)),
    splinter_vote = run$votes[, "Splinter"],
    splinter_seats = run$seats[, "Splinter"],
    pis_seats = run$seats[, "PiS"],
    pis_seats_base = baseline$seats[, "PiS"],
    right_base = bloc_seats(baseline, RIGHT_BLOC),
    right_split = bloc_seats(run, RIGHT_BLOC),
    right_split_incl = bloc_seats(run, c(RIGHT_BLOC, "Splinter")),
    govt_base = bloc_seats(baseline, GOVT_BLOC),
    govt_split = bloc_seats(run, GOVT_BLOC),
    govt_split_incl = bloc_seats(run, c(GOVT_BLOC, "Splinter")),
    ko_splinter = run$seats[, "KO"] + run$seats[, "Splinter"]
  ) %>%
    mutate(
      cleared = splinter_vote >= THRESHOLD,
      right_cost = right_split_incl - right_base,
      pis_cost = pis_seats - pis_seats_base,
      # Pivotal: neither bloc can govern alone, but either could with the
      # new party on board.
      pivotal = right_split < MAJORITY &
        govt_split < MAJORITY &
        (right_split_incl >= MAJORITY | govt_split_incl >= MAJORITY)
    )
})

# Where each scenario's vote comes from, in points of the national vote,
# and what each donor gives up as a share of its own electorate.
scenario_sources <- imap_dfr(scenario_runs, function(run, label) {
  tibble(
    scenario = label,
    source = c(DONORS, "Mobilised"),
    vote_pp = apply(run$sources, 2, median) * 100
  ) %>%
    left_join(
      tibble(
        source = DONORS,
        donor_share = apply(shares_mat, 2, median) * 100
      ),
      by = "source"
    ) %>%
    mutate(
      share_of_donor = if_else(
        is.na(donor_share),
        NA_real_,
        vote_pp / donor_share
      )
    )
})

#####Scenario grid: how much of the pool does the party convert?#####
grid_draws_idx <- seq_len(min(N_DRAWS_GRID, nrow(shares_mat)))
shares_grid <- shares_mat[grid_draws_idx, , drop = FALSE]

scenarios <- map_dfr(REALISATION_GRID, function(r) {
  tr <- make_transfers(nrow(shares_grid), r)
  mob <- pmax(
    0,
    rnorm(nrow(shares_grid), MOBILISATION_MEAN * r, MOBILISATION_SD * r)
  )
  run <- simulate_election(shares_grid, tr, mob)

  tibble(
    realisation = r,
    pis_transfer = r * splinter_pools$pool[splinter_pools$party == "PiS"],
    splinter_vote = median(run$votes[, "Splinter"]) * 100,
    splinter_vote_lo = quantile(run$votes[, "Splinter"], 0.10) * 100,
    splinter_vote_hi = quantile(run$votes[, "Splinter"], 0.90) * 100,
    prob_clears = mean(run$votes[, "Splinter"] >= THRESHOLD),
    splinter_seats = median(run$seats[, "Splinter"]),
    pis_seats = median(run$seats[, "PiS"]),
    ko_seats = median(run$seats[, "KO"]),
    right_seats = median(rowSums(run$seats[, RIGHT_BLOC, drop = FALSE])),
    right_incl_seats = median(rowSums(
      run$seats[, c(RIGHT_BLOC, "Splinter"), drop = FALSE]
    )),
    govt_seats = median(rowSums(run$seats[, GOVT_BLOC, drop = FALSE])),
    prob_right_majority = mean(
      rowSums(run$seats[, c(RIGHT_BLOC, "Splinter"), drop = FALSE]) >= MAJORITY
    ),
    prob_right_majority_excl = mean(
      rowSums(run$seats[, RIGHT_BLOC, drop = FALSE]) >= MAJORITY
    ),
    prob_govt_majority = mean(
      rowSums(run$seats[, GOVT_BLOC, drop = FALSE]) >= MAJORITY
    )
  )
})

saveRDS(
  list(
    pools = splinter_pools,
    pgsw = pgsw_pools,
    bdcs = bdcs_pools,
    realisation = realisation_cal,
    baseline = baseline_summary,
    scenario_summaries = scenario_summaries,
    scenario_blocs = scenario_blocs,
    scenario_sources = scenario_sources,
    grid = scenarios,
    parameters = list(
      scenario_levels = SCENARIO_LEVELS,
      scenario_labels = SCENARIO_LABELS,
      observed_conversion = REALISATION_OBSERVED,
      transfer_kappa = TRANSFER_KAPPA,
      mobilisation_mean = MOBILISATION_MEAN,
      urban_tilt = URBAN_TILT,
      threshold = THRESHOLD,
      date = Sys.Date()
    )
  ),
  here("data", "splinter_results.rds")
)

#####Figure: the pool of available voters#####
pool_plot_data <- splinter_pools %>%
  mutate(
    party = factor(
      DISPLAY_NAMES[party],
      levels = DISPLAY_NAMES[splinter_pools$party[order(splinter_pools$pool)]]
    )
  )

pools_fig <- ggplot(pool_plot_data, aes(x = pool, y = party, fill = party)) +
  geom_col(width = 0.6, show.legend = FALSE) +
  geom_text(
    aes(label = pl_pct(pool, accuracy = 0.1)),
    hjust = -0.2,
    size = 3,
    family = "Jost"
  ) +
  scale_fill_manual(values = PARTY_COLORS) +
  scale_x_continuous(
    labels = pl_pct_axis(),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    x = "Odsetek wyborców partii dostępnych dla centroprawicowego rozłamu",
    y = "",
    title = "Do ilu wyborców każdej partii mógłby dotrzeć umiarkowany rozłam z PiS",
    subtitle = str_wrap(
      paste(
        "Dostępni to w przypadku PiS wyborcy, którzy lubią Morawieckiego i wolą",
        "go od Kaczyńskiego, oraz ci o gospodarczo prawicowych poglądach, którzy",
        "nie są chłodni ani wobec PiS, ani wobec partii centrowych; w przypadku",
        "pozostałych partii — wyłącznie ta druga grupa. To górna granica",
        "możliwości nowej partii, a nie prognoza jej wyniku."
      ),
      width = 110
    ),
    caption = ""
  ) +
  theme_plots()

ggsave(
  pools_fig,
  file = here("figures", "splinter_pools.png"),
  width = 7,
  height = 4,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Figure: counterfactual vote shares#####
make_vote_figure <- function(run, title, subtitle) {
  vote_draws <- as_tibble(run$votes) %>%
    mutate(.draw = row_number()) %>%
    pivot_longer(-.draw, names_to = "party", values_to = "vote") %>%
    filter(party != "MN") %>%
    mutate(
      party = factor(DISPLAY_NAMES[party], levels = unname(DISPLAY_NAMES))
    ) %>%
    droplevels()

  vote_medians <- vote_draws %>%
    group_by(party) %>%
    summarise(est = median(vote) * 100, .groups = "drop")

  vote_draws %>%
    ggplot(aes(
      y = reorder(party, vote),
      x = vote,
      color = party
    )) +
    geom_vline(
      xintercept = THRESHOLD,
      color = "grey60",
      linetype = "dashed",
      linewidth = 0.5
    ) +
    stat_interval(
      aes(x = vote, color_ramp = after_stat(.width)),
      .width = ppoints(100)
    ) %>%
      partition(vars(party)) +
    scale_color_manual(name = " ", values = PARTY_COLORS, guide = "none") +
    ggdist::scale_color_ramp_continuous(range = c(1, 0), guide = "none") +
    scale_y_discrete(name = "") +
    geom_text(
      data = vote_medians,
      aes(y = party, x = est / 100, label = pl_num(est)),
      size = 3.5,
      hjust = 0.5,
      vjust = -1,
      family = "Jost",
      inherit.aes = FALSE
    ) +
    scale_x_continuous(
      breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4),
      labels = c("0", "5", "10", "20", "30", "40")
    ) +
    expand_limits(x = 0) +
    labs(
      y = "",
      x = "",
      title = title,
      subtitle = str_wrap(subtitle, width = 110),
      caption = "Liczby to mediany; szerokość pasma pokazuje niepewność oszacowania."
    ) +
    theme_plots()
}

iwalk(scenario_runs, function(run, label) {
  fig <- make_vote_figure(
    run,
    paste0("Poparcie partii w scenariuszu: ", tolower(label)),
    paste0(
      "Nowa partia pozyskuje ",
      pl_pct(SCENARIO_LEVELS[match(label, SCENARIO_LABELS)]),
      " dostępnej jej puli wyborców w każdym elektoracie. Linia przerywana: próg 5%."
    )
  )
  ggsave(
    fig,
    file = here(
      "figures",
      paste0(
        "splinter_vote_",
        round(SCENARIO_LEVELS[match(label, SCENARIO_LABELS)] * 100),
        ".png"
      )
    ),
    width = 7,
    height = 5,
    units = "cm",
    dpi = 600,
    scale = 3,
    bg = "white"
  )
})

# The headline comparison: the new party's own vote in each scenario.
splinter_vote_draws <- imap_dfr(scenario_runs, function(run, label) {
  tibble(scenario = label, vote = run$votes[, "Splinter"])
}) %>%
  mutate(scenario = factor(scenario, levels = SCENARIO_LABELS))

splinter_vote_stats <- splinter_vote_draws %>%
  group_by(scenario) %>%
  summarise(
    est = median(vote) * 100,
    prob = mean(vote >= THRESHOLD),
    .groups = "drop"
  )

scenario_vote_fig <- splinter_vote_draws %>%
  ggplot(aes(y = fct_rev(scenario), x = vote)) +
  geom_vline(
    xintercept = THRESHOLD,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  stat_interval(
    aes(color_ramp = after_stat(.width)),
    .width = ppoints(100),
    colour = PARTY_COLORS[["Rozłamowcy z PiS"]]
  ) +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide = "none") +
  geom_text(
    data = splinter_vote_stats,
    aes(
      y = scenario,
      x = est / 100,
      label = paste0(pl_num(est), "%")
    ),
    size = 3.5,
    vjust = -1.2,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_text(
    data = splinter_vote_stats,
    aes(
      y = scenario,
      x = 0,
      label = paste0("szansa na przekroczenie progu: ", pl_num(prob * 100, 0), "%")
    ),
    size = 2.8,
    hjust = 0,
    vjust = 2.6,
    colour = "grey30",
    family = "Jost",
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = c(0, 0.02, 0.05, 0.08, 0.1, 0.12),
    labels = function(x) paste0(pl_axis_num(x * 100), "%")
  ) +
  expand_limits(x = 0) +
  labs(
    y = "",
    x = "",
    title = "Wynik nowej partii w trzech scenariuszach",
    subtitle = str_wrap(
      paste(
        "Scenariusze różnią się wyłącznie tym, jaką część dostępnej puli",
        "wyborców nowa partia rzeczywiście pozyskuje. Linia przerywana:",
        "próg 5%."
      ),
      width = 110
    ),
    caption = "Liczby to mediany; szerokość pasma pokazuje niepewność oszacowania."
  ) +
  theme_plots()

ggsave(
  scenario_vote_fig,
  file = here("figures", "splinter_scenario_vote.png"),
  width = 7,
  height = 4,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Figure: seats#####
build_coalitions_static <- function(get_seats_fn) {
  short_names <- c(
    "KO" = "KO",
    "Polska2050" = "P2050",
    "Lewica" = "Lewica",
    "PSL" = "PSL",
    "PiS" = "PiS",
    "Splinter" = "Rozłam",
    "Konfederacja" = "Konf.",
    "KKP" = "KKP",
    "Razem" = "Razem"
  )
  all_parties <- names(short_names)
  active_parties <- all_parties[sapply(all_parties, function(p) {
    get_seats_fn(p) > 0
  })]
  active_parties <- active_parties[order(-sapply(active_parties, get_seats_fn))]

  # As in app.R, plus the splinter: a party ejected from PiS over its
  # direction is not a plausible partner for Razem, and the mutual
  # animosity of the split makes a PiS-splinter reunion a coalition of
  # last resort rather than an obvious one — it is retained here, but
  # flagged in the console summary.
  forbidden <- list(
    c("Konfederacja", "Lewica"),
    c("Konfederacja", "Razem"),
    c("KKP", "Lewica"),
    c("KKP", "Razem"),
    c("KKP", "KO"),
    c("PiS", "KO"),
    c("PiS", "Lewica"),
    c("Splinter", "Razem")
  )
  is_compatible <- function(parties) {
    for (fp in forbidden) {
      if (all(fp %in% parties)) {
        return(FALSE)
      }
    }
    TRUE
  }

  majority_parties <- active_parties[sapply(active_parties, function(p) {
    get_seats_fn(p) >= MAJORITY
  })]
  coalitions <- list()
  for (p in majority_parties) {
    coalitions[[length(coalitions) + 1]] <- list(
      name = short_names[p],
      seats = get_seats_fn(p)
    )
  }
  if (length(active_parties) >= 2) {
    for (size in 2:length(active_parties)) {
      for (combo in combn(active_parties, size, simplify = FALSE)) {
        if (any(majority_parties %in% combo)) {
          next
        }
        if (!is_compatible(combo)) {
          next
        }
        total <- sum(sapply(combo, get_seats_fn))
        if (total >= MAJORITY) {
          coalitions[[length(coalitions) + 1]] <- list(
            name = paste(short_names[combo], collapse = " + "),
            seats = total
          )
        }
      }
    }
  }
  coalitions[order(-sapply(coalitions, function(x) x$seats))]
}

seat_frame <- function(summary_df) {
  summary_df %>%
    filter(party != "MN") %>%
    transmute(
      party_key = party,
      party = factor(DISPLAY_NAMES[party], levels = DISPLAY_NAMES),
      y = seats_med,
      ymin = seats_lo,
      ymax = seats_hi,
      in2023 = SEATS_2023[party_key],
      diffPres = sprintf("(%+d)", y - in2023)
    ) %>%
    mutate(party = reorder(party, -y))
}

coalitions_for <- function(frame) {
  build_coalitions_static(function(p) {
    v <- frame$y[frame$party_key == p]
    if (length(v) == 0) 0L else v[[1]]
  })
}

coalition_labels <- function(coalitions) {
  if (length(coalitions) == 0) {
    return(data.frame(x = numeric(0), y = numeric(0), label = character(0)))
  }
  seats <- sapply(coalitions, `[[`, "seats")
  y <- seats
  for (i in seq_along(y)[-1]) {
    if (y[i - 1] - y[i] < 15) y[i] <- y[i - 1] - 15
  }
  data.frame(
    x = 5,
    y = y,
    label = paste0(
      sapply(coalitions, `[[`, "name"),
      ": ",
      seats,
      " mandatów"
    ),
    stringsAsFactors = FALSE
  )
}

make_seats_figure <- function(frame, title, subtitle, coalition_label_df) {
  ggplot(
    data = frame,
    mapping = aes(x = party, y = y, fill = party)
  ) +
  geom_bar(stat = "identity", width = .75, show.legend = FALSE) +
  geom_hline(
    yintercept = c(MAJORITY, VETO_MAJORITY, CONSTITUTIONAL_MAJORITY),
    colour = "gray10",
    linetype = 3
  ) +
  # "Rozłamowcy z PiS" is long enough to collide with its neighbour.
  scale_x_discrete(labels = function(x) str_wrap(x, width = 12)) +
  scale_y_continuous(
    'Liczba mandatów',
    limits = c(0, 320),
    breaks = c(0, 50, 100, 150, 200, MAJORITY, VETO_MAJORITY, CONSTITUTIONAL_MAJORITY)
  ) +
  scale_fill_manual(name = "Partia", values = PARTY_COLORS) +
  geom_label(
    data = data.frame(x = 2, y = MAJORITY, label = "Większość sejmowa"),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = "grey95",
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = coalition_label_df,
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.3,
    fill = "white",
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_text(
    aes(x = as.numeric(party) - 0.03, y = y + 18, label = y),
    size = 3,
    family = "Jost",
    hjust = 1
  ) +
  geom_text(
    aes(x = as.numeric(party) + 0.03, y = y + 18, label = diffPres),
    size = 2.5,
    family = "Jost",
    fontface = "italic",
    hjust = 0
  ) +
  geom_text(
    aes(x = party, y = y + 8, label = paste0("(", ymin, "–", ymax, ")")),
    size = 2,
    family = "Jost"
  ) +
    labs(
      x = "",
      y = "Liczba mandatów",
      title = title,
      subtitle = str_wrap(subtitle, width = 120),
      caption = "Wartości w nawiasach kursywą pokazują zmianę względem wyniku wyborów z 2023 roku; w nawiasach pod nimi — 80-procentowe przedziały wiarygodności."
    ) +
    theme_plots()
}

# One seat chart per scenario, plus the coalitions each one permits.
scenario_frames <- map(SCENARIO_LABELS, function(label) {
  seat_frame(scenario_summaries %>% filter(scenario == label))
}) %>%
  set_names(SCENARIO_LABELS)

scenario_coalitions <- map(scenario_frames, coalitions_for)

iwalk(scenario_frames, function(frame, label) {
  level <- SCENARIO_LEVELS[match(label, SCENARIO_LABELS)]
  vote <- scenario_summaries$vote_med[
    scenario_summaries$scenario == label &
      scenario_summaries$party == "Splinter"
  ]
  fig <- make_seats_figure(
    frame,
    paste0("Sejm w scenariuszu: ", tolower(label)),
    paste0(
      "Nowa partia pozyskuje ",
      pl_pct(level),
      " dostępnej jej puli, co daje jej ",
      pl_num(vote),
      "% głosów. Mediana szacowanej liczby mandatów; suma może nie wynosić 460."
    ),
    coalition_labels(scenario_coalitions[[label]])
  )
  ggsave(
    fig,
    file = here(
      "figures",
      paste0("splinter_seats_", round(level * 100), ".png")
    ),
    width = 7,
    height = 5,
    units = "cm",
    dpi = 600,
    scale = 3,
    bg = "white"
  )
})

#####Figure: the cost of the split#####
SERIES_RIGHT <- "Blok prawicy (z rozłamowcami)"
SERIES_GOVT <- "Blok rządzący"
SERIES_SPLINTER <- "Rozłamowcy z PiS"

scenario_long <- scenarios %>%
  select(
    splinter_vote,
    `PiS` = pis_seats,
    !!SERIES_SPLINTER := splinter_seats,
    `KO` = ko_seats,
    !!SERIES_RIGHT := right_incl_seats,
    !!SERIES_GOVT := govt_seats
  ) %>%
  pivot_longer(-splinter_vote, names_to = "series", values_to = "seats") %>%
  mutate(
    series = factor(
      series,
      levels = c(
        SERIES_RIGHT,
        SERIES_GOVT,
        "PiS",
        "KO",
        SERIES_SPLINTER
      )
    )
  )

scenario_fig <- ggplot(
  scenario_long,
  aes(x = splinter_vote, y = seats, colour = series, linetype = series)
) +
  geom_hline(yintercept = MAJORITY, linetype = 3, colour = "gray10") +
  geom_vline(xintercept = THRESHOLD * 100, linetype = 2, colour = "grey60") +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  annotate(
    geom = "text",
    x = THRESHOLD * 100,
    y = 120,
    label = "próg 5%",
    hjust = -0.1,
    size = 2.8,
    family = "Jost",
    colour = "grey40"
  ) +
  annotate(
    geom = "text",
    x = max(scenarios$splinter_vote),
    y = MAJORITY + 12,
    label = "większość sejmowa",
    hjust = 1,
    size = 2.8,
    family = "Jost",
    colour = "grey40"
  ) +
  scale_colour_manual(
    name = "",
    values = setNames(
      c("midnightblue", "orange", "blue", "goldenrod", "steelblue"),
      c(SERIES_RIGHT, SERIES_GOVT, "PiS", "KO", SERIES_SPLINTER)
    )
  ) +
  scale_linetype_manual(
    name = "",
    values = setNames(
      c("solid", "solid", "22", "22", "22"),
      c(SERIES_RIGHT, SERIES_GOVT, "PiS", "KO", SERIES_SPLINTER)
    )
  ) +
  scale_x_continuous(labels = function(x) paste0(pl_axis_num(x), "%")) +
  labs(
    x = "Poparcie dla nowej partii",
    y = "Liczba mandatów",
    title = "Koszt rozłamu",
    subtitle = str_wrap(
      paste(
        "Każdy punkt to scenariusz, w którym nowa partia pozyskuje inną część dostępnej",
        "puli wyborców — od zera (po lewej) do całości (po prawej).",
        "Blok prawicy = PiS + Konfederacja + KKP + rozłamowcy; blok rządzący =",
        "KO + Polska 2050 + PSL + Lewica."
      ),
      width = 110
    ),
    caption = "Mediany z rozkładów a posteriori. Poniżej progu 5% głosy nowej partii przepadają, a blok prawicy traci mandaty, nie zyskując żadnych."
  ) +
  theme_plots()

ggsave(
  scenario_fig,
  file = here("figures", "splinter_scenarios.png"),
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Figure: probability the new party clears the threshold#####
PROB_CLEARS <- "Nowa partia przekracza próg 5%"
PROB_RIGHT <- "Większość bloku prawicy"
PROB_GOVT <- "Większość bloku rządzącego"

threshold_fig <- scenarios %>%
  select(
    splinter_vote,
    !!PROB_CLEARS := prob_clears,
    !!PROB_RIGHT := prob_right_majority,
    !!PROB_GOVT := prob_govt_majority
  ) %>%
  pivot_longer(-splinter_vote, names_to = "series", values_to = "prob") %>%
  mutate(series = factor(series, levels = c(PROB_CLEARS, PROB_RIGHT, PROB_GOVT))) %>%
  ggplot(aes(x = splinter_vote, y = prob, colour = series)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_colour_manual(
    name = "",
    values = setNames(
      c("steelblue", "midnightblue", "orange"),
      c(PROB_CLEARS, PROB_RIGHT, PROB_GOVT)
    )
  ) +
  scale_x_continuous(labels = function(x) paste0(pl_axis_num(x), "%")) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = pl_pct_axis()
  ) +
  labs(
    x = "Poparcie dla nowej partii",
    y = "Prawdopodobieństwo",
    title = "Ryzyko progu wyborczego i arytmetyka koalicyjna",
    subtitle = str_wrap(
      paste(
        "Prawdopodobieństwa a posteriori w tych samych scenariuszach.",
        "Przetrwanie samej nowej partii jest osią, wokół której obraca się",
        "cała pozostała arytmetyka."
      ),
      width = 110
    ),
    caption = ""
  ) +
  theme_plots()

ggsave(
  threshold_fig,
  file = here("figures", "splinter_threshold.png"),
  width = 7,
  height = 4,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Figure: where the new party's vote comes from#####
# Stacked composition in each scenario: the mix is fixed by the pools,
# so what changes with the conversion rate is the size of the bar, not
# its shape — which is itself the finding.
sources_plot_data <- scenario_sources %>%
  filter(vote_pp > 0.02) %>%
  mutate(
    scenario = factor(scenario, levels = SCENARIO_LABELS),
    label_name = if_else(
      source == "Mobilised",
      "Nowi wyborcy",
      DISPLAY_NAMES[source]
    ),
    label_name = factor(
      label_name,
      levels = c(
        "PiS",
        "Konfederacja",
        "KKP",
        "KO",
        "Polska 2050",
        "PSL",
        "Razem",
        "Lewica",
        "Nowi wyborcy"
      )
    )
  )

SOURCE_COLORS <- c(PARTY_COLORS, "Nowi wyborcy" = "gray50")

scenario_sources_fig <- ggplot(
  sources_plot_data,
  aes(x = vote_pp, y = fct_rev(scenario), fill = label_name)
) +
  geom_col(width = 0.6) +
  geom_vline(
    xintercept = THRESHOLD * 100,
    colour = "grey30",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  scale_fill_manual(name = "", values = SOURCE_COLORS) +
  scale_x_continuous(
    labels = function(x) pl_axis_num(x),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(
    x = "Wynik nowej partii (punkty procentowe) i jego pochodzenie",
    y = "",
    title = "Skąd pochodzą głosy nowej partii",
    subtitle = str_wrap(
      paste(
        "Skład poparcia jest w każdym scenariuszu taki sam — zmienia się",
        "tylko jego wielkość. Ponad dwie trzecie głosów nowej partii",
        "pochodzi z dotychczasowej prawicy niezależnie od tego, jak dobrze",
        "sobie ona radzi. Linia przerywana: próg 5%."
      ),
      width = 110
    ),
    caption = ""
  ) +
  theme_plots()

ggsave(
  scenario_sources_fig,
  file = here("figures", "splinter_scenario_sources.png"),
  width = 7,
  height = 4,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Figure: where the new party would win its seats#####
const_map_path <- here("data", "const_map.rds")
if (file.exists(const_map_path)) {
  const_map <- readRDS(const_map_path)
} else {
  const <- st_read(
    here("data-raw", "GRED_20190215_Poland_2011.shp"),
    quiet = TRUE
  )
  const_id_map <- create_const_id_mapping()
  const_map <- const %>%
    select(cst, cst_n, geometry) %>%
    mutate(id = const_id_map$id[match(cst, const_id_map$cst)])
}

# Constituency seats from a single allocation at the posterior median,
# for the middle scenario.
MAP_LEVEL <- SCENARIO_LEVELS[2]
median_shares <- matrix(
  apply(shares_mat, 2, median),
  nrow = 1,
  dimnames = list(NULL, DONORS)
)
median_transfers <- matrix(
  MAP_LEVEL * splinter_pools$pool[match(DONORS, splinter_pools$party)],
  nrow = 1,
  dimnames = list(NULL, DONORS)
)
median_transfers[is.na(median_transfers)] <- 0

const_seats <- local({
  s <- median_shares[1, ]
  tr <- median_transfers[1, ]
  donor_c <- sweep(base_c, 2, s * (1 - tr), "*")
  splinter_c <- rowSums(sweep(base_c, 2, s * tr, "*"))
  splinter_c <- splinter_c *
    (1 + MOBILISATION_MEAN * MAP_LEVEL * total_valid / sum(splinter_c))
  splinter_c <- (1 - URBAN_TILT) * splinter_c +
    URBAN_TILT * sum(splinter_c) * ko_profile

  v_c <- cbind(
    donor_c[, setdiff(DONORS, "Other"), drop = FALSE],
    Splinter = splinter_c,
    MN = mn_votes
  )
  nat <- colSums(v_c) / (total_valid * (1 + MOBILISATION_MEAN * MAP_LEVEL))
  below <- names(nat)[nat < THRESHOLD & names(nat) != "MN"]
  if (length(below) > 0) v_c[, below] <- 0
  v_c <- v_c[, SEAT_PARTIES, drop = FALSE]

  alloc <- t(sapply(seq_len(nrow(v_c)), function(k) {
    dhondt(v_c[k, ], weights_c$magnitude[k])
  }))
  colnames(alloc) <- SEAT_PARTIES
  as_tibble(alloc) %>% mutate(id = weights_c$okreg)
})

saveRDS(const_seats, here("data", "splinter_constituency_seats.rds"))

label_points <- st_point_on_surface(const_map) %>% arrange(id)
label_points <- st_coordinates(label_points) %>%
  as_tibble() %>%
  mutate(id = sort(const_map$id))
colnames(label_points) <- c("x", "y", "id")

map_data <- const_map %>%
  left_join(const_seats, by = "id") %>%
  left_join(label_points, by = "id")

splinter_map <- ggplot(map_data) +
  geom_sf(aes(fill = as.integer(Splinter))) +
  theme(aspect.ratio = 1) +
  geom_label(
    aes(x = x, y = y, group = Splinter, label = Splinter),
    fill = "white"
  ) +
  scale_fill_gradient(
    name = "Rozłamowcy z PiS",
    # A small party wins at most a seat or two per constituency, so a
    # fixed 0-20 scale as in PTP.R would render the map blank.
    limits = c(0, max(2, max(map_data$Splinter, na.rm = TRUE))),
    low = "white",
    high = "steelblue",
    guide = "colorbar"
  ) +
  labs(
    title = "Liczba mandatów rozłamowców z PiS w okręgach wyborczych",
    subtitle = str_wrap(
      paste0(
        "Scenariusz środkowy: nowa partia pozyskuje ",
        pl_pct(MAP_LEVEL),
        " dostępnej jej puli. Rozkład regionalny odzwierciedla profile ",
        "poparcia partii, które oddają jej głosy."
      ),
      width = 110
    ),
    caption = ""
  ) +
  theme_plots_map()

ggsave(
  splinter_map,
  file = here("figures", "splinter_seats_map.png"),
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Summary#####
cat("\n==== Available pool by electorate ====\n")
print(splinter_pools %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

cat("\n==== Conversion rate, from the panel ====\n")
if (is.null(realisation_cal)) {
  cat("Panel unavailable; fallback rate used.\n")
} else {
  cat(
    "May 2023 ratings -> October 2023 vote, n =",
    realisation_cal$n_respondents,
    "over",
    realisation_cal$n_pairs,
    "party pairs\n"
  )
  cat(
    "  warm availability (rates target 6+):    ",
    sprintf("%.3f\n", realisation_cal$pooled_warm)
  )
  cat(
    "  strict availability (6+ and above own): ",
    sprintf("%.3f\n", realisation_cal$pooled_strict)
  )
  print(
    realisation_cal$rates %>%
      filter(n_warm >= 15) %>%
      transmute(
        from,
        to,
        n_warm,
        rate_warm = round(moved_warm / avail_warm, 3),
        n_strict,
        rate_strict = round(moved_strict / avail_strict, 3)
      ) %>%
      arrange(desc(n_warm))
  )
}
cat(
  "\nThree scenarios convert",
  paste(scales::percent(SCENARIO_LEVELS), collapse = ", "),
  "of that pool\n"
)
cat(
  "\nPiS parliamentary party lost to the split:",
  scales::percent(MP_DEFECTION_SHARE, accuracy = 0.1),
  "of its MPs\n"
)

cat("\n==== Baseline (no split) ====\n")
print(
  baseline_summary %>%
    filter(seats_med > 0 | vote_med > 1) %>%
    select(party, vote_med, seats_med, seats_lo, seats_hi) %>%
    mutate(vote_med = round(vote_med, 1)) %>%
    arrange(desc(seats_med))
)

for (label in SCENARIO_LABELS) {
  level <- SCENARIO_LEVELS[match(label, SCENARIO_LABELS)]
  cat(
    "\n==== Scenario:", label, "(", scales::percent(level), "of the pool ) ====\n"
  )
  print(
    scenario_summaries %>%
      filter(scenario == label, seats_med > 0 | vote_med > 1) %>%
      select(party, vote_med, vote_lo, vote_hi, seats_med, seats_lo, seats_hi) %>%
      mutate(across(starts_with("vote"), ~ round(.x, 1))) %>%
      arrange(desc(seats_med))
  )

  cat("\nWhere its vote comes from (points of the national vote):\n")
  print(
    scenario_sources %>%
      filter(scenario == label, vote_pp > 0.02) %>%
      transmute(
        source,
        vote_pp = round(vote_pp, 2),
        share_of_donor = round(share_of_donor, 3)
      ) %>%
      arrange(desc(vote_pp))
  )

  b <- scenario_blocs %>% filter(scenario == label)
  cat(
    "\nVote:",
    sprintf(
      "%.1f%% (80%% CI %.1f-%.1f); Pr(clears 5%%) = %.2f\n",
      median(b$splinter_vote) * 100,
      quantile(b$splinter_vote, 0.10) * 100,
      quantile(b$splinter_vote, 0.90) * 100,
      mean(b$cleared)
    )
  )
  cat(
    "PiS seats:",
    sprintf(
      "%d without the split, %d with it (%+d)\n",
      round(median(b$pis_seats_base)),
      round(median(b$pis_seats)),
      round(median(b$pis_seats)) - round(median(b$pis_seats_base))
    )
  )
  cat(
    "Right bloc incl. new party vs. baseline:",
    sprintf("%+d seats\n", round(median(b$right_cost)))
  )
  cat(
    "Pr(right majority incl. / excl. new party):",
    sprintf(
      "%.2f / %.2f\n",
      mean(b$right_split_incl >= MAJORITY),
      mean(b$right_split >= MAJORITY)
    )
  )
  cat(
    "Pr(governing bloc majority, alone / with new party):",
    sprintf(
      "%.2f / %.2f\n",
      mean(b$govt_split >= MAJORITY),
      mean(b$govt_split_incl >= MAJORITY)
    )
  )
  cat("KO + new party:", sprintf("%d seats\n", round(median(b$ko_splinter))))
  cat("Pr(new party pivotal):", sprintf("%.2f\n", mean(b$pivotal)))

  cat("\nMajority coalitions:\n")
  co_list <- scenario_coalitions[[label]]
  if (length(co_list) == 0) {
    cat("  none\n")
  } else {
    for (co in co_list) {
      cat(sprintf("  %-40s %d seats\n", co$name, co$seats))
    }
  }
}

cat("\n==== Scenario grid ====\n")
print(
  scenarios %>%
    transmute(
      realisation,
      pis_transfer = round(pis_transfer, 3),
      splinter_vote = round(splinter_vote, 1),
      prob_clears = round(prob_clears, 2),
      splinter_seats,
      pis_seats,
      right_incl_seats,
      govt_seats,
      prob_right_majority = round(prob_right_majority, 2),
      prob_govt_majority = round(prob_govt_majority, 2)
    )
)

cat(
  "\nNote: coalitions pairing the new party with PiS are retained as\n",
  "arithmetically possible, but an acrimonious expulsion makes them\n",
  "politically costly.\n",
  sep = ""
)
