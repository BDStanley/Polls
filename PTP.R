#####Prepare workspace#####
system("git pull")
pacman::p_load(
  tidyverse,
  googledrive,
  rio,
  readxl,
  sf,
  glue,
  sjlabelled,
  lubridate,
  brms,
  stringr,
  tidybayes,
  ggdist,
  ggblend,
  seatdist
)

set.seed(780045)

# Constants
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
PARTY_COLORS <- c(
  "PiS" = "blue",
  "KO" = "orange",
  "Polska 2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "KKP" = "brown",
  "Lewica" = "red",
  "Razem" = "purple",
  "MN" = "yellow",
  "Other" = "gray50"
)
TINY_CONSTANT <- 0.0005

# Theme functions
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

my_date_format <- function() {
  function(x) {
    m <- format(x, "%b")
    y <- format(x, "\n%Y")
    ifelse(duplicated(y), m, paste(m, y))
  }
}

options(mc.cores = parallel::detectCores())

# Optimize threading/cores for brms
total_cores <- parallel::detectCores()
n_chains <- 4
cores_per_chain <- max(1, floor(total_cores / n_chains))
threads_per_chain <- max(1, floor(total_cores / n_chains))

#####Helper functions#####
# Apply threshold and normalize
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

# Calculate seats for constituency
calculate_constituency_seats <- function(data, weights, party_cols_list) {
  # Join weights data by okreg (select only needed columns to avoid conflicts)
  data_joined <- data %>%
    left_join(
      weights %>%
        select(
          okreg,
          magnitude,
          electors,
          validvotes,
          TDcoef,
          Lewicacoef,
          PiScoef,
          Konfcoef,
          KOcoef
        ),
      by = "okreg"
    )

  # Calculate weighted votes for each party
  for (party in party_cols_list) {
    coef_col <- case_when(
      party %in% c("PiS") ~ "PiScoef",
      party %in% c("KO") ~ "KOcoef",
      party %in% c("Lewica", "Razem") ~ "Lewicacoef",
      party %in% c("Konfederacja", "KKP") ~ "Konfcoef",
      party %in% c("Polska 2050", "PSL") ~ "TDcoef",
      TRUE ~ NA_character_
    )

    if (!is.na(coef_col)) {
      data_joined[[party]] <- data_joined$validvotes *
        data_joined[[party]] *
        data_joined[[coef_col]]
    }
  }

  data_joined
}

# Create constituency ID mapping lookup table
create_const_id_mapping <- function() {
  tibble(
    cst = 1:41,
    id = c(
      24,
      27,
      4,
      7,
      28,
      34,
      25,
      26,
      29,
      36,
      31,
      33,
      37,
      40,
      13,
      12,
      22,
      1,
      6,
      14,
      35,
      21,
      10,
      38,
      39,
      16,
      17,
      30,
      23,
      18,
      11,
      32,
      41,
      15,
      5,
      19,
      20,
      2,
      3,
      8,
      9
    )
  )
}

# Generate seat map plots
generate_seat_map <- function(
  plotdata,
  party_name,
  display_name,
  color,
  limits = c(0, 20)
) {
  ggplot(plotdata) +
    geom_sf(aes(fill = as.integer(.data[[party_name]]))) +
    theme(aspect.ratio = 1) +
    geom_label(
      aes(
        x = x,
        y = y,
        group = .data[[party_name]],
        label = .data[[party_name]]
      ),
      fill = "white"
    ) +
    scale_fill_gradient(
      name = display_name,
      limits = limits,
      low = "white",
      high = color,
      guide = "colorbar"
    ) +
    labs(
      title = paste("Constituency-level share of seats for", display_name),
      subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
      caption = ""
    ) +
    theme_plots_map()
}

#####Read in, adjust and subset data#####
source("poll data scraper.R")

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

# Adjust for "Don't Know" responses
polls <- polls %>%
  mutate(across(all_of(PARTY_COLS[1:8]), ~ 100 / ((100 - DK)) * .x))

# Calculate time variables
polls <- polls %>%
  mutate(
    time = as.integer(difftime(midDate, min(midDate), units = "days")),
    pollster = as.integer(factor(org)),
    time = interval(min(midDate), midDate) / years(1)
  )

# Convert percentages to proportions
polls <- polls %>%
  mutate(across(
    all_of(PARTY_COLS[1:8]),
    ~ as.numeric(str_remove(as.character(.x), "%")) / 100
  ))

# Calculate Other and check totals
# Clamp Other to TINY_CONSTANT so it is always present in the model;
# apply_threshold_and_normalize will rebase all columns to sum to 1.
polls <- polls %>%
  mutate(
    Other = pmax(1 - rowSums(across(all_of(PARTY_COLS[1:8]))), TINY_CONSTANT),
    check = rowSums(across(all_of(PARTY_COLS)))
  )

# Apply threshold fix and normalize
polls <- apply_threshold_and_normalize(polls, PARTY_COLS)

# Filter to include only polls after June 10th, 2025
#polls <- polls %>%
#filter(midDate > as.Date('2025-06-10'))

# Load weights and shapefile
weights <- read_excel('2023_elec_percentages.xlsx')
const <- st_read('GRED_20190215_Poland_2011.shp', quiet = TRUE)

# Generate pollster names
names <- glue_collapse(
  get_labels(as.factor(get_labels(polls$org))),
  ", ",
  last = " and "
)

#####Run model#####
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
  control = list(adapt_delta = .99, max_treedepth = 17)
)

#####House effects#####
# Calculate house effects by comparing pollster-specific predictions to average trend
# Get the average trend (no random effects)
today <- interval(min(polls$midDate), Sys.Date()) / years(1)

avg_trend <- add_epred_draws(
  object = m1,
  newdata = tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  summarise(avg_support = median(.epred), .groups = "drop")

# Get pollster names mapping
pollster_names <- polls %>%
  distinct(pollster, org) %>%
  arrange(pollster)

# Get predictions for each pollster (with random effects)
pollster_effects <- expand_grid(
  time = today,
  pollster = unique(polls$pollster)
) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NULL) %>%
  group_by(pollster, .category) %>%
  summarise(
    pollster_support = median(.epred),
    .groups = "drop"
  ) %>%
  left_join(avg_trend, by = ".category") %>%
  mutate(
    house_effect_pp = (pollster_support - avg_support) * 100,
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      )
    )
  ) %>%
  left_join(pollster_names, by = "pollster") %>%
  filter(.category != "Other") # Exclude "Other" category

# Get full posterior draws for house effects
pollster_effects_draws <- expand_grid(
  time = today,
  pollster = unique(polls$pollster)
) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NULL)

# Get average trend draws (without pollster effects)
avg_trend_draws <- add_epred_draws(
  object = m1,
  newdata = tibble(time = today),
  re_formula = NA
) %>%
  select(.draw, .category, avg_epred = .epred)

# Calculate house effects as difference
house_effects_data <- pollster_effects_draws %>%
  left_join(avg_trend_draws, by = c(".draw", ".category")) %>%
  mutate(
    house_effect_pp = (.epred - avg_epred) * 100,
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      )
    )
  ) %>%
  left_join(pollster_names, by = "pollster") %>%
  filter(.category != "Other")

# Calculate medians for ordering
medians_house <- house_effects_data %>%
  group_by(org, .category) %>%
  summarise(median_effect = median(house_effect_pp), .groups = "drop")

# Create house effects plot
house_effects_plot <- house_effects_data %>%
  mutate(org = factor(org, levels = sort(unique(org)))) %>%
  ggplot(aes(
    y = org,
    x = house_effect_pp,
    color = .category
  )) +
  geom_vline(
    xintercept = 0,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  stat_interval(
    aes(x = house_effect_pp, color_ramp = after_stat(.width)),
    .width = ppoints(100)
  ) %>%
    partition(vars(.category)) +
  scale_fill_manual(values = PARTY_COLORS, guide = "none") +
  scale_color_manual(name = " ", values = PARTY_COLORS, guide = "none") +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide = "none") +
  scale_y_discrete(
    name = "",
    expand = expansion(add = c(0.6, 1)),
    limits = rev
  ) +
  geom_text(
    data = medians_house %>%
      mutate(org = factor(org, levels = sort(unique(org)))),
    aes(y = org, x = median_effect, label = round(median_effect, 1)),
    size = 3,
    hjust = 0.5,
    vjust = -1,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  coord_cartesian(clip = "off") +
  facet_wrap(~.category, ncol = 2, scales = "free_x") +
  labs(
    x = "House effect (percentage points)",
    y = "",
    title = "Polling house effects by party",
    subtitle = "Systematic deviations from the average trend for each pollster",
    caption = "Positive values indicate the pollster tends to show higher support for that party"
  ) +
  theme_plots() +
  theme(
    strip.text = element_text(
      family = "Jost Medium",
      face = "plain",
      size = rel(1)
    ),
    panel.spacing = unit(1.5, "lines"),
    plot.margin = unit(c(1, 0.5, 0.5, 0.5), "cm")
  )

ggsave(
  house_effects_plot,
  file = "house_effects.png",
  width = 8,
  height = 12,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Trend plot#####
today <- interval(min(polls$midDate), Sys.Date()) / years(1)

pred_dta <- tibble(
  time = seq(0, today, length.out = nrow(polls)),
  date = as.Date(time * 365, origin = min(polls$midDate))
) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NA) %>%
  group_by(date, .category) %>%
  rename(party = .category) %>%
  mutate(
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      )
    )
  ) %>%
  filter(party != "Other") # Exclude "Other" from plot

point_dta <- polls %>%
  select(midDate, org, all_of(PARTY_COLS)) %>%
  pivot_longer(
    cols = -c(midDate, org),
    names_to = "party",
    values_to = "est"
  ) %>%
  mutate(
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Polska2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Polska 2050",
        "PSL",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other"
      )
    )
  ) %>%
  filter(party != "Other") # Exclude "Other" from plot

# Save pre-computed data for Shiny app (small summaries only)
trend_lines <- pred_dta %>%
  group_by(date, party) %>%
  summarise(median_epred = median(.epred), .groups = "drop")

date_summaries <- pred_dta %>%
  group_by(date, party) %>%
  summarise(
    median_pct = round(median(.epred) * 100, 1),
    lower_pct = round(quantile(.epred, 0.10) * 100, 1),
    upper_pct = round(quantile(.epred, 0.90) * 100, 1),
    .groups = "drop"
  )

saveRDS(trend_lines, "trend_lines.rds")
saveRDS(date_summaries, "date_summaries.rds")
saveRDS(point_dta, "point_dta.rds")

trends_parl <- pred_dta %>%
  ggplot(aes(x = date, color = party, fill = party)) +
  ggdist::stat_lineribbon(
    aes(y = .epred, fill_ramp = after_stat(.width)),
    .width = seq(0, 0.95, 0.01)
  ) |>
    partition(vars(party)) |>
    blend("multiply") +
  geom_point(
    data = point_dta,
    aes(x = midDate, y = est, colour = party, fill = party),
    size = 1,
    show.legend = FALSE
  ) +
  scale_color_manual(values = PARTY_COLORS) +
  scale_fill_manual(values = PARTY_COLORS, guide = "none") +
  ggdist::scale_fill_ramp_continuous(range = c(1, 0), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "1 month", labels = my_date_format()) +
  coord_cartesian(
    xlim = c(min(polls$midDate), max(polls$midDate)),
    ylim = c(0, .5)
  ) +
  labs(
    y = "",
    x = "",
    title = "Trends",
    subtitle = str_wrap(
      str_c("Data from ", paste(names, collapse = ", "), "."),
      width = 120
    ),
    color = "",
    caption = "."
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill = NA))) +
  theme_plots()

ggsave(
  trends_parl,
  file = "trends_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white",
  device = png(type = "cairo")
)

#####Latest plot#####
plotdraws <- add_epred_draws(
  object = m1,
  newdata = tibble(time = today),
  re_formula = NA
) %>%
  group_by(.category) %>%
  mutate(
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other",
        "PSL",
        "Polska2050"
      ),
      labels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other",
        "PSL",
        "Polska 2050"
      )
    )
  ) %>%
  filter(.category != "Other") # Exclude "Other" from plot

medians <- plotdraws %>%
  summarise(est = median(.epred) * 100, .groups = "drop")

# Calculate dynamic probability comparison
comparison_data <- plotdraws %>%
  pivot_wider(names_from = .category, values_from = .epred) %>%
  mutate(
    PiS_leading = PiS > KO,
    KO_leading = KO > PiS
  )

pis_median <- medians$est[medians$.category == "PiS"] / 100
ko_median <- medians$est[medians$.category == "KO"] / 100

if (pis_median > ko_median) {
  lead_prob <- mean(comparison_data$PiS_leading)
  lead_text <- paste("Pr(PiS > KO) = ", round(lead_prob, 2))
  lead_party <- "PiS"
} else {
  lead_prob <- mean(comparison_data$KO_leading)
  lead_text <- paste("Pr(KO > PiS) = ", round(lead_prob, 2))
  lead_party <- "KO"
}

# Calculate 5% threshold probabilities
# Focus on parties within 2 percentage points of 5% threshold (i.e., 3% to 7%)
threshold_probs <- plotdraws %>%
  group_by(.category) %>%
  summarise(
    median = median(.epred),
    lower_95 = quantile(.epred, 0.025),
    upper_95 = quantile(.epred, 0.975),
    prob_above_5 = mean(.epred >= 0.05),
    .groups = "drop"
  ) %>%
  filter(
    median >= 0.02 & median <= 0.08
  ) %>%
  mutate(
    # Display either Pr(≥5%) or Pr(<5%) depending on which side of threshold
    threshold_text = ifelse(
      median >= 0.05,
      paste("Pr(≥5%) = ", round(prob_above_5, 2)),
      paste("Pr(<5%) = ", round(1 - prob_above_5, 2))
    )
  )

latest_parl <- plotdraws %>%
  ggplot(aes(
    y = reorder(.category, dplyr::desc(-.epred)),
    x = .epred,
    color = .category
  )) +
  geom_vline(
    xintercept = 0.05,
    color = "grey60",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  stat_interval(
    aes(x = .epred, color_ramp = after_stat(.width)),
    .width = ppoints(100)
  ) %>%
    partition(vars(.category)) +
  scale_fill_manual(values = PARTY_COLORS, guide = "none") +
  scale_color_manual(name = " ", values = PARTY_COLORS, guide = "none") +
  ggdist::scale_color_ramp_continuous(range = c(1, 0), guide = "none") +
  scale_y_discrete(name = "") +
  geom_text(
    data = medians,
    aes(y = .category, x = est / 100, label = round(est, 0)),
    size = 3.5,
    hjust = 0.5,
    vjust = -1,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  annotate(
    geom = "text",
    label = lead_text,
    y = lead_party,
    x = quantile(plotdraws$.epred[plotdraws$.category == lead_party], 0.005),
    adj = c(1),
    family = "Jost",
    fontface = "plain",
    size = 3,
    color = "red"
  ) +
  {
    if (nrow(threshold_probs) > 0) {
      lapply(1:nrow(threshold_probs), function(i) {
        party_name <- threshold_probs$.category[i]
        threshold_text <- threshold_probs$threshold_text[i]
        x_pos <- quantile(
          plotdraws$.epred[plotdraws$.category == party_name],
          0.995
        )

        annotate(
          geom = "text",
          label = threshold_text,
          y = party_name,
          x = x_pos,
          adj = c(0),
          family = "Jost",
          fontface = "plain",
          size = 3,
          color = "red"
        )
      })
    }
  } +
  scale_x_continuous(
    breaks = c(0, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5),
    labels = c("0", "5", "10", "20", "30", "40", "50")
  ) +
  expand_limits(x = 0) +
  labs(
    y = "",
    x = "",
    title = "Latest estimates",
    subtitle = str_wrap(
      str_c("Data from ", paste(names, collapse = ", "), "."),
      width = 120
    ),
    color = "",
    caption = "."
  ) +
  theme_plots()

ggsave(
  latest_parl,
  file = "latest_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Seat maps#####
library(seatdist)

median_PiS <- ifelse(
  medians$est[medians$.category == "PiS"] >= 5,
  medians$est[medians$.category == "PiS"],
  0
)
median_KO <- ifelse(
  medians$est[medians$.category == "KO"] >= 5,
  medians$est[medians$.category == "KO"],
  0
)
median_Lewica <- ifelse(
  medians$est[medians$.category == "Lewica"] >= 5,
  medians$est[medians$.category == "Lewica"],
  0
)
median_Razem <- ifelse(
  medians$est[medians$.category == "Razem"] >= 5,
  medians$est[medians$.category == "Razem"],
  0
)
median_Konfederacja <- ifelse(
  medians$est[medians$.category == "Konfederacja"] >= 5,
  medians$est[medians$.category == "Konfederacja"],
  0
)
median_KKP <- ifelse(
  medians$est[medians$.category == "KKP"] >= 5,
  medians$est[medians$.category == "KKP"],
  0
)
median_Polska2050 <- ifelse(
  medians$est[medians$.category == "Polska 2050"] >= 5,
  medians$est[medians$.category == "Polska 2050"],
  0
)
median_PSL <- ifelse(
  medians$est[medians$.category == "PSL"] >= 5,
  medians$est[medians$.category == "PSL"],
  0
)

PiSpct <- round(weights$PiScoef * median_PiS, digits = 2)
KOpct <- round(weights$KOcoef * median_KO, digits = 2)
Lewicapct <- round(weights$Lewicacoef * median_Lewica, digits = 2)
Razempct <- round(weights$Lewicacoef * median_Razem, digits = 2) # Using same coef as Lewica
Konfederacjapct <- round(weights$Konfcoef * median_Konfederacja, digits = 2)
KKPpct <- round(weights$Konfcoef * median_KKP, digits = 2) # Using same coef as Konfederacja
Polska2050pct <- round(weights$TDcoef * median_Polska2050, digits = 2)
PSLpct <- round(weights$TDcoef * median_PSL, digits = 2)
MNpct <- c(
  0.12,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  5.37,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0,
  0
)

KOest <- (weights$validvotes / 100) * KOpct
PiSest <- (weights$validvotes / 100) * PiSpct
Lewicaest <- (weights$validvotes / 100) * Lewicapct
Razemest <- (weights$validvotes / 100) * Razempct
Konfederacjaest <- (weights$validvotes / 100) * Konfederacjapct
KKPest <- (weights$validvotes / 100) * KKPpct
Polska2050est <- (weights$validvotes / 100) * Polska2050pct
PSLest <- (weights$validvotes / 100) * PSLpct
MNest <- (weights$validvotes / 100) * MNpct

poldHondt <- data.frame(
  KO = rep(1, 42),
  Konfederacja = rep(1, 42),
  KKP = rep(1, 42),
  Lewica = rep(1, 42),
  Razem = rep(1, 42),
  MN = rep(1, 42),
  PiS = rep(1, 42),
  Polska2050 = rep(1, 42),
  PSL = rep(1, 42)
)

for (i in 1:42) {
  poldHondt[i, ] <- c(giveseats(
    v = c(
      KOest[i],
      Konfederacjaest[i],
      KKPest[i],
      Lewicaest[i],
      Razemest[i],
      MNest[i],
      PiSest[i],
      Polska2050est[i],
      PSLest[i]
    ),
    ns = weights$magnitude[i],
    method = "dh",
    thresh = 5
  ))$seats
}

#seats table
seats <- cbind(poldHondt, weights)
row.names(seats) <- weights$name
keep <- c(
  "KO",
  "Konfederacja",
  "KKP",
  "Lewica",
  "Razem",
  "MN",
  "PiS",
  "Polska2050",
  "PSL"
)
colnames(seats) <- c(
  "KO",
  "Konfederacja",
  "KKP",
  "Lewica",
  "Razem",
  "MN",
  "PiS",
  "Polska2050",
  "PSL"
)
seats <- seats[keep]
seats <- seats[-1, ]
seats$id <- 1:41
seats$PiSKO <- abs(seats$PiS - seats$KO)
seats$PiSmKO <- seats$PiS - seats$KO

#regional maps
const$id <- 0
const$id[const$cst == 1] <- 24
const$id[const$cst == 2] <- 27
const$id[const$cst == 3] <- 4
const$id[const$cst == 4] <- 7
const$id[const$cst == 5] <- 28
const$id[const$cst == 6] <- 34
const$id[const$cst == 7] <- 25
const$id[const$cst == 8] <- 26
const$id[const$cst == 9] <- 29
const$id[const$cst == 10] <- 36
const$id[const$cst == 11] <- 31
const$id[const$cst == 12] <- 33
const$id[const$cst == 13] <- 37
const$id[const$cst == 14] <- 40
const$id[const$cst == 15] <- 13
const$id[const$cst == 16] <- 12
const$id[const$cst == 17] <- 22
const$id[const$cst == 18] <- 1
const$id[const$cst == 19] <- 6
const$id[const$cst == 20] <- 14
const$id[const$cst == 21] <- 35
const$id[const$cst == 22] <- 21
const$id[const$cst == 23] <- 10
const$id[const$cst == 24] <- 38
const$id[const$cst == 25] <- 39
const$id[const$cst == 26] <- 16
const$id[const$cst == 27] <- 17
const$id[const$cst == 28] <- 30
const$id[const$cst == 29] <- 23
const$id[const$cst == 30] <- 18
const$id[const$cst == 31] <- 11
const$id[const$cst == 32] <- 32
const$id[const$cst == 33] <- 41
const$id[const$cst == 34] <- 15
const$id[const$cst == 35] <- 5
const$id[const$cst == 36] <- 19
const$id[const$cst == 37] <- 20
const$id[const$cst == 38] <- 2
const$id[const$cst == 39] <- 3
const$id[const$cst == 40] <- 8
const$id[const$cst == 41] <- 9

label_points <- st_point_on_surface(const) %>%
  arrange(., id)
label_points <- st_coordinates(label_points) %>%
  as_tibble() %>%
  mutate(id = 1:n())
colnames(label_points) <- c("x", "y", "id")

plotdata <- merge(const, seats, by = "id")
plotdata <- merge(plotdata, label_points, by = "id")

p_pis <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(PiS))) +
  theme(aspect.ratio = 1) +
  geom_label(aes(x = x, y = y, group = PiS, label = PiS), fill = "white") +
  scale_fill_gradient(
    name = "PiS",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "blue",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for PiS",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_pis,
  file = "PiS_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_ko <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(KO))) +
  theme(aspect.ratio = 1) +
  geom_label(aes(x = x, y = y, group = KO, label = KO), fill = "white") +
  scale_fill_gradient(
    name = "KO",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "orange",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for Koalicja Obywatelska",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_ko,
  file = "KO_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_lewica <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(Lewica))) +
  theme(aspect.ratio = 1) +
  geom_label(
    aes(x = x, y = y, group = Lewica, label = Lewica),
    fill = "white"
  ) +
  scale_fill_gradient(
    name = "Lewica",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "red",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for Lewica",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_lewica,
  file = "Lewica_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_razem <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(Razem))) +
  theme(aspect.ratio = 1) +
  geom_label(aes(x = x, y = y, group = Razem, label = Razem), fill = "white") +
  scale_fill_gradient(
    name = "Razem",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "purple",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for Razem",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_razem,
  file = "Razem_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_PSL <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(PSL))) +
  theme(aspect.ratio = 1) +
  geom_label(aes(x = x, y = y, group = PSL, label = PSL), fill = "white") +
  scale_fill_gradient(
    name = "PSL",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "darkgreen",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for PSL",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_PSL,
  file = "PSL_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_konfederacja <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(Konfederacja))) +
  theme(aspect.ratio = 1) +
  geom_label(
    aes(x = x, y = y, group = Konfederacja, label = Konfederacja),
    fill = "white"
  ) +
  scale_fill_gradient(
    name = "Konfederacja",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "midnightblue",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for Konfederacja",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_konfederacja,
  file = "Konfederacja_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_P2050 <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(Polska2050))) +
  theme(aspect.ratio = 1) +
  geom_label(
    aes(x = x, y = y, group = Polska2050, label = Polska2050),
    fill = "white"
  ) +
  scale_fill_gradient(
    name = "Polska2050",
    limits = c(min = 0, max = 20),
    low = "white",
    high = "goldenrod",
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level share of seats for Polska 2050",
    subtitle = "Seat distribution reflects regional levels of support at October 2023 election",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_P2050,
  file = "P2050_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

p_pis_ko <- ggplot(plotdata) +
  geom_sf(aes(fill = as.integer(PiSmKO))) +
  theme(aspect.ratio = 1) +
  scale_fill_gradient2(
    name = "PiSKO",
    limits = c(min = -20, max = 20),
    low = "orange",
    mid = "white",
    high = "blue",
    midpoint = 0,
    guide = "colorbar"
  ) +
  labs(
    title = "Constituency-level differences in share of seats for PiS and Koalicja Obywatelska",
    subtitle = "Constituencies in shades of blue have more PiS MPs; constituencies in orange have more KO MPs;\nconstituencies in white have equal numbers of PiS and KO MPs",
    caption = ""
  ) +
  theme_plots_map()
ggsave(
  p_pis_ko,
  file = "PiSKO_seats.png",
  width = 7,
  height = 7,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####Seats plot#####
plotdraws_seats <- add_epred_draws(
  object = m1,
  newdata = tibble(time = today),
  re_formula = NA,
  ndraws = 1000
) %>%
  mutate(.draw = row_number()) %>%
  group_by(.category, .draw) %>%
  mutate(
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other",
        "PSL",
        "Polska2050"
      ),
      labels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Other",
        "PSL",
        "Polska 2050"
      )
    )
  )

# Calculate party medians for threshold application
party_medians <- plotdraws_seats %>%
  ungroup() %>%
  group_by(.category) %>%
  summarise(median_value = median(.epred), .groups = "drop")

# Apply 5% threshold based on medians
plotdraws_wide <- plotdraws_seats %>%
  pivot_wider(names_from = .category, values_from = .epred) %>%
  mutate(MN = rnorm(n(), mean = 0.079, sd = 0.00001))

# Apply threshold - set parties below 5% to zero
plotdraws_wide <- plotdraws_wide %>%
  mutate(
    across(
      any_of(c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Polska 2050",
        "PSL"
      )),
      ~ {
        party_name <- cur_column()
        med_val <- party_medians$median_value[
          party_medians$.category == party_name
        ]
        if (length(med_val) > 0 && !is.na(med_val) && med_val < 0.05) {
          0
        } else {
          .x
        }
      }
    )
  )

# Expand to constituencies
consts <- plotdraws_wide %>%
  uncount(41, .id = "okreg") %>%
  calculate_constituency_seats(
    weights %>% filter(okreg != 0),
    c(
      "PiS",
      "KO",
      "Lewica",
      "Razem",
      "Konfederacja",
      "KKP",
      "Polska 2050",
      "PSL"
    )
  )

# Handle MN special case (only okreg 21)
consts <- consts %>%
  mutate(MN = ifelse(okreg == 21, validvotes * MN, 0))

# Vectorized seat allocation
poldHondt_sim <- consts %>%
  rowwise() %>%
  mutate(
    seats = list(
      giveseats(
        v = c(
          KO,
          Konfederacja,
          KKP,
          Lewica,
          Razem,
          MN,
          PiS,
          `Polska 2050`,
          PSL
        ),
        ns = magnitude,
        method = "dh",
        thresh = 0
      )$seats
    )
  ) %>%
  ungroup() %>%
  mutate(
    KO_seats = map_dbl(seats, ~ .x[1]),
    Konfederacja_seats = map_dbl(seats, ~ .x[2]),
    KKP_seats = map_dbl(seats, ~ .x[3]),
    Lewica_seats = map_dbl(seats, ~ .x[4]),
    Razem_seats = map_dbl(seats, ~ .x[5]),
    MN_seats = map_dbl(seats, ~ .x[6]),
    PiS_seats = map_dbl(seats, ~ .x[7]),
    Polska2050_seats = map_dbl(seats, ~ .x[8]),
    PSL_seats = map_dbl(seats, ~ .x[9])
  ) %>%
  group_by(.draw) %>%
  summarise(
    KO = sum(KO_seats),
    PiS = sum(PiS_seats),
    Konfederacja = sum(Konfederacja_seats),
    KKP = sum(KKP_seats),
    `Polska 2050` = sum(Polska2050_seats),
    PSL = sum(PSL_seats),
    MN = sum(MN_seats),
    Lewica = sum(Lewica_seats),
    Razem = sum(Razem_seats),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      "KO",
      "Konfederacja",
      "KKP",
      "Lewica",
      "Razem",
      "MN",
      "PiS",
      "Polska 2050",
      "PSL"
    ),
    names_to = "party",
    values_to = "seats"
  )

# Calculate seat statistics
frame <- poldHondt_sim %>%
  group_by(party) %>%
  summarise(median_qi(seats, .width = 0.8), .groups = "drop") %>%
  mutate(across(c(y, ymin, ymax), ~ round(.x, 0)))

# Add 2023 baseline
seats_2023 <- c(
  KO = 157,
  PiS = 194,
  Lewica = 19,
  Razem = 7,
  MN = 0,
  Konfederacja = 18,
  KKP = 0,
  `Polska 2050` = 33,
  PSL = 32
)

frame <- frame %>%
  mutate(
    in2023 = seats_2023[party],
    party = factor(
      party,
      levels = c(
        "PiS",
        "KO",
        "Lewica",
        "Razem",
        "Konfederacja",
        "KKP",
        "Polska 2050",
        "PSL",
        "MN"
      )
    ),
    diffPres = sprintf("(%+d)", y - in2023),
    party = reorder(party, -y)
  )

# Calculate coalition statistics
coalition_pis_konf_kkp <- frame$y[frame$party == "PiS"] +
  frame$y[frame$party == "Konfederacja"] +
  frame$y[frame$party == "KKP"]
coalition_opposition <- sum(frame$y[
  frame$party %in% c("KO", "Lewica", "Polska 2050", "PSL")
])
coalition_ko_konf <- frame$y[frame$party == "KO"] +
  frame$y[frame$party == "Konfederacja"]
coalition_pis_konf <- frame$y[frame$party == "PiS"] +
  frame$y[frame$party == "Konfederacja"]

pis_konf_kkp_status <- ifelse(
  coalition_pis_konf_kkp >= 231,
  "Majority",
  "No majority"
)
opposition_status <- ifelse(
  coalition_opposition >= 231,
  "Majority",
  "No majority"
)
ko_konf_status <- ifelse(
  coalition_ko_konf >= 231,
  "Majority",
  "No majority"
)
pis_konf_status <- ifelse(
  coalition_pis_konf >= 231,
  "Majority",
  "No majority"
)

pis_konf_kkp_text <- paste0(
  "PiS + Konfederacja + KKP: ",
  coalition_pis_konf_kkp,
  " seats"
)
opposition_text <- paste0(
  "KO + Lewica + Polska 2050 + PSL: ",
  coalition_opposition,
  " seats"
)
ko_konf_text <- paste0(
  "KO + Konfederacja: ",
  coalition_ko_konf,
  " seats"
)
pis_konf_text <- paste0(
  "PiS + Konfederacja: ",
  coalition_pis_konf,
  " seats"
)

# Generate seats plot
seats_parl <- ggplot(
  data = frame,
  mapping = aes(x = party, y = y, fill = party)
) +
  geom_bar(stat = "identity", width = .75, show.legend = FALSE) +
  geom_hline(yintercept = c(231, 276, 307), colour = "gray10", linetype = 3) +
  scale_y_continuous(
    'Number of seats',
    limits = c(0, 320),
    breaks = c(0, 50, 100, 150, 200, 231, 276, 307)
  ) +
  scale_fill_manual(name = "Party", values = PARTY_COLORS) +
  geom_label(
    data = data.frame(x = 2, y = 231, label = "Legislative majority"),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = "grey95",
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 2, y = 276, label = "Overturn presidential veto"),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = "grey95",
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 2, y = 307, label = "Constitutional majority"),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = "grey95",
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 5, y = 280, label = pis_konf_kkp_text),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = ifelse(coalition_pis_konf_kkp >= 231, "lightgreen", "lightcoral"),
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 5, y = 250, label = opposition_text),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = ifelse(coalition_opposition >= 231, "lightgreen", "lightcoral"),
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 5, y = 220, label = ko_konf_text),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = ifelse(coalition_ko_konf >= 231, "lightgreen", "lightcoral"),
    linewidth = 0,
    family = "Jost",
    inherit.aes = FALSE
  ) +
  geom_label(
    data = data.frame(x = 5, y = 190, label = pis_konf_text),
    aes(x = x, y = y, label = label),
    hjust = 0,
    size = 2.5,
    fill = ifelse(coalition_pis_konf >= 231, "lightgreen", "lightcoral"),
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
    aes(x = party, y = y + 8, label = paste0("(", ymin, "\u2013", ymax, ")")),
    size = 2,
    family = "Jost"
  ) +
  labs(
    x = "",
    y = "Number of seats",
    title = "Estimated share of seats",
    subtitle = "Median estimated seat share with 80% credible intervals. Sum total may not equal 460.",
    caption = "Figures in brackets show change from 2023 share of seats."
  ) +
  theme_plots()

ggsave(
  seats_parl,
  file = "seats_parl.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

source("PTP_hypothetical.R")

#####Deploy Shiny app#####
source("deploy.R")

#####Save to Github and sync to iCloud#####
system("git add -A")
system('git commit -m "Update $(date +"%Y-%m-%d %H:%M:%S")"')
system("git push")
system(
  'rsync -av --delete --size-only --exclude=".git" "/Users/benstanley/Positron/Polls/" "/Users/benstanley/Library/Mobile Documents/com~apple~CloudDocs/Polls/"'
)
system(
  'rsync -av --include="*.png" --exclude="*" "/Users/benstanley/Positron/Polls/" "/Users/benstanley/Positron/Website/docs/PTP images/"'
)
