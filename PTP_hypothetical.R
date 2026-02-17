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
  "KOplus" = "darkorange",
  "Polska2050" = "goldenrod",
  "PSL" = "darkgreen",
  "Konfederacja" = "midnightblue",
  "KKP" = "brown",
  "Lewica" = "red",
  "Lewicaplus" = "red",
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

#####Read in, adjust and subset data#####
source("poll data scraper.R")

polls_base <- polls_cleaned %>%
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
polls_base <- polls_base %>%
  mutate(across(all_of(PARTY_COLS[1:8]), ~ 100 / ((100 - DK)) * .x))

# Calculate time variables
polls_base <- polls_base %>%
  mutate(
    time = as.integer(difftime(midDate, min(midDate), units = "days")),
    pollster = as.integer(factor(org)),
    time = interval(min(midDate), midDate) / years(1)
  )

# Convert percentages to proportions
polls_base <- polls_base %>%
  mutate(across(
    all_of(PARTY_COLS[1:8]),
    ~ as.numeric(str_remove(as.character(.x), "%")) / 100
  ))

# Calculate Other and check totals
# Clamp Other to TINY_CONSTANT so it is always present in the model;
# apply_threshold_and_normalize will rebase all columns to sum to 1.
polls_base <- polls_base %>%
  mutate(
    Other = pmax(1 - rowSums(across(all_of(PARTY_COLS[1:8]))), TINY_CONSTANT),
    check = rowSums(across(all_of(PARTY_COLS)))
  )

# Load weights and shapefile
weights <- read_excel('2023_elec_percentages.xlsx')
const <- st_read('GRED_20190215_Poland_2011.shp', quiet = TRUE)

# Generate pollster names
names <- glue_collapse(
  get_labels(as.factor(get_labels(polls_base$org))),
  ", ",
  last = " and "
)

#####SCENARIO 1: KO + Lewica + Polska2050 + PSL Coalition#####
cat("\n=== SCENARIO 1: KO + Lewica + Polska 2050 + PSL Coalition ===\n")
cat(
  "Merging polling data for KO, Lewica, Polska2050, and PSL into KOplus...\n\n"
)

# Create Scenario 1 polling data with merged KOplus
PARTY_COLS_S1 <- c("PiS", "KOplus", "Razem", "Konfederacja", "KKP", "Other")

polls_s1 <- polls_base %>%
  mutate(
    KOplus = KO + Lewica + Polska2050 + PSL
  ) %>%
  select(
    startDate,
    endDate,
    org,
    midDate,
    time,
    pollster,
    PiS,
    KOplus,
    Razem,
    Konfederacja,
    KKP,
    Other
  )

# Apply threshold fix and normalize for Scenario 1
polls_s1 <- apply_threshold_and_normalize(polls_s1, PARTY_COLS_S1)

cat("Running Bayesian model for Scenario 1...\n")

# Run model for Scenario 1
m_s1 <- brm(
  formula = bf(
    outcome ~ 1 + s(time, k = 8, bs = "cs", m = 2) + (1 | pollster)
  ),
  family = dirichlet(link = "logit", refcat = "Other"),
  prior = prior(normal(0, 1.5), class = "Intercept", dpar = "muPiS") +
    prior(exponential(3), class = "sd", dpar = "muPiS") +
    prior(exponential(3), class = "sds", dpar = "muPiS") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKOplus") +
    prior(exponential(3), class = "sd", dpar = "muKOplus") +
    prior(exponential(3), class = "sds", dpar = "muKOplus") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muRazem") +
    prior(exponential(3), class = "sd", dpar = "muRazem") +
    prior(exponential(3), class = "sds", dpar = "muRazem") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKonfederacja") +
    prior(exponential(3), class = "sd", dpar = "muKonfederacja") +
    prior(exponential(3), class = "sds", dpar = "muKonfederacja") +
    prior(normal(0, 1.5), class = "Intercept", dpar = "muKKP") +
    prior(exponential(3), class = "sd", dpar = "muKKP") +
    prior(exponential(3), class = "sds", dpar = "muKKP") +
    prior(gamma(2, 0.1), class = "phi"),
  data = polls_s1,
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

# Get current estimates for Scenario 1
today <- interval(min(polls_base$midDate), Sys.Date()) / years(1)

plotdraws_s1 <- add_epred_draws(
  object = m_s1,
  newdata = tibble(time = today),
  re_formula = NA,
  ndraws = 1000
) %>%
  group_by(.category) %>%
  mutate(
    .category = factor(
      .category,
      levels = c("PiS", "KOplus", "Razem", "Konfederacja", "KKP", "Other"),
      labels = c("PiS", "KO+", "Razem", "Konfederacja", "KKP", "Other")
    )
  ) %>%
  filter(.category != "Other")

medians_s1 <- plotdraws_s1 %>%
  summarise(est = median(.epred) * 100, .groups = "drop")

cat("Vote share estimates:\n")
print(medians_s1)

# Calculate seats using KO's 2023 regional distribution
median_KOplus <- ifelse(
  medians_s1$est[medians_s1$.category == "KO+"] >= 5,
  medians_s1$est[medians_s1$.category == "KO+"],
  0
)
median_PiS_s1 <- ifelse(
  medians_s1$est[medians_s1$.category == "PiS"] >= 5,
  medians_s1$est[medians_s1$.category == "PiS"],
  0
)
median_Razem_s1 <- ifelse(
  medians_s1$est[medians_s1$.category == "Razem"] >= 5,
  medians_s1$est[medians_s1$.category == "Razem"],
  0
)
median_Konfederacja_s1 <- ifelse(
  medians_s1$est[medians_s1$.category == "Konfederacja"] >= 5,
  medians_s1$est[medians_s1$.category == "Konfederacja"],
  0
)
median_KKP_s1 <- ifelse(
  medians_s1$est[medians_s1$.category == "KKP"] >= 5,
  medians_s1$est[medians_s1$.category == "KKP"],
  0
)

# Use KO's 2023 coefficient for regional distribution
KOpluspct_s1 <- round(weights$KOcoef * median_KOplus, digits = 2)
PiSpct_s1 <- round(weights$PiScoef * median_PiS_s1, digits = 2)
Razempct_s1 <- round(weights$Lewicacoef * median_Razem_s1, digits = 2)
Konfederacjapct_s1 <- round(
  weights$Konfcoef * median_Konfederacja_s1,
  digits = 2
)
KKPpct_s1 <- round(weights$Konfcoef * median_KKP_s1, digits = 2)
MNpct <- c(0.12, rep(0, 20), 5.37, rep(0, 20))

# Convert to votes
KOplusest_s1 <- (weights$validvotes / 100) * KOpluspct_s1
PiSest_s1 <- (weights$validvotes / 100) * PiSpct_s1
Razemest_s1 <- (weights$validvotes / 100) * Razempct_s1
Konfederacjaest_s1 <- (weights$validvotes / 100) * Konfederacjapct_s1
KKPest_s1 <- (weights$validvotes / 100) * KKPpct_s1
MNest <- (weights$validvotes / 100) * MNpct

# Allocate seats using D'Hondt
poldHondt_s1 <- data.frame(
  KOplus = rep(0, 42),
  Konfederacja = rep(0, 42),
  KKP = rep(0, 42),
  Razem = rep(0, 42),
  MN = rep(0, 42),
  PiS = rep(0, 42)
)

for (i in 1:42) {
  poldHondt_s1[i, ] <- giveseats(
    v = c(
      KOplusest_s1[i],
      Konfederacjaest_s1[i],
      KKPest_s1[i],
      Razemest_s1[i],
      MNest[i],
      PiSest_s1[i]
    ),
    ns = weights$magnitude[i],
    method = "dh",
    thresh = 0
  )$seats
}

seats_s1 <- colSums(poldHondt_s1[-1, ])
cat("\nSeat distribution:\n")
print(seats_s1)
cat(sprintf("Total seats: %d\n\n", sum(seats_s1)))

# Create seat plot for Scenario 1
seats_data_s1 <- tibble(
  party = names(seats_s1),
  seats = as.numeric(seats_s1)
) %>%
  mutate(
    party_label = case_when(
      party == "KOplus" ~ "KO+",
      TRUE ~ as.character(party)
    )
  ) %>%
  arrange(desc(seats)) %>%
  mutate(
    party_label = factor(party_label, levels = party_label)
  )

plot_s1 <- ggplot(
  seats_data_s1,
  aes(x = party_label, y = seats, fill = party)
) +
  geom_bar(stat = "identity", width = .75, show.legend = FALSE) +
  geom_hline(yintercept = c(231, 276, 307), colour = "gray10", linetype = 3) +
  scale_x_discrete(limits = levels(seats_data_s1$party_label)) +
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
  geom_text(
    aes(x = party_label, y = seats + 10, label = seats),
    size = 4,
    family = "Jost"
  ) +
  labs(
    x = "",
    y = "Number of seats",
    title = "Hypothetical scenario: KO/Lewica/Polska 2050/PSL electoral coalition",
    caption = "Polling merged before model estimation. Constituency distribution based on KO's 2023 vote shares.",
  ) +
  theme_plots()

ggsave(
  plot_s1,
  file = "scenario1_KO_plus_seats.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)

#####SCENARIO 2: Lewica + Razem Coalition#####
cat("\n=== SCENARIO 2: Lewica + Razem Coalition ===\n")
cat("Merging polling data for Lewica and Razem into Lewicaplus...\n\n")

# Create Scenario 2 polling data with merged Lewicaplus
PARTY_COLS_S2 <- c(
  "PiS",
  "KO",
  "Lewicaplus",
  "Polska2050",
  "PSL",
  "Konfederacja",
  "KKP",
  "Other"
)

polls_s2 <- polls_base %>%
  mutate(
    Lewicaplus = Lewica + Razem
  ) %>%
  select(
    startDate,
    endDate,
    org,
    midDate,
    time,
    pollster,
    PiS,
    KO,
    Lewicaplus,
    Polska2050,
    PSL,
    Konfederacja,
    KKP,
    Other
  )

# Apply threshold fix and normalize for Scenario 2
polls_s2 <- apply_threshold_and_normalize(polls_s2, PARTY_COLS_S2)

cat("Running Bayesian model for Scenario 2...\n")

# Run model for Scenario 2
m_s2 <- brm(
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
    prior(normal(0, 1.5), class = "Intercept", dpar = "muLewicaplus") +
    prior(exponential(3), class = "sd", dpar = "muLewicaplus") +
    prior(exponential(3), class = "sds", dpar = "muLewicaplus") +
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
  data = polls_s2,
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

# Get current estimates for Scenario 2
plotdraws_s2 <- add_epred_draws(
  object = m_s2,
  newdata = tibble(time = today),
  re_formula = NA,
  ndraws = 1000
) %>%
  group_by(.category) %>%
  mutate(
    .category = factor(
      .category,
      levels = c(
        "PiS",
        "KO",
        "Lewicaplus",
        "Polska2050",
        "PSL",
        "Konfederacja",
        "KKP",
        "Other"
      ),
      labels = c(
        "PiS",
        "KO",
        "Lewica+",
        "Polska 2050",
        "PSL",
        "Konfederacja",
        "KKP",
        "Other"
      )
    )
  ) %>%
  filter(.category != "Other")

medians_s2 <- plotdraws_s2 %>%
  summarise(est = median(.epred) * 100, .groups = "drop")

cat("Vote share estimates:\n")
print(medians_s2)

# Calculate seats using Lewica's 2023 regional distribution
median_Lewicaplus <- ifelse(
  medians_s2$est[medians_s2$.category == "Lewica+"] >= 5,
  medians_s2$est[medians_s2$.category == "Lewica+"],
  0
)
median_PiS_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "PiS"] >= 5,
  medians_s2$est[medians_s2$.category == "PiS"],
  0
)
median_KO_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "KO"] >= 5,
  medians_s2$est[medians_s2$.category == "KO"],
  0
)
median_Polska2050_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "Polska 2050"] >= 5,
  medians_s2$est[medians_s2$.category == "Polska 2050"],
  0
)
median_PSL_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "PSL"] >= 5,
  medians_s2$est[medians_s2$.category == "PSL"],
  0
)
median_Konfederacja_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "Konfederacja"] >= 5,
  medians_s2$est[medians_s2$.category == "Konfederacja"],
  0
)
median_KKP_s2 <- ifelse(
  medians_s2$est[medians_s2$.category == "KKP"] >= 5,
  medians_s2$est[medians_s2$.category == "KKP"],
  0
)

# Use Lewica's 2023 coefficient for regional distribution
Lewicapluspct_s2 <- round(weights$Lewicacoef * median_Lewicaplus, digits = 2)
PiSpct_s2 <- round(weights$PiScoef * median_PiS_s2, digits = 2)
KOpct_s2 <- round(weights$KOcoef * median_KO_s2, digits = 2)
Polska2050pct_s2 <- round(weights$TDcoef * median_Polska2050_s2, digits = 2)
PSLpct_s2 <- round(weights$TDcoef * median_PSL_s2, digits = 2)
Konfederacjapct_s2 <- round(
  weights$Konfcoef * median_Konfederacja_s2,
  digits = 2
)
KKPpct_s2 <- round(weights$Konfcoef * median_KKP_s2, digits = 2)

# Convert to votes
Lewicaplusest_s2 <- (weights$validvotes / 100) * Lewicapluspct_s2
PiSest_s2 <- (weights$validvotes / 100) * PiSpct_s2
KOest_s2 <- (weights$validvotes / 100) * KOpct_s2
Polska2050est_s2 <- (weights$validvotes / 100) * Polska2050pct_s2
PSLest_s2 <- (weights$validvotes / 100) * PSLpct_s2
Konfederacjaest_s2 <- (weights$validvotes / 100) * Konfederacjapct_s2
KKPest_s2 <- (weights$validvotes / 100) * KKPpct_s2

# Allocate seats using D'Hondt
poldHondt_s2 <- data.frame(
  KO = rep(0, 42),
  Konfederacja = rep(0, 42),
  KKP = rep(0, 42),
  Lewicaplus = rep(0, 42),
  MN = rep(0, 42),
  PiS = rep(0, 42),
  Polska2050 = rep(0, 42),
  PSL = rep(0, 42)
)

for (i in 1:42) {
  poldHondt_s2[i, ] <- giveseats(
    v = c(
      KOest_s2[i],
      Konfederacjaest_s2[i],
      KKPest_s2[i],
      Lewicaplusest_s2[i],
      MNest[i],
      PiSest_s2[i],
      Polska2050est_s2[i],
      PSLest_s2[i]
    ),
    ns = weights$magnitude[i],
    method = "dh",
    thresh = 0
  )$seats
}

seats_s2 <- colSums(poldHondt_s2[-1, ])
cat("\nSeat distribution:\n")
print(seats_s2)
cat(sprintf("Total seats: %d\n\n", sum(seats_s2)))

# Create seat plot for Scenario 2
seats_data_s2 <- tibble(
  party = names(seats_s2),
  seats = as.numeric(seats_s2)
) %>%
  mutate(
    party_label = case_when(
      party == "Lewicaplus" ~ "Lewica+",
      party == "Polska2050" ~ "Polska 2050",
      TRUE ~ as.character(party)
    )
  ) %>%
  arrange(desc(seats)) %>%
  mutate(
    party_label = factor(party_label, levels = party_label)
  )

plot_s2 <- ggplot(
  seats_data_s2,
  aes(x = party_label, y = seats, fill = party)
) +
  geom_bar(stat = "identity", width = .75, show.legend = FALSE) +
  geom_hline(yintercept = c(231, 276, 307), colour = "gray10", linetype = 3) +
  scale_x_discrete(limits = levels(seats_data_s2$party_label)) +
  scale_y_continuous(
    'Number of seats',
    limits = c(0, 250),
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
  geom_text(
    aes(x = party_label, y = seats + 10, label = seats),
    size = 4,
    family = "Jost"
  ) +
  labs(
    x = "",
    y = "Number of seats",
    title = "Hypothetical scenario: Lewica/Razem electoral coalition",
    caption = "Polling merged before model estimation. Constituency distribution based on Lewica's 2023 vote shares.",
  ) +
  theme_plots()

ggsave(
  plot_s2,
  file = "scenario2_Lewica_plus_seats.png",
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)
