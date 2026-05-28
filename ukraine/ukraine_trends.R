#####Prepare workspace#####
if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.r-project.org")
}

pkgs <- c(
  "tidyverse",
  "readxl",
  "glue",
  "lubridate",
  "brms",
  "tidybayes",
  "ggdist",
  "ggblend",
  "here"
)
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  pak::pkg_install(missing_pkgs, ask = FALSE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

set.seed(780045)

# Constants
CANDIDATE_COLS <- c("Zelenskiy", "Zaluzhniy", "Budanov", "Poroshenko")
CANDIDATE_COLORS <- c(
  "Zelenskiy" = "#005BBB",
  "Zaluzhniy" = "#D62728",
  "Budanov" = "#2CA02C",
  "Poroshenko" = "#FFD500"
)

# Theme functions (copied from PTP.R)
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

#####Read in and prepare data#####
polls_raw <- read_excel(
  here("ukraine", "Election_Chart. 240426 14.xlsx"),
  sheet = "Datawrapper Format"
)

polls <- polls_raw %>%
  transmute(
    midDate = as.Date(Datum),
    org = as.factor(pollster),
    Zelenskiy = `Vladimir Zelenskiy (individual, scope-normalised)`,
    Zaluzhniy = `Valerii Zaluzhniy (individual, scope-normalised)`,
    Budanov = `Kyrylo Budanov (individual, scope-normalised)`,
    Poroshenko = `Pyotr Poroshenko (individual, scope-normalised)`
  ) %>%
  filter(!is.na(midDate), midDate >= as.Date("2022-05-01"))

# Long format: one row per (poll, candidate); drop missing entries
polls_long <- polls %>%
  pivot_longer(
    cols = all_of(CANDIDATE_COLS),
    names_to = "candidate",
    values_to = "support"
  ) %>%
  filter(!is.na(support)) %>%
  mutate(
    support = as.numeric(support) / 100,
    support = pmin(pmax(support, 0.0005), 0.9995),
    candidate = factor(candidate, levels = CANDIDATE_COLS),
    pollster = as.integer(org),
    time = interval(min(midDate), midDate) / years(1)
  )

# Pollster names for subtitle
names <- glue_collapse(
  sort(unique(as.character(polls_long$org))),
  ", ",
  last = " and "
)

#####Run model#####
m1 <- brm(
  formula = bf(
    support ~ candidate +
      s(time, by = candidate, k = 5, bs = "cs", m = 2) +
      (1 | pollster)
  ),
  family = Beta(link = "logit"),
  prior = prior(normal(0, 1.5), class = "Intercept") +
    prior(normal(0, 1.5), class = "b") +
    prior(exponential(3), class = "sd") +
    prior(exponential(15), class = "sds") +
    prior(gamma(2, 0.1), class = "phi"),
  data = polls_long,
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

#####Trend plot#####
last_poll <- max(polls_long$midDate)
last_time <- interval(min(polls_long$midDate), last_poll) / years(1)

pred_dta <- expand_grid(
  time = seq(0, last_time, length.out = 300),
  candidate = factor(CANDIDATE_COLS, levels = CANDIDATE_COLS)
) %>%
  mutate(
    date = as.Date(time * 365, origin = min(polls_long$midDate))
  ) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NA) %>%
  group_by(date, candidate) %>%
  summarise(
    median = median(.epred),
    lower = quantile(.epred, 0.025),
    upper = quantile(.epred, 0.975),
    .groups = "drop"
  )

point_dta <- polls_long %>%
  select(midDate, org, candidate, est = support)

end_dta <- pred_dta %>%
  group_by(candidate) %>%
  filter(date == max(date)) %>%
  ungroup() %>%
  mutate(label = paste0(round(median * 100), "%"))

trends_ukr <- pred_dta %>%
  ggplot(aes(x = date, color = candidate, fill = candidate)) +
  geom_ribbon(
    aes(ymin = lower, ymax = upper),
    alpha = 0.2,
    colour = NA
  ) +
  geom_line(aes(y = median), linewidth = 0.8) +
  geom_point(
    data = point_dta,
    aes(x = midDate, y = est, colour = candidate, fill = candidate),
    size = 1,
    show.legend = FALSE
  ) +
  geom_text(
    data = end_dta,
    aes(x = date, y = median, label = label, colour = candidate),
    hjust = -0.2,
    vjust = 0.5,
    size = 3,
    family = "Jost Medium",
    fontface = "plain",
    show.legend = FALSE,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = CANDIDATE_COLORS) +
  scale_fill_manual(values = CANDIDATE_COLORS, guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(date_breaks = "6 months", labels = my_date_format()) +
  coord_cartesian(
    xlim = c(min(polls_long$midDate), max(polls_long$midDate) + 60),
    ylim = c(0, 1),
    clip = "off"
  ) +
  labs(
    y = "",
    x = "",
    title = "Trends in support for Ukrainian presidential candidates",
    color = "",
    caption = "."
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1, fill = NA))) +
  theme_plots()

ggsave(
  trends_ukr,
  file = here("ukraine", "trends_ukraine.png"),
  width = 7,
  height = 5,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white",
  device = png(type = "cairo")
)

#####House effects#####
pollster_names <- polls_long %>%
  distinct(pollster, org) %>%
  arrange(pollster)

# Average trend at end of data (no random effects)
avg_trend_draws <- expand_grid(
  time = last_time,
  candidate = factor(CANDIDATE_COLS, levels = CANDIDATE_COLS)
) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NA) %>%
  ungroup() %>%
  select(.draw, candidate, avg_epred = .epred)

# Pollster-specific predictions (with random effects)
pollster_effects_draws <- expand_grid(
  time = last_time,
  candidate = factor(CANDIDATE_COLS, levels = CANDIDATE_COLS),
  pollster = unique(polls_long$pollster)
) %>%
  add_epred_draws(object = m1, newdata = ., re_formula = NULL)

house_effects_data <- pollster_effects_draws %>%
  left_join(avg_trend_draws, by = c(".draw", "candidate")) %>%
  mutate(house_effect_pp = (.epred - avg_epred) * 100) %>%
  left_join(pollster_names, by = "pollster")

medians_house <- house_effects_data %>%
  group_by(org, candidate) %>%
  summarise(median_effect = median(house_effect_pp), .groups = "drop")

house_effects_plot <- house_effects_data %>%
  mutate(org = factor(org, levels = sort(unique(org)))) %>%
  ggplot(aes(
    y = org,
    x = house_effect_pp,
    color = candidate
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
    partition(vars(candidate)) +
  scale_fill_manual(values = CANDIDATE_COLORS, guide = "none") +
  scale_color_manual(name = " ", values = CANDIDATE_COLORS, guide = "none") +
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
  facet_wrap(~candidate, ncol = 2, scales = "free_x") +
  labs(
    x = "House effect (percentage points)",
    y = "",
    title = "Polling house effects by candidate",
    subtitle = "Systematic deviations from the average trend for each pollster",
    caption = "Positive values indicate the pollster tends to show higher support for that candidate"
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
  file = here("ukraine", "house_effects.png"),
  width = 8,
  height = 12,
  units = "cm",
  dpi = 600,
  scale = 3,
  bg = "white"
)
