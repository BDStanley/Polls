if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.r-project.org")
}
pkgs <- c("dplyr", "lubridate", "stringr", "rvest")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  pak::pkg_install(missing_pkgs, ask = FALSE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

url <- "https://en.wikipedia.org/wiki/Opinion_polling_for_the_next_Polish_parliamentary_election"
page <- read_html(url)

# Extract all tables and find the polling-firm tables (2026 and 2025).
# Wikipedia inserts/removes summary tables over time, so locate the poll
# tables by their header rather than relying on a fixed table index.
tables <- page %>% html_table(fill = TRUE)
poll_table_idx <- which(sapply(tables, function(t) {
  ncol(t) >= 1 && names(t)[1] == "Polling firm/Link"
}))
polls_2026 <- tables[[poll_table_idx[1]]]
polls_2025 <- tables[[poll_table_idx[2]]]

# Function to parse fieldwork dates and create start and end dates, with dynamic year
parse_fieldwork_dates <- function(date_string, year) {
  date_string <- str_trim(date_string)
  date_string <- str_replace_all(date_string, "\"", "")

  # Split en dash, em dash, or hyphen
  if (str_detect(date_string, "(–|-|—)")) {
    parts <- unlist(str_split(date_string, "(–|-|—)"))
    start_part <- str_trim(parts[1])
    end_part <- str_trim(parts[2])

    # Extract month from end_part
    month_end <- str_extract(end_part, "[A-Za-zżółćęśąźń]+")
    month_end <- str_to_title(month_end)

    # Extract month from start_part if present; if not, use month from end_part
    month_start <- str_extract(start_part, "[A-Za-zżółćęśąźń]+")
    if (is.na(month_start)) {
      month_start <- month_end
    } else {
      month_start <- str_to_title(month_start)
    }

    # Extract days
    start_day <- str_extract(start_part, "\\d+")
    end_day <- str_extract(end_part, "\\d+")

    # Parse end date first with the given year
    end_date <- dmy(paste(end_day, month_end, year))

    # Parse start date - initially try with the same year
    start_date <- dmy(paste(start_day, month_start, year))

    # If start date is after end date, the start must be in the previous year
    # This handles polls that span the year boundary (e.g., Dec 2025 - Jan 2026)
    if (!is.na(start_date) && !is.na(end_date) && start_date > end_date) {
      start_year <- as.character(as.numeric(year) - 1)
      start_date <- dmy(paste(start_day, month_start, start_year))
    }
  } else {
    # Single date case
    start_date <- dmy(paste(date_string, year))
    end_date <- start_date
  }

  return(list(start_date = start_date, end_date = end_date))
}

# Function to extract just the polling agency name
extract_pollster_name <- function(pollster_string) {
  pollster_string <- str_trim(str_replace_all(pollster_string, "\"", ""))
  pollster_name <- str_split(pollster_string, " / ")[[1]][1]
  pollster_name <- str_trim(pollster_name)
  pollster_name <- str_replace_all(pollster_name, ",,", "")
  return(pollster_name)
}

# Function to standardize polling organization names
standardize_pollster_name <- function(pollster_name) {
  pollster_lower <- tolower(pollster_name)
  if (grepl("opinia\\s*24", pollster_lower)) {
    return("Opinia 24")
  } else if (grepl("^ipsos", pollster_lower)) {
    return("IPSOS")
  } else if (grepl("^ibris", pollster_lower)) {
    return("IBRiS")
  } else if (grepl("united\\s*surveys", pollster_lower)) {
    return("United Surveys")
  } else if (grepl("^cbos", pollster_lower)) {
    return("CBOS")
  } else if (grepl("research\\s*partner", pollster_lower)) {
    return("Research Partner")
  } else if (grepl("^pollster", pollster_lower)) {
    return("Pollster")
  } else if (grepl("social\\s*changes", pollster_lower)) {
    return("Social Changes")
  } else if (grepl("^ogb", pollster_lower)) {
    return("OGB")
  } else {
    return(pollster_name)
  }
}

# Strip Wikipedia footnote markers ("7.4[b]", "7.25[h]") before coercing a
# reported share to a number. Without this the whole cell parses to NA and the
# reading is silently lost.
parse_pct <- function(x) {
  as.numeric(str_remove_all(as.character(x), "\\[[a-zA-Z0-9]+\\]"))
}

# Main cleaning process
clean_polish_poll_data <- function(polls, year) {
  # Keep at most the first 15 columns, but don't error if the source table
  # has fewer (Wikipedia's column count changes as parties are added/removed).
  polls_clean_cols <- polls[, seq_len(min(ncol(polls), 15))]

  # R+ (Rozwój Plus) only appears once the table starts testing it, so the
  # earlier tables have no such column at all.
  if (!"R+" %in% names(polls_clean_cols)) {
    polls_clean_cols$`R+` <- NA_character_
  }

  cleaned_data <- polls_clean_cols %>%
    filter(
      !is.na(`Polling firm/Link`) &
        `Polling firm/Link` != "" &
        `Polling firm/Link` != "Polling firm/Link" &
        !grepl(
          "Presidential election|2023 parliamentary election|The Third Way|Confederation",
          `Polling firm/Link`,
          ignore.case = TRUE
        ) &
        !grepl("The Third Way|Confederation", Fieldworkdate, ignore.case = TRUE)
    ) %>%
    select(
      pollster_raw = `Polling firm/Link`,
      fieldwork_date = Fieldworkdate,
      law_and_justice = `PiS`,
      rozwoj_plus = `R+`,
      civic_coalition = `KO`,
      poland_2050 = `Polska 2050`,
      polish_peoples_party = `PSL`,
      the_left = `Lewica`,
      together = Razem,
      confederation = Konfederacja,
      confederation_crown = `KKP`,
      dont_know = `Don't know`
    ) %>%
    mutate(
      pollster_raw = sapply(pollster_raw, extract_pollster_name),
      pollster = sapply(pollster_raw, standardize_pollster_name)
    ) %>%
    select(-pollster_raw) %>%
    # Wikipedia merges the PiS and R+ cells (colspan=2) in every poll that did
    # not test R+ as a separate party, so html_table() copies the PiS figure
    # across into the R+ column. An identical string therefore means R+ is
    # still counted inside PiS, not that it polled at PiS's level.
    mutate(
      rplus_separate = !is.na(rozwoj_plus) &
        as.character(rozwoj_plus) != as.character(law_and_justice)
    ) %>%
    mutate(
      law_and_justice = parse_pct(law_and_justice),
      rozwoj_plus = ifelse(rplus_separate, parse_pct(rozwoj_plus), 0),
      civic_coalition = parse_pct(civic_coalition),
      poland_2050 = parse_pct(poland_2050),
      polish_peoples_party = parse_pct(polish_peoples_party),
      the_left = parse_pct(the_left),
      together = parse_pct(together),
      confederation = parse_pct(confederation),
      confederation_crown = parse_pct(confederation_crown),
      dont_know = parse_pct(dont_know)
    ) %>%
    mutate(
      rozwoj_plus = ifelse(is.na(rozwoj_plus), 0, rozwoj_plus),
      the_left = ifelse(is.na(the_left), 0, the_left),
      together = ifelse(is.na(together), 0, together),
      polish_peoples_party = ifelse(
        is.na(polish_peoples_party),
        0,
        polish_peoples_party
      ),
      poland_2050 = ifelse(is.na(poland_2050), 0, poland_2050),
      confederation_crown = ifelse(
        is.na(confederation_crown),
        0,
        confederation_crown
      ),
      dont_know = ifelse(is.na(dont_know), 0, dont_know)
    )

  # Where a pollster published both a baseline reading (R+ inside PiS) and an
  # R+ scenario reading from the same fieldwork, Wikipedia lists them as two
  # rows. Keep the scenario reading, which is the one that separates the two
  # parties, and drop its baseline twin so the fieldwork enters the model once.
  fieldwork_key <- paste(cleaned_data$pollster, cleaned_data$fieldwork_date)
  scenario_keys <- fieldwork_key[cleaned_data$rplus_separate]
  cleaned_data <- cleaned_data[
    !(!cleaned_data$rplus_separate & fieldwork_key %in% scenario_keys),
  ]

  date_results <- lapply(cleaned_data$fieldwork_date, function(date_string) {
    parse_fieldwork_dates(date_string, year)
  })

  cleaned_data$start_date <- sapply(date_results, function(x) {
    as.character(x$start_date)
  })
  cleaned_data$end_date <- sapply(date_results, function(x) {
    as.character(x$end_date)
  })

  cleaned_data$start_date <- as.Date(cleaned_data$start_date)
  cleaned_data$end_date <- as.Date(cleaned_data$end_date)

  final_data <- cleaned_data %>%
    mutate(
      june_17_2025 = as.Date("2025-06-17"),
      june_10_2025 = as.Date("2025-06-10"),
      march_10_2025 = as.Date("2025-03-10"),
      trzecia_droga_combined = ifelse(
        end_date > june_17_2025,
        poland_2050 + polish_peoples_party,
        poland_2050
      ),
      polska_2050_split = trzecia_droga_combined * 0.6,
      psl_split = trzecia_droga_combined * 0.4,
      lewica_separate = the_left,
      razem_separate = together,
      confederation_clean = ifelse(
        end_date <= march_10_2025,
        confederation,
        confederation
      ),
      # KKP handling: separate column from June 10, 2025 onwards
      kkp_separate = ifelse(end_date >= june_10_2025, confederation_crown, 0),
      kkp_to_other = ifelse(end_date < june_10_2025, confederation_crown, 0)
    ) %>%
    mutate(
      total_parties = law_and_justice +
        rozwoj_plus +
        civic_coalition +
        polska_2050_split +
        psl_split +
        lewica_separate +
        razem_separate +
        confederation_clean +
        kkp_separate +
        dont_know,
      Other = pmax(0, 100 - total_parties + kkp_to_other)
    ) %>%
    select(
      org = pollster,
      startDate = start_date,
      endDate = end_date,
      PiS = law_and_justice,
      Rplus = rozwoj_plus,
      KO = civic_coalition,
      Polska2050 = polska_2050_split,
      PSL = psl_split,
      Lewica = lewica_separate,
      Razem = razem_separate,
      Konfederacja = confederation_clean,
      KKP = kkp_separate,
      DK = dont_know,
      Other
    ) %>%
    filter(!is.na(startDate) & !is.na(endDate) & !is.na(PiS)) %>%
    arrange(desc(endDate))

  return(final_data)
}

# Process both 2026 and 2025 polls separately
polls_cleaned_2026 <- clean_polish_poll_data(polls_2026, "2026")
polls_cleaned_2025 <- clean_polish_poll_data(polls_2025, "2025")

# Merge both datasets
polls_cleaned <- bind_rows(polls_cleaned_2026, polls_cleaned_2025) %>%
  arrange(desc(endDate))

cat("Cleaned Polish polling data summary:\n")
cat("Total polls:", nrow(polls_cleaned), "\n")
cat(
  "Date range:",
  as.character(min(polls_cleaned$startDate)),
  "to",
  as.character(max(polls_cleaned$endDate)),
  "\n"
)
cat("Missing 'DK' values:", sum(is.na(polls_cleaned$DK)), "\n")
cat(
  "Polls reporting R+ separately from PiS:",
  sum(polls_cleaned$Rplus > 0),
  "of",
  nrow(polls_cleaned),
  "\n\n"
)

numeric_cols <- c(
  "PiS",
  "Rplus",
  "KO",
  "Polska2050",
  "PSL",
  "Lewica",
  "Razem",
  "Konfederacja",
  "KKP",
  "DK",
  "Other"
)
cat("Column ranges:\n")
for (col in numeric_cols) {
  values <- polls_cleaned[[col]]
  cat(sprintf(
    "%-20s: %5.1f - %5.1f (Missing: %d)\n",
    col,
    min(values, na.rm = TRUE),
    max(values, na.rm = TRUE),
    sum(is.na(values))
  ))
}

cat("\nFirst 5 rows of cleaned data:\n")
print(head(polls_cleaned, 5))
