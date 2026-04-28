# Deploy Shiny app to ShinyApps.io
#
# First-time setup (run once interactively):
# install.packages("rsconnect")
# rsconnect::setAccountInfo(name='ben-stanley',
# 			  token='4FE2D0A6A51DA03688330D965EE15E43',
# 			  secret='yazOJQcGO6rQ1RjYffzUKnmQgelx3FWAi9a/w8lG')
# Get your token/secret from: https://www.shinyapps.io/admin/#/tokens

if (!requireNamespace("pak", quietly = TRUE)) {
  install.packages("pak", repos = "https://cran.r-project.org")
}
pkgs <- c("rsconnect", "here")
missing_pkgs <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing_pkgs) > 0) {
  pak::pkg_install(missing_pkgs, ask = FALSE)
}
invisible(lapply(pkgs, library, character.only = TRUE))

deployApp(
  appDir = here(),
  appPrimaryDoc = "R/app.R",
  appFiles = c(
    "R/app.R",
    "data/trend_lines.rds",
    "data/date_summaries.rds",
    "data/point_dta.rds",
    "data/weekly_summaries.rds",
    "data/constituency_seats.rds",
    "data/const_map.rds",
    "data/const_map_cartogram.rds",
    "data/sim_weights.rds"
  ),
  appName = "polish-polls",
  forceUpdate = TRUE
)
