# Deploy Shiny app to ShinyApps.io
#
# First-time setup (run once interactively):
# install.packages("rsconnect")
# rsconnect::setAccountInfo(name='ben-stanley',
# 			  token='4FE2D0A6A51DA03688330D965EE15E43',
# 			  secret='yazOJQcGO6rQ1RjYffzUKnmQgelx3FWAi9a/w8lG')
# Get your token/secret from: https://www.shinyapps.io/admin/#/tokens

library(rsconnect)

deployApp(
  appDir = ".",
  appFiles = c("app.R", "trend_lines.rds", "date_summaries.rds", "point_dta.rds"),
  appName = "polish-polls",
  forceUpdate = TRUE
)
