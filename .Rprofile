# Set user library as first in library paths
user_lib <- Sys.getenv("R_LIBS_USER")
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE)
}
.libPaths(c(user_lib, .libPaths()))

# Load pacman package on startup
if (!require("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)
p_update()
