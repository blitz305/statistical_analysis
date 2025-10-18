dir_mid <- "mid_result"
dir_result <- "result"

# Ensure directories exist
if (!dir.exists(dir_mid)) dir.create(dir_mid, recursive = TRUE, showWarnings = FALSE)
if (!dir.exists(dir_result)) dir.create(dir_result, recursive = TRUE, showWarnings = FALSE)

path_mid <- function(filename) file.path(dir_mid, filename)
path_result <- function(filename) file.path(dir_result, filename)


