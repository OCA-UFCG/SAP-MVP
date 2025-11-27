# requirements.R
# Automatically detect and install all packages used in your app
# Just to mention that global.r load all packages used in the app 

# List of R files to scan (add others if needed)
r_files <- c("app.R", "global.R", ".Rhistory", "./R/electre_tri_b_func.R", "./R/mod_analise.R", "./R/mod_preproc.R")



# Extract all library() and require() calls
packages <- unique(unlist(lapply(r_files, function(f) {
  if (file.exists(f)) {
    lines <- readLines(f, warn = FALSE)
    libs <- gsub(".*(library|require)\\((['\"]?)([A-Za-z0-9\\.]+)\\2\\).*", "\\3", grep("(library|require)\\(", lines, value = TRUE))
    libs[libs != lines]  # only return actual package names
  } else {
    character(0)
  }
})))

# Remove any duplicates and base packages
base_pkgs <- rownames(installed.packages(priority = "base"))
packages <- setdiff(packages, base_pkgs)

# Print packages to install
cat("Installing packages:", paste(packages, collapse = ", "), "\n")

# Install missing ones
if (length(packages) > 0) {
  install.packages(setdiff(packages, installed.packages()[, "Package"]))
} else {
  cat("No additional packages detected.\n")
}
