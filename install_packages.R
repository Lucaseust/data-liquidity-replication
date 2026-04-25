required_packages <- c(
  "jsonlite",
  "dplyr",
  "tidyr",
  "stringr",
  "ggplot2",
  "scales",
  "fixest",
  "QCA",
  "kableExtra",
  "MASS",
  "rmarkdown",
  "knitr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing, repos = "https://cloud.r-project.org")
}

cat("Package check complete.\n")
