required_packages <- c(
  "jsonlite",
  "dplyr",
  "tidyr",
  "stringr",
  "readr",
  "ggplot2",
  "scales",
  "QCA",
  "kableExtra",
  "MASS",
  "sandwich",
  "clubSandwich",
  "rmarkdown",
  "knitr"
)

installed <- rownames(installed.packages())
missing <- setdiff(required_packages, installed)

if (length(missing) > 0) {
  install.packages(missing)
}

still_missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(still_missing) > 0) {
  stop(
    "Package installation incomplete: ",
    paste(still_missing, collapse = ", ")
  )
}

cat("Package check complete.\n")
