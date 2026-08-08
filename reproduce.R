ensure_pandoc <- function() {
  if (rmarkdown::pandoc_available()) return(invisible(TRUE))

  candidates <- c(
    "C:/Program Files/RStudio/resources/app/bin/quarto/bin/tools",
    "C:/Program Files/Pandoc",
    file.path(Sys.getenv("LOCALAPPDATA"), "Pandoc")
  )

  candidates <- candidates[file.exists(file.path(candidates, "pandoc.exe"))]
  if (length(candidates) > 0) {
    Sys.setenv(RSTUDIO_PANDOC = candidates[[1]])
  }

  if (!rmarkdown::pandoc_available()) {
    stop("Pandoc is required to render the R Markdown report.")
  }

  invisible(TRUE)
}

dir.create("outputs", showWarnings = FALSE)

cat("\n=== Regression checks ===\n")
source("extract_results.R")

cat("\n=== Productization and missing-rating robustness checks ===\n")
source("robustness_extra.R")

cat("\n=== fsQCA checks ===\n")
source("extract_fsqca.R")

cat("\n=== Rendering report ===\n")
ensure_pandoc()
rmarkdown::render(
  "ch2_empirical_report.Rmd",
  output_dir = "outputs",
  quiet = FALSE
)

cat("\nReplication complete. See outputs/ch2_empirical_report.html\n")
