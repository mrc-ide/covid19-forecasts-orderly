rmarkdown::render(
  "produce_full_report.Rmd",
  output_format = rmarkdown::html_document(
    self_contained = FALSE, theme = "cerulean"
  )
)
