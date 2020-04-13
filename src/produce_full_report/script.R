rmarkdown::render(
  "produce_full_report.Rmd",
  output_format = rmdformats::readthedown(
    self_contained = FALSE,
    css = "mystyle.css"
  )
)
