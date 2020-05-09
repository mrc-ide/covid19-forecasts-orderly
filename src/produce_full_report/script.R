rmarkdown::render(
  "produce_full_report.Rmd",
  output_format = rmdformats::readthedown(
    self_contained = FALSE,
    css = "mystyle.css",
    include = list(
      before_body = "header.html",
      after_body = "footer.html"
    )
  )
)
