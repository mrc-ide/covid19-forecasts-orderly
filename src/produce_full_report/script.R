rmarkdown::render(
  "produce_full_report.Rmd",
  output_format = rmdformats::readthedown(
    self_contained = FALSE,
    css = "other_files/mystyle.css",
    include = list(
      before_body = "other_files/header.html",
      after_body = "other_files/footer.html"
    )
    ),
  output_file = "index.html"
)
