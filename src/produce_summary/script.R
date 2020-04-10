rmarkdown::render(
  input = "produce_summary.Rmd",
  output_format = rmarkdown::html_fragment(),
  output_file = "summary.html"
)
