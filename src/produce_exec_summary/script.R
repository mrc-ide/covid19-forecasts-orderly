rmarkdown::render(
  input = "produce_exec_summary.Rmd",
  output_format = rmarkdown::html_fragment(),
  output_file = "executive_summary.html"
)
