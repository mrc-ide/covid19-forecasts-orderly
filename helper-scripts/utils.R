date2words <- function(x) format(x, "%d %B")
phase_for_week <- function(start, end) {
  yr1 <- year(start)
  yr2 <- year(end)
  out <- case_when(
    yr1 == yr2 ~ glue("{date2words(start)}-{date2words(end)} {yr1}"),
    yr1 != yr2 ~ glue("{date2words(start)} {yr1}-{date2words(end)} {yr2}")
  )
  out
}
