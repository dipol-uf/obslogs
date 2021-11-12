box::use(
  readxl[read_xls],
  dplyr[transmute, filter],
  lubridate[as_date],
  forcats[as_factor],
  readr[parse_double, parse_integer]
  
)

#' @export
get_duf_obs <- function(duf_log_path = duf_log) {
  read_xls(
    duf_log_path,
    col_types = c(
      "date",
      "text",
      "text",
      "text",
      "text",
      "numeric",
      "text"
    )) |>
    transmute(
      Date = as_date(Date),
      Type = as_factor(Type),
      Object = `Star name`,
      ExpTime = `Exposure time` |> parse_double(),
      N = (!!as.name("# observations")) |> parse_integer(),
      Focus,
      Comment = Comments,
      Instrument = as_factor("DIPol-UF"),
      Telescope = as_factor("NOT")
    ) |>
    filter(!is.na(Object))
}