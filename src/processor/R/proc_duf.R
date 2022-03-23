get_duf_obs <- function(duf_log_path) {
  readxl::read_xls(
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
    dplyr::transmute(
      Date = lubridate::as_date(Date),
      Type = forcats::as_factor(Type),
      Object = `Star name`,
      ExpTime = `Exposure time` |> readr::parse_double(),
      N = (!!as.name("# observations")) |> readr::parse_integer(),
      Focus,
      Comment = Comments,
      Instrument = forcats::as_factor("DIPol-UF"),
      Telescope = forcats::as_factor("NOT")
    ) |>
    dplyr::filter(!is.na(Object))
}