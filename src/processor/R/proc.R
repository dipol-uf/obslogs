#' Processes Dipol-UF and Dipol-2 observation logs
#' Generates observation databases as plaintext, json, csv and sqlite.
#' @param path Root directory. `path/"input"` is searched for input files.
#' @param out Where to write output.
#' @export
process_observation_logs <- function(
  path = fs::path("."),
  out = fs::path(path, "output")
) {

  input <- fs::path(path, "input")
  duf_log <- fs::path(input, "Observation log.xls")
  dp2_log <- fs::path(input, "Obs_log.txt")

  future::plan(
    future::cluster(
      workers = max(c(parallel::detectCores() - 2L, 2L))
    )
  )

  dplyr::bind_rows(
    get_duf_obs(duf_log),
    get_dp2_obs(dp2_log)
  ) |>
    dplyr::transmute(
      Date, Object, Type, ExpTime, N, Focus,
      Inst = Instrument,
      Tlscp = Telescope,
      Comment
    ) |>
    dplyr::arrange(Date) -> data


  output <- fs::dir_create(out)

  data |>
    readr::write_csv(fs::path(output, "obslog.csv"))

  data |>
    dplyr::select(-Comment) |>
    write_fixed(
      fs::path(output, "obslog.txt")
    )

  data |>
    jsonlite::write_json(fs::path(output, "obslog.json"), pretty = TRUE)

  invisible(NULL)
}