input <- fs::dir_create("input")
duf_log <- fs::path(input, "Observation log.xls")
dp2_log <- fs::path(input, "Obs_log.txt")

future::plan(
  future::cluster(
    workers = max(c(parallel::detectCores() - 2L, 2L))
  )
)

dplyr::bind_rows(
  get_duf_obs(),
  get_dp2_obs()
) |>
  dplyr::transmute(
    Date, Object, Type, ExpTime, N, Focus,
    Inst = Instrument,
    Tlscp = Telescope,
    Comment
  ) |>
  dplyr::arrange(Date) -> data
  

output <- fs::dir_create("output")

data |>
  readr::write_csv(fs::path(output, "obslog.csv"))

data |>
  dplyr::select(-Comment) |>
  write_fixed(
    fs::path(output, "obslog.txt")
  )

data |>
  jsonlite::write_json(fs::path(output, "obslog.json"), pretty = TRUE)