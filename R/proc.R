box::use(
  duf = ./R/proc_duf[get_duf_obs],
  dp2 = ./R/proc_dp2[get_dp2_obs],
  ./R/write_fixed[write_fixed],
  fs[dir_create, fs_path = path],
  future[plan, cluster],
  parallel[detectCores],
  dplyr[bind_rows, transmute, arrange, select],
  readr[write_csv],
  jsonlite[write_json]
)


input <- dir_create("input")
duf_log <- fs_path(input, "Observation log.xls")
dp2_log <- fs_path(input, "Obs_log.txt")

plan(
  cluster(
    workers = max(c(detectCores() - 2L, 2L))
  )
)

bind_rows(
  get_duf_obs(duf_log),
  get_dp2_obs(dp2_log)
) |>
  transmute(
    Date, Object, Type, ExpTime, N, Focus,
    Inst = Instrument,
    Tlscp = Telescope,
    Comment
  ) |>
  arrange(Date) -> data


output <- dir_create("output")

data |>
  write_csv(fs_path(output, "obslog.csv"))

data |>
  select(-Comment) |>
  write_fixed(
    fs_path(output, "obslog.txt")
  )

data |>
  write_json(fs_path(output, "obslog.json"), pretty = TRUE)