box::use(
  duf = ./R/proc_duf[get_duf_obs],
  dp2 = ./R/proc_dp2[get_dp2_obs],
  ./R/write_fixed[write_fixed],
  fs[dir_create, fs_path = path],
  future[plan, cluster],
  parallel[detectCores],
  dplyr[bind_rows, transmute, arrange, select, n, mutate],
  readr[write_csv],
  jsonlite[write_json],
  DBI[db_connect = dbConnect, db_disconnect = dbDisconnect, db_write_table = dbWriteTable],
  RSQLite[sqlite = SQLite]
)


input <- dir_create("input")
duf_log <- fs_path(input, "Observation log.xls")
dp2_log <- fs_path(input, "Obs_log.txt")

plan(
  cluster(
    workers = min(max(c(detectCores() - 2L, 2L)), 6L)
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


con <- db_connect(sqlite(), dbname = fs_path(output, "obslog.sqlite"))
tryCatch({
    db_write_table(
      con, 
      "obslog", 
      data |> mutate(Date = as.character(Date), Key = seq_len(n()), .before = Date),
      overwrite = TRUE,
      field.types = c(
        Key = "INTEGER PRIMARY KEY",
        Date = "CHARACTER(10)",
        Object = "VARCHAR(16)",
        Type = "VARCHAR(24)",
        ExpTime = "REAL",
        N = "INTEGER",
        Focus = "REAL",
        Inst = "VARCHAR(12)",
        Tlscp = "VARCHAR(12)",
        Comment = "VARCHAR"
      )
    )
  },
  finally = db_disconnect(con)
)