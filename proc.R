duf_log <- fs::path("Observation log.xls")
dp2_log <- fs::path("Obs_log.txt")

get_duf_obs <- function(duf_log_path = duf_log) {
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


get_date_comment <- function(str) {
  str <- vctrs::vec_slice(str, 1L)
  stringr::str_match(
    str,
    "^\\s*(\\d{1,2})\\s*[/7]\\s*(\\d{1,2})\\s*[/7]\\s*(\\d{1,2})\\s*(.*)\\s*$"
  ) |>
    magrittr::extract(, -1L) -> parsed
  
  list(
    Date = lubridate::dmy(paste(parsed[1], parsed[2], parsed[3], sep = "/")),
    Comment = stringr::str_to_sentence(parsed[4])
  )
}

exp_time_pattern <- "~?((?:\\d+|\\d+[\\.,]\\d+)(?:\\s+sec)?)"
obj_name_pattern <- "([\\w\\+\\.]+(?:\\s{1,3}[\\w\\+\\.]+)?)"#"([\\w\\+\\.]+|[\\w\\+]+\\s{1,3}[\\w\\+]+)"
dp2_pattern_1 <- paste0(
  "^\\s*",
  "(?:\\*(?:[Nn]ot|[Aa]lso\\s*not)\\*\\s*)?",
  obj_name_pattern,                            # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "\\s+",
  "(\\d+(?:/\\d+)?)",                          # N
  "\\s+",
  exp_time_pattern,                            # ExpTime
  "(?:\\s+([\\w -]+))?"                        # Type
)

dp2_pattern_2 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  exp_time_pattern,                            # ExpTime
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "(?:\\s+(\\d+(?:/\\d+)?))?",                 # N
  "(?:\\s+([\\w -]+))?"                        # Type
)

dp2_pattern_3 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "\\s+",
  exp_time_pattern,                            # ExpTime
  "(?:\\s+([\\w -]+))?"                        # Type
)

dp2_pattern_4 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)"             # Focus
)

parse_with_pattern <- function(
  txt, pattern, names
) {
  txt |>
    stringr::str_match(pattern) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(c("DISCARD", names))
}

get_objects <- function(str) {
  patterns = vctrs::vec_c(
    dp2_pattern_1,
    dp2_pattern_2,
    dp2_pattern_3,
    dp2_pattern_4
  )
  str <- vctrs::vec_slice(str, -1L) |>
    stringr::str_replace("\\t", " ")

  patterns |>
    purrr::reduce(stringr::str_subset, negate = TRUE, .init = str) |>
    stringr::str_trim() |>
    stringr::str_to_sentence() -> comments

  parse_with_pattern(
      str,
      dp2_pattern_1,
      c("Object", "Focus", "N", "ExpTime", "Description")
  ) -> match

  if (all(is.na(match))) {
    parse_with_pattern(
        str,
        dp2_pattern_2,
        c("Object", "ExpTime", "Focus", "N", "Description")
    ) -> match
  }

  if (all(is.na(match))) {
     parse_with_pattern(
        str,
        dp2_pattern_3,
        c("Object", "Focus", "ExpTime", "Description")
    ) -> match
  }


  match <- match |>
    dplyr::filter(!is.na(DISCARD)) |>
    dplyr::select(-DISCARD)

  list(Objects = match, Comments = comments)
}

get_dp2_obs <- function(dp2_log_path = dp2_log) {
  txt <- brio::read_lines(dp2_log_path)
  start <- stringr::str_which(
    txt,
    "^\\s*\\d{1,2}\\s*[/7]\\s*\\d{1,2}\\s*[/7]\\s*\\d{1,2}"
  )
  end <- vctrs::vec_c(start[-1], vctrs::vec_size(txt) + 1L)

  purrr::map2(start, end, ~vctrs::vec_slice(txt, seq(.x, .y - 1L))) |>
    purrr::map(~vctrs::vec_slice(.x, nzchar(.x))) |>
    purrr::map(stringr::str_subset, "^\\s*-+\\s*$", negate = TRUE) -> nights

  nights |>
    purrr::map_dfr(function(x) {
      dt_cm <- get_date_comment(x)
      obj_cm <-  get_objects(x)
      cm <- c(dt_cm$Comment, obj_cm$Comments)
      cm <- vctrs::vec_slice(cm, nzchar(cm)) |>
        paste(collapse = "; ")

      data <- dplyr::mutate(
          obj_cm$Objects,
          Date = dt_cm$Date,
          .before = dplyr::everything()
        ) |>
        dplyr::mutate(Comment = cm)
      # Currently not using comments
      data
    }) |>
    dplyr::mutate(
      Focus = Focus |>
        stringr::str_replace("=", "-") |>
        stringr::str_replace(",", ".") |>
        stringr::str_replace("-{2,}|n/a", NA_character_) |>
        readr::parse_double(),
      N = N |> 
        strsplit("/") |>
        purrr::map(readr::parse_integer) |>
        purrr::map_int(sum),
      ExpTime = ExpTime |>
        stringr::str_replace("\\s*sec", "") |>
        readr::parse_double(),
      Object = dplyr::if_else(
        ExpTime > 1800,
        paste(Object, ExpTime),
        Object
      ),
      ExpTime = dplyr::if_else(ExpTime > 1800, NA_real_, ExpTime),
      Instrument = forcats::as_factor("DIPol-2"),
      Telescope = forcats::as_factor("T60")
    ) -> result

    which(is.na(result[["Date"]])) -> na_dates

    vctrs::vec_c(0L, which(diff(na_dates) != 1L), length(na_dates)) -> na_groups
    purrr::map2(
      na_groups[-length(na_groups)] + 1L,
      na_groups[-1],
      ~list(
        Start = na_dates[.x],
        End = na_dates[.y],
        Value = lubridate::ymd("1970/01/01") +
          lubridate::days(
            0.5 * (
              unclass(result[["Date"]][na_dates[.x] - 1L]) +
              unclass(result[["Date"]][na_dates[.y] + 1L])
            )
          )
      )
    ) -> na_gaps

    new_dates <- vctrs::vec_init(result[["Date"]], vctrs::vec_size(result))
    for (item in na_gaps) {
      vctrs::vec_slice(
        new_dates, 
        seq(from = item[["Start"]], to = item[["End"]])
      ) <- item[["Value"]]
    }

  result |>
    dplyr::mutate(
      Date = dplyr::if_else(is.na(Date), new_dates, Date),
      Date = dplyr::if_else(
        Date < lubridate::ymd("2010/01/01"),
        Date + lubridate::years(10L),
        Date
      )
    )
}

# dplyr::bind_rows(
#   get_duf_obs() -> data1,
#   get_dp2_obs() -> data2
# ) |>
#   dplyr::transmute(
#     Date, Object, Type, ExpTime, N, Focus, 
#     Inst = Instrument,
#     Tlscp = Telescope,
#     Comment
#   ) |>
#   dplyr::arrange(Date) -> data
  

output <- fs::dir_create("output")

data |>
  readr::write_csv(fs::path(output, "obslog.csv"))
data |>
  dplyr::select(-Comment) |>
  write_fixed(
    fs::path(output, "obslog.txt")
  )