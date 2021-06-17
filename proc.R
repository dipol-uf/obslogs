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
      Night = Date,
      Type = forcats::as_factor(Type),
      Object = forcats::as_factor(`Star name`),
      Exptime = `Exposure time`,
      N = !!as.name("# observations"),
      Focus,
      Comment = Comments
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
    dp2_pattern_3
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
    dplyr::filter(!is.na(DISCARD)) #|>
    # dplyr::select(-DISCARD)

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
    # vctrs::vec_slice(100:103) |>
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
        )
      # Currently not using comments
      data
    }) |>
    dplyr::mutate(
      Focus = Focus |>
        stringr::str_replace("=", "-") |>
        readr::parse_double(),
      N = readr::parse_integer(N),
      ExpTime = ExpTime |>
        stringr::str_replace("\\s*sec", "")
    )

}


get_dp2_obs() |>
  # dplyr::filter(!is.na(DISCARD)) |>
  print() -> data# |>
  # readr::write_csv("test.csv")
#   # print(n = 300)
