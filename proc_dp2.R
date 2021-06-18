

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
obj_name_pattern <- "([\\w\\+\\.]+(?:\\s{1,3}[\\w\\+\\.]+)?)"
type_pattern <- "(?:\\s+([\\w -|]+))?"
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
  type_pattern                                 # Type
)

dp2_pattern_2 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  exp_time_pattern,                            # ExpTime
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "(?:\\s+(\\d+(?:/\\d+)?))?",                 # N
  type_pattern                                 # Type
)

dp2_pattern_3 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "\\s+",
  exp_time_pattern,                            # ExpTime
  type_pattern                                 # Type
)

dp2_pattern_4 <- paste0(
  "^\\s*",
  obj_name_pattern,                            # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "(?:\\s+(\\d+(?:/\\d+)?))?",                 # N
  type_pattern                                 # Type
)

parse_with_pattern <- function(
  txt, pattern, names, id
) {
  txt |>
    stringr::str_match(pattern) |>
    tibble::as_tibble(.name_repair = "minimal") |>
    rlang::set_names(c("DISCARD", names)) |>
    dplyr::mutate(
      Id = seq_len(dplyr::n()),
      MatchId = id,
      .before = dplyr::everything()
    )
}

get_objects <- function(str) {
  patterns <- vctrs::vec_c(
    dp2_pattern_1,
    dp2_pattern_2,
    dp2_pattern_3,
    dp2_pattern_4
  )
  str <- vctrs::vec_slice(str, -1L) |>
    stringr::str_replace("\t", " ")

  patterns |>
    purrr::reduce(stringr::str_subset, negate = TRUE, .init = str) |>
    stringr::str_trim() |>
    stringr::str_to_sentence() -> comments

  parse_with_pattern(
      str,
      dp2_pattern_1,
      c("Object", "Focus", "N", "ExpTime", "Description"),
      1L
  ) -> match_1

  parse_with_pattern(
      str,
      dp2_pattern_2,
      c("Object", "ExpTime", "Focus", "N", "Description"),
      2L
  ) -> match_2

  parse_with_pattern(
      str,
      dp2_pattern_3,
      c("Object", "Focus", "ExpTime", "Description"),
      3L
  ) -> match_3

  parse_with_pattern(
      str,
      dp2_pattern_4,
      c("Object", "Focus", "N", "Description"),
      4L
  ) -> match_4


  match <- dplyr::bind_rows(
    match_1,
    match_2,
    match_3,
    match_4
  ) |>
    dplyr::filter(!is.na(DISCARD))

  match |>
    dplyr::mutate(
      Test = purrr::pmap_int(
        match, 
        function(...) {
          args <- rlang::list2(...)
          result <-
            purrr::map2_int(
              args,
              rev(seq_along(args)),
               ~is.na(.x) * .y
            ) |> sum()

          n_exp_time <- nchar(stringr::str_replace(args$ExpTime, "\\s*sec", ""))
          if (vctrs::vec_is_empty(n_exp_time) || rlang::is_na(n_exp_time)) {
            n_exp_time <- 2L
          }
          
          if (
            rlang::is_na(args$ExpTime) &&
            rlang::is_na(args$N) && 
            rlang::is_na(args$Description)
          ) {
            .Machine$integer.max
          } else {
            result + n_exp_time
          }
        }
      )
    ) -> match


  match |>
    dplyr::group_split(Id) |>
    purrr::map(
      dplyr::filter,
      Test == min(Test)
    ) |>
    purrr::map(dplyr::arrange, MatchId) |>
    purrr::map_dfr(vctrs::vec_slice, 1L)  -> match

  if (vctrs::vec_size(match) != 0L) {
    match <- match |>
      dplyr::filter(Test < .Machine$integer.max) |>
      dplyr::select(-DISCARD, -Id, -MatchId, -Test)
  }

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
    furrr::future_map_dfr(
    # purrr::map_dfr(
      function(x) {
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

        data
      }
      ,.progress = TRUE
    ) |>
    dplyr::mutate(
      Focus = Focus |>
        stringr::str_replace("=", "-") |>
        stringr::str_replace(",", ".") |>
        stringr::str_replace("^-+$|n/a", NA_character_) |>
        readr::parse_double(),
      N = N |> 
        strsplit("/") |>
        purrr::map(readr::parse_integer) |>
        purrr::map_int(sum),
      ExpTime = ExpTime |>
        stringr::str_replace("\\s*sec", "") |>
        readr::parse_double(),
      Object = dplyr::if_else(
        !is.na(ExpTime) & ExpTime > 1800,
        paste(Object, ExpTime),
        Object
      ),
      ExpTime = dplyr::if_else(ExpTime > 1800, NA_real_, ExpTime),
      Instrument = forcats::as_factor("DIPol-2"),
      Telescope = forcats::as_factor("T60")
    ) -> result

    which(is.na(result[["Date"]])) -> na_dates
    if (!vctrs::vec_is_empty(na_dates)) {
      vctrs::vec_c(
        0L,
        which(diff(na_dates) != 1L),
        length(na_dates)
      ) -> na_groups
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
        Date = dplyr::if_else(is.na(Date), new_dates, Date)
      ) -> result

  }
  result |>
    dplyr::mutate(
      Date = dplyr::if_else(
        Date < lubridate::ymd("2010/01/01"),
        Date + lubridate::years(10L),
        Date
      )
    )
}
