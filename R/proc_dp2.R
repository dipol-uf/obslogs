box::use(
  vctrs[vec_slice, vec_c, vec_size, vec_is_empty, vec_init, `vec_slice<-`],
  stringr[str_match, str_to_sentence, str_which, str_subset, str_replace, str_trim],
  purrr[map, map2, map_int, reduce, pmap_int, map2_int, map_dfr],
  furrr[future_map_dfr],
  dplyr[mutate, everything, if_else, n, bind_rows, filter, group_split, arrange, select],
  readr[parse_double, parse_integer],
  forcats[as_factor],
  magrittr[extract],
  brio[read_lines],
  lubridate[dmy, ymd, days, years],
  tibble[as_tibble],
  rlang[set_names, list2, is_na]
)

get_date_comment <- function(str) {
  str <- vec_slice(str, 1L)
  str_match(
    str,
    "^\\s*(\\d{1,2})\\s*[/7]\\s*(\\d{1,2})\\s*[/7]\\s*(\\d{1,2})\\s*(.*)\\s*$"
  ) |>
    extract(, -1L) -> parsed

  list(
    Date = dmy(paste(parsed[1], parsed[2], parsed[3], sep = "/")),
    Comment = str_to_sentence(parsed[4])
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
    str_match(pattern) |>
    as_tibble(.name_repair = "minimal") |>
    set_names(c("DISCARD", names)) |>
    mutate(
      Id = seq_len(n()),
      MatchId = id,
      .before = everything()
    )
}

get_objects <- function(str) {
  patterns <- vec_c(
    dp2_pattern_1,
    dp2_pattern_2,
    dp2_pattern_3,
    dp2_pattern_4
  )
  str <- vec_slice(str, -1L) |>
    str_replace("\t", " ")

  patterns |>
    reduce(str_subset, negate = TRUE, .init = str) |>
    str_trim() |>
    str_to_sentence() -> comments

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


  match <- bind_rows(
    match_1,
    match_2,
    match_3,
    match_4
  ) |>
    filter(!is.na(DISCARD))

  match |>
    mutate(
      Test = pmap_int(
        match,
        function(...) {
          args <- list2(...)
          result <-
            map2_int(
              args,
              rev(seq_along(args)),
               ~is.na(.x) * .y
            ) |> sum()

          n_exp_time <- nchar(str_replace(args$ExpTime, "\\s*sec", ""))
          if (vec_is_empty(n_exp_time) || is_na(n_exp_time)) {
            n_exp_time <- 2L
          }

          if (
            is_na(args$ExpTime) &&
            is_na(args$N) &&
            is_na(args$Description)
          ) {
            .Machine$integer.max
          } else {
            result + n_exp_time
          }
        }
      )
    ) -> match


  match |>
    group_split(Id) |>
    map(
      filter,
      Test == min(Test)
    ) |>
    map(arrange, MatchId) |>
    map_dfr(vec_slice, 1L)  -> match

  if (vec_size(match) != 0L) {
    match <- match |>
      filter(Test < .Machine$integer.max) |>
      select(-DISCARD, -Id, -MatchId, -Test)
  }

  list(Objects = match, Comments = comments)
}



#' @export
get_dp2_obs <- function(dp2_log_path = dp2_log) {
  txt <- read_lines(dp2_log_path)
  start <- str_which(
    txt,
    "^\\s*\\d{1,2}\\s*[/7]\\s*\\d{1,2}\\s*[/7]\\s*\\d{1,2}"
  )
  end <- vec_c(start[-1], vec_size(txt) + 1L)

  map2(start, end, ~vec_slice(txt, seq(.x, .y - 1L))) |>
    map(~vec_slice(.x, nzchar(.x))) |>
    map(str_subset, "^\\s*-+\\s*$", negate = TRUE) -> nights

  nights |>
    future_map_dfr(
    # purrr::map_dfr(
      function(x) {
        dt_cm <- get_date_comment(x)
        obj_cm <-  get_objects(x)
        cm <- c(dt_cm$Comment, obj_cm$Comments)
        cm <- vec_slice(cm, nzchar(cm)) |>
          paste(collapse = "; ")

        data <- mutate(
            obj_cm$Objects,
            Date = dt_cm$Date,
            .before = everything()
          ) |>
          mutate(Comment = cm)

        data
      },
      .progress = TRUE
    ) |>
    mutate(
      Focus = Focus |>
        str_replace("=", "-") |>
        str_replace(",", ".") |>
        str_replace("^-+$|n/a", NA_character_) |>
        parse_double(),
      N = N |> 
        strsplit("/") |>
        map(parse_integer) |>
        map_int(sum),
      ExpTime = ExpTime |>
        str_replace("\\s*sec", "") |>
        parse_double(),
      Object = if_else(
        !is.na(ExpTime) & ExpTime > 1800,
        paste(Object, ExpTime),
        Object
      ),
      ExpTime = if_else(ExpTime > 1800, NA_real_, ExpTime),
      Instrument = as_factor("DIPol-2"),
      Telescope = as_factor("T60")
    ) -> result

    which(is.na(result[["Date"]])) -> na_dates
    if (!vec_is_empty(na_dates)) {
      vec_c(
        0L,
        which(diff(na_dates) != 1L),
        length(na_dates)
      ) -> na_groups
      map2(
        na_groups[-length(na_groups)] + 1L,
        na_groups[-1],
        ~list(
          Start = na_dates[.x],
          End = na_dates[.y],
          Value = ymd("1970/01/01") +
            days(
              0.5 * (
                unclass(result[["Date"]][na_dates[.x] - 1L]) +
                unclass(result[["Date"]][na_dates[.y] + 1L])
              )
            )
        )
      ) -> na_gaps

      new_dates <- vec_init(result[["Date"]], vec_size(result))
      for (item in na_gaps) {
        vec_slice(
          new_dates,
          seq(from = item[["Start"]], to = item[["End"]])
        ) <- item[["Value"]]
      }
    result |>
      mutate(
        Date = if_else(is.na(Date), new_dates, Date)
      ) -> result

  }
  result |>
    mutate(
      Date = if_else(
        Date < ymd("2010/01/01"),
        Date + years(10L),
        Date
      )
    )
}
