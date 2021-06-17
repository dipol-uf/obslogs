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
  return(parsed)
  tibble::tibble(
    Date = lubridate::dmy(paste(parsed[1], parsed[2], parsed[3], sep = "/")),
    Comment = stringr::str_to_sentence(parsed[4])
  )
}

dp2_pattern_1 <- paste0(
  "^\\s*",
  "(?:\\*(?:[Nn]ot|[Aa]lso\\s*not)\\*\\s*)?",
  "([\\w\\+\\.]+|[\\w\\+]+\\s{1,3}[\\w\\+]+)", # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "\\s+",
  "(\\d+(?:/\\d+)?)",                          # N
  "\\s+",
  "(\\d+|\\d+[\\.,]\\d+)",                     # ExpTime
  "(?:\\s+([\\w -]+))?"                        # Type
)

dp2_pattern_2 <- paste0(
  "^\\s*",
  "([\\w\\+\\.]+|[\\w\\+]+\\s{1,3}[\\w\\+]+)", # Object name
  "\\s+",
  "(\\d+|\\d+[\\.,]\\d+|\\d+-\\d+)",           #ExpTime
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "(?:\\s+(\\d+(?:/\\d+)?))?",                 # N
  "(?:\\s+([\\w -]+))?"                         # Type
)

dp2_pattern_3 <- paste0(
  "^\\s*",
  "([\\w\\+\\.]+|[\\w\\+]+\\s{1,3}[\\w\\+]+)", # Object name
  "\\s+",
  "([+-]?\\d+[\\.,]?\\d+|-+||n/a)",            # Focus
  "\\s+",
  "~?(\\d+|\\d+[\\.,]\\d+)",                    # ExpTime
  "(?:\\s+([\\w -]+))?"                         # Type
)

get_objects <- function(
  str,
  patterns = vctrs::vec_c(
    dp2_pattern_1,
    dp2_pattern_2,
    dp2_pattern_3
  )
) {
  str <- vctrs::vec_slice(str, -1L) |>
    stringr::str_replace("\\t", " ")

  patterns |>
    purrr::reduce(stringr::str_subset, negate = TRUE, .init = str) |>
    stringr::str_trim()  -> comments

  # list(objects = objects, comments = comments)
  comments
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
    vctrs::vec_slice(1:200) |>
    purrr::map(get_date_comment) 

}


get_dp2_obs() |>
   print()
#   # print(n = 300)
