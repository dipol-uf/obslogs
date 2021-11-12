box::use(
  vctrs[vec_ptype, vec_assert, vec_is, vec_size, vec_slice, vec_in, vec_c],
  purrr[map, map_chr, map2_dfr, map2_int, pmap_chr, map2],
  dplyr[case_when, mutate, if_else, select, left_join, across, everything],
  tibble[tibble, as_tibble],
  rlang[is_null, abort, set_names, list2, exec],
  stringr[str_match_all],
  glue[glue],
  brio[write_lines]
)

write_fixed <- function(data, file, formats = NULL) {
  dt_ptype <- vec_ptype(data)
  file <- vec_assert(file, size = 1L)
  ptypes <- map(dt_ptype, vec_ptype)
  comp_ptypes <- map_chr(
    ptypes,
    ~case_when(
      vctrs::vec_is(.x, logical()) ~ "lgl",
      vctrs::vec_is(.x, integer()) ~ "int",
      vctrs::vec_is(.x, double()) ~ "dbl",
      vctrs::vec_is(.x, complex()) ~ "cmp",
      TRUE ~ "chr"
    )
  )

  default_formats <- tibble(
    Col = names(comp_ptypes),
    Type = case_when(
      comp_ptypes == "lgl" ~ "s",
      comp_ptypes == "int" ~ "d",
      comp_ptypes == "dbl" ~ "g",
      comp_ptypes == "cmp" ~ "s",
      TRUE ~ "s"
    )
  )

  pattern <- paste0(
    "%(?:(\\d{1,2})\\$)?",
    "((?:-|\\+|0|#|\\ )+)?",
    "(\\d*\\.\\d+|\\d+)?",
    "([dioxXfeEgGaAs])"
  )

  data_names <- data |> names()
  if (is_null(formats)) {
    formats <- c(`.NA` = "")
  } else if (formats |> names() |> is_null()) {
    if (!isTRUE(vec_size(formats) == vec_size(data_names))) {
      abort("The number of unnamed columns should match the number of columns")
    }

    formats <- set_names(formats, data_names)
  } else {
    formats <- vec_slice(
      formats, 
      vec_in(names(formats), data_names)
    )
  }



  str_match_all(formats, pattern) |> 
    map2_dfr(
      names(formats),
      ~as_tibble(.x, .name_repair = "minimal")|>
       set_names(c("Match", "Arg", "Special", "Width", "Type")) |>
        mutate(Col = .y)
    ) |>
    select(-Match) -> provided_formats

  left_join(
    default_formats, 
    provided_formats, 
    by = "Col",
    suffix = c(".def", ".prov")
  ) |>
  mutate(
    Type = if_else(is.na(Type.prov), Type.def, Type.prov),
    .keep = "unused"
  ) |>
  mutate(
    across(
      everything(),
       ~if_else(is.na(.), "", .)
    ),
    Frmt = glue("%{Special}{Width}{Type}")
  ) -> new_formats

  data |>
    as.list() |>
    map2(
      new_formats[["Frmt"]],
      ~sprintf(.y, .x)
    ) |>
    as_tibble() -> formatted_data

  formatted_data |>
    as.list() |>
    map2_int(
      names(formatted_data),
       ~max(vec_c(nchar(.x), nchar(.y))) + 1L
    ) -> col_sizes

  final_format <- glue("%{seq_along(col_sizes)}${col_sizes}s") |>
    paste(collapse = "  ")

  c(
   exec(sprintf, final_format, !!!names(formatted_data)),
    pmap_chr(
      as.list(formatted_data),
      function(...) {
        args <- list2(...)
        exec(sprintf, final_format, !!!args)
      }
    )
  ) |>
  write_lines(file)
}
