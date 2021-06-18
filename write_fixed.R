write_fixed <- function(data, file, formats) {
  dt_ptype <- vctrs::vec_ptype(data)
  ptypes <- purrr::map(dt_ptype, vctrs::vec_ptype)
  comp_ptypes <- purrr::map_chr(
    ptypes,
    ~dplyr::case_when(
      vctrs::vec_is(.x, logical()) ~ "lgl",
      vctrs::vec_is(.x, integer()) ~ "int",
      vctrs::vec_is(.x, double()) ~ "dbl",
      vctrs::vec_is(.x, complex()) ~ "cmp",
      TRUE ~ "chr"
    )
  )

  pattern <- paste0(
    "%(?:(\\d{1,2})\\$)?",
    "((?:-|\\+|0|#|\\ )+)?",
    "(\\d*\\.\\d+|\\d+)?",
    "([dioxXfeEgGaAs])"
  )

  data_names <- data |> names()
  if (formats |> names() |> rlang::is_null()) {
    if (!isTRUE(vctrs::vec_size(formats) == vctrs::vec_size(data_names))) {
      rlang::abort("The number of unnamed columns should match the number of columns")
    }

    formats <- rlang::set_names(formats, data_names)
  } else {
    formats <- vctrs::vec_slice(
      formats, 
      vctrs::vec_in(names(formats), data_names)
    )
  }



  stringr::str_match_all(formats, pattern) |>
    purrr::map2_dfr(
      names(formats),
      ~tibble::as_tibble(.x, .name_repair = "unique") |>
        dplyr::mutate(Col = .y)
    ) |>
    rlang::set_names(c("Match", "Arg", "Special", "Width", "Type", "Col")) |>
    dplyr::select(-Match)


}

# sprintf("%1$-+0# 16.6f", pi) |> print()

data |>
  write_fixed(formats = c(
   Comment = "%1$8s", Date = "%8s", Focus = "%f"
   )
  ) |>
  print()

