write_fixed <- function(data, file, formats = NULL) {
  dt_ptype <- vctrs::vec_ptype(data)
  file <- vctrs::vec_assert(file, size = 1L)
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

  default_formats <- tibble::tibble(
    Col = names(comp_ptypes),
    Type = dplyr::case_when(
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
  if (rlang::is_null(formats)) {
    formats <- c(`.NA` = "")
  } else if (formats |> names() |> rlang::is_null()) {
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
      ~tibble::as_tibble(.x, .name_repair = "minimal")|>
        rlang::set_names(c("Match", "Arg", "Special", "Width", "Type")) |>
        dplyr::mutate(Col = .y)
    ) |>
    dplyr::select(-Match) -> provided_formats

  dplyr::left_join(
    default_formats, 
    provided_formats, 
    by = "Col",
    suffix = c(".def", ".prov")
  ) |>
  dplyr::mutate(
    Type = dplyr::if_else(is.na(Type.prov), Type.def, Type.prov),
    .keep = "unused"
  ) |>
  dplyr::mutate(
    dplyr::across(
      dplyr::everything(),
       ~dplyr::if_else(is.na(.), "", .)
    ),
    Frmt = glue::glue("%{Special}{Width}{Type}")
  ) -> new_formats

  data |>
    as.list() |>
    purrr::map2(
      new_formats[["Frmt"]],
      ~sprintf(.y, .x)
    ) |>
    tibble::as_tibble() -> formatted_data

  formatted_data |>
    as.list() |>
    purrr::map2_int(
      names(formatted_data),
       ~max(c(nchar(.x), nchar(.y))) + 1L
    ) -> col_sizes

  final_format <- glue::glue("%{seq_along(col_sizes)}${col_sizes}s") |>
    paste(collapse = "  ")

  c(
    rlang::exec(sprintf, final_format, !!!names(formatted_data)),
    purrr::pmap_chr(
      as.list(formatted_data),
      function(...) {
        args <- rlang::list2(...)
        rlang::exec(sprintf, final_format, !!!args)
      }
    )
  ) |>
  brio::write_lines(file)
}

# sprintf("%1$-+0# 16.6f", pi) |> print()

# data |>
#   vctrs::vec_slice(100:200) |>
#   write_fixed(
#     file = "test.txt",
#     formats = c(
#       Comment = "%1$8s", Date = "%8s", Focus = "%f"
#     )
#   ) |>
#   print()

