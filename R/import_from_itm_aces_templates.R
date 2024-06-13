# ITM templates; CLC (version 5), BFR (version 1) ----
#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
read_lab_file_itm <- function(path, sheet, range, ...) {
  purrr::map2(
    path, range,
    \(x,y) readxl::read_xlsx(x, sheet, y, .name_repair = "unique_quiet", ...)
  ) |>
    purrr::list_rbind(names_to = "source")
}

## Headers / general info ----
#' Title
#'
#' @param path
#' @param sheet
#' @param range_1
#' @param range_2
#' @param range_3
#'
#' @return
#' @export
#'
#' @examples
read_lab_file_itm_header <- function(
    path,
    sheet = "summery",
    range_1 = "A1:D7",
    range_2 = "G1:J4",
    range_3 = "K1:O5"
) {

  methods_1 <- purrr::map(
    path,
    \(x) method_header(x, sheet, range_1)
  ) |>
    purrr::list_rbind(names_to = "source") |>
    dplyr::select(-na, -sampling_place) |>
    dplyr::mutate(
      date_of_arrival = janitor::excel_numeric_to_date(as.numeric(date_of_arrival))
    )

  methods_2 <- purrr::map(
    path,
    \(x) method_header(x, sheet, range_2)
  ) |>
    purrr::list_rbind(names_to = "source") |>
    dplyr::mutate(
      by = toupper(by),
      date_of_clean_up = dplyr::case_when(
        stringr::str_detect(date_of_clean_up, "\\/") ~ lubridate::ymd(date_of_clean_up),
        TRUE ~ janitor::excel_numeric_to_date(as.numeric(date_of_clean_up))
      )
    ) |>
    dplyr::rename(clean_up_by = by)

  methods_3 <- purrr::map(
    path,
    \(x) method_header(x, sheet, range_3) |>
      dplyr::rename(
        dplyr::any_of(
          c(
            date_of_analysis = "date_of_gc_ecd_analysis",
            date_of_analysis = "date_of_gc_ms_analysis"
          )
        )
      )
  ) |>
    purrr::list_rbind(names_to = "source") |>
    dplyr::select(-tidyselect::matches("^na")) |>
    dplyr::rename(analysed_by = by) |>
    dplyr::mutate(
      analysed_by = toupper(analysed_by),
      evaluated_by = toupper(evaluated_by),
      date_of_analysis = janitor::excel_numeric_to_date(as.numeric(date_of_analysis))
    )

  methods_all <- methods_1 |>
    dplyr::left_join(methods_2, dplyr::join_by(source)) |>
    dplyr::left_join(methods_3, dplyr::join_by(source)) |>
    dplyr::select(-sampling_commune_county)

  # Note that a few exceptions have not been dealt with in the output
  methods_all
}

# SMNH templates, 2019-onwards ----

## Version 1 ----

# To do:
# - argument cols_first_5 will likely fail for other sheets than "results",
#   must be change
# - is the lab option necessary, should we read that from "general info" sheet instead?
# - can we add functionality with get_data_range(), now we need to supply a data range?

#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param col_names
#' @param cols_first_5
#'
#' @return
#' @export
#'
#' @examples
import_new_template <- function(
    path, sheet, range, col_names = TRUE,
    cols_first_5 = c("accnr", "labcode", "species", "sampling_site", "sample_tissue")
) {

  readxl::read_excel(
    path,
    sheet = sheet,
    range = range,
    col_names = col_names,
    .name_repair = "unique_quiet"
  ) |>
    dplyr::rename_with(~ cols_first_5, `...1`:`...5`)
}

## Version 2 ----

#' Title
#'
#' @param path
#' @param sheet
#' @param range
#'
#' @return
#' @export
#'
#' @examples
import_new_template_data <- function(
    path,
    sheet = c("results", "uncertainty"),
    range
) {

  sheet <- match.arg(sheet)

  .x <- readxl::read_xlsx(
    path = path, sheet = sheet, range = range,
    .name_repair = "unique_quiet",
    .rename = TRUE
  )

  if (sheet == "results") {
    .x <- .x |>
      dplyr::rename(
        accnr = ...1,
        labcode = ...2,
        species = ...3,
        sampling_site = ...4,
        sampling_tissue = ...5
      )

  } else if (sheet == "uncertainty") {
    .x <- .x |>
      dplyr::rename(
        accnr = ...1,
        labcode = ...2,
        species = ...3,
        sampling_site = ...4
      )
  }

  if (rename) {
    .x |> rename_contaminants()
  } else {
    .x
  }

}

# Template helper functions ----

## Method header ----

# To do:
# - add description, when/where is this function used?

#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param clean_names
#'
#' @return
#' @export
#'
#' @examples
method_header <- function(
    path, sheet, range,
    clean_names = TRUE) {

  out <- readxl::read_xlsx(
    path = path, sheet = sheet, range = range,
    col_names = FALSE,
    .name_repair = "unique_quiet"
  ) |>
    janitor::remove_empty(which = "cols") |>
    tidyr::pivot_wider(
      names_from = 1,
      values_from = 2
    )

  if (clean_names) {
    out |>
      janitor::clean_names()
  }
}

## Detect data range ----
#' Title
#'
#' @param path
#' @param sheet
#' @param start_chr
#' @param start_row
#' @param end_chr
#' @param start_col
#' @param stop_col
#' @param as.range
#' @param n
#' @param search_column
#'
#' @return
#' @export
#'
#' @examples
get_data_range <- function(
    path, sheet,
    start_chr,
    start_row = NULL,
    end_chr = NULL,
    start_col = 1,
    stop_col = NULL,
    as.range = TRUE,
    n = 1,
    search_column = character
) {

  x <- tidyxl::xlsx_cells(
    path = path,
    sheets = sheet,
    include_blank_cells = TRUE
  )

  x <- x |>
    dplyr::filter(
      dplyr::between(row, dplyr::coalesce(start_row, 1), Inf)
    )

  # Find start position (row AND column),
  # by searching for a specific string.
  top_left <- x |>
    dplyr::filter(stringr::str_detect( {{ search_column }}, start_chr)) |>
    dplyr::slice_min(row)

  # Find bottom left
  # Search downwards
  # bottom_left <- x |>
  #   dplyr::filter(row >= top_left$row & col == top_left$col) |>
  #   dplyr::mutate(
  #     is_blank = dplyr::case_when(
  #       content == "" ~ TRUE,
  #       TRUE ~ is_blank
  #     )
  #   ) |>
  #   dplyr::filter(!is_blank & lead(is_blank)) |>
  #   dplyr::slice_head(n = n)

  bottom_left <- x |>
    dplyr::filter(row >= top_left$row & col == top_left$col) |>
    dplyr::mutate(
      is_blank = dplyr::case_when(
        content == "" ~ TRUE,
        TRUE ~ is_blank
      )
    )

  bottom_test <- bottom_left |>
    dplyr::filter(is_blank)

  bottom_left <- bottom_left |>
    dplyr::filter(
      dplyr::case_when(
        nrow(bottom_test) == 0 ~ !is_blank & row == max(row),
        TRUE ~ !is_blank & dplyr::lead(is_blank)
      )
    ) |>
    dplyr::slice_head(n = n)

  r <- cellranger::cell_rows(c(top_left$row, bottom_left$row))

  # Find top right, by searching for a specific string.
  # Search results are restricted to the same row as top_left$row

  if (!is.null(end_chr)) {
    top_right <- x |>
      dplyr::filter(
        stringr::str_detect(character, end_chr) & row == top_left$row
      ) |>
      dplyr::slice_head(n = n)

    # Find bottom right, temporary
    bottom_right <- x |>
      dplyr::filter(row == bottom_left$row & col == top_right$col) |>
      dplyr::slice_head(n = n)

    r$lr[2] <- top_right$col
  }

  if (is.numeric(stop_col) & is.null(end_chr)) {
    r$lr[2] <- stop_col
  }

  r$ul[2] <- start_col

  if (as.range) {
    cellranger::as.range(r, fo = "A1")
  } else {
    r
  }

}

#' Title
#'
#' @param path
#' @param sheet
#' @param search_str
#' @param i
#' @param n_rows
#' @param col_names
#'
#' @return
#' @export
#'
#' @examples
read_excel_search <- function(
    path,
    sheet,
    search_str,
    i = 1,
    n_rows = NA,
    col_names = TRUE
){

  x <- readxl::read_excel(
    path,
    sheet,
    col_names = FALSE,
    .name_repair = "unique_quiet"
  )

  start_idx <- which(x == search_str, arr.ind = TRUE)[i]

  if (!is.na(n_rows) & is.numeric(n_rows)) {
    n_rows <- start_idx + 1 + n_rows
  }

  readxl::read_excel(
    path,
    sheet,
    cellranger::cell_rows(c(start_idx + 1, n_rows)),
    col_names = col_names,
    .name_repair = "unique_quiet"
  )
}
