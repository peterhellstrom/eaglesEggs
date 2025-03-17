# ITM templates; CLC (version 5), BFR (version 1) ----
#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param ...
#'
#' @returns
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
#' @returns
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
#   must be changed
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
#' @returns
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
#' @returns
#' @export
#'
#' @examples
import_new_template_data <- function(
    path,
    sheet = c("results", "uncertainty"),
    range,
    rename = TRUE
) {

  sheet <- match.arg(sheet)

  .x <- readxl::read_xlsx(
    path = path, sheet = sheet, range = range,
    .name_repair = "unique_quiet"
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
#' @returns
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
