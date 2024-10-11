#' Title
#'
#' @param .data
#' @param .na_values
#'
#' @return
#' @export
#'
#' @examples
fix_null_strings <- function(.data, .na_values = c("", "-")) {
  .data |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::where(is.character),
        \(x) dplyr::if_else(x %in% .na_values, NA_character_, x) |>
          stringr::str_trim()
      )
    )
}
