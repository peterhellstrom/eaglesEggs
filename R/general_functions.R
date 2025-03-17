#' Title
#'
#' @param .data
#' @param .na_values
#'
#' @returns
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

#' Title
#'
#' @param dates
#' @param origin
#'
#' @returns
#' @export
#'
#' @examples
convert_old_dates <- function(dates, origin = "1899-12-30") {

  suppressWarnings({

    dates_numeric <- as.numeric(dates)

    lubridate::as_date(
      ifelse(
        is.na(dates_numeric),
        lubridate::ymd(dates),
        lubridate::ymd(origin) + lubridate::days(as.integer(dates_numeric))
      )
    )
  })

}
