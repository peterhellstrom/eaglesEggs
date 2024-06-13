#' Title
#'
#' @param .cols
#' @param .na_values
#' @param .na_numeric
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
add_sum_contaminants <- function(
    .cols,
    .na_values = c(-9, -99, -99.99),
    .na_numeric = -99.99,
    na.rm = TRUE
) {
  dplyr::case_when(
    dplyr::if_all( {{ .cols }}, is.na) ~ NA,
    dplyr::if_all( {{ .cols }}, \(x) x %in% .na_values) ~ .na_numeric,
    .default = base::rowSums(
      dplyr::across(
        {{ .cols }},
        \(x) fix_below_loq(base::replace(x, x %in% .na_values, NA))
      ), na.rm = na.rm
    )
  )
}

#' Title
#'
#' @param .x
#' @param .cols
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
add_sum_vars <- function(.x, .cols, na.rm = TRUE) {

  .z <- .x |>
    dplyr::select( {{ .cols }} ) |>
    dplyr::mutate(
      dplyr::across(tidyselect::everything(), fix_below_loq)
    )

  base::rowSums(.z, na.rm = na.rm)
}

#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
add_cpd_sum <- function(.data) {
  .data %>%
    dplyr::mutate(
      sHCH = rowSums(
        dplyr::select(., tidyselect::matches("HCH$|LINDAN")) %>%
          dplyr::mutate_all(dplyr::na_if, -99.99) %>%
          fix_below_loq, na.rm = TRUE),
      sDDT = rowSums(
        dplyr::select(., tidyselect::starts_with("DD")) %>%
          dplyr::mutate_all(na_if, -99.99) %>%
          fix_below_loq, na.rm = TRUE),
      sPCB7 = rowSums(
        dplyr::select(., tidyselect::starts_with("CB")) %>%
          dplyr::mutate_all(na_if, -99.99) %>%
          fix_below_loq, na.rm = TRUE),
      sPBDE5 = rowSums(
        dplyr::select(., tidyselect::matches("^BDE[^28$]")) %>%
          dplyr::mutate_all(na_if, -99.99) %>%
          fix_below_loq, na.rm = TRUE),
      sPBDE6 = rowSums(
        dplyr::select(., tidyselect::starts_with("BDE")) %>%
          dplyr::mutate_all(na_if, -99.99) %>%
          fix_below_loq, na.rm = TRUE))
}
