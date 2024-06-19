# Calculate uncertainty for CLC (pesticides) and BFRs,
# according to ACES specifications.
# Note these functions [limits and coefficients] assume
# that data (.x) are expressed as lipid weight ng / g (ppb)
# Also note that different versions of these calculations exist.
# I have used formulas in templates from 2016
# In templates from ACES from 2012, there are three different cut-off levels
# rather than two as used here. Precision increases with concentration in sample.

#' Title
#'
#' @param .x
#' @param limit
#' @param coef_above
#' @param coef_below
#'
#' @return
#' @export
#'
#' @examples
calculate_uncertainty <- function(.x, limit, coef_above, coef_below) {
  dplyr::case_when(
    .x < 0 ~ NA_real_,
    .x > limit ~ coef_above * .x,
    TRUE ~ coef_below * .x
  )
}

#' Title
#'
#' @param data
#' @param column
#' @param cut
#' @param const1
#' @param const2
#'
#' @return
#' @export
#'
#' @examples
calculate_uncertainty_2 <- function(data, column, cut, const1, const2) {
  data |>
    dplyr::mutate(
      {{ column }} := dplyr::case_when(
        {{ column }} < 0 ~ NA_real_,
        dplyr::between( {{ column }}, 0, cut) ~ const1 * {{ column }},
        {{ column }} > cut ~ const2 * {{ column }},
        TRUE ~ NA_real_)
    )
}

# This function works both on data in wide format
# (intended use, hence the use of .col as argument)
# and for data in long format, where .col instead refers to a variable.
# Should work for different parameter naming schemes,
# e.g. CB52 and `CB-52`.
#' Title
#'
#' @param .x
#' @param .col
#'
#' @return
#' @export
#'
#' @examples
add_uncertainty_aces <- function(.x, .col = dplyr::cur_column()) {

  dplyr::case_when(
    stringr::str_detect(.col, "HCB") ~ calculate_uncertainty(.x, 50, 0.29, 0.36),
    stringr::str_detect(.col, "HCH$|LINDAN") ~ calculate_uncertainty(.x, 50, 0.34, 0.4),
    stringr::str_detect(.col, "DDE|DDD") ~ calculate_uncertainty(.x, 50, 0.31, 0.43),
    stringr::str_detect(.col, "DDT") ~ calculate_uncertainty(.x, 50, 0.38, 0.52),
    stringr::str_detect(.col, "CB*.52") ~ calculate_uncertainty(.x, 50, 0.3, 0.49),
    stringr::str_detect(.col, "^(?!.*52).*CB.*$") ~ calculate_uncertainty(.x, 50, 0.29, 0.36),
    stringr::str_detect(.col, "BDE") ~ calculate_uncertainty(.x, 2, 0.58, 0.73),
    stringr::str_detect(.col, "HBCD") ~ calculate_uncertainty(.x, 25, 0.64, 1.03),
    TRUE ~ NA_real_
  )
}
