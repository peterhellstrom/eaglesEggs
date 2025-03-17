#' Title
#'
#' @param .cols
#' @param .na_values
#' @param .na_numeric
#' @param na.rm
#'
#' @returns
#' @export
#'
#' @examples
add_sum_contaminants <- function(
    .cols,
    .na_values = c(-9, -99, -99.99),
    .na_numeric = -99.99,
    .bound = "medium",
    na.rm = TRUE
) {
  dplyr::case_when(
    dplyr::if_all( {{ .cols }} , is.na) ~ NA,
    dplyr::if_all( {{ .cols }} , \(x) x %in% .na_values) ~ .na_numeric,
    .default = base::rowSums(
      dplyr::across(
        {{ .cols }} ,
        \(x) fix_below_loq(
          base::replace(x, x %in% .na_values, NA),
          .bound = .bound
        )
      ), na.rm = na.rm
    )
  )
}
