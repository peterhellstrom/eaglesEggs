#' Title
#'
#' @param x
#' @param na_vec
#'
#' @return
#' @export
#'
#' @examples
na_if_n <- function(x, na_vec = c(-99.99, -9, -99, 0, NA, NaN, Inf)) {
  base::replace(x, base::which(x %in% na_vec), NA)
}

#' Title
#'
#' @param x
#' @param missing
#'
#' @return
#' @export
#'
#' @examples
fix_missing <- function(x, missing = -99.99) {
  x[x %in% missing] <- NA
  x
}

#' Title
#'
#' @param x
#' @param missing
#' @param rm.missing
#'
#' @return
#' @export
#'
#' @examples
fix_below_loq <- function(x, missing = -99.99, rm.missing = TRUE) {
  if (rm.missing) {
    x <- fix_missing(x, missing)
  }
  inds <- x < 0 & !x %in% missing & !is.na(x)
  x[inds] <- abs(x[inds] / 2)
  x
}

#' Title
#'
#' @param .x
#'
#' @return
#' @export
#'
#' @examples
fix_below_loq <- function(.x) {
  -base::pmin(-.x, .x/2)
}

#' Title
#'
#' @param x
#' @param missing
#'
#' @return
#' @export
#'
#' @examples
fix_below_loq_2 <- function(x, missing = -99.99) {
  ifelse(x < 0 & !x %in% missing & !is.na(x), abs(x / 2), x) }

#' Title
#'
#' @param .x
#' @param .na_values
#'
#' @return
#' @export
#'
#' @examples
fix_below_loq_3 <- function(.x, .na_values = c(-99.99, -9, -99)) {
  x <- dplyr::na_if(.x, .na_values)
  dplyr::if_else(.x < 0, base::abs(.x / 2), .x)
}

# take sum of a variable containing e.g. -99.99, i.e. treat a numeric value as NA

#' Title
#'
#' @param x
#' @param na.rm
#' @param missing
#'
#' @return
#' @export
#'
#' @examples
sum_below_loq <- function(x, na.rm = TRUE, missing = -99.99) {
  z <- x[!is.na(x) & !x %in% missing]
  sum(abs(z[z < 0] / 2), na.rm = na.rm) + sum(z[z >= 0], na.rm = na.rm)
}

#' Title
#'
#' @param x
#' @param missing
#' @param na.rm
#'
#' @return
#' @export
#'
#' @examples
sum_below_loq_2 <- function(x, missing = -99.99, na.rm = TRUE) {
  sum(fix_below_loq_2(ifelse(!x %in% missing & !is.na(x), x, NA)), na.rm = na.rm) }

# Transform measured values from fresh weight basis to lipid weight basis
# Ignore specified numeric value (e.g. -99.99), zeros and NAs
# However, it ignores the situation where FPRC is NA!

#' Title
#'
#' @param x
#' @param FPRC
#' @param missing
#'
#' @return
#' @export
#'
#' @examples
fresh_to_lipid <- function(x, FPRC, missing = -99.99) {
  dplyr::if_else(!x %in% missing & !is.na(x) & x != 0, 100 * x / FPRC, x) }

#lipid_to_fresh <- function(lw, FPRC, missing=-99.99)

