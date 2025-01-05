#' Title
#'
#' @param x
#' @param na_vec
#'
#' @return
#' @export
#'
#' @examples
fix_missing <- function(
    .x,
    .replace_values = c(-99.99, -9, -99, 0, NA, NaN, Inf),
    .replace_with = NA,
    method = c("replace", "subset")
) {

  method <- match.arg(method, c("replace", "subset"))

  if (method == "replace") {
    base::replace(.x, base::which(.x %in% .replace_values), .replace_with)
  } else if (method == "subset") {
    .x[.x %in% .replace_values] <- .replace_with
    .x
  }

}

#' Title
#'
#' @param .x
#' @param .bound
#' @param .replace_na
#'
#' @return
#' @export
#'
#' @examples
fix_below_loq <- function(
    .x,
    .bound = c("medium", "lower", "upper"),
    .replace = TRUE,
    .replace_values = c(-99.99, -9, -99, 0, NA, NaN, Inf),
    .replace_with = NA
) {

  .bound <- match.arg(.bound, c("medium", "lower", "upper"))

  if (.replace) {
    .x <- fix_missing(.x, .replace_values, .replace_with)
  }

  if (.bound == "medium") {
    pmax(.x, -.x/2)
  } else if (.bound == "lower") {
    pmax(.x, 0)
  } else if (.bound == "upper") {
    # pmax(.x, -.x)
    abs(.x)
  }

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
  sum(
    fix_below_loq_2(
      ifelse(!x %in% missing & !is.na(x), x, NA)
    ),
    na.rm = na.rm
  ) }

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
