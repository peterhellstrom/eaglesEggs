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

# z <- c(4, -99.99, -99, -9, 3, 12.21, 15, NA, NaN, Inf, 0, -7.5)
#
# na_if_n(z)
# na_if_n(z) |> fix_below_loq()
#
# sum(z)
# sum(na_if_n(z), na.rm = TRUE)
# sum(na_if_n(z) |> fix_below_loq(), na.rm = TRUE)
#
# tibble(z) |>
#   mutate(
#     z_2 = na_if_n(z),
#     z_3 = fix_below_loq(z_2)
#   )


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

# sPCB7 = reduce(select(., starts_with("CB")) %>% fix_below_loq, `+`)
# select(clc, starts_with("DD")) %>% mutate_all(na_if, -99.99) %>% fix_below_loq()
# select(bfr, matches("^BDE[^28]")) %>% mutate_all(na_if, -99.99) %>% fix_below_loq()

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

#blq <- function(z, missing = -99.99) {
# Should also check that "" or NA is NA
# 1) Change all values equal to -99.99 to NA
#z[z == missing] <- NA
# 2) Find all negative values (excluding -99.99) that is not NA
#inds <- z < 0 & z != missing & !is.na(z)
# 3) Use half the absolute value for all negative values
#z[inds] <- abs(z[inds] / 2)
#z
#}


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

# fix_below_loq_2 does not transform -99.99 (or another given value) to NA
# also, blq and fix_below_loq is faster

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

# Examples:
#x <- c(5,-2,2,NA,-14,-99.99, -9)
#fix_missing(x)
#fix_missing(x, missing=c(-99.99, -9))
#fix_below_loq(x, rm.missing=TRUE)
#fix_below_loq(x, missing=c(-99.99, -9))
#fix_below_loq(x, rm.missing=FALSE)

#fix_below_loq(x)
#fix_below_loq_2(x)
#fix_below_loq_2(x, missing=c(-99.99, -9))

#sum_below_loq(x, missing=c(-99.99, -9))
#sum_below_loq_2(x, missing=c(-99.99, -9))

# IMPORTANT NOTE! This function does NOT (yet) deal with pooled samples,
# e.g. C2007/01425-01431
# Should be possible to extend with an optional regex group?

#' Title
#'
#' @param x
#' @param regex
#' @param exclude_non_matches
#'
#' @return
#' @export
#'
#' @examples
format_accnr <- function(
    x,
    regex = "(?<catalogue>[A-Za-z]{1})(?<accyear>[0-9]{2,4})\\/(?<nr>[0-9]{3,5})",
    exclude_non_matches = FALSE
) {

  m <- stringr::str_match(x, regex)
  colnames(m)[1] <- "accnr"

  out <- m |>
    tibble::as_tibble() |>
    dplyr::mutate(
      catalogue = toupper(catalogue),
      accyear = dplyr::case_when(
        dplyr::between(as.numeric(accyear), 60, 99) ~ stringr::str_c("19", accyear),
        dplyr::between(as.numeric(accyear), 00, 59) ~ stringr::str_c("20", accyear),
        TRUE ~ accyear
      ),
      nr = stringr::str_pad(nr, width = 5, pad = "0", side = "left"),
      accnr_new = stringr::str_c(catalogue, accyear, "/", nr)
    )

  if (!exclude_non_matches) {
    out <- out |>
      dplyr::mutate(
        accnr_new = dplyr::case_when(
          !is.na(accnr) ~ accnr_new,
          TRUE ~ x
        )
      )
  }

  out |>
    dplyr::pull(accnr_new)
}

# v <- c("C99/7899", "C1999/07899", "ZÄ96", "C*05/7032", "c2013/07156-07157")
# format_accnr(v)
# format_accnr(v, exclude_non_matches = FALSE)
# format_accnr(v |> str_replace("\\*", ""))
# format_accnr(v |> str_replace("\\*", ""), exclude_non_matches = FALSE)
