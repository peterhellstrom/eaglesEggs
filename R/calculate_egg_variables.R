#' Title
#'
#' @param egg_length
#' @param egg_width
#' @param shell_thickness
#' @param unit
#' @param type
#' @param replace_na_shell_thickness
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
egg_volume <- function(
    egg_length,
    egg_width,
    shell_thickness,
    unit = NULL,
    type = NULL,
    replace_na_shell_thickness = TRUE,
    a = 0.0373,
    b = 35.3
) {

  if (is.null(unit)) unit <- "mm"
  if (is.null(type)) type <- "inner"

  # unit <- match.arg(unit)
  # type = match.arg(type)

  a <- dplyr::case_when(
    unit == "cm" ~ a * 100,
    unit == "mm" ~ a,
    TRUE ~ NA_real_
  )

  shell_thickness <- dplyr::case_when(
    type == "outer" ~ 0,
    type == "inner" ~ shell_thickness,
    TRUE ~ NA_real_
  )

  shell_thickness <- dplyr::case_when(
    replace_na_shell_thickness ~ tidyr::replace_na(shell_thickness, 0),
    TRUE ~ shell_thickness
  )

  x <- (egg_length - 2 * shell_thickness) *
    (egg_width - 2 * shell_thickness)

  a * x - b

}

# egg_volume(71.65, 58.39, 0.63)
# egg_volume(71.65, 58.39, 0.63, unit = "mm")
# egg_volume(7.165, 5.839, 0.063, unit = "cm")
# egg_volume(7.165, 5.839, 0.063, unit = "cm", type = "outer")
#
# egg_volume(7.165, 5.839, NA, unit = "cm", replace_na_shell_thickness = FALSE)
#
# z <- tribble(
#   ~egg_length, ~egg_width, ~shell_thickness, ~unit,
#   71.65, 58.39, 0.63, "mm",
#   7.165, 5.839, 0.063, "cm",
#   7.165, 5.839, NA, "cm"
# ) |>
#   mutate(
#     vol = egg_volume(
#       egg_length, egg_width, shell_thickness, unit,
#       replace_na_shell_thickness = FALSE
#     )
#   )
#
# z


# Egg volume = (0.0373 x Length[mm] x Width[mm]) - 35.3; Stickel et al. (1973)
# use coalesce here?
#' Title
#'
#' @param egg_length
#' @param egg_width
#' @param vol_type
#' @param shell_thickness
#' @param a
#' @param b
#'
#' @return
#' @export
#'
#' @examples
egg_vol <- function(
    egg_length, egg_width,
    vol_type = c("outer", "inner"),
    shell_thickness = NULL,
    a = 0.0373, b = 35.3
) {

  vol_type <- match.arg(vol_type)
  if (vol_type == "outer") {
    shell_thickness <- 0
  } else if (vol_type == "inner" && is.null(shell_thickness)) {
    shell_thickness <- NA
  }

  (a * (egg_length - 2*shell_thickness) * (egg_width - 2*shell_thickness)) - b
}
#egg_vol(72, 58.85, "outer")
#egg_vol(72, 58.85, "inner", 0.6)
#egg_vol(72, 58.85, "inner")
#egg_vol(72, 58.85, "inner", 0)

# Egg shell index = (1000 * ShellWeight[g]) / (Length[mm] * Width[mm])

#' Title
#'
#' @param egg_length
#' @param egg_width
#' @param shell_weight
#'
#' @return
#' @export
#'
#' @examples
egg_shell_index <- function(egg_length, egg_width, shell_weight) {
  (1000 * shell_weight) / (egg_length * egg_width) }

# Egg desiccation index = (EggWeight[g] - ShellWeight[g]) / [Inner] Egg Volume

#' Title
#'
#' @param egg_weight
#' @param shell_weight
#' @param egg_volume
#'
#' @return
#' @export
#'
#' @examples
egg_desicc_index <- function(egg_weight, shell_weight, egg_volume) {
  (egg_weight - shell_weight) / egg_volume }

# Fettinnehåll i gram: EggFatG = (fprc * (EggWeight - ShellWeight)) / 100

#' Title
#'
#' @param egg_weight
#' @param shell_weight
#' @param fprc
#'
#' @return
#' @export
#'
#' @examples
egg_fat_weight <- function(egg_weight, shell_weight, fprc) {
  (fprc * (egg_weight - shell_weight)) / 100 }

# Ursprunglig fett%: EggFatPrc = 100 * EggFatG / [Inner] Egg Volume
#' Title
#'
#' @param fat_weight
#' @param egg_volume
#'
#' @return
#' @export
#'
#' @examples
egg_fprc_orig <- function(fat_weight, egg_volume) {
  100 * fat_weight / egg_volume }

# Embryonal development correction factor
# difficult if embryo has not been measured as a a numeric value,
# and just given as a symbolic value or in range
#' Title
#'
#' @param embryo_length
#' @param step
#'
#' @return
#' @export
#'
#' @examples
egg_embryo_corr <- function(embryo_length, step = 75) {
	x <- readr::parse_number(embryo_length)
	ifelse(x >= step, step * (1/x), 1) }

# egg_embryo_corr(c(0, 30, 75, NA, 100, 110, ">85.45", "100m45", 1))
# Check: should this return NA or 1 if embryo.length is NA OR 0?
# Also, check variable embryo (Yes/No) and embryo.length = the latter should be 0 if embryo is No?
# What happens when multiplying measured values - or multiply only values where ecf < 1?

# Empirical support for the correction?
# Check relationship between FPRC and embryo length...
# curve(
#   ifelse(x >= 75, 75*(1/x), 1),
#   from = 0, to = 160, n = 500,
#   ylim = c(0, 1),
#   bty = "l",
#   xlab = "Embryo length", ylab = "Correction factor",
#   font.lab = 2, las = 1
# )
# abline(v = 75, lty = 2, col = "red")
