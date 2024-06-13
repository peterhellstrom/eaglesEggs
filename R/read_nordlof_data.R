#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
bde_read_raw <- function(data, ...) {

  data |>
    dplyr::filter(labcode != "FALSE") |>
    dplyr::select(Region, labcode, Congener, halt = `pg/g fettvikt`) |>
    dplyr::mutate(
      halt = halt / 1000
    ) |>
    tidyr::pivot_wider(
      names_from = Congener,
      values_from = halt
    ) |>
    dplyr::select(
      Region,
      labcode,
      ...
    ) |>
    dplyr::rename_with(
      \(x) stringr::str_replace_all(x, c(" " = "", "^P" = "")),
      tidyselect::starts_with("PBDE")
    ) |>
    dplyr::arrange(labcode)
}

#' Title
#'
#' @param x
#' @param y
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
bde_diff <- function(x, y, ...) {
  x |>
    dplyr::left_join(y, dplyr::join_by(labcode)) |>
    dplyr::select(-Region.y) |>
    tidyr::pivot_longer(
      ...,
      names_to = "congener",
      values_to = "value"
    ) |>
    dplyr::arrange(labcode, congener) |>
    tidyr::separate_wider_delim(
      congener,
      delim = ".",
      names = c("congener", "source")
    ) |>
    dplyr::mutate(
      source = dplyr::case_when(
        source == "x" ~ "Raw data",
        source == "y" ~ "Edited data",
        TRUE ~ NA_character_
      )
    ) |>
    tidyr::pivot_wider(names_from = source) |>
    dplyr::mutate(
      d = `Raw data` - `Edited data`,
      d_near = dplyr::near(`Raw data`, `Edited data`)
    )
}
