#' Title
#'
#' @param DV
#' @param ...
#'
#' @returns
#' @export
#'
#' @examples
stn_reg_import <- function(DV = NULL, ...) {

  url_base <- "https://stationsregister.miljodatasamverkan.se/docs/atom"

  if (is.null(DV)) {
    path <- glue::glue("{url_base}/SE_EF_StnReg/SE_EF_StnReg.zip")
  } else {
    path <- dplyr::case_when(
      DV == "Stralningsmatningar" ~
        glue::glue(
          "{url_base}/SE_EF_StnReg_{DV}/SE_EF_StnReg_{DV}.zip"
        ),
      TRUE ~
        glue::glue(
          "{url_base}/SE_EF_StnReg_DV_{DV}/SE_EF_StnReg_DV_{DV}.zip"
        )
    )
  }

  sf::read_sf(
    file.path("/vsizip", "vsicurl", path), ...) |>
    tidyr::unnest_wider(name, names_sep = "_") |>
    sf::st_sf() |>
    dplyr::rename(
      identifier_ = name_1,
      name = name_2
    ) |>
    dplyr::select(-identifier_) |>
    dplyr::relocate(name, .after = identifier)
}
