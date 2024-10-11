#' Title
#'
#' @param DV 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
stn_reg_import <- function(DV = NULL, ...) {
  
  if (is.null(DV)) {
    path <- "https://stationsregister.miljodatasamverkan.se/docs/atom/SE_EF_StnReg/SE_EF_StnReg.gml.zip"
  } else {
    path <- dplyr::case_when(
      DV == "Stralningsmatningar" ~ 
        glue::glue(
          "https://stationsregister.miljodatasamverkan.se/docs/atom/SE_EF_StnReg_{DV}/SE_EF_StnReg_{DV}.gml.zip"
        ),
      TRUE ~ 
        glue::glue(
          "https://stationsregister.miljodatasamverkan.se/docs/atom/SE_EF_StnReg_DV_{DV}/SE_EF_StnReg_DV_{DV}.gml.zip"
        )
    )
  }
  
  sf::read_sf(
    file.path("/vsizip", "vsicurl", path),
    ...
  ) |> 
    tidyr::unnest_wider(name, names_sep = "_") |> 
    sf::st_sf() |> 
    dplyr::rename(
      identifier_ = name_1,
      name = name_2
    ) |> 
    dplyr::select(-identifier_) |> 
    dplyr::relocate(name, .after = identifier)
}
