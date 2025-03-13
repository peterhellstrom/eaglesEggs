#' Title
#'
#' @param path
#' @param sheet
#' @param include_dates
#'
#' @return
#' @export
#'
#' @examples
get_samples <- function(
    path = "data_files.xlsx",
    sheet = "samples",
    include_dates = FALSE
) {
  out <- readxl::read_xlsx(path, sheet) |>
    dplyr::mutate(
      ORGAN = stringi::stri_trans_general(
        materialtyp,
        'Latin-ASCII'
      ) |>
        toupper(),
      ORGAN = dplyr::if_else(str_sub(ORGAN, 1, 3) == "AGG", "AGG", ORGAN)
    ) |>
    dplyr::rename(
      PROV_KOD_ORIGINAL = accnr,
      PROV_KOD_LABB = labcode
    ) |>
    dplyr::select(
      PROV_KOD_ORIGINAL,
      PROV_KOD_LABB,
      art,
      materialtyp,
      ORGAN,
      analystyp,
      analyslab,
      analysmetod,
      tidyselect::ends_with("datum")
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("datum"), as.Date
      )
    )
  if (!include_dates) {
    out <- out |>
      dplyr::select(
        -tidyselect::ends_with("datum")
      )
  }
  out
}

#' Title
#'
#' @param path
#' @param sheet
#'
#' @return
#' @export
#'
#' @examples
get_samples_ov_xl <- function(
    path = "esb/excel_catalogs/tmp_OV_katalog_havsorn.xlsx",
    sheet = "samples"
) {

  readxl::read_xlsx(path, sheet) |>
    dplyr::mutate(
      provberedningsdatum = as.Date(provberedningsdatum)
    ) |>
    dplyr::rename(
      PROV_KOD_ORIGINAL = accnr
    )
}

#' Title
#'
#' @param codes_path
#'
#' @return
#' @export
#'
#' @examples
get_samples_species <- function(
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2.wtse")
) {

  samples_species <- get_samples() |>
    dplyr::select(art) |>
    dplyr::distinct() |>
    dplyr::rename(ART = art) |>
    dplyr::mutate(
      ART = stringi::stri_trans_general(ART, 'Latin-ASCII')
    )

  samples_species |>
    dplyr::left_join(
      get_codes_species(path = codes_path),
      dplyr::join_by(ART)
    ) |>
    dplyr::mutate(ARTDIST = 0)
}
