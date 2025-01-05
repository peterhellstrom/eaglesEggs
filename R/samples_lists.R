#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param sheet_names
#'
#' @return
#' @export
#'
#' @examples
wtse_eggs_1996 <- function(
    path = "eggs-1996.xlsx",
    sheet = "data",
    range = "A3:AG501",
    sheet_names = "column_names"
) {

  # readxl::excel_sheets(path)

  # New column names ----
  col_names <- readxl::read_xlsx(path, sheet_names)

  # Read data and rename columns ----
  # Note that some columns are repeated, probably for easier reading on
  # older smaller screens available at the time of creation of data set.

  x <- readxl::read_xlsx(
    path, sheet, range,
    col_names = FALSE,
    col_types = col_names$col_type,
    .name_repair = "unique_quiet"
  )

  x |>
    rlang::set_names(col_names$col_name_new)

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
get_samples <- function(
    path = "data_files.xlsx",
    sheet = "samples",
    include_dates = FALSE
) {
  out <- readxl::read_xlsx(path, sheet) |>
    dplyr::mutate(
      ORGAN = stringi::stri_trans_general(materialtyp, 'Latin-ASCII') |>
        toupper(),
      ORGAN = dplyr::if_else(str_sub(ORGAN, 1, 3) == "AGG", "AGG", ORGAN)
    ) |>
    dplyr::rename(
      PROV_KOD_ORIGINAL = accnr,
      PROV_KOD_LABB = labcode
    ) |>
    dplyr::select(
      PROV_KOD_ORIGINAL, PROV_KOD_LABB,
      art, materialtyp, ORGAN,
      analystyp, analyslab, analysmetod,
      tidyselect::ends_with("datum")
    ) |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::ends_with("datum"), as.Date
      )
    )
  if (!include_dates) {
    out <- out |>
      dplyr::select(-tidyselect::ends_with("datum"))
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
      Provberedningsdatum = as.Date(Provberedningsdatum)
    ) |>
    dplyr::rename(
      PROV_KOD_ORIGINAL = Accnr
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
    codes_path = system.file("extdata", "codelist_wtse.xlsx", package = "MoCiS2")
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
