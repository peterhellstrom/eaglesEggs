#' Title
#'
#' @param .data
#' @param .pattern
#' @param .cols
#'
#' @return
#' @export
#'
#' @examples
rename_contaminants <- function(
    .data,
    .pattern = c(
      "\\s+" = "",
      "," = "",
      "\\*" = "",
      "-" = "",
      "FPRC" = "fat_percentage",
      "K$" = "",
      "\\(" = "",
      "\\)" = "",
      "\\%" = "",
      "\\/" = "_",
      "^header$" = "accnr",
      "LABKODICB$" = "labcode",
      "LABKOD$" = "labcode",
      "^site$" = "samplingsite"
    ),
    .cols = tidyselect::everything()
) {
  .data |>
    dplyr::rename_with(
      \(x) stringr::str_replace_all(x, .pattern),
      tidyselect::all_of( {{ .cols }} )
    )
}

# .data |> rename_at(starts_with("BDE"), \(x) str_replace_all(x, "\\s+", "")
