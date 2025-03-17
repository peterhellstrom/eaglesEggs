#' Title
#'
#' @param path
#' @param sheet
#' @param start_chr
#' @param start_row
#' @param end_chr
#' @param start_col
#' @param stop_col
#' @param as.range
#' @param n
#' @param search_column
#'
#' @returns
#' @export
#'
#' @examples
get_data_range <- function(
    path, sheet,
    start_chr,
    start_row = NULL,
    end_chr = NULL,
    start_col = 1,
    stop_col = NULL,
    as.range = TRUE,
    n = 1,
    search_column = character
) {
  
  x <- tidyxl::xlsx_cells(
    path = path,
    sheets = sheet,
    include_blank_cells = TRUE
  )
  
  x <- x |>
    dplyr::filter(
      dplyr::between(row, dplyr::coalesce(start_row, 1), Inf)
    )
  
  # Find start position (row AND column),
  # by searching for a specific string.
  top_left <- x |>
    dplyr::filter(stringr::str_detect( {{ search_column }}, start_chr)) |>
    dplyr::slice_min(row)
  
  # Find bottom left
  # Search downwards
  # bottom_left <- x |>
  #   dplyr::filter(row >= top_left$row & col == top_left$col) |>
  #   dplyr::mutate(
  #     is_blank = dplyr::case_when(
  #       content == "" ~ TRUE,
  #       TRUE ~ is_blank
  #     )
  #   ) |>
  #   dplyr::filter(!is_blank & lead(is_blank)) |>
  #   dplyr::slice_head(n = n)
  
  bottom_left <- x |>
    dplyr::filter(row >= top_left$row & col == top_left$col) |>
    dplyr::mutate(
      is_blank = dplyr::case_when(
        content == "" ~ TRUE,
        TRUE ~ is_blank
      )
    )
  
  bottom_test <- bottom_left |>
    dplyr::filter(is_blank)
  
  bottom_left <- bottom_left |>
    dplyr::filter(
      dplyr::case_when(
        nrow(bottom_test) == 0 ~ !is_blank & row == max(row),
        TRUE ~ !is_blank & dplyr::lead(is_blank)
      )
    ) |>
    dplyr::slice_head(n = n)
  
  r <- cellranger::cell_rows(c(top_left$row, bottom_left$row))
  
  # Find top right, by searching for a specific string.
  # Search results are restricted to the same row as top_left$row
  
  if (!is.null(end_chr)) {
    top_right <- x |>
      dplyr::filter(
        stringr::str_detect(character, end_chr) & row == top_left$row
      ) |>
      dplyr::slice_head(n = n)
    
    # Find bottom right, temporary
    bottom_right <- x |>
      dplyr::filter(row == bottom_left$row & col == top_right$col) |>
      dplyr::slice_head(n = n)
    
    r$lr[2] <- top_right$col
  }
  
  if (is.numeric(stop_col) & is.null(end_chr)) {
    r$lr[2] <- stop_col
  }
  
  r$ul[2] <- start_col
  
  if (as.range) {
    cellranger::as.range(r, fo = "A1")
  } else {
    r
  }
  
}

#' Title
#'
#' @param path
#' @param sheet
#' @param search_str
#' @param i
#' @param n_rows
#' @param col_names
#'
#' @returns
#' @export
#'
#' @examples
read_excel_search <- function(
    path,
    sheet,
    search_str,
    i = 1,
    n_rows = NA,
    col_names = TRUE
){
  
  x <- readxl::read_excel(
    path,
    sheet,
    col_names = FALSE,
    .name_repair = "unique_quiet"
  )
  
  start_idx <- which(x == search_str, arr.ind = TRUE)[i]
  
  if (!is.na(n_rows) & is.numeric(n_rows)) {
    n_rows <- start_idx + 1 + n_rows
  }
  
  readxl::read_excel(
    path,
    sheet,
    cellranger::cell_rows(c(start_idx + 1, n_rows)),
    col_names = col_names,
    .name_repair = "unique_quiet"
  )
}
