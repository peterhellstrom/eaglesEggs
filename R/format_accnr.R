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
    .x,
    .regex = "(?<catalogue>[A-Za-z]{1})(?<accyear>[0-9]{2,4})\\/(?<startnr>[0-9]{3,5})(?:\\-(?<endnr>[0-9]{3,5}))?",
    .exclude_non_matches = FALSE
) {

  # .x <- stringr::str_remove(.x, "\\*")

  regex_matches <- stringr::str_match(.x, .regex)
  colnames(regex_matches)[1] <- "accnr"

  out <- regex_matches |>
    tibble::as_tibble() |>
    dplyr::mutate(
      catalogue = toupper(catalogue),
      accyear = dplyr::case_when(
        dplyr::between(as.numeric(accyear), 60, 99) ~ stringr::str_c("19", accyear),
        dplyr::between(as.numeric(accyear), 00, 59) ~ stringr::str_c("20", accyear),
        TRUE ~ accyear
      ),
      startnr = stringr::str_pad(startnr, width = 5, pad = "0", side = "left"),
      endnr = stringr::str_pad(endnr, width = 5, pad = "0", side = "left"),
      accnr_new = case_when(
        is.na(endnr) ~ stringr::str_c(catalogue, accyear, "/", startnr),
        !is.na(endnr) ~ stringr::str_c(catalogue, accyear, "/", startnr, "-", endnr),
        TRUE ~ NA_character_
      )
    )

  if (!.exclude_non_matches) {
    out <- out |>
      dplyr::mutate(
        accnr_new = dplyr::case_when(
          !is.na(accnr) ~ accnr_new,
          TRUE ~ .x
        )
      )
  }

  out |>
    dplyr::pull(accnr_new)
}

# v <- c("C99/7899", "C1999/07899", "ZÄ96", "C*05/7032", "c2013/07156-07157")
# format_accnr(v)
# format_accnr(v, .exclude_non_matches = FALSE)
# format_accnr(v |> str_remove("\\*"))
# format_accnr(v |> str_remove("\\*"), .exclude_non_matches = TRUE)

#' Title
#'
#' @param .data
#' @param .variable
#'
#' @return
#' @export
#'
#' @examples
add_accnr <- function(.data, .variable = "PROV_KOD_ORIGINAL") {
  .data |>
    dplyr::mutate(
      dplyr::across(
        tidyselect::any_of("accession_id"),
        \(x) eaglesEggs::ESB_id2accnr(x),
        .names = .variable
      ),
      .after = tidyselect::any_of("accession_id")
    )
}

#' Title
#'
#' @param accnr
#'
#' @return
#' @export
#'
#' @examples
ESB_accnr2id <- function (accnr) {
  accnr <- stringr::str_remove(accnr, "/")
  nr <- stringr::str_sub(accnr, -5)
  yr <- stringr::str_sub(accnr, -8, -6)
  lst <- toupper(stringr::str_sub(accnr, 1, 1))
  prefix <- dplyr::case_when(
    lst == "A" ~ "1",
    lst == "B" ~ "2",
    lst == "C" ~ "3",
    lst == "D" ~ "4",
    lst == "G" ~ "7",
    lst == "H" ~ "8",
    lst == "L" ~ "5",
    lst == "P" ~ "9",
    lst == "X" ~ "6",
    TRUE ~ "0"
  )
  id <- stringr::str_c(prefix, yr, nr)
  id
}

#' Title
#'
#' @param id
#'
#' @return
#' @export
#'
#' @examples
ESB_id2accnr <- function (id) {

  nr <- stringr::str_sub(id, -5)
  yr <- stringr::str_sub(id, -8, -6)
  lst <- toupper(stringr::str_sub(id, 1, 1))

  yr <- dplyr::case_when(
    stringr::str_sub(yr, 1, 1) |> as.numeric() %in% c(0:6) ~ stringr::str_c("2", yr),
    TRUE ~ stringr::str_c("1", yr)
  )

  prefix <- dplyr::case_when(
    lst == "1" ~ "A",
    lst == "2" ~ "B",
    lst == "3" ~ "C",
    lst == "4" ~ "D",
    lst == "7" ~ "G",
    lst == "8" ~ "H",
    lst == "5" ~ "L",
    lst == "9" ~ "P",
    lst == "6" ~ "X",
    TRUE ~ "-"
  )
  accnr <- stringr::str_c(prefix, yr, "/", nr)
  accnr
}

# accnr <- c("C1986/07048", "C86/7048")

# accnr |>
#  eaglesEggs::format_accnr() |>
#  ESB_accnr2id()

# id <- "398607048"

# id |>
#  ESB_id2accnr()
