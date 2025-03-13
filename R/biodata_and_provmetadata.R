#' Title
#'
#' @param path
#' @param sheet
#' @param include_labcode
#' @param skip
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_eggs <- function(
    path = "eggs.xlsx",
    sheet = "eggs",
    include_labcode = FALSE,
    skip = 1,
    ...
) {

  eggs <- readxl::read_xlsx(path, sheet, skip = skip, ...) |>
    dplyr::select(
      PROV_KOD_ORIGINAL = accnr,
      PROV_KOD_LABB = labcode_clc,
      PROVPLATS_ANALYSMALL = locality
    ) |>
    dplyr::filter(
      !is.na(PROV_KOD_ORIGINAL) & PROV_KOD_ORIGINAL != "kull"
    )

  if (include_labcode) {
    out <- eggs |>
      dplyr::select(
        PROV_KOD_ORIGINAL,
        PROV_KOD_LABB,
        PROVPLATS_ANALYSMALL
      )
  } else {
    out <- eggs |>
      dplyr::select(
        PROV_KOD_ORIGINAL,
        PROVPLATS_ANALYSMALL
      )
  }
  out |>
    dplyr::distinct() |>
    dplyr::mutate(
      GENUS = "Haliaeetus albicilla"
    )
}

#' Title
#'
#' @param path
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
wtse_provdata_biota <- function(
    path = "eggs.xlsx",
    ...
) {

  wtse_sites_eggs(path = path, ...) |>
    dplyr::select(PROV_KOD_ORIGINAL) |>
    dplyr::distinct() |>
    dplyr::mutate(
      ART = "Havsorn",
      DYNTAXA_TAXON_ID = "100067",
      KON = "X",
      ANTAL = 1,
      KOMMENTAR_PROV = NA_character_
    )
}

#' Title
#'
#' @param dsn
#' @param path
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_provmetadata <- function(
    path = "eggs.xlsx",
    dsn = "Havsorn_Data"
) {

  sites <- eagles::wtse_sites(
    odbc_name = dsn,
    add_monitoring = FALSE
  )

  eggs <- wtse_eggs_from_excel(path = path)

  sites <- sites |>
    dplyr::select(Region, Lokalkod, Lan) |>
    dplyr::mutate(
      PROVDATA_TYP = "BIOTA",
      # This is the default value, but has to be changed
      # depending on time (pre-1989 not NMO) and space
      # (LST Y perhaps RMO, Projekt Havsörn sites not officially included
      # in the NMO programme)
      PROVTAG_SYFTE = "NMO",
      PROVTAG_ORG = dplyr::case_when(
        Lan == "Y" ~ "LST-Y",
        TRUE ~ "NRM"
      ),
      UNDERSOKNINGSTYP = NA_character_,
      PROVPLATS_MILJO = dplyr::case_when(
        stringr::str_detect(Region, "(?i)kust") ~ "HAV-BRACKV",
        stringr::str_detect(Region, "(?i)inland") ~ "SJO-SOTV-RINN",
        TRUE ~ NA_character_
      ),
      PROVPLATS_TYP = "Bakgrund",
      ACKR_PROV = "Nej",
      PLATTFORM = "SAKNAS",
      PROVTAG_MET = "Aggplockning",
      DIREKT_BEHA = "KYLT"
    ) |>
    dplyr::select(
      -Region, -Lan
    ) |>
    dplyr::rename(
      PROVPLATS_ANALYSMALL = Lokalkod
    )

  sites
}

#' Title
#'
#' @param path
#' @param skip
#' @param sheet
#' @param rename
#' @param collector_org
#' @param accnr_only
#'
#' @return
#' @export
#'
#' @examples
wtse_eggs_from_excel <- function(
    path = "eggs.xlsx",
    sheet = "eggs",
    skip = 1,
    rename = TRUE,
    collector_org = "PROVTAG_ORG",
    accnr_only = TRUE
) {

  x <- readxl::read_xlsx(path, sheet, skip = skip)

  # Convert dates (beware of problems with Excel dates < 1901-01-01)
  out <- x |>
    dplyr::mutate(
      discovery_date_start = convert_old_dates(discovery_date_start)
    ) |>
    fix_null_strings()

  if (rename) {
    out <- out |>
      # Rename variables to match codelist with NRM parameter codes.
      dplyr::rename(
        # SGU variable names
        PROV_KOD_ORIGINAL = accnr,
        PROVTAG_DAT = discovery_date_start,
        # NRM prc code list (codelist_wtse.xlsx in MoCiS2)
        TOTV = egg_weight,
        TOTL = egg_length,
        BRED = egg_width,
        SKLV = shell_weight,
        FOSL = embryo_length,
        FOSV = embryo_weight
      ) |>
      dplyr::mutate(
        # How should SEX/KON be treated, this is not even a valid
        # parameter if an egg is not fertilized?
        KON = "I",
        # Assume no pools!
        ANTAL = 1,
        ANTAL_DAGAR = 1
      )
  }

  if (!is.null(collector_org)) {
    if (rename) {
      date_col <- "PROVTAG_DAT"
    } else {
      date_col <- "discovery_date_start"
    }

    out <- out |>
      dplyr::mutate(
        {{ collector_org }} := dplyr::case_when(
          stringr::str_detect("collector", "Schönbeck|Hellqvist|Gullstrand") ~ "OBF",
          # NOF: lägger in Harnesk här också:
          stringr::str_detect("collector", "Lindström|Öhman|Harnesk") ~ "NOF",
          stringr::str_detect("collector", "Strandtorn|Lundberg") ~ "RINGMARKARE",
          stringr::str_detect("collector", "Anders Johansson") ~ "RINGMARKARE",
          stringr::str_detect("collector", "Hjulström") ~ "RINGMARKARE",
          stringr::str_detect("collector", "Folkesson") ~ "SKOF",
          stringr::str_detect("collector", "Hellström") ~ "NRM",
          stringr::str_detect("collector", "Westerlund") ~ "ROSLAGSNATUR",
          stringr::str_detect("collector", "Smitterberg|Hjernquist") ~ "GOF_GOTL",
          stringr::str_detect("collector", "Peter Nilsson|Frans Olofsson") ~ "LST-Y",
          stringr::str_detect("collector", "Englund|Karelius|Thuresson") ~ "GLOF",
          # Hur blir det med Modighs ägg insamlade i Västerbotten om
          # VOF_VASTERB kommer före modigh i denna case_when-sats?
          stringr::str_detect("collector", "Wallin|Enetjärn") ~ "VOF_VASTERB",
          # Modigh verksam/anställd inom NATURSKYDDSFORENINGEN 2006-2019, anställd av NRM 2020
          # därefter utan affiliering till organisation eller förening
          stringr::str_detect("collector", "Modigh") & get(date_col) > "2021-01-01" ~ "RINGMARKARE",
          stringr::str_detect("collector", "Modigh") & dplyr::between(get(date_col), as.Date("2020-01-01"), as.Date("2020-12-31")) ~ "NRM",
          stringr::str_detect("collector", "Modigh") & get(date_col) < "2019-12-31" ~ "NATURSKYDDSFORENINGEN",
          stringr::str_detect("collector", "Helander") & get(date_col) < "1984-12-31" ~ "NATURSKYDDSFORENINGEN",
          # Björn Helander: mellan 1985-2005 då han övergick till heltidstjänst på NRM? Sätt SNF, alla insamlingstillstånd verkar ha gått på SNF...
          stringr::str_detect("collector", "Helander") & dplyr::between(get(date_col), as.Date("1985-01-01"), as.Date("2005-12-31")) ~ "NATURSKYDDSFORENINGEN",
          stringr::str_detect("collector", "Helander") & dplyr::between(get(date_col), as.Date("2006-01-01"), as.Date("2013-08-01")) ~ "NRM",
          # Hur ska Björn Helander hanteras efter pensionering? Namn på företag, men haft affiliering till NRM!
          stringr::str_detect("collector", "Helander") & get(date_col) > "2013-08-01" ~ "NRM",
          dplyr::between(get(date_col), as.Date("1964-01-01"), as.Date("1989-01-01")) ~ "NATURSKYDDSFORENINGEN",
          get(date_col) < "1964-01-01" ~ "SAKNAS",
          TRUE ~ "SAKNAS"
        )
      ) |>
      dplyr::relocate(
        {{ collector_org }},
        .after = collector
      )
  }

  if (rename) {
    accnr_col <- "PROV_KOD_ORIGINAL"
  } else {
    accnr_col <- "accnr"
  }

  if (accnr_only) {
    out <- out |>
      dplyr::filter(
        !is.na(get(accnr_col)) & get(accnr_col) != "kull"
      )
  }

  out
}


#' Title
#'
#' @param path
#' @param sheet
#' @param range
#' @param sheet_names
#' @param convert_dates
#' @param convert_dates_column
#'
#' @returns
#' @export
#'
#' @examples
wtse_eggs_1996 <- function(
    path = "eggs-1996.xlsx",
    sheet = "data",
    range = "A3:AG501",
    sheet_names = "column_names",
    convert_dates = FALSE,
    convert_dates_column = "discovery_date_start"
) {

  # readxl::excel_sheets(path)

  # New column names
  col_names <- readxl::read_xlsx(
    path,
    sheet_names
  )

  # Read data and rename columns
  # Note that some columns are repeated.

  x <- readxl::read_xlsx(
    path,
    sheet,
    range,
    col_names = FALSE,
    col_types = col_names$col_type,
    .name_repair = "unique_quiet"
  )

  out <- x |>
    rlang::set_names(col_names$col_name_new)

  if (convert_dates) {
    out <- out |>
      dplyr::mutate(
        {{ convert_dates_column }} := dplyr::case_when(
          base::nchar(discovery_date_start) == 6 ~
            stringr::str_c(stringr::str_sub(discovery_year, 1, 2), discovery_date_start),
          base::nchar(discovery_date_start) == 4 ~
            stringr::str_c(stringr::str_sub(discovery_year, 1, 2), discovery_date_start, "01")
        ) |>
          lubridate::ymd()
      )
  }

  out

}

# Convert embryo column (0/1) to a text note column,
# indicating if egg contained an embry or not? Where to
# put that commentary column?
#' Title
#'
#' @param path
#' @param extended_types
#' @param rename
#' @param accnr_col
#' @param all_cols
#' @param ...
#' @param query
#'
#' @return
#' @export
#'
#' @examples
wtse_eggs_from_esbase <- function(
    path = "esb/esbase/esbase_dump.db",
    extended_types = TRUE,
    query = "SELECT * FROM v_egg WHERE species_id = 597",
    rename = TRUE,
    accnr_col = "PROV_KOD_ORIGINAL",
    all_cols = FALSE,
    ...
) {

  on.exit(DBI::dbDisconnect(con))

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    path,
    extended_types = extended_types
  )

  out <- DBI::dbGetQuery(con, query) |>
    eaglesEggs::fix_null_strings() |>
    eaglesEggs::add_accnr(.variable = accnr_col)

  # shell_thickness1 = shell_thickness_corrected (? - rarely used)
  # shell_thickness2 = shell_thickness_with_membrane
  # shell_thickness3 = shell_thickness_without_membrane
  # hinna = membrane_thickness

  if (!all_cols) {
    out <- out |>
      dplyr::select(
        accnr_col,
        accession_id,
        discovery_date_start,
        discovery_date_end,
        species_id,
        egg_weight,
        starts_with("shell"),
        starts_with("egg"),
        starts_with("embryo")
      )
  }

  if (rename) {
    out <- out |>
      dplyr::rename(
        PROVTAG_DAT = discovery_date_start,
        TOTV = egg_weight,
        TOTL = egg_length,
        BRED = egg_width,
        SKLV = shell_weight,
        SKLI = shell_index,
        FOSL = embryo_length
      ) |>
      # Note that ORGAN has to be included
      # in analysdata (as we can have data from multiple tissues per accnr)
      dplyr::mutate(
        # How should SEX/KON be treated, this is not even a valid
        # parameter if an egg is not fertilized?
        KON = "I",
        # Assume no pooled samples!
        ANTAL = 1,
        ANTAL_DAGAR = 1
      )
  }

  out
}


#' Title
#'
#' @param path
#' @param sheet
#' @param output
#' @param remove_empty_cols
#'
#' @returns
#' @export
#'
#' @examples
wtse_eggs_from_ov <- function(
    path = "esb/excel_catalogs/OV-Havsörn.xlsx",
    sheet = "OV Havsörn (edit)",
    output = c("samples", "specimens"),
    remove_empty_cols = TRUE
) {

  output <- match.arg(output, c("samples", "specimens"))

  x <- readxl::read_xlsx(
    "esb/excel_catalogs/OV-Havsörn.xlsx",
    "OV Havsörn (edit)",
    .name_repair = "unique_quiet"
  )

  if (output == "specimens") {
    # Accessionsförda exemplar/individer ----
    out <- x |>
      dplyr::select(1:`Uppdatering av`)

    if (remove_empty_cols) {
      out <- out |>
        # Remove empty columns (roughly 80!)
        janitor::remove_empty(which = "cols")
    }

  } else if (output == "samples") {
    # Provberedning (subsampling) ----

    samples_repeated <- x |>
      dplyr::select(
        `Accessions nummer`,
        tidyselect::starts_with("Materialtyp"):tidyselect::last_col()
      )

    ## Breakpoints for repeated columns ----

    # Each new sample starts at this column indices:
    samples_repeated_start <- stringr::str_which(
      names(samples_repeated),
      "Materialtyp"
    )

    # Calculate end position for each sample,
    # replace final NA with maximum number of columns.
    samples_repeated_stop <- tidyr::replace_na(
      dplyr::lead(samples_repeated_start) - 1,
      base::ncol(samples_repeated)
    )

    ## Split repeated columns into lists ----
    out <- purrr::map2(
      samples_repeated_start,
      samples_repeated_stop,
      \(x, y) samples_repeated |>
        dplyr::select(1L, tidyselect::all_of(x:y)) |>
        dplyr::rename_with(
          \(x) stringr::str_remove(x, "(\\...)([0-9]{3})")
        ) |>
        dplyr::mutate(
          dplyr::across(tidyselect::matches(c("Kommentar")), as.character),
        ) |>
        # Remove row if all columns are NA (i.e. no data)
        dplyr::filter(
          !dplyr::if_all(2:tidyselect::last_col(), is.na)
        ) |>
        dplyr::rename(
          tidyselect::any_of(
            c(Provberedningsdat = "Prov-berednings-dat")
          )
        )
    ) |>
      purrr::list_rbind() |>
      dplyr::rename(
        accnr = `Accessions nummer`,
        materialtyp = Materialtyp,
        provvikt = `Provvikt (g), enskilda prover`,
        delvikt = `Delvikt i homogenat (g), poolade`,
        analystyp = Analystyp,
        analytiker = Analytiker,
        provberedningsdatum = `Provberedningsdat`,
        provberedare = Provberedare,
        kommentar = Kommentar,
        kommentar2 = `Kommentar 2`
      ) |>
      dplyr::mutate(
        provberedningsdatum = as.Date(provberedningsdatum)
      ) |>
      dplyr::arrange(
        accnr, materialtyp, analystyp
      )
  }

  out

}
