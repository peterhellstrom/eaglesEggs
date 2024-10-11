#' Title
#'
#' @param path
#' @param sheet
#' @param include_labcode
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_eggs <- function(
    path = "eggs.xlsx",
    sheet = "Eggs",
    include_labcode = FALSE
) {

  eggs <- readxl::read_xlsx(path, sheet) |>
    dplyr::select(
      PROV_KOD_ORIGINAL = `Acc-nr`,
      PROV_KOD_LABB = analysnr_Clc,
      PROVPLATS_ANALYSMALL = LKD
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
#' @return
#' @export
#'
#' @examples
wtse_provdata_biota <- function(
    path = "eggs.xlsx"
) {

  wtse_sites_eggs(path = path) |>
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
      # UNDERSOKNINGSTYP = "Havsörn, bestånd (Version 1.0)",
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
}

#' Title
#'
#' @param path
#' @param sheet
#' @param length_unit
#'
#' @return
#' @export
#'
#' @examples
wtse_eggs_from_excel <- function(
    path = "eggs.xlsx",
    sheet = "Eggs",
    length_unit = c("cm", "mm")
) {

  length_unit <- match.arg(length_unit, c("cm", "mm"))

  out <- readxl::read_xlsx(path, sheet) |>
    dplyr::select(
      PROV_KOD_ORIGINAL = `Acc-nr`,
      discovery_date_start = ins.datum,
      collector = collector,
      specimen_weight = `ins.vikt (g)`,
      egg_length = `längd (mm)`,
      egg_width = `bredd (mm)`,
      shell_weight = `skalvikt (g)`,
      egg_membrane_thickness = `hinna (mm)`,
      embryo_weight = `fostervikt (g)`,
      embryo_length = `fosterlängd (mm)`
    ) |>
    dplyr::filter(
      !is.na(PROV_KOD_ORIGINAL) & PROV_KOD_ORIGINAL != "kull"
    ) |>
    dplyr::mutate(
      discovery_date_start = janitor::convert_to_date(
        discovery_date_start,
        string_conversion_failure = "warning"
      )
    ) |>
    eaglesEggs::fix_null_strings() |>
    dplyr::rename(
      PROVTAG_DAT = discovery_date_start,
      TOTV = specimen_weight,
      TOTL = egg_length,
      BRED = egg_width,
      SKLV = shell_weight,
      FOSL = embryo_length
    ) |>
    dplyr::mutate(
      # Calculate shell index!
      # SKLI = shell_index,
      # How should SEX/KON be treated, this is not even a valid
      # parameter if an egg is not fertilized?
      KON = "I",
      #Assume no pools!
      ANTAL = 1,
      # Assume no pools to start with!
      ANTAL_DAGAR = 1
    )

  out <- out |>
    dplyr::mutate(
      PROVTAG_ORG = dplyr::case_when(
        stringr::str_detect(collector, "Schönbeck") ~ "OBF",
        # NOF: lägger in Harnesk här också, även om han kanske inte har stark koppling till föreningen?
        stringr::str_detect(collector, "Lindström|Öhman|Harnesk") ~ "NOF",
        stringr::str_detect(collector, "Strandtorn|Lundberg") ~ "RINGMARKARE",
        stringr::str_detect(collector, "Anders Johansson") ~ "RINGMARKARE",
        stringr::str_detect(collector, "Hjulström") ~ "RINGMARKARE",
        stringr::str_detect(collector, "Folkesson") ~ "SKOF",
        stringr::str_detect(collector, "Hellström") ~ "NRM",
        stringr::str_detect(collector, "Westerlund") ~ "ROSLAGSNATUR",
        stringr::str_detect(collector, "Smitterberg|Hjernquist") ~ "GOF_GOTL",
        stringr::str_detect(collector, "Peter Nilsson|Frans Olofsson") ~ "LST-Y",
        stringr::str_detect(collector, "Englund|Karelius|Thuresson") ~ "GLOF",
        stringr::str_detect(collector, "Wallin|Enetjärn") ~ "VOF_VASTERB",
        # Modigh verksam/anställd inom NATURSKYDDSFORENINGEN 2006-2019, anställd av NRM 2020
        # därefter utan affiliering till organisation eller förening
        stringr::str_detect(collector, "Modigh") & PROVTAG_DAT > "2021-01-01" ~ "RINGMARKARE",
        stringr::str_detect(collector, "Modigh") & dplyr::between(PROVTAG_DAT, as.Date("2020-01-01"), as.Date("2020-12-31")) ~ "NRM",
        stringr::str_detect(collector, "Modigh") & PROVTAG_DAT < "2019-12-31" ~ "NATURSKYDDSFORENINGEN",
        stringr::str_detect(collector, "Helander") & PROVTAG_DAT < "1984-12-31" ~ "NATURSKYDDSFORENINGEN",
        # Björn Helander: mellan 1985-2005 då han övergår till heltidstjänst på NRM?
        stringr::str_detect(collector, "Helander") & dplyr::between(PROVTAG_DAT, as.Date("1985-01-01"), as.Date("2005-12-31")) ~ "NRM",
        stringr::str_detect(collector, "Helander") & dplyr::between(PROVTAG_DAT, as.Date("2006-01-01"), as.Date("2013-08-01")) ~ "NRM",
        # Hur ska Björn Helander hanteras efter pensionering? Namn på företag, men haft affiliering till NRM!
        stringr::str_detect(collector, "Helander") & PROVTAG_DAT > "2013-08-01" ~ "NRM",
        PROVTAG_DAT < "1989-01-01" ~ "NATURSKYDDSFORENINGEN"
      )
    ) |>
    dplyr::relocate(
      PROVTAG_ORG,
      .after = collector
    )

  if (length_unit == "cm") {
    out <- out |>
      dplyr::mutate(
        # Convert from mm to cm
        # (as this is the unit we have specified in our codelist!)
        TOTL = TOTL / 10,
        BRED = BRED / 10
      )
  }

  out
}

# We could construct another view from ESBase with
# names according to NRM_PARAMETERKOD, but...

# Convert embryo column (0/1) to a text note column,
# indicating if egg contained an embry or not? Where to
# put that commentary column?
#' Title
#'
#' @param path
#' @param extended_types
#' @param query
#' @param length_unit
#'
#' @return
#' @export
#'
#' @examples
wtse_eggs_from_esbase <- function(
    path = "esb/esbase/esbase_dump.db",
    extended_types = TRUE,
    query = "SELECT * FROM v_egg WHERE species_id = 597",
    length_unit = c("cm", "mm")
) {

  on.exit(DBI::dbDisconnect(con))

  length_unit <- match.arg(length_unit, c("cm", "mm"))

  con <- DBI::dbConnect(
    RSQLite::SQLite(),
    path,
    extended_types = extended_types
  )

  out <- DBI::dbGetQuery(con, query) |>
    eaglesEggs::fix_null_strings() |>
    dplyr::select(
      accession_id,
      discovery_date_start,
      species_id,
      specimen_weight,
      starts_with("shell"),
      starts_with("egg"),
      starts_with("embryo")
    ) |>
    eaglesEggs::add_accnr() |>
    dplyr::select(
      -accession_id,
      -species_id,
      -embryo_weight,
      # perhaps turn embryo column into a text comment later
      -embryo
    ) |>
    dplyr::rename(
      PROVTAG_DAT = discovery_date_start,
      TOTV = specimen_weight,
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
      # Assume no pools to start with!
      ANTAL_DAGAR = 1
    )

  if (length_unit == "cm") {
    out <- out |>
      dplyr::mutate(
        # Convert from mm to cm
        # (as this is the unit we have specified in our codelist!)
        TOTL = TOTL / 10,
        BRED = BRED / 10
      )
  }

  out
}
