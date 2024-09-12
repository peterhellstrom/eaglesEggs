# Construct WTSE provmetadata and biodata ----
#' Title
#'
#' @param dsn 
#'
#' @return
#' @export
#'
#' @examples
wtse_sites_provmetadata <- function(
    dsn = "Havsorn_Data") {
  eagles::wtse_sites(odbc_name = dsn, add_monitoring = FALSE) |> 
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
      UNDERSOKNINGSTYP = "Havsörn, bestånd (Version 1.0)",
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
eggs_from_excel <- function(
    path = "eggs.xlsx",
    sheet = "Eggs",
    length_unit = c("cm", "mm")
) {
  
  length_unit <- match.arg(length_unit, c("cm", "mm"))
  
  out <- readxl::read_xlsx(path, sheet) |> 
    dplyr::select(
      PROV_KOD_ORIGINAL = `Acc-nr`,
      discovery_date_start = ins.datum,
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
      # SKLI = shell_index,
      FOSL = embryo_length
    ) |> 
    dplyr::mutate(
      # How should SEX/KON be treated, this is not even a valid
      # parameter if an egg is not fertilized?
      KON = "U",
      #Assume no pools!
      ANTAL = 1,
      # Assume no pools to start with!
      ANTAL_DAGAR = 1
    )
  
  if (length_unit == "cm") {
    out <- out |> 
      dplyr::mutate(
        # Convert from mm to cm 
        # (as this is the unit we have specified in our codelist!)
        TOTL = TOTL/10,
        BRED = BRED/10
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
eggs_from_esbase <- function(
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
      KON = "U",
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
        TOTL = TOTL/10,
        BRED = BRED/10
      )
  }
  
  out
}
