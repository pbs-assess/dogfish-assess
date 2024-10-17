tidy_catch_dogfish <- function(
    dat,
    areas = NULL,
    ...
) {
  dat <- set_fishing_year(dat, ...)

  if (!is.null(areas)) {
    dat$area <- assign_areas(dat$major_stat_area_name, areas)
    dat <- dat[!is.na(dat$area), , drop = FALSE]
  } else {
    dat$area <- "Coastwide"
  }

  dat <- filter(dat, !is.na(species_common_name), !is.na(year)) %>%
    group_by(year, species_common_name, gear, area) %>%
    summarise(
      landed_kg = sum(landed_kg, na.rm = TRUE),
      discarded_kg = sum(discarded_kg, na.rm = TRUE),
      landed_pcs = sum(landed_pcs, na.rm = TRUE),
      discarded_pcs = sum(discarded_pcs, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(species_common_name, year)

  # Note that discarded_kg only includes trawl discards from 1996+ and
  # trap/hook and line discards from 2006+
  catches <- mutate(dat,
    gear = dplyr::recode(gear,
      UNKNOWN = "Unknown/trawl",
      `BOTTOM TRAWL` = "Bottom trawl",
      `HOOK AND LINE` = "Hook and line",
      `LONGLINE` = "Hook and line",
      `MIDWATER TRAWL` = "Midwater trawl",
      `TRAP` = "Trap",
      `UNKNOWN TRAWL` = "Unknown/trawl"
    )
  ) %>%
    select(year, area, species_common_name, gear, landed_kg, discarded_kg, discarded_pcs)

  # cm <- reshape2::melt(catches,
  #   id.vars = c("year", "species_common_name", "area", "gear")
  # )

  all_catch <- group_by(catches, year, species_common_name, area, gear) %>%
    summarise(
      landed_kg = sum(landed_kg, na.rm = TRUE),
      discarded_kg = sum(discarded_kg, na.rm = TRUE),
      discarded_pcs = sum(discarded_pcs, na.rm = TRUE),
      .groups = "drop")

  all_catch
}
