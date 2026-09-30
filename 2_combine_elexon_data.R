# Elexon Data adjusted for curtailment and outages ####

# libraries ####
require(dplyr)
require(tidyr)
require(data.table)
require(arrow)


wind_bmus_alt_fname <- "data/wind_bmu_alt.csv"
if (!file.exists(wind_bmus_alt_fname)) {
  power_park <- readxl::read_xlsx(
    "data/power-park-modules.xlsx",
    sheet = 1,
    n_max = 252
  ) %>%
    rename(
      elexonBmUnit = `Settlement BMU name`,
      capacity_alt = `BMU or Large Reg Cap only`
    )
  names(power_park) <- tolower(gsub(" ", "_", names(power_park)))
  wind.bmus.alt <- wind.bmus %>%
    left_join(
      power_park %>% dplyr::select(elexonbmunit, capacity_alt),
      by = c("elexonBmUnit" = "elexonbmunit")
    ) %>%
    mutate(
      diff_cap = abs(generationCapacity - capacity_alt),
      capacity = case_when(
        is.na(capacity_alt) ~ generationCapacity,
        capacity_alt <= 0 ~ generationCapacity,
        diff_cap > generationCapacity * 0.1 ~ capacity_alt,
        TRUE ~ generationCapacity
      )
    )
} else {
  wind.bmus.alt <- read.csv(wind_bmus_alt_fname)
}

# read data ####
path <- "../elexon_data/data_by_year"
year_seq <- seq(2019, 2025, 1)
year_seq <- seq(2025, 2025, 1)
# generation
bmu_df <- lapply(year_seq, function(x) {
  file_path <- file.path(
    path,
    paste0("wind_gen_bmu_", x, ".csv.gz")
  )
  fread(file_path)
}) %>%
  bind_rows()
# curtailment
curt_df <- lapply(year_seq, function(x) {
  file_path <- file.path(
    path,
    paste0("wind_curt_bmu_", x, ".csv.gz")
  )
  fread(file_path)
}) %>%
  bind_rows() %>%
  filter(dataType == "Tagged")

gen_adj <- bmu_df %>%
  select(
    settlementDate,
    settlementPeriod,
    bmUnit,
    halfHourEndTime,
    quantity
  ) %>%
  unique() %>%
  mutate(settlementDate = as.Date(settlementDate)) %>%
  # head(30) %>%
  left_join(
    curt_df %>%
      select(settlementDate, settlementPeriod, bmUnit, totalVolumeAccepted) %>%
      unique() %>%
      mutate(settlementDate = as.Date(settlementDate)),
    by = c("settlementDate", "settlementPeriod", "bmUnit")
  ) %>%
  mutate(
    curtailment = replace_na(-totalVolumeAccepted, 0),
    potential = quantity + curtailment
  ) %>%
  left_join(
    wind.bmus.alt %>%
      select(elexonBmUnit, bmUnitName, capacity) %>%
      unique(),
    by = c("bmUnit" = "elexonBmUnit")
  ) %>%
  filter(
    capacity > 0,
    # potential > 0
  ) %>%
  mutate(
    potential = case_when(
      potential < 0 ~ 0,
      potential > capacity ~ capacity,
      TRUE ~ potential
    ),
    cap_factor = ifelse(capacity > 0, potential / capacity)
  )

# catalog with REPD variables

ref_catalog_2025 <- read.csv(
  gzfile(file.path("data/ref_catalog_wind_2025_era.csv.gz"))
) %>%
  mutate(operational_date = as.Date(operational_date))

remit_df <- lapply(
  year_seq,
  \(y) {
    read_parquet(
      file.path("~/Documents/elexon/", sprintf("remit_all_%d.parquet", y))
    )
  }
) %>%
  bind_rows()

# get BmUnit
remit_df <- remit_df %>%
  mutate(
    elexonBmUnit = case_when(
      # asset matches bmUnit
      assetId %in% wind.bmus.alt$elexonBmUnit ~ assetId,
      # asset matches with T_
      paste0("T_", assetId) %in% wind.bmus.alt$elexonBmUnit ~ paste0(
        "T_",
        assetId
      ),
      # affected unit matches
      toupper(affectedUnit) %in% wind.bmus.alt$elexonBmUnit ~ affectedUnit,
      # affected unit matches with T_
      paste0("T_", toupper(affectedUnit)) %in%
        wind.bmus.alt$elexonBmUnit ~ paste0("T_", toupper(affectedUnit)),
      # special cases
      grepl("LARYO", assetId) ~ paste0("T_", gsub("O", "W", assetId)),
      grepl("RAMP", assetId) ~ paste0("T_", gsub("RAMP", "RMPNO", assetId)),
      # else
      TRUE ~ NA
    ),
    inelexon = !is.na(elexonBmUnit)
  ) %>%
  left_join(
    ref_catalog_2025 %>% select(bmUnit, tech_typ, lon, lat),
    by = c("elexonBmUnit" = "bmUnit")
  )

remit_wf <- remit_df %>%
  filter(
    eventStatus == "Active",
    !is.na(elexonBmUnit),
    # grepl("Wind", fuelType)
    eventStartTime < eventEndTime,
    !is.na(normalCapacity)
  ) %>%
  mutate(
    mincapacity = ifelse(
      is.na(outageProfile),
      availableCapacity,
      purrr::map_dbl(outageProfile, ~ min(.x$capacity))
    ),
    capacity_impact = normalCapacity - mincapacity,
    capacity_imp_perc = capacity_impact / normalCapacity * 100
  ) %>%
  filter(capacity_impact > 0) %>%
  unique() %>%
  mutate(
    eventEndTime = as.POSIXct(
      eventEndTime,
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    ),
    eventStartTime = as.POSIXct(
      eventStartTime,
      format = "%Y-%m-%dT%H:%M:%OSZ",
      tz = "UTC"
    ),
    duration = as.numeric(difftime(
      eventEndTime,
      eventStartTime,
      units = "hours"
    ))
  )

write_parquet(
  remit_wf,
  sink = "~/Documents/elexon/remit_wind.parquet"
)

remit_wf <- read_parquet("~/Documents/elexon/remit_wind.parquet") %>%
  unnest(outageProfile, keep_empty = TRUE) %>%
  mutate(
    startTime = if_else(
      is.na(startTime),
      eventStartTime,
      as.POSIXct(startTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    ), #%>%
    # ymd_hms(., format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    endTime = if_else(
      is.na(endTime),
      eventEndTime,
      as.POSIXct(endTime, format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
    ), #%>%
    # ymd_hms(., format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
    capacity = if_else(is.na(capacity), availableCapacity, capacity),
  ) %>%
  mutate(
    duration = as.numeric(difftime(
      endTime,
      startTime,
      units = "hours"
    ))
  ) %>%
  filter(!is.na(capacity), duration > 0) %>%
  rename(outageCapacity = capacity) %>%
  select(
    startTime,
    # startTime2,
    endTime,
    elexonBmUnit,
    normalCapacity,
    outageCapacity,
    duration
  )


remit_dt <- as.data.table(remit_wf)
gen_dt <- as.data.table(gen_adj)

# remit_dt[, startTime := ymd_hms(startTime, tz = "UTC")]
# remit_dt[, endTime := ymd_hms(endTime, tz = "UTC")]

# gen_adj should have a POSIXct datetime column: generationTime
# gen_dt[, halfHourEndTime := ymd_hms(halfHourEndTime, tz = "UTC")]

gen_dt[, start := halfHourEndTime - lubridate::minutes(30)]
gen_dt[, end := halfHourEndTime]
setkey(gen_dt, bmUnit, start, end)
setkey(remit_dt, elexonBmUnit, startTime, endTime)

result <- foverlaps(
  gen_dt,
  remit_dt,
  by.x = c("bmUnit", "start", "end"),
  by.y = c("elexonBmUnit", "startTime", "endTime"),
  type = "within",
  nomatch = NA
)

arrow::write_parquet(
  result,
  file.path("~/Documents/elexon", "gen_adj_v2.parquet")
)
