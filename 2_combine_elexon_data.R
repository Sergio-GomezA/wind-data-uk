# Elexon Data adjusted for curtailment and outages ####

# libraries ####
require(dplyr)
require(tidyr)
require(data.table)
require(arrow)

# read data ####
path <- "~/Documents/elexon/data_by_year"
year_seq <- seq(2019, 2025, 1)
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


# add outages ####
# remit_df <- lapply(
#   2019:2025,
#   \(y) {
#     read_parquet(
#       file.path("~/Documents/elexon/", sprintf("remit_all_%d.parquet", y))
#     )
#   }
# ) %>%
#   bind_rows()

# remit_df <- remit_df %>%
#   filter(eventStatus == "Active") %>%
#   mutate(
#     elexonBmUnit = case_when(
#       # asset matches bmUnit
#       assetId %in% wind.bmus.alt$elexonBmUnit ~ assetId,
#       # asset matches with T_
#       paste0("T_", assetId) %in% wind.bmus.alt$elexonBmUnit ~ paste0(
#         "T_",
#         assetId
#       ),
#       # affected unit matches
#       toupper(affectedUnit) %in% wind.bmus.alt$elexonBmUnit ~ affectedUnit,
#       # affected unit matches with T_
#       paste0("T_", toupper(affectedUnit)) %in%
#         wind.bmus.alt$elexonBmUnit ~ paste0("T_", toupper(affectedUnit)),
#       # special cases
#       grepl("LARYO", assetId) ~ paste0("T_", gsub("O", "W", assetId)),
#       grepl("RAMP", assetId) ~ paste0("T_", gsub("RAMP", "RMPNO", assetId)),
#       # else
#       TRUE ~ NA
#     ),
#     inelexon = !is.na(elexonBmUnit)
#   ) %>%
#   left_join(
#     ref_catalog_2025 %>% select(bmUnit, tech_typ),
#     by = c("elexonBmUnit" = "bmUnit")
#   ) %>%
#   filter(
#     !is.na(elexonBmUnit)
#   ) %>%
#   unique() %>%
#   unnest(outageProfile, keep_empty = TRUE) %>%
#   mutate(
#     startTime = if_else(
#       is.na(startTime),
#       eventStartTime,
#       startTime %>%
#         ymd_hms(., format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
#     ),
#     endTime = if_else(
#       is.na(endTime),
#       eventEndTime,
#       endTime %>%
#         ymd_hms(., format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
#     ),
#     capacity = if_else(is.na(capacity), availableCapacity, capacity),
#   ) %>%
#   # mutate(
#   #   endTime = if_else(endTime < startTime, startTime + hours(1), endTime)
#   # ) %>%
#   filter(!is.na(capacity)) %>%
#   rename(outageCapacity = capacity) %>%
#   select(
#     startTime,
#     endTime,
#     elexonBmUnit,
#     normalCapacity,
#     outageCapacity
#   )

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
