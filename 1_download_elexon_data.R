# download data

source("aux_funct.R")

path <- "../elexon_data"

dir.create(
  file.path(path, "data_by_year"),
  recursive = TRUE,
  showWarnings = FALSE
)

# list of units
url <- "https://data.elexon.co.uk/bmrs/api/v1/reference/bmunits/all"
response <- GET(url, accept("text/plain"))
json_data <- content(response, "text", encoding = "UTF-8")
bmus <- fromJSON(json_data)
wind.bmus <- bmus %>%
  filter(
    grepl("WIND", fuelType) |
      grepl("wind|offshore", leadPartyName, ignore.case = TRUE) |
      grepl("wind farm|windfarm", bmUnitName, ignore.case = TRUE)
  ) %>%
  filter(!is.na(elexonBmUnit)) %>%
  mutate(generationCapacity = as.numeric(generationCapacity))

wind.bmus %>%
  write.csv(
    .,
    gzfile(file.path("data", "wind_bmu_2.csv.gz"))
  )


# generation by unit
bmugen0 <- get_bmugen(
  year = 2025,
  path = file.path(path, "data_by_year"),
  end_time = "2025-12-31 00:00:00 BST"
)
years <- 2019:2024
lapply(
  years,
  \(y) {
    get_bmugen(
      year = y,
      path = file.path(path, "data_by_year")
    )
  }
)

# Curtailment
bmucurt0 <- get_curtailment(
  year = 2025,
  path = file.path(path, "data_by_year"),
  end_time = "2025-12-31 00:00:00 BST"
)

curt_df <- lapply(
  years,
  \(y) {
    get_curtailment(
      year = y,
      end_time = "2025-12-31 00:00:00 BST",
      path = file.path(path, "data_by_year")
    )
  }
)

# Remit data (outages)
remit0 <- get_remit(
  year = 2025,
  path = file.path(path, "data_by_year"),
  end_time = "2025-12-31 00:00:00 BST"
)
remit_full <- lapply(
  years,
  \(y) get_remit(year = y, path = file.path(path, "data_by_year"))
)
