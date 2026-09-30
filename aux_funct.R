require(data.table)
require(dplyr)
require(tidyr)
require(ggplot2)
require(ggsci)
require(terra)
require(fst)
require(stringr)
require(httr)
require(jsonlite)
require(arrow)
require(geosphere)

theme_set(theme_bw())

mypalette <- pal_aaas()(3)[c(1, 3)] # offshore / onshore
regime_palette <- pal_lancet()(3)[c(2, 3, 1)]
## functions

## get GWA data

get_gwa_data <- function(
  country,
  variable,
  height,
  url0 = "https://globalwindatlas.info/api/gis/country",
  filename = NULL,
  path = NULL,
  overwrite = FALSE
) {
  # browser()
  if (is.null(filename)) {
    filename <- sprintf("%s_%s_%sm.tif", country, variable, height)
  }
  if (is.null(path)) {
    dest <- filename
  } else {
    if (!dir.exists(path)) {
      dir.create(path, recursive = TRUE)
    }
    dest <- file.path(path, filename)
  }

  # construct URL
  url <- sprintf("%s/%s/%s/%s", url0, country, variable, height)

  if (file.exists(dest) && !overwrite) {
    r <- terra::rast(dest)
    return(r)
  }

  resp <- httr::GET(url, httr::write_disk(dest, overwrite = TRUE))
  httr::stop_for_status(resp)
  ct <- httr::headers(resp)[["content-type"]]

  if (is.null(ct)) {
    ct <- ""
  }
  if (!grepl("tiff|geotiff|image/tiff", ct, ignore.case = TRUE)) {
    unlink(dest)
    stop("Download did not return a GeoTIFF (content-type: ", ct, ").")
  }
  r <- terra::rast(dest)

  r
}


process_gwa_tif <- function(
  data,
  agg_fact = 5L,
  ...
) {
  if (agg_fact > 1) {
    df_agg <- data |>
      terra::aggregate(fact = agg_fact, fun = mean, na.rm = TRUE) |>
      terra::as.data.frame(., xy = TRUE, na.rm = TRUE)
  } else {
    df_agg <- terra::as.data.frame(data, xy = TRUE, na.rm = TRUE)
  }

  var_name <- names(df_agg)[3]
  meta_info <- strsplit(var_name, "_|m")[[1]]
  df_agg <- df_agg |>
    mutate(
      country = meta_info[1],
      variable = meta_info[2],
      height = meta_info[3]
    ) |>
    rename(value = !!var_name)
  df_agg
}


combined_gwa_data <- function(
  countries = c("GBR", "IRL", "IMN"),
  variable = "wind-speed",
  height = 100,
  agg_fact = 20L,
  path = "~/Documents/GWA/",
  overwrite = TRUE,
  show_fig = TRUE,
  save_df = FALSE,
  mcores = 8,
  ...
) {
  d_countries <- lapply(
    c("GBR", "IRL", "IMN"),
    \(x) {
      get_gwa_data(
        country = x,
        var = variable,
        height = height,
        path = "~/Documents/GWA/"
      )
    }
  )

  d_combined <- Reduce(merge, d_countries)

  out_path <- file.path(path, sprintf("merged_%s_%sm.tif", variable, height))

  writeRaster(d_combined, out_path, overwrite = TRUE)

  df_combined <-
    lapply(
      d_countries,
      function(x) {
        process_gwa_tif(
          data = resample(x, d_combined, threads = mcores),
          agg_fact = agg_fact
        )
      }
    ) |>
    bind_rows()

  if (save_df) {
    write_fst(
      df_combined,
      file.path(
        path,
        sprintf("df_%s_%dm_agg%d.fst", variable, height, agg_fact)
      ),
      compress = 100
    )
  }

  plot <- df_combined |>
    ggplot(aes(x = x, y = y, fill = value)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "mako",
      na.value = "transparent",
      name = case_when(
        grepl("Weibull", variable) ~ "",
        TRUE ~ "m/s"
      ),
      ...
    ) +
    coord_quickmap() +
    theme_minimal() +
    theme(legend.position = "bottom") +
    labs(
      x = "lon",
      y = "lat",
      title = sprintf("%s at %dm", gsub("-", " ", variable), height)
    )

  if (show_fig) {
    print(plot)
  }
  fig_name <- sprintf("fig/%s_%dm_merged_agg%d.png", variable, height, agg_fact)
  ggsave(plot = plot, filename = fig_name, width = 5, height = 7)

  invisible(list(data = df_combined, plot = plot))
}


clean_names <- function(name) {
  name %>%
    iconv(from = "", to = "UTF-8", sub = "") %>% # Fix encoding
    tolower() %>% # Lowercase
    str_replace_all("[^a-z0-9 ]", " ") %>% # Remove punctuation
    str_replace_all("\\s+", " ") %>% # Remove extra spaces
    str_trim()
}


get_agpt <- function(
  year = 2024,
  path,
  file_name = sprintf("aggregated_gen_type_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/AGPT/stream?"

      url <- sprintf(
        "%spublishDateTimeFrom=%s&publishDateTimeTo=%s&format=json",
        base_url,
        dt0_enc,
        dt1_enc
      )

      response <- GET(url, accept("text/plain"))
      json_data <- content(response, "text", encoding = "UTF-8")
      # browser()
      agpt0 <- json_data %>%
        fromJSON(flatten = T) %>%
        mutate(across(
          matches("Time"),
          ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
        ))
      agpt0
    }
  )

  agptyear <- extraction %>% bind_rows()

  agptyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(agptyear)
}


get_dwgs <- function(
  year = 2024,
  path,
  file_name = sprintf("dayahead_generation_windsolar_%d.csv.gz", year)
) {
  time_seq <- dates <- seq.Date(
    from = as.Date(sprintf("%d-01-01", year)),
    to = as.Date(sprintf("%d-12-31", year)),
    by = "4 days"
  )

  # if(day(time_seq[length(time_seq)])!=31)

  time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/DGWS/stream?"

      url <- sprintf(
        "%spublishDateTimeFrom=%s&publishDateTimeTo=%s&format=json",
        base_url,
        dt0_enc,
        dt1_enc
      )

      response <- GET(url, accept("text/plain"))
      json_data <- content(response, "text", encoding = "UTF-8")
      # browser()
      dwgs0 <- tryCatch(
        json_data %>%
          fromJSON(flatten = T), #%>%
        # mutate(across(matches("Time"), ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")))
        error = function(e) {
          warning(sprintf(
            "Data extraction for day %s failed. Response: %s\n",
            t0,
            response$status_code
          ))
          return(NULL)
        }
      )
      dwgs0
    }
  )

  dwgsyear <- extraction %>% bind_rows()

  dwgsyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(dwgsyear)
}

get_bmugen <- function(
  year = 2024,
  path,
  file_name = sprintf("wind_gen_bmu_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/B1610/stream?"

      url <- sprintf(
        "%sfrom=%s&to=%s&bmUnit=",
        base_url,
        dt0_enc,
        dt1_enc
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          # browser() sf
          generation.bmu <- tryCatch(
            fromJSON(json_data) %>%
              mutate(across(
                matches("Time"),
                ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
              )),
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows()

      bmu_df
    }
  )

  bmugenyear <- extraction %>% bind_rows()

  bmugenyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(bmugenyear)
}

get_bod <- function(
  year = 2024,
  path,
  file_name = sprintf("bid_offer_data_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/BOD/stream?"

      url <- sprintf(
        "%sfrom=%s&to=%s&bmUnit=",
        base_url,
        dt0_enc,
        dt1_enc
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          generation.bmu <- tryCatch(
            fromJSON(json_data) %>%
              mutate(across(
                matches("Time"),
                ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
              )),
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows()

      bmu_df
    }
  )

  bmugenyear <- extraction %>% bind_rows()

  bmugenyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(bmugenyear)
}

get_boa <- function(
  year = 2024,
  path,
  file_name = sprintf("bid_offer_accept_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/BOALF/stream?"

      url <- sprintf(
        "%sfrom=%s&to=%s&bmUnit=",
        base_url,
        dt0_enc,
        dt1_enc
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          generation.bmu <- tryCatch(
            fromJSON(json_data) %>%
              mutate(across(
                matches("Time"),
                ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
              )),
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows()

      bmu_df
    }
  )

  bmugenyear <- extraction %>% bind_rows()

  bmugenyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(bmugenyear)
}

get_pn <- function(
  year = 2024,
  path,
  file_name = sprintf("phys_notif_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/datasets/PN/stream?"

      url <- sprintf(
        "%sfrom=%s&to=%s&bmUnit=",
        base_url,
        dt0_enc,
        dt1_enc
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          generation.bmu <- tryCatch(
            fromJSON(json_data) %>%
              mutate(across(
                matches("Time"),
                ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
              )),
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows()

      bmu_df
    }
  )

  bmugenyear <- extraction %>% bind_rows()

  bmugenyear %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(bmugenyear)
}

get_outturn <- function(
  year = 2024,
  path,
  file_name = sprintf("outturn_gen_type_%d.csv.gz", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "4 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "4 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq),

    \(i) {
      t0 <- as.character(time_seq[i])
      # t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      # dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      # dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/balancing/settlement/indicative/volumes/all/bid/"

      url <- sprintf(
        "%s%s?&bmUnit=",
        base_url,
        dt0_enc
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          generation.bmu <- tryCatch(
            fromJSON(json_data) %>%
              mutate(across(
                matches("Time"),
                ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
              )),
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows()
    }
  )

  df <- extraction %>% bind_rows()

  df %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(df)
}


get_curtailment <- function(
  year = 2024,
  path,
  file_name = sprintf("wind_curt_bmu_%d.csv.gz", year),
  end_time = NULL
) {
  # browser()
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "1 day"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "1 day"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  extraction <- lapply(
    seq_along(time_seq),

    \(i) {
      # if (i == length(time_seq)) {
      #   browser()
      # }
      t0 <- as.character(time_seq[i])
      # t1 <- as.character(time_seq[i + 1])
      # dt0 <- paste(t0, "01:00")
      # dt1 <- paste(t1, "01:00")
      # dt0_enc <- URLencode(dt0, reserved = TRUE)
      # dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/balancing/settlement/indicative/volumes/all/bid/"

      url <- sprintf(
        "%s%s?&bmUnit=",
        base_url,
        t0
      )

      fragments <- 3
      n_bmu <- nrow(wind.bmus)
      parts_vec <- seq(1, n_bmu, length.out = fragments + 1) %>% trunc()

      # list strings in fragments
      bmu_strings <- mapply(
        \(left, right) {
          bmu.string <- wind.bmus %>%
            slice(left:right) %>%
            pull(elexonBmUnit) %>%
            paste(., collapse = "&bmUnit=")

          # print(bmu.string)
        },
        left = parts_vec[1:fragments] + c(0, rep(1, fragments - 1)),
        right = parts_vec[-1]
      )

      # query data by fragment
      bmu_df <- lapply(
        bmu_strings,
        \(string) {
          # browser()
          url <- paste0(url, string)
          response <- GET(url, accept("text/plain"))
          json_data <- content(response, "text", encoding = "UTF-8")
          generation.bmu <- tryCatch(
            fromJSON(json_data)$data,
            error = function(e) {
              warning(sprintf(
                "Data extraction for day %s failed. Response: %s\n",
                t0,
                response$status_code
              ))
              return(NULL)
            }
          )
        }
      ) %>%
        bind_rows() %>%
        mutate(across(
          matches("Time"),
          ~ as.POSIXct(., format = "%Y-%m-%dT%H:%M:%OS")
        ))
    }
  )

  df <- extraction %>% bind_rows()

  df %>%
    write.csv(
      gzfile(file.path(path, file_name))
    )

  invisible(df)
}

get_remit <- function(
  year = 2024,
  path,
  file_name = sprintf("remit_all_%d.parquet", year),
  end_time = NULL
) {
  if (is.null(end_time)) {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(sprintf("%d-12-31", year)),
      by = "7 days"
    )
    time_seq <- c(time_seq, as.Date(sprintf("%d-01-01", year + 1)))
  } else {
    time_seq <- dates <- seq.Date(
      from = as.Date(sprintf("%d-01-01", year)),
      to = as.Date(end_time),
      by = "7 days"
    )
    if (time_seq[length(time_seq)] < end_time) time_seq <- c(time_seq, end_time)
  }

  # browser()
  extraction <- lapply(
    seq_along(time_seq[-1]),

    \(i) {
      t0 <- as.character(time_seq[i])
      t1 <- as.character(time_seq[i + 1])
      dt0 <- paste(t0, "01:00")
      dt1 <- paste(t1, "01:00")
      dt0_enc <- URLencode(dt0, reserved = TRUE)
      dt1_enc <- URLencode(dt1, reserved = TRUE)

      base_url <- "https://data.elexon.co.uk/bmrs/api/v1/remit/list/by-event/stream?"

      url <- sprintf(
        "%sfrom=%s&to=%s&latestRevisionOnly=true&profileOnly=false",
        base_url,
        dt0_enc,
        dt1_enc
      )
      # browser()
      response <- GET(url, accept("text/plain"))
      json_data <- content(response, "text", encoding = "UTF-8")
      msg_tbl <- fromJSON(json_data)

      msg_tbl <- tryCatch(
        msg_tbl %>%
          mutate(
            json = purrr::map(url, function(u) {
              resp <- request(u) |> req_perform()
              resp_body_json(resp, simplifyVector = TRUE)
            })
          ), #
        error = function(e) {
          warning(sprintf(
            "Data extraction for day %s failed. message: %s\n",
            t0,
            msg_tbl
          ))
          # browser()
          return(NULL)
        }
      )
      purrr::map(msg_tbl$json, "data") %>% bind_rows()
    }
  )

  df <- extraction %>% bind_rows()

  write_parquet(
    df,
    file.path(path, file_name)
  )
  invisible(df)
}

interp_log_ws <- function(h, z1, z2, u1, u2) {
  u1 + (log(h / z1) / log(z2 / z1)) * (u2 - u1)
}
