transform_metadata_to_df <- function(rawdata){
  rawdata[[1]] |>
    map(as_tibble) |>
    list_rbind() |> 
    mutate(latestData = map_chr(latestData, 1, .default=NA_character_)) |>
    mutate(latestData = as_datetime(latestData, tz = "UTC")) |>
    unnest_wider(location) |> 
    unnest_wider(latLon)
}

#Problem 4a
to_iso8601 <- function(dt, off){
  #Add offset to input datetime
  ndt <- dt + lubridate::days(off)
  
  #Format to ISO8601
  iso8601_dt <- format(ndt, format = "%Y-%m-%dT%H:%M:%SZ")

  return(iso8601_dt) 
}

#Problem 5
library(jsonlite)

transform_volumes <- function(json_data) {
  edges <- json_data$trafficData$volume$byHour$edges
  
  df <- purrr::map_dfr(edges, ~ data.frame(
    from = .x$node$from,
    to = .x$node$to,
    volume = .x$node$total$volumeNumbers$volume
  ))
  
  return(df)
}


