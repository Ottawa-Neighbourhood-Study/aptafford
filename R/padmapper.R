

#' Scrape apartments from PadMapper within Ottawa
#'
#' @param old_data Optional tbl_df previous collected with this function. New
#'                 results will be added, minus duplicates.
#' @param polite_pause Numeric value in seconds to pause beween API calls. Default is 5.
#' @param verbose Boolean. Would you like lots of updates to the console? Default is TRUE.
#' @param lat_min Numeric. Minimum latitude for bounding box. Default 45.6.
#' @param lat_max Numeric. Maximum latitude for bounding box. Default 44.9.
#' @param lng_min Numeric. Minimum longitude for bounding box. Default -76.4.
#' @param lng_max Numeric. Maximum longitude for bounding box. Default -75.4.
#'
#' @return A tbl_df with apartment data scraped from PadMapper.
#' @export
#'
#' @examples
#' \dontrun{
#' apartments <- padmapper_scrape()
#' }
padmapper_scrape <- function(old_data = NA,
                             lat_min = 44.9,
                             lat_max = 45.6,
                             lng_min = -76.4,
                             lng_max = -75.4,
                             polite_pause = 5, verbose = TRUE) {

  # set these to NULL for dplyr data masking
  formatted_address <- min_bathrooms <- min_bedrooms <- min_price <- min_square_feet <- count <- NULL

  # NOTE: Removing for now the save-to-file feature, to keep function simple
  #       save results after you get them!
  # #' @param filepath Optional character filepath to save results. Should be just directory name.
  # #'                 If set to NA, will not save results automatically. Defaults to "data".
  # filepath = "data",
  filepath <- NA


  # note: this should be handled with devtools::use_pipe() when it's a real package
  `%>%` <- magrittr::`%>%`

  # seems to first get bundle with xz_token and csrf
  if (verbose) message("Getting session token...")
  response <- httr::GET(url = "https://www.padmapper.com/api/t/1/bundle")

  bundle <- response %>%
    httr::content()

  ########## GET MAP-BASED LIST OF APARTMENTS
  # POST to pins API to get up to 10k results in the Ottawa region

  pins_url <- "https://www.padmapper.com/api/t/1/pins"

  # query body to send to API, including a lat/lon bounding box that is slightly
  # larger than the bounding box of onsr::ons_shp

  pins_body <- list(
    external = "true",
    limit = 10000,
    longTerm = "false",
    maxLat = lat_max,
    maxLng = lng_max,
    minLat = lat_min,
    minLng = lng_min,
    minPrice = 0,
    shortTerm = "false",
    transits = ''
  )

  # Send request to API
  if (verbose) message("Getting listings in Ottawa region...")
  pins_resp <- httr::POST(url = pins_url,
                          body = pins_body,
                          encode = "json",
                          httr::add_headers("X-CSRFToken" = bundle$csrf,
                                            "X-Zumper-XZ-Token" = bundle$xz_token))

  # if HTTP response status is not 200 (success), send error message and return API response.
  if (!httr::status_code(pins_resp) == 200) {
    message(sprintf("Error: pins API returned status code %s. Dumping API response for debugging.", httr::status_code(pins_resp)))
    return(pins_resp)
  }

  # Parse response
  apts <- httr::content(pins_resp)

  # this has both single units and buildings with many units
  biglist_tidy <- apts %>%
    purrr::map(function(x) {x$image_ids = paste0(x$image_ids, collapse = ", ")
    x}) %>%
    purrr::map(function(x) {
      purrr::map(x, function(y) {
        # message(y)
        if (is.null(y)) result <- NA
        if (!is.null(y)) result <- y
        return(result)
        #dplyr::if_else(is.null(y), NA_integer_, y)
      })

    }) %>%
    purrr::map_dfr(dplyr::as_tibble)

  # if we received nothing, return an empty tibble
  if (nrow(biglist_tidy) == 0) return (dplyr::tibble())

  # Handle single units
  single_units <- dplyr::filter(biglist_tidy, count == 1) %>%
    dplyr::rename(bedrooms = min_bedrooms,
                  bathrooms = min_bathrooms,
                  price = min_price,
                  square_feet = min_square_feet) %>%
    dplyr::select(-dplyr::contains("max"))

  # handle multi-units, which tell you how many units and give a range of costs.
  # we want to know the costs for each unit
  multi_units <- dplyr::filter(biglist_tidy, count > 1)

  # each building has a unique pb_id, but it can show up in more than one listing
  # e.g. if it's a complex with two street addresses but treated as one legal unit
  pb_ids <- unique(multi_units$pb_id)

  # # function to extract data for unit, not used
  # extract_unit_data <- function(y) {
  #   result <- y
  #   if (is.null(y)) result <- NA
  #   if (length(y) > 1) result <- paste0(y, collapse = ", ")
  #   if (length(y) == 0) result <- NA
  #   result
  # }

  # loop through multi-unit buildings
  if (verbose) message("Getting details for multi-unit listings...")
  results <- dplyr::tibble()

  for (i in 1:length(pb_ids)){
    Sys.sleep(polite_pause)

    if (verbose) message(sprintf("\r  Building %s/%s, ~%s mins %s seconds remaining       ", i, length(pb_ids), ((length(pb_ids)-i)*5) %/% 60, ((length(pb_ids)-i)*5) %% 60), appendLF = FALSE)  #message(sprintf("  Building %s/%s", i, length(pb_ids)))

    pb_url <- paste0("https://www.padmapper.com/api/t/1/pages/buildings/p", pb_ids[[i]])

    pb_building <- httr::GET(url = pb_url,
                             encode = "json",
                             httr::add_headers("X-CSRFToken" = bundle$csrf,
                                               "X-Zumper-XZ-Token" = bundle$xz_token))

    content_building <- httr::content(pb_building)


    # if there are no floorplans present, skip to next item in the list.
    # this could happen if e.g. the prior data list is stale and the units have rented
    if (length(content_building$floorplan_listings) == 0) next

    fortidying <- content_building$floorplan_listings

    floorplans_tidy <- fortidying %>%
      purrr::map(function(x) {
        x <- append(x, x$listing_location)
        x$listing_location <- NULL
        x$media <- NULL
        x$neighborhood <- NULL
        x$amenity_groups <- NULL
        x$prices <- NULL
        x$features <- NULL

        purrr::map(x, function(y) {
          result <- y
          if (is.null(y)) result <- NA
          if (length(y) > 1) result <- paste0(y, collapse = ", ")
          if (length(y) == 0) result <- NA
          result
        })

      }) %>%
      purrr::map_dfr(dplyr::as_tibble)

    result <- floorplans_tidy %>%
      dplyr::select(dplyr::any_of(names(single_units)), "formatted_address", -"address") %>%
      dplyr::rename(address = formatted_address)

    results <- dplyr::bind_rows(results, result)
  }

  if (verbose) message("Done checking multi-unit buildings.")

  # remove any duplicates that slipped through
  results <- dplyr::distinct(results)

  apartments <- dplyr::bind_rows(single_units, results) %>%
    dplyr::distinct() %>%
    dplyr::mutate(date_scraped = as.character(Sys.Date()))

  # if we have old data, put it together and clean up duplicates
  # we're comparing all columns EXCEPT the date the data was collected!
  if (!all(is.na(old_data))){
    if (verbose) message("Combining new data with old data.")

    # ensure that the date_scraped column, if present, is in character format
    if ("date_scraped" %in% names(old_data)) old_data$date_scraped <- as.character(old_data$date_scraped)

    # wrap this in try() so that if it fails we will still return the old data
    try(apartments <- dplyr::bind_rows(old_data, apartments))

    apartments <- dplyr::distinct(apartments, dplyr::across(!dplyr::contains("date_scraped")), .keep_all = TRUE)
  }

  if (!is.na(filepath)){
    if (verbose) message("Writing to file.")
    try(readr::write_csv(apartments, sprintf("%s/padmapper-%s.csv", filepath, Sys.Date())))
  }

  return(apartments)
}
