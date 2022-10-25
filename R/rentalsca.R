

#' Scrape rental unit information from Rentals.ca
#'
#' @param cities Character vector. Must match rentals.ca's internal naming.
#' Defaults to c("ottawa", "cumberland-on", "nepean", "gloucester", "kanata")
#' @param verbose Boolean. Controls console output.
#'
#' @return A `tbl_df` with rental unit information.
#' @export
rentalsca_scrape <- function(cities = c("ottawa", "cumberland-on", "nepean", "gloucester", "kanata"), verbose = TRUE){

  # for dplyr data masking
  location <- id <- name <- NULL

  #return(cities)
  # test pagination
  #cities <- c("ottawa", "cumberland-on", "nepean", "gloucester", "kanata")
  #cities <- c( "gloucester", "kanata")
  #cities <- "kanata"
  #cities <- c( "cumberland-on", "nepean", "gloucester", "kanata")
  results <- dplyr::tibble()

  for (city in cities){
    # start at page 1
    page <- 1
    last_page <- 100000

    # we do a while loop, since we get the actual number of pages later from the API
    while (page <= last_page) {
      if (verbose) message(paste(city, "page", page, "of",last_page))
      url <- sprintf("https://rentals.ca/phoenix/api/v1.0.2/listings?details=mid2&obj_path=%s&p=%s", city, page)

      num_tries <- 0
      while (num_tries < 5){
        # try to call GET API
        worked <- TRUE # assume it worked by default. this may be iffy!
        resp <- tryCatch(httr::GET(url), error = function(e) e)

        if ("error" %in% class(resp)){
          worked <- FALSE
          message("no API response")
        } else {

          if (!httr::status_code(resp) == 200) {
            worked <- FALSE
            message ("Status code not 200")
          } else {

            result_raw <-  tryCatch(jsonlite::fromJSON(httr::content(resp, encoding = "UTF-8", type = "text/json")),
                                    error = function(e) e)

            if ("error" %in% class(result_raw)){
              worked <- FALSE
              message("didn't get JSON response")
            }

          } # end else !status_code == 200
        } #end else error in class resp

        # if it worked, we're done
        if (worked) break
        message("didn't work, waiting 30 seconds and trying again")
        num_tries <- num_tries + 1
        Sys.sleep(30)
      } #end while num_tries < 5

      result_raw <- resp %>%
        httr::content(encoding = "UTF-8", type = "text/json") %>%
        jsonlite::fromJSON()

      # don't need to do this each time but we will anyway
      last_page <- result_raw$meta$pagination$last_page

      result_nested <- dplyr::as_tibble(result_raw$data$listings) %>%
        tidyr::unnest(location) %>%
        dplyr::select(-dplyr::any_of(c("photos", "is_user_promoted", "photo", "has_tour", "has_3D_tour", "photo_count", "featured_status",
                                       "format", "favourite", "raw_property_type", "x", "y", "owner", "rent_range", "baths_range", "beds_range",
                                       "dimensions_range", "view_on_map_url", "pet_friendly"))) %>%
        dplyr::rename(location_id = id,
                      location_name = name)

      result <- result_nested %>%
        tidyr::unnest(units)

      # error handing in case of e.g. mismatched vector types in this result
      results_bindtry <- tryCatch( dplyr::bind_rows(results, result),
                                   error = function(e) {
                                     cat("Error: can't bind rows: ", e,
                                       file = stderr() )
                                   })

      if (! "error" %in% class(results_bindtry)) results <- results_bindtry

      page <- page + 1
      if (verbose) message("Pausing respectfully...")
      Sys.sleep(5)

    } # end while page < last_page

  } # end for city in cities

  return(results)
}
