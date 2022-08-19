



#' Scrape rental unit data from Kijiji for Ottawa, ON
#'
#' @param num_pages How many pages to scrape? Default 10.
#' @param old_urls Optional. List of old URLs already scraped so we don't scrape
#'                 them again.
#'
#' @return A `tbl_df` with rental unit data.
#' @export
#'
#' @examples \dontrun{apts <- kijiji_scrape(num_pages = 1)}
kijiji_scrape <- function(num_pages = 10, old_urls = NA){

  # find any new urls in the first num_pages (default 10) pages
  new_urls <- update_urls(old_urls, num_pages)

  message(sprintf("Found %d new unique urls.              \r", length(new_urls)), appendLF = FALSE)

  # scrape those new urls
  new_results <- scrape_urls(new_urls)

  # tidy them up a bit
  new_results <- new_results %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    dplyr::mutate(date_scraped = as.character(Sys.Date())) %>%
    dplyr::distinct()

  return(new_results)
}


# function to load most recent urls, get newest num_pages (10) pages, find urls that aren't
# already on the list, return them
update_urls <- function(latest_urls, num_pages = 10){

  # get the urls on the first 10 pages
  new_urls <- get_urls(start_page = 1, end_page = num_pages)

  # find the urls that aren't already in our list
  truly_new_urls <- new_urls[!new_urls %in% latest_urls]

  # return the new ones
  return(truly_new_urls)

}


scrape_urls <- function(all_urls){
  results <- dplyr::tibble()

  start_url <- 1
  # loop through urls, catch errors and retry
  for (i in start_url:length(all_urls)){
    url <- all_urls[[i]]

    # only try each url a few times in case it's just not working
    num_tries <- 0
    # basic error handling and retrying
    while(TRUE){
      message(sprintf("%d/%d: %s                                                                                        \r", i, length(all_urls), url), appendLF = FALSE)
      result <- try(scrape_apt(url))

      if (!"try_error" %in% class(result)) break
      # didn't work
      num_tries <- num_tries + 1
      if (try > 10) {
        message("Too many errrors, skipping this one.")
        result <- NA
        break
      }
      message("Error. Waiting 10 seconds and trying again...")
      Sys.sleep(10)
    }


    # make sure we got a tbl_df result before we try to bind rows
    valid_result <- ("tbl_df" %in% class(result))

    if (valid_result) results <- dplyr::bind_rows(results, result)

    Sys.sleep(0.5)
  }
  # in case we need to restart
  start_url <- i

  return(results)
}

## get urls

get_urls <- function(start_page = 1, end_page = 10){
  "https://www.kijiji.ca/b-apartments-condos/ottawa/page-2/c37l1700185?ad=offering"
  all_urls <- character(0)

  message(sprintf("Getting all urls from page %d to %d...", start_page, end_page))
  for (i in start_page:end_page){
    message(sprintf("Page %d...      \r", i), appendLF = FALSE)
    if (i == 1) kijiji_url <- "https://www.kijiji.ca/b-apartments-condos/ottawa/c37l1700185"

    if (i != 1) kijiji_url <- sprintf("https://www.kijiji.ca/b-apartments-condos/ottawa/page-%d/c37l1700185?ad=offering", i)


    k_html <- rvest::read_html(kijiji_url)

    these_urls <- k_html %>%
      rvest::html_elements(".title .title") %>%
      rvest::html_attr("href")

    these_urls <- paste0("https://www.kijiji.ca", these_urls)

    all_urls <- c(all_urls, these_urls)
    Sys.sleep(2)
  }

  all_urls <- unique(all_urls)

  #message(sprintf("Found %d unique urls.", length(all_urls)))

  return(all_urls)
}


scrape_apt <- function(url){

  # for clean R CMD CHECK with dplyr data masking
  orig <- NULL

  # we keep looping until we get valid results or 10 tries
  # it seems to always work eventually
  num_tries <- 0
  while (TRUE){
    num_tries <- num_tries + 1
    apt_html <- rvest::read_html(url)

    #apt_html
    does_it_exist <- rvest::html_elements(apt_html, "h1")


    apt_htmltext <- apt_html %>%
      rvest::html_elements('script[type="text/javascript"]') %>%
      rvest::html_text()

      apt_json <- dplyr::tibble(orig = apt_htmltext) %>%
      dplyr::filter(stringr::str_detect(orig, "window.__data")) %>%
      dplyr::mutate(orig = stringr::str_remove(orig, "window\\.__data\\="),
             orig = stringr::str_remove(orig, ";$")) %>%
      unlist()

    apt_data <- try(jsonlite::fromJSON(apt_json), silent = TRUE)

    if (!"try-error" %in% class(apt_data)) break

    # if we have tried too many times:
    if (num_tries > 10) {
      message("ERROR! Too many retries to get valid json. Returning empty result.")
      return(dplyr::tibble())
    }

    message ("ERROR! We didn't get good json! Waiting 10 seconds to try again...                                                          \r", appendLF = FALSE)
    Sys.sleep(10)

  }

  title <- purrr::pluck(apt_data, "config", "VIP", "title", .default = NA_character_)# apt_data$config$VIP$title
  description <- purrr::pluck(apt_data, "config", "VIP", "description", .default = NA_character_) # apt_data$config$VIP$description

  lots_info <- purrr::pluck(apt_data, "config", "VIP", "gptData", "gptTargetting", .default = NA_character_) #apt_data$config$VIP$gptData$gptTargetting
  price <- purrr::pluck(lots_info, "price", .default = NA_character_) #lots_info$price

  location <- purrr::pluck(apt_data, "config", "VIP", "adLocation", .default = NA_character_)# apt_data$config$VIP$adLocation
  address <- purrr::pluck(location, "mapAddress", .default = NA_character_)# location$mapAddress
  lat <- purrr::pluck(location, "latitude", .default = NA_character_)
  lon <- purrr::pluck(location, "longitude", .default = NA_character_)

  # extract other information
  more_info <- purrr::pluck(apt_data, "config", "VIP", "adAttributes", "localeSpecificValues", "en", .default = NA_character_)

  # if we get a good result, it's a long data frame. pivot it wider
  more_info_clean <- dplyr::tibble() # empty result

  if (!all(is.na(more_info))){
    more_info_clean <- tidyr::pivot_wider(more_info, names_from = "label", values_from = "value") %>%
      janitor::clean_names()
  }


  result <- dplyr::tibble(title, price, description, address, lat, lon, url)

  # if we got more info, add it
  if (nrow(more_info_clean) > 0) result <- dplyr::bind_cols(result, more_info_clean)

  return(result)
}
