# testing scraping realtor.ca for single family home rentals
#library(tidyverse)
#library(httr)

#' Scrape rental units from Realtor.ca
#'
#' @param max_pages Number of pages to capture.
#' @param lat_max Maximum latitude. Default values give Ottawa region.
#' @param lon_max Maximum longitude.
#' @param lat_min Minimum latitude.
#' @param lon_min Minimum longitude.
#'
#' @return A tbl_df containing unique rental units in the bounding box.
#' @export
realtor_scrape <- function(max_pages = 5,
                           lat_max=45.61892,
                           lon_max=-75.02541,
                           lat_min=44.87931,
                           lon_min=-76.57449){

  # for dplyr data masking
  name <- value <- NULL

  # raw text of powershell API query from google dev console
  powershell_text <- '$session = New-Object Microsoft.PowerShell.Commands.WebRequestSession
$session.UserAgent = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/102.0.0.0 Safari/537.36"
$session.Cookies.Add((New-Object System.Net.Cookie("visid_incap_2269415", "BsyA1JZ2QDqTDVjD4nv0MnvloGIAAAAAQUIPAAAAAAD0Yzr7d+PkG2zIVq17rc0w", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("gig_bootstrap_3_mrQiIl6ov44s2X3j6NGWVZ9SDDtplqV7WgdcyEpGYnYxl7ygDWPQHqQqtpSiUfko", "gigya-pr_ver4", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_gac_UA-12908513-11", "1.1656092541.CjwKCAjwwdWVBhA4EiwAjcYJEFKFFamKhzE7ZHDMTWnRMHGzJZEj3V0qIjrtQ10k5a9oHMoFuDNrVBoCK3kQAvD_BwE", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("ASP.NET_SessionId", "yzpdswtn51nmhk015ot2mbsm", "/", "api2.realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("visid_incap_2271082", "KnRmrOz7ScG+LwkkmqbfqZz3tWIAAAAAQUIPAAAAAAA5OrF8caN+aa26xf4c6SvE", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("nlbi_2271082", "yszlYdG55WMoTfJGNo2IPAAAAAAmZ6AKl0rFI0DFQ5v/l7ZF", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("nlbi_2269415", "fENLHIl03gB8p6bbn2FYxQAAAABq7kcAv7DU7SfqlPAVLhhy", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("incap_ses_530_2269415", "kC26I7kKqXnYtyRz5PBaB1UvyGIAAAAAo9FH9dRolMLPXr8fY1XnTw==", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_gid", "GA1.2.817476854.1657286488", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_dc_gtm_UA-12908513-11", "1", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("reese84", "3:c1+01YidsUYOi4Y4ZXloDg==:g+dBsXXhLPknIxPxCXTpz/Xl8LzAcmGAdIkk+14n2qcqmqeEjbrCRJI5lmuB/1rh3CseWWTfxJKw+RHMh/XQf+5LTNCBn5lyqGrxKOd/Hcrqi3busKdHFpJCmAEDUvoDycYUYRZLWZZzmkj8iZLx/EGpcYXq6uVD0XFKd+R3BeGsmW9FLC1WGe3mEFqIPmu0uJtqRK821PN3rHtbK4nIPqgaB86S+ts4DsFqDhQVaidUncj5Hsc63EeCoFulT0hfe7bfetPa9MTObP230JFOWp32WTlteiaZPLzzAZMJTbhsw4NBUaFB2xXq2WGQ4aXBq0gOdvOlOE5sS2sCtjloohHLJuEkPrBIJkVVumAWw/pFh5KBZrm/2UPkpGuzWVJ1HIDLtobkzG7ohoNikT8zINL2ci5WmpjMXni30J2uJrjNAfFzCD7Ert0EkRgV7dwQ:Qu5s5WbsXCzl4OssXptXImMize0avt0iQ1nelUdSdVA=", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_gali", "homeMoreFiltersSearchBtn", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("incap_ses_530_2271082", "+WMuOnyxtAzjxSRz5PBaB20vyGIAAAAAOvHozR2SH3n5Z7zetqdGqQ==", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_4c_", "%7B%22_4c_s_%22%3A%22fZJfi5wwFMW%2FypDnyZA%2FauK8lSmUhYWWQunjEM3VCTNjJGa028Hv3hvX3e12oSIaf7nnJN6TO5lO0JE9L3IldJFzVii5JWd4Gsj%2BToKz6TWSPREiV4xXDWVCCppJZmhlpKR5UYJlOTMcONmSX8mrEJlWhVJlzuYtqfvV405qbwG9eLnj2Y4z2gwoib8RUZExHPfB21sdj%2FGpT4UTVJvBnnHCwuhqOE7OxtPiINgbPYFrTzFhphfch%2FSBo8l11k%2F%2Fylb6KiulQloFPw2QlIdT8FfYcCYQe%2BwE%2Bbko0m4DNBDCUoZfg4tpnwHMJfqwq83KsIFvmC740XTtzbSw7uzRty3YzQM2nzTmMgCyb8GPrqtTydcumuA8woO%2FdTEku4PpjE1W32FwFrrozMWHg79eIbjaXP5y6sd1mYvHiWSIKW%2FxkXgfLI6%2FfDr%2BePicyqTGpFL8OzwGmeIcoyPzmmWuJE7pUgqGUUVcRBcZS9f8vMwSLX9frYUqPlZbGM7R9%2FSlLdD9R68%2B6kf3chgBdFNBaSlj2tCsyUpaaqVoAyArVbGy4a%2BHMf2ZxlsysVpy%2Few4z38A%22%7D", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("nlbi_2269415_2147483392", "Kw4+VtHomnx9D/lqn2FYxQAAAAAUn/9rrUnsNLwyByWBYuzj", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_ga_Y07J3B53QP", "GS1.1.1657286487.4.1.1657286511.36", "/", ".realtor.ca")))
$session.Cookies.Add((New-Object System.Net.Cookie("_ga", "GA1.1.1387957286.1654711677", "/", ".realtor.ca")))
Invoke-WebRequest -UseBasicParsing -Uri "https://api2.realtor.ca/Listing.svc/PropertySearch_Post" `
-Method "POST" `
-WebSession $session `
-Headers @{
"authority"="api2.realtor.ca"
  "method"="POST"
  "path"="/Listing.svc/PropertySearch_Post"
  "scheme"="https"
  "accept"="*/*"
  "accept-encoding"="gzip, deflate, br"
  "accept-language"="en-CA,en-GB;q=0.9,en-US;q=0.8,en;q=0.7"
  "origin"="https://www.realtor.ca"
  "referer"="https://www.realtor.ca/"
  "sec-ch-ua"="`" Not A;Brand`";v=`"99`", `"Chromium`";v=`"102`", `"Google Chrome`";v=`"102`""
  "sec-ch-ua-mobile"="?0"
  "sec-ch-ua-platform"="`"Windows`""
  "sec-fetch-dest"="empty"
  "sec-fetch-mode"="cors"
  "sec-fetch-site"="same-site"
} `
-ContentType "application/x-www-form-urlencoded; charset=UTF-8" `
-Body "ZoomLevel=10&LatitudeMax=45.61892&LongitudeMax=-75.02541&LatitudeMin=44.87931&LongitudeMin=-76.57449&Sort=6-D&PropertyTypeGroupID=1&PropertySearchTypeId=1&TransactionTypeId=3&Currency=CAD&RecordsPerPage=200&ApplicationId=1&CultureId=1&Version=7.0&CurrentPage=1"'



  powershell_headers <- powershell_text %>%
    stringr::str_extract(stringr::regex("(?<=Headers @\\{).*?(?=\\})", dotall = TRUE))

  powershell_headers_tib <- powershell_headers %>%
    stringr::str_split("\n") %>%
    unlist()

  powershell_headers_tib <- dplyr::tibble(raw = powershell_headers_tib) %>%
    tidyr::separate(col = "raw", into =c( "name", "value"), sep = "=", extra = "merge") %>%
    dplyr::mutate(name = stringr::str_remove_all(name,   '"'),
           value = stringr::str_remove_all(value, '^"|"$'),
           name = stringr::str_squish(name),
           value = stringr::str_squish(value)
    ) %>%
    tidyr::drop_na()

  powershell_headers_vec <- powershell_headers_tib$value
  names(powershell_headers_vec) <- powershell_headers_tib$name

  # cookie taken from chrome dev console, no idea how long it will be valid
  cookietest <- "visid_incap_2269415=BsyA1JZ2QDqTDVjD4nv0MnvloGIAAAAAQUIPAAAAAAD0Yzr7d+PkG2zIVq17rc0w; gig_bootstrap_3_mrQiIl6ov44s2X3j6NGWVZ9SDDtplqV7WgdcyEpGYnYxl7ygDWPQHqQqtpSiUfko=gigya-pr_ver4; _gac_UA-12908513-11=1.1656092541.CjwKCAjwwdWVBhA4EiwAjcYJEFKFFamKhzE7ZHDMTWnRMHGzJZEj3V0qIjrtQ10k5a9oHMoFuDNrVBoCK3kQAvD_BwE; ASP.NET_SessionId=yzpdswtn51nmhk015ot2mbsm; visid_incap_2271082=KnRmrOz7ScG+LwkkmqbfqZz3tWIAAAAAQUIPAAAAAAA5OrF8caN+aa26xf4c6SvE; nlbi_2271082=yszlYdG55WMoTfJGNo2IPAAAAAAmZ6AKl0rFI0DFQ5v/l7ZF; nlbi_2269415=fENLHIl03gB8p6bbn2FYxQAAAABq7kcAv7DU7SfqlPAVLhhy; _gid=GA1.2.817476854.1657286488; incap_ses_530_2269415=EZPCfY44rWDMqy9z5PBaB6JAyGIAAAAAAoVcERb3UEbU0xn2JjmRbg==; reese84=3:Feafg5Es24T8eeZR5zHIfQ==:p958WOzlxiHCYDRR8VhhLAfhrTwDW/Wzc9hi1I+xk4eDJ228KYU6LJngNImr4f3wr7xCEAl+txiWS+tIkUDNYnlFzJCjQRWfvG3rlBWBfT5zf9rt8pf1ZjjxG1T8BLyihbL5hvdYkJC65OyH/Ry7F2KDeBEB9dOsdgpHcRLRyRvUbZSuG3n76MrDML7XlODH1drluscKZV9Miz7gvi5RaaYAqQ03Sq7eiJFioYHbz2omd3DpJzFW3CY1f6+L7tPUSKF9ed7F5TN24fzyKU2whJbnxbAOfQBfkeYPw0jBcdtc3ONtIAVnWycVXO1EppI6KqZMmlhGv90N1LcvvgXFExVUWgxEQIfPAAPF2BT5MDMswrIv3ODPBghj50GQy6pF8JtaF1jbB0JhaEgIrYDc3B8ATBtL5P5ttm2BNBHiG28oG8sWtjwt80AwrdcJDHwz:QKAJQyJXaNldIYJCISAk3x2UclyItu02Us3PLfs8skE=; _dc_gtm_UA-12908513-11=1; incap_ses_530_2271082=kN9vNs/CRSRv7DNz5PBaB9JGyGIAAAAAsW+h3/dGvcxUDvv+GbLrrw==; nlbi_2269415_2147483392=i+LsJcQhGFrwWF1+n2FYxQAAAABmW/L4+NuS7c/2/FGF1KEt; _4c_=%7B%22_4c_s_%22%3A%22jVRRa%2BM4EP4rxcfmKXIlWbKsQB6WFEogu112exzcS1GsSSKaWD5ZibdX%2Bt9vZJxkb%2Fc46oAjffPNp%2FHMaF6zfgdNNmOlVFxzSZXUdJo9w0uXzV6z4Gz6O2WzjHOpKFtvCOUFJ6KghqxNURBZarBUUsOAZdPs%2B6Clq0pIKmWlp1mM%2B2zGpeZ0eN6mWd2Ooq9Z7S2gONM5EzmjZNOhRvwbIcIFxXUbvD3W8Sm%2BtInYw%2Fqms89osHByNTz1zsbdoMDpFd2B2%2B5igmk1wG1IG1z1rrG%2B%2F9ltRC9uulCIroPvO0iei13wB7hhlCPsMTXZH4NHijbABkIYaLjrXExxBjD76ENemxHDjF5hMsAr02yPZgtjZCu%2F3YK9WWI1so3Zd4DYl%2BBPrqkT5aGJJjiP4MIfmxiS3MI0xiapr9A5C010Zu%2FDwh8OEFxt9sOJF0vKZqqkwMXeoznJYvGn%2BEp4Gyyu7z8%2B%2Fb68SzEVldLYFVWZY0WFYqxUKSvHkHR3Mbbd7Pa27%2Fv8%2BrG3B9P%2B9qf3hxWcYD9ndLLAsyHMhcyxuQrOP%2FAFUTJXWmtJJysTXTxa%2BGS%2BJ0rJKs0nK99sLyhRIleMFz9wXTMXIsfgCvYDF1GiyryqVEkn33yI85LcTb7cg1%2Fabr4t6NOGCwbxLzlB7LM5wPwhRtMbjOgDpw%2BfJ5jsFkJ8ecRWuw%2F%2B2C7v5uyCfgMT6l2yLS3Cj8E0namj882I8cniiH3Q1C%2Fzxce7VHYYzNeyI%2FYYHFY5fIK483i1cG%2BsS6yhPDaRLWzMcR%2FTNrVivTdd52oL3XP0bfZ2vmKqUJWmXJZyvGJVKc4XrD2NF0z8m804K35lj9Lk3JzQ%2FI%2B%2F%2BNX%2F5M4zApMv1xswpGTKEsEYEG2tIKoCAwWwqtTqMiNw3uAUkVKzUZJV14g2Z0nBtalrXRJlNpygYk00tSVOIcFNzdJQKbKfohRa%2FcdXNmfJa4LfM6xcG0fHy4wU%2BHun8%2BXMd5329vYP%22%7D; _ga=GA1.2.1387957286.1654711677; _ga_Y07J3B53QP=GS1.1.1657292494.5.1.1657292521.33; _gali=mapMoreFiltersSearchBtn"
  names(cookietest) <- "cookie"

  powershell_headers_vec <- c(powershell_headers_vec, cookietest)

  powershell_body <- "ZoomLevel=10&LatitudeMax=45.61892&LongitudeMax=-75.02541&LatitudeMin=44.87931&LongitudeMin=-76.57449&Sort=6-D&PropertyTypeGroupID=1&PropertySearchTypeId=1&TransactionTypeId=3&Currency=CAD&RecordsPerPage=200&ApplicationId=1&CultureId=1&Version=7.0&CurrentPage="

  # set up for loop

  results <- list()
  #max_pages <- 5
  for (i in 1:max_pages){

    message(sprintf("Getting page %s of results..", i))
    body_post <- paste0(powershell_body, i)

    body_post <-sprintf("ZoomLevel=10&LatitudeMax=%s&LongitudeMax=%s&LatitudeMin=%s&LongitudeMin=%s&Sort=6-D&PropertyTypeGroupID=1&PropertySearchTypeId=1&TransactionTypeId=3&Currency=CAD&RecordsPerPage=200&ApplicationId=1&CultureId=1&Version=7.0&CurrentPage=%s",
                        lat_max,
                        lon_max,
                        lat_min,
                        lon_min,
                        i)

    resp <- httr::POST(
      url = "https://api2.realtor.ca/Listing.svc/PropertySearch_Post",
      httr::add_headers(powershell_headers_vec),
      body = body_post,
      encode = "raw",
      httr::content_type_json(),
      httr::verbose()
    )

    result <- httr::content(resp)

    if (result$ErrorCode$Id != 200) {
      message("API error:")
      print(result$ErrorCode)
      Sys.sleep(5)
      next
    }

    message("Success, adding results")

    results[[i]] <- result$Results

    if (i < max_pages){
      message("Waiting for next page...")
      Sys.sleep(5)
    }
  }

  # function to parse results into tidy data frame
  flatten_data <- function(x) {
    x$Individual <- NULL
    x$Property$Photo <- NULL
    x$Media <- NULL
    x$Property$Parking <- NULL

    x %>%
      unlist() %>%
      as.list() %>%
      dplyr::as_tibble() %>%
      janitor::clean_names()
  }


  message("Tidying results...")

  data_tidy <- results %>%
    unlist(recursive = FALSE) %>%
    purrr::map_dfr(function(x) {
      #message(x$Id)
      flatten_data(x)})

  # make sure they're all unique
  data_tidy <- data_tidy %>%
    dplyr::distinct()

  return (data_tidy)
}
