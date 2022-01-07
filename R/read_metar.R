#' Read metar data
#' @param stations METAR stations
#' @param hours number of hours back, max =120
#' @param path filepath of output csv
#' @export 

weather_abbrev <- c("BC", "BL", "DR", "FZ", "MI", "PR", "SH", "TS", "DZ", "GR", "GS", "IC", "PL", "RA", "SG", "SN", "UP", "BR", "DU", "FG", "FU", "HZ", "SA", "VA", "DS", "FC", "PO", "SQ", "SS")
cloud_abbrev <- c("CLR", "SKC", "NSC", "FEW", "SCT", "BKN", "OVC")

read_metar <- function(stations, hours=48, path, weather_abbrev=weather_abbrev, cloud_abbrev=cloud_abbrev){
  
  if(exists(path)==F){
    cat("Nonexistant path. Creating new csv\n")
    }

  cat("\n", path, "\n")
  
  cat("\n", stations,"\n")
  
  metar_data <- purrr::map(.x=stations,.y=hours, function(.x,.y){
    url = sprintf("https://www.aviationweather.gov/metar/data?ids=%s&format=raw&hours=%d&taf=off&layout=on", .x, .y)
    stdata <- rvest::read_html(url)%>%
      rvest::html_elements("code")%>%
      rvest::html_text()
    return(stdata)
  })%>%
    unlist
  
  cat("Results:",length(metar_data),"\n")
  
  ##
  
  
  metar_tib <<- purrr::map_df(metar_data,function(i){
    # cat(i, "\n")
    # cat(str_which(metar_data, str_sub(i,1,12)),"\n")
    
    sep <- stringr::str_split(i," ")%>%
      unlist()#%>%
    # .[1:str_which(., "^RMK$")-1]
    
    dplyr::tibble(
      station = stringr::str_sub(i, 1, 4),
      tempdew = stringr::str_extract(i, "M?[:digit:]+/M?[:digit:]+(?=\\s)"),
      pressure = stringr::str_extract(i, "(?<=A)[:digit:]{4}"),
      wind = stringr::str_extract(i, "[:alnum:]*(?=KT)"),
      vis = stringr::str_extract(i, "([:digit:]+\\s[:digit:]+/)?[:digit:]*(?=SM)"), # sep[str_which(sep, "[:digit:]*SM")]
      auto = dplyr::case_when(
        stringr::str_detect(i,"AUTO")==F ~ 0,
        TRUE ~ 1
      ),
      datetime = stringr::str_extract(i,"[:digit:]{6}(?=Z)"),
      rmk = stringr::str_extract(i, "(?<=RMK)([:graph:]*[:blank:]*)*"),
      weather = paste(sep[stringr::str_which(sep, paste(weather_abbrev, collapse = "|"))], collapse = ";"),
      skycover = paste(sep[stringr::str_which(sep, paste(cloud_abbrev, collapse = "|"))], collapse = ";"),
      needfix = case_when(
        stringr::str_detect(i,"\\$")==T ~ 1,
        TRUE ~ 0
      )
    )%>%
      mutate(
        obsdate = case_when(
          # as.numeric(str_sub(datetime,1,2))>day(today("UTC"))
          as.numeric(stringr::str_sub(datetime,1,2))>lubridate::day(lubridate::today("UTC")) ~ paste0(lubridate::year(lubridate::today("UTC")-(day(today("UTC"))+1)), stringr::str_pad(lubridate::month(lubridate::today("UTC")-(lubridate::day(lubridate::today("UTC"))+1)),side = "left", pad = "0",width = 2), stringr::str_sub(datetime,1,2)),
          TRUE ~ paste0(lubridate::year(lubridate::today("UTC")), stringr::str_pad(lubridate::month(lubridate::today("UTC")),pad = "0",width = 2,side = "left"), stringr::str_sub(datetime,1,2))
        ),
        dplyr::across(
          .fns = stringr::str_trim
        ),
        dplyr::across(
          .fns=function(x) dplyr::na_if(x,"")
        )
      )
  })
  
  cat("Rows:",nrow(metar_tib),"\n")
  
  
  lastid <- max(current$entryid)%>%
    as.numeric()
  
  if(exists(path)==F){
    metar_tib%>%
      dplyr::arrange(obsdate, datetime)%>%
      dplyr::mutate(
        entryid = as.character(dplyr::row_number())
      )%>%
      dplyr::select(entryid, dplyr::everything())%>%
      readr::write_csv(file=path)
  }
  
  current <<- vroom::vroom(
    path,
    col_types = vroom::cols(
      entryid = vroom::col_integer(),
      .default = vroom::col_character()
    )
  )
  
  lastid <- max(current$entryid)%>%
    as.numeric()
  
  new_data <- dplyr::anti_join(metar_tib,current)%>%
    dplyr::arrange(obsdate, datetime)%>%
    dplyr::mutate(
      entryid = as.character(lastid + dplyr::row_number())
    )%>%
    dplyr::select(entryid, dplyr::everything())
  
  #
  # write_csv(
  #   x = new_data,
  #   file = "data/metar_data.csv",
  #   append = T
  # )
  return(new_data)
  
}