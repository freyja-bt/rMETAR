
weather_abbrev <- c("BC", "BL", "DR", "FZ", "MI", "PR", "SH", "TS", "DZ", "GR", "GS", "IC", "PL", "RA", "SG", "SN", "UP", "BR", "DU", "FG", "FU", "HZ", "SA", "VA", "DS", "FC", "PO", "SQ", "SS")

cloud_abbrev <- c("CLR", "SKC", "NSC", "FEW", "SCT", "BKN", "OVC")

#' Read metar data
#'
#' @param stations  METAR stations
#' @param hours number of hours back, max =120
#' @param wa vector of weather abbreviations
#' @param ca vector of cloud-cover abbreviations
#'
#' @return
#' @export
#'
#' @examples
#' read_metar(stations="KATL")
read_metar <- function(stations, hours=48, wa=weather_abbrev, ca=cloud_abbrev){

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
  
  
  metar_tib <- purrr::map_df(metar_data,function(i){
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
      weather = paste(sep[stringr::str_which(sep, paste(wa, collapse = "|"))], collapse = ";"),
      skycover = paste(sep[stringr::str_which(sep, paste(ca, collapse = "|"))], collapse = ";"),
      needfix = dplyr::case_when(
        stringr::str_detect(i,"\\$")==T ~ 1,
        TRUE ~ 0
      )
    )%>%
      dplyr::mutate(
        obsdate = dplyr::case_when(
          # as.numeric(str_sub(datetime,1,2))>day(today("UTC"))
          as.numeric(stringr::str_sub(datetime,1,2))>lubridate::day(lubridate::today("UTC")) ~ paste0(lubridate::year(lubridate::today("UTC")-(lubridate::day(lubridate::today("UTC"))+1)), stringr::str_pad(lubridate::month(lubridate::today("UTC")-(lubridate::day(lubridate::today("UTC"))+1)),side = "left", pad = "0",width = 2), stringr::str_sub(datetime,1,2)),
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
  
  return(metar_tib)
}