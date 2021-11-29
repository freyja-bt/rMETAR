library(dplyr)
library(tidyr)
library(xml2)
library(rvest)
library(lubridate)
library(vroom)
library(purrr)
library(stringr)
library(readr)

weather_abbrev <- c("BC", "BL", "DR", "FZ", "MI", "PR", "SH", "TS", "DZ", "GR", "GS", "IC", "PL", "RA", "SG", "SN", "UP", "BR", "DU", "FG", "FU", "HZ", "SA", "VA", "DS", "FC", "PO", "SQ", "SS")
cloud_abbrev <- c("CLR", "SKC", "NSC", "FEW", "SCT", "BKN", "OVC")

gather_metar <- function(stations, hours=48, path){

  current <<- vroom(
    path,
    col_types = cols(
      .default = col_character()
      )
    )

  cat("\n", path, "\n")

  cat("\n", stations,"\n")

  metar_data <- map(.x=stations,.y=hours, function(.x,.y){
    url = sprintf("https://www.aviationweather.gov/metar/data?ids=%s&format=raw&hours=%d&taf=off&layout=on", .x, .y)
    stdata <- read_html(url)%>%
      html_elements("code")%>%
      html_text()
    return(stdata)
    })%>%
    unlist

  cat("Results:",length(metar_data),"\n")

  ##


  metar_tib <<- map_df(metar_data,function(i){

    sep <- str_split(i," ")%>%
      unlist()%>%
      .[1:str_which(., "^RMK$")-1]

    tibble(
      station = str_sub(i, 1, 4),
      tempdew = str_extract(i, "M?[:digit:]+/M?[:digit:]+(?=\\s)"),
      pressure = str_extract(i, "(?<=A)[:digit:]{4}"),
      wind = str_extract(i, "[:alnum:]*(?=KT)"),
      vis = str_extract(i, "([:digit:]+\\s[:digit:]+/)?[:digit:]*(?=SM)"), # sep[str_which(sep, "[:digit:]*SM")]
      auto = case_when(
        str_detect(i,"AUTO")==F ~ 0,
        TRUE ~ 1
        ),
      datetime = str_extract(i,"[:digit:]{6}(?=Z)"),
      rmk = str_extract(i, "(?<=RMK)([:graph:]*[:blank:]*)*"),
      weather = paste(sep[str_which(sep, paste(weather_abbrev, collapse = "|"))], collapse = ";"),
      skycover = paste(sep[str_which(sep, paste(cloud_abbrev, collapse = "|"))], collapse = ";"),
      needfix = case_when(
        str_detect(i,"\\$")==T ~ 1,
        TRUE ~ 0
        )
      )%>%
    mutate(
      obsdate = case_when(
        # as.numeric(str_sub(datetime,1,2))>day(today("UTC"))
        as.numeric(str_sub(datetime,1,2))>day(today("UTC")) ~ paste0(year(today("UTC")-(day(today("UTC"))+1)), str_pad(month(today("UTC")-(day(today("UTC"))+1)),side = "left", pad = "0",width = 2), str_sub(datetime,1,2)),
        TRUE ~ paste0(year(today("UTC")), str_pad(month(today("UTC")),pad = "0",width = 2,side = "left"), str_sub(datetime,1,2))
        ),
      across(
        .fns = str_trim
      ),
      across(
        .fns=\(x) na_if(x,"")
        )
      )
    })

  cat("Rows:",nrow(metar_tib),"\n")


  lastid <- last(current$entryid)%>%
    as.numeric()

  new_data <- anti_join(metar_tib,current)%>%
    mutate(
      entryid = as.character(lastid + row_number())
      )%>%
    select(entryid, everything())

#
# write_csv(
#   x = new_data,
#   file = "data/metar_data.csv",
#   append = T
# )
return(new_data)

}


new_entries <- gather_metar(stations = c("KPDK","KATL"), path = "data/metar_runs.csv")

write_csv(new_entries, "data/metar_runs.csv", append = T, quote="needed")
