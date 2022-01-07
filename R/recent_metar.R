#' Isolate recent data
#'
#' @param x previously imported metar data
#' @param path filepath of existing csv
#' 
#' @export
recent_metar <- function(x, path){
  
  cat("\n", path, "\n")
  
  current <- vroom::vroom(
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
  
  return(new_data)
  
}