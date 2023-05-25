new_entries <- rMETAR::read_metar(stations = c("KPDK","KATL"), hours=120)%>%
  rMETAR::recent_metar(path = "raw_data/metar_runs.csv")

readr::write_csv(new_entries, "raw_data/metar_runs.csv", append = T)
