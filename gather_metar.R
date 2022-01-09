
new_entries <- read_metar(stations = c("KPDK","KATL"), hours=120)%>%
  recent_metar(path = "data/metar_runs.csv")

readr::write_csv(new_entries, "data/metar_runs.csv", append = T)
