

library(tidyverse)
library(sf)

# BC Boundaries
bound <- bcmaps::bc_bound_hres()%>%
  st_as_sf(crs = 4326)

# WSC Basins and clip to BC (remove Alberta and territory basins)
wsc_basins <- bcmaps::wsc_drainages()%>%
  filter(MAJOR_DRAINAGE_AREA_CD != "05",
         !SUB_SUB_DRAINAGE_AREA_CD %in% c("07AA","07AB",
                                          "10ED","10FA")) %>%
  st_as_sf(crs = 4326) %>%
  st_intersection(bound) %>%
  st_transform(crs = 4326)

# View the map
mapview::mapview(wsc_basins, zcol = "SUB_SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "SUB_DRAINAGE_AREA_CD")
mapview::mapview(wsc_basins, zcol = "MAJOR_DRAINAGE_AREA_CD")


# Group subbasins to create major and sub-basin groups
trending_basins <- wsc_basins  %>%
  mutate(Major_Basin = case_when(SUB_DRAINAGE_AREA_CD == "08N" ~ "Columbia",
                                 SUB_SUB_DRAINAGE_AREA_CD %in% c("08MH") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("08M","08L","08K","08J") ~ "Fraser",
                                 SUB_DRAINAGE_AREA_CD %in% c("08H","08G","08F","08O") ~ "Coastal",
                                 SUB_DRAINAGE_AREA_CD %in% c("07F","07E") ~ "Peace",
                                 SUB_DRAINAGE_AREA_CD %in% c("10A","10B","10C","10D","10E","10F") ~ "Liard",
                                 SUB_DRAINAGE_AREA_CD %in% c("08A","08B","08C","08D","08E","09A") ~ "Northwest"),
         Sub_Basin = case_when(SUB_SUB_DRAINAGE_AREA_CD == "08NL" ~ "Similkameen",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NM" ~ "Okanagan",
                               SUB_SUB_DRAINAGE_AREA_CD == "08NN" ~ "Kettle",
                               #SUB_SUB_DRAINAGE_AREA_CD %in% c("08NL","08NM","08NN") ~ "Similkameen-Okanagan-Kettle",
                               SUB_DRAINAGE_AREA_CD == "08H" ~ "Vancouver Island",
                               SUB_DRAINAGE_AREA_CD == "08O" ~ "Haida Gwaii",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GD","08GC","08GB","08GA","08MH") ~ "South Coast",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08GF","08GE") ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08F" ~ "Central and North Coast",
                               SUB_DRAINAGE_AREA_CD == "08D" ~ "Nass",
                               SUB_DRAINAGE_AREA_CD == "08J" ~ "Nechako",
                               SUB_DRAINAGE_AREA_CD == "08E" ~ "Skeena",
                               SUB_DRAINAGE_AREA_CD == "08C" ~ "Stikine",
                               SUB_DRAINAGE_AREA_CD == "09A" ~ "Yukon",
                               SUB_DRAINAGE_AREA_CD == "08L" ~ "Thompson",
                               SUB_DRAINAGE_AREA_CD == "07F" ~ "Upper Peace",
                               SUB_DRAINAGE_AREA_CD == "07E" ~ "Williston Lake",
                               SUB_DRAINAGE_AREA_CD == "10C" ~ "Fort Nelson",
                               SUB_DRAINAGE_AREA_CD %in% c("10A","10B") ~ "Upper and Central Liard",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08KA","08KB","08KD","08KC","08KE","08KG","08KF") ~ "Upper Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MD","08ME","08MG","08MF","08MC") ~ "Lower Fraser",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NC","08NB","08NA","08ND","08NE") ~ "Columbia",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08MB","08MA") ~ "Chilcotin",
                               SUB_SUB_DRAINAGE_AREA_CD == "08KH" ~ "Quesnel",
                               SUB_SUB_DRAINAGE_AREA_CD %in% c("08NG","08NF","08NK","08NP",
                                                               "08NH", "08NJ") ~ "Kootenay"))
# make simple table for viewing data
trending_basins_table <- trending_basins %>%
  st_drop_geometry()

# view the basin groupings by all basins
mapview::mapview(trending_basins, zcol = "Major_Basin")
mapview::mapview(trending_basins, zcol = "Sub_Basin")

# Merge major basins and view features
major_basins <- trending_basins %>%
  group_by(Major_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()
mapview::mapview(list(major_basins), zcol = c("Major_Basin"))

# Merge sub basins and view features
sub_basins <- trending_basins %>%
  group_by(Sub_Basin) %>%
  summarize(geometry = sf::st_union(geometry)) %>%
  ungroup()
mapview::mapview(list(sub_basins), zcol = c("Sub_Basin"))

### save the shapefiles?

###





# Import stations with clusters and regimes and view the data
loc <-"C:/Users/jgoetz/OneDrive - Government of BC/Projects/Streamflow Trending Indicator/SOE 2024/Trending Cluster Recommendations.xlsx"
stns <- readxl::read_xlsx(loc, sheet = "station_cluster") %>%
  left_join(tidyhydat::hy_stations()) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_intersection(trending_basins %>% select(Major_Basin, Sub_Basin))
mapview::mapview(list(stns), zcol = c("clustGroup"))
mapview::mapview(list(stns), zcol = c("Regime"))
mapview::mapview(list(stns), zcol = c("Major_Basin"))
mapview::mapview(list(stns), zcol = c("Sub_Basin"))

# View the stations with their major and sub basins
mapview::mapview(list(major_basins,stns), zcol = c("Major_Basin","Major_Basin"))
mapview::mapview(list(major_basins,stns), zcol = c("Major_Basin","Regime"))
mapview::mapview(list(sub_basins,stns), zcol = c("Sub_Basin","Major_Basin"))
mapview::mapview(list(sub_basins,stns), zcol = c("Sub_Basin","Regime"))






