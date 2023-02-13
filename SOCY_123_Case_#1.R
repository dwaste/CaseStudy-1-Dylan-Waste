library(tidyverse)
library(readxl)
library(rnaturalearth)
library(leaflet)
library(sf)

vac.dat <- read_excel("/Users/dwaste/Desktop/India_Vaccine_Policy_Data.xlsx")

pop.dat <- read_excel("/Users/dwaste/Downloads/country_data_2021.xlsx")

world <- ne_countries(scale = "medium",
                      returnclass = "sf")

world <- world %>%
  mutate(Country = recode(name, "Dominican Rep." = "Dominican Republic",
                          "Côte d'Ivoire" = "Ivory Coast", 
                          "Saint Lucia" = "St. Lucia",
                          "St. Kitts and Nevis" = "St. Kitts & Nevis", 
                          "St. Vin. and Gren." = "St. Vincent & Grenadines",
                          "Antigua and Barb." = "Antigua & Barbuda",
                          "Dem. Rep. Congo" = "DR Congo", 
                          "Swaziland" = "Eswatini", 
                          "Solomon Is." = "Solomon Islands",
                          "Trinidad and Tobago"  = "Trinidad & Tobago", 
                          "Morocco"  = "Morocoo",
                          "United Arab Emirates"  = "UAE",
                          "São Tomé and Principe"  = "Sao Tome & Principe",
                          "United Kingdom"  = "UK",
                          "Sierra Leone"  = "Seirra Leone",
                          "Lao PDR"  = "Laos",
                          "Guinea-Bissau"  = "Guinea Bissau",
                          "United States"  = "USA"))

full.dat <- right_join(world, vac.dat, by = "Country")

pop.dat <- pop.dat %>%
  mutate(Country = recode(Country.Name, "Antigua and Barbuda" = "Antigua & Barbuda",
                          "Bahamas, The" = "Bahamas", 
                          "Côte d'Ivoire" = "Ivory Coast",
                          "Congo, Dem. Rep." = "DR Congo", 
                          "Cabo Verde" = "Cape Verde",
                          "Egypt, Arab Rep." = "Egypt",
                          "Gambia, The" = "Gambia", 
                          "Iran, Islamic Rep." = "Iran", 
                          "Syrian Arab Republic"  = "Syria", 
                          "Trinidad and Tobago"  = "Trinidad & Tobago",
                          "St. Vincent & Grenadines"  = "St. Vincent & Grenadines",
                          "Yemen, Rep."  = "Yemen",
                          "Morocco"  = "Morocoo",
                          "United Arab Emirates"  = "UAE",
                          "Sao Tome and Principe"  = "Sao Tome & Principe",
                          "United Kingdom"  = "UK",
                          "Sierra Leone"  = "Seirra Leone",
                          "Lao PDR"  = "Laos",
                          "Guinea-Bissau"  = "Guinea Bissau",
                          "United States"  = "USA"))

fin.dat <- left_join(full.dat, pop.dat, by = "Country")

fin.dat <- fin.dat %>%
  mutate(G.Rate = ((G.Quantity * 100000)/Nat.Pop.2021 * 1000)) %>%
  mutate(C.Rate = ((C.Quantity * 100000)/Nat.Pop.2021 * 1000)) %>%
  mutate(COVAX.Rate = ((COVAX.Quantity * 100000)/Nat.Pop.2021 * 1000)) %>%
  mutate(log.G = (log(G.Quantity + 1))) %>%
  mutate(log.C = (log(C.Quantity + 1))) %>%
  mutate(log.COVAX = (log(COVAX.Quantity + 1)))

palGrant <- colorNumeric(c("#ffffff00", "#088811"), domain = fin.dat$G.Quantity, 1:10)

palCom <- colorNumeric(c("#ffffff00", "#0c0fb7"), domain = fin.dat$C.Quantity, 1:10)

palCOVAX <- colorNumeric(c("#ffffff00", "#760f0d"), domain = fin.dat$COVAX.Quantity, 1:10)
  
log.palGrant <- colorNumeric(c("#ffffff00", "#088811"), domain = fin.dat$log.G, 1:10)

log.palCom <- colorNumeric(c("#ffffff00", "#0c0fb7"), domain = fin.dat$log.C, 1:10)

log.palCOVAX <- colorNumeric(c("#ffffff00", "#760f0d"), domain = fin.dat$log.COVAX, 1:10)

int.vac.plot <- leaflet(fin.dat) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(group = "Maitri Doses", fill = ~G.Quantity, fillColor = ~palGrant(G.Quantity), weight = 4, 
              opacity = 0, label = ~name,
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$G.Rate))) %>%
  addPolygons(group = "Commerical Doses", fill = ~C.Quantity, fillColor = ~palCom(C.Quantity), weight = 4,
              opacity = 0, label = ~name,
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$C.Rate))) %>%
  addPolygons(group = "COVAX Doses", fill = ~COVAX.Quantity, fillColor = ~palCOVAX(COVAX.Quantity), weight = 4,
              opacity = 0, label = ~name, 
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$COVAX.Rate))) %>%
  addLegend("topleft", title = paste0("<center><i>Maitri</i> <br>Doses <br>(lakhs)"), pal = palGrant, values = ~G.Quantity, opacity = 1, group = "Maitri Doses", na.label = "None") %>%
  addLegend("topright", title = paste0("<center>Comm. <br>Doses <br>(lakhs)"), pal = palCom, values = ~C.Quantity, opacity = 1, group = "Commerical Doses", na.label = "None") %>%
  addLegend("bottomleft", title = paste0("<center>COVAX <br>Doses <br>(lakhs)"), pal = palCOVAX, values = ~COVAX.Quantity, opacity = 1, group = "COVAX Doses", na.label = "None") %>%
  addLayersControl(overlayGroups = c("Maitri Doses", "Commerical Doses", "COVAX Doses"), 
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
  hideGroup(c("Maitri Doses", "Commerical Doses", "COVAX Doses"))

log.int.vac.plot <- leaflet(fin.dat) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(group = "Maitri Doses", fill = ~log.G, fillColor = ~log.palGrant(log.G), weight = 4, 
              opacity = 0, label = ~name,
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$G.Rate))) %>%
  addPolygons(group = "Commerical Doses", fill = ~log.C, fillColor = ~log.palCom(log.C), weight = 4,
              opacity = 0, label = ~name,
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$C.Rate))) %>%
  addPolygons(group = "COVAX Doses", fill = ~log.COVAX, fillColor = ~log.palCOVAX(log.COVAX), weight = 4,
              opacity = 0, label = ~name, 
              popup = paste0("<center><b>", fin.dat$Country, "</b><br>Population: ", fin.dat$Nat.Pop.2021, "<br><i>Dose Rate per 1,000: ", round(fin.dat$COVAX.Rate))) %>%
  addLegend("topleft", title = paste0("<center><i>Maitri</i> <br>Doses"), pal = log.palGrant, values = ~log.G, opacity = 1, group = "Maitri Doses", na.label = "None") %>%
  addLegend("topright", title = paste0("<center>Comm. <br>Doses"), pal = log.palCom, values = ~log.C, opacity = 1, group = "Commerical Doses", na.label = "None") %>%
  addLegend("bottomleft", title = paste0("<center>COVAX <br>Doses"), pal = log.palCOVAX, values = ~log.COVAX, opacity = 1, group = "COVAX Doses", na.label = "None") %>%
  addLayersControl(overlayGroups = c("Maitri Doses", "Commerical Doses", "COVAX Doses"), 
                   options = layersControlOptions(collapsed = TRUE, autoZIndex = FALSE)) %>%
  hideGroup(c("Maitri Doses", "Commerical Doses", "COVAX Doses"))

log.int.vac.plot

int.vac.plot




  


