library(HovavLoadPackage2)
library(readxl)
library(leaflet)
source("Covid7days_N_weeks.R")


# Choose how many weeks ---------------------------------------------------

Weeks <- 6


# Load latest COVID data by area number -----------------------------------
# These MHO files contain: 
#   town_code, town, agas_code (אזור סטטיסטי), date (numbers for for every day), and for each (combination of date, town, agas_code):
#   accumulated tested, new_tested_on_date, accumulated cases, new_cases_on_date, accumulated_recoveries, newrecoveries_on_date,
#   new_hospitalizied_on_date, accumulated_deaths, new_deaths_on_date


LatestFile <- "geographic-summary-per-day-2020-09-16.xlsx"

{ # load data file 
  geographic_summary_per_day <- read_excel(LatestFile, 
                                         col_types = c("text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text", "text", "text", "text", "text", 
                                                       "text"))

geographic_summary_per_day <- geographic_summary_per_day %>% mutate(date = anytime::anydate(date)) # convert date from charachter to date
geographic_summary_per_day <- geographic_summary_per_day %>% mutate(town_code = as.numeric(town_code), agas_code = as.numeric(agas_code)) # easier comparisons between files
geographic_summary_per_day <- geographic_summary_per_day %>% filter(town_code != 0) # no town - not relevant
geographic_summary_per_day <- geographic_summary_per_day %>% rename(STAT08 = agas_code) # STAT08 / agas_code is the statistical area number (given a town code)
}

# names(geographic_summary_per_day)

# Load Statistical Areas --------------------------------------------------
# StatisticalAreas contains town_code, STAT08 code (אזור סטטיסטי), Shem_Yishuv, and geometry of the polygon
# As of this writing there are 3,061 statistical areas in the file
if (!exists("StatisticalAreas") | !exists("population_by_statistical_area_")) { 
  source("Map helper - statistical areas.R")
}


# Combine Weeks CovidGeos to a single tibble ------------------------------

CovidGeo <- Covid7days_N_weeks(CovidGeo = geographic_summary_per_day, n = 0, population2000 = population_by_statistical_area_)

if (Weeks > 1) {
  for (i in 1:(Weeks-1)) {
    CovidGeo = left_join(
      CovidGeo,
      Covid7days_N_weeks(CovidGeo = geographic_summary_per_day, n = i, population2000 = population_by_statistical_area_)
    )
  }
}



# Add Geographical Polygons -----------------------------------------------

DBleaflet <- StatisticalAreas %>% 
  left_join(CovidGeo) %>% 
  filter(!is.na(Shem_Yis_1)) %>% 
  mutate(across(contains("Color"), function(x) ifelse(is.na(x), "grey", x))) %>% 
  mutate(across(contains("Label7"), function(x) ifelse(is.na(x), paste0("Not in MOH file<br/>Town: ", Shem_Yis_1, " Statistical area: ", STAT08), x))) %>% 
  mutate(across(contains("GName"), function(x) ifelse(is.na(x), first(x[!is.na(x)]), x) ))


# Create the map ----------------------------------------------------------
{
Map <- DBleaflet %>% 
  leaflet() %>% 
  addTiles() %>% 
  fitBounds(lng1 = 34.747, lat1 = 32.032, lng2 = 34.842, lat2 = 32.133) 

Layers <- DBleaflet %>% select(contains("GName_")) %>% names()
Layers <- Layers[1:(length(Layers) - 1)] %>% gsub(pattern = "GName_", replacement = "") #%>% gsub(pattern = "_", replacement = " ")
Groups <- list()

for (Layer_i in Layers) {
  Color = (DBleaflet %>% select(contains("Color") & contains(Layer_i)) %>% names())[1]
  Label = (DBleaflet %>% select(contains("Label7") & contains(Layer_i)) %>% names())[1]
  GroupN = (DBleaflet %>% select(contains("GName") & contains(Layer_i)) %>% names())[1] 
  Groups <- c(Groups, (DBleaflet %>% pull(GroupN))[1]) %>% unlist
  Map <- Map %>% 
    addPolygons(fillColor = DBleaflet %>% pull(!! Color), fillOpacity = 0.5, opacity = 0.1,
                label = DBleaflet %>% pull(Label) %>% lapply(htmltools::HTML),
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "15px",
                  direction = "auto",
                ),
                group = (DBleaflet %>% pull(GroupN))[1]
    ) 
}

Map <- Map %>% 
  addLayersControl(
    baseGroups = Groups,
    #overlayGroups = c("Quakes", "Outline"),
    options = layersControlOptions(collapsed = FALSE),
    position = "bottomright"
  )

Map  
}
