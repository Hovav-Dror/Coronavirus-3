CovidAccumulated <- function(CovidGeo, population2000 = population_by_statistical_area_, StatisticalAreas = StatisticalAreas ) {
  # a function that calculate for each statistical area how many cases accumulated
  
  
  CovidGeo <- CovidGeo %>% 
    select(town, town_code, STAT08, date, accumulated_cases) %>% 
    arrange(desc(date))
  
  # LastDate <- CovidGeo %>% filter(!is.na(date)) %>% slice(1) %>% pull(date) - (n * 7)
  # Month <- lubridate::month(LastDate, label = T, locale = "USA") %>% as.character()
  # Day <- lubridate::day(LastDate) %>% as.character()
  # LDay <- stringr::str_sub(Day,-1,-1) # last digit of the day
  # Day <- case_when(
  #   nchar(Day) == 2 & as.numeric(Day) < 21 ~ paste0(Day, "th"),
  #   LDay == "1" ~ paste0(Day, "st"),
  #   LDay == "2" ~ paste0(Day, "nd"),
  #   LDay == "3" ~ paste0(Day, "rd"),
  #   TRUE ~ paste0(Day, "th")
  # )
  
  CovidGeo <- CovidGeo %>% 
    #filter(date <= LastDate, date > LastDate - 7) %>% # keep only the relevant week
    group_by(town, town_code, STAT08) %>% 
    arrange(desc(date)) %>% 
    slice(1) %>% 
    mutate(Special = case_when(
      first(accumulated_cases) == "<15" ~ "<15",
      #last(accumulated_cases) == "<15" ~ "First week above 15",
      TRUE ~ "CalculateValue"
    )) %>% 
    summarise(Special = first(Special), Accumulated_all = as.numeric(first(accumulated_cases))) %>% 
    group_by(town, town_code) %>% 
    mutate(STAT08 = ifelse(is.na(STAT08) & n() == 1, 1, STAT08)) %>% 
    ungroup %>% 
    filter(!is.na(STAT08)) 
  
  
  CovidGeo <- CovidGeo %>% select(-town) %>% 
    full_join(population2000 %>% rename(town = town_name)) %>% 
    mutate(
      G_accumulated = case_when(
        Under2000 ~ "Population Under 2000",
        Special == "<15" ~ "<15",
        #Special == "First week above 15" ~ "First week above 15",
        #as.numeric(Accumulated_all) == 0 ~ "0",
        as.numeric(Accumulated_all) <= 25 ~ "15-25",
        as.numeric(Accumulated_all) <= 50 ~ "26-50",
        as.numeric(Accumulated_all) <= 80 ~ "51-80",
        as.numeric(Accumulated_all) > 80 ~ ">80"
      ),
      G_Color = case_when(
        G_accumulated == "Population Under 2000" ~ "grey",
        G_accumulated == "<15"                   ~ "white",
        #G_accumulated == "First week above 15"   ~ "yellow",
        #G_accumulated == "0"                     ~ "white",
        G_accumulated == "15-25"                   ~ "yellow",
        G_accumulated == "26-50"                   ~ "orange",
        G_accumulated == "51-80"                 ~ "red",
        G_accumulated == ">80"                  ~ "black",
      )
    ) %>% 
    mutate(
      Accumulated_all = case_when(
        Under2000 ~ "Population Under 2000<br/>MHO does not report cases",
        Special == "<15" ~ "Healthy Area<br/><15 cases accumulated<br/>since March 2020",
        #Special == "First week above 15" ~ "First week above 15<br/>(from March 2020)<br/>No data on exact cases this week",
        TRUE ~ paste0(Accumulated_all)
      )) %>% 
    mutate(
      Label7 = sprintf(
        "Accumulated cases since March: <strong>%s</strong><br/><br/>Town: %s Statistical area: %g<br/>Population: %s<br/>Neighborhoods: %s<br/>Streets: %s<br/>",
        Accumulated_all, town, STAT08, comma(parse_number(Population), accuracy = 1), Neighborhoods, Streets
      ),
      GName = "Accumulated Cases"
    ) %>% 
    select(town_code, STAT08, Population, G_Color, Label7, GName)
  
  
  
  names(CovidGeo)[4:6] <- paste0(names(CovidGeo)[4:6], "_Accu")
  
  CovidGeo 
  
}

# CovidGeo <- geographic_summary_per_day
# rm(CovidGeo)
# rm(n)
# rm(LastDate)
# rm(population2000)
# rm(Month)
# rm(Day)
# rm(Lday)
