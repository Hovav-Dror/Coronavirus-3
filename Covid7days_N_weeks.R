Covid7days_N_weeks <- function(CovidGeo, n = 0, population2000 = population_by_statistical_area_, StatisticalAreas = StatisticalAreas ) {
  # a function that calculate for each statistical area how many cases in the last 7 days ending n weeks ago


    CovidGeo <- CovidGeo %>% 
    select(town, town_code, STAT08, date, accumulated_cases) %>% 
    arrange(desc(date))
  
    LastDate <- CovidGeo %>% filter(!is.na(date)) %>% slice(1) %>% pull(date) - (n * 7)
    Month <- lubridate::month(LastDate, label = T, locale = "USA") %>% as.character()
    Day <- lubridate::day(LastDate) %>% as.character()
    LDay <- stringr::str_sub(Day,-1,-1) # last digit of the day
    Day <- case_when(
      nchar(Day) == 2 & as.numeric(Day) < 21 ~ paste0(Day, "th"),
      LDay == "1" ~ paste0(Day, "st"),
      LDay == "2" ~ paste0(Day, "nd"),
      LDay == "3" ~ paste0(Day, "rd"),
      TRUE ~ paste0(Day, "th")
    )

  CovidGeo <- CovidGeo %>% 
    filter(date <= LastDate, date > LastDate - 7) %>% # keep only the relevant week
    group_by(town, town_code, STAT08) %>% 
    arrange(desc(date)) %>% 
    mutate(Special = case_when(
      first(accumulated_cases) == "<15" ~ "<15",
      last(accumulated_cases) == "<15" ~ "First week above 15",
      TRUE ~ "CalculateValue"
    )) %>% 
    summarise(Special = first(Special), Accumulated7 = as.numeric(first(accumulated_cases)) - as.numeric(last(accumulated_cases))) %>% 
    group_by(town, town_code) %>% 
    #mutate(STAT08 = ifelse(is.na(STAT08) & n() == 1, MinTownSTAT08Code(town_code), STAT08)) %>%
    mutate(STAT08 = ifelse(is.na(STAT08) & n() == 1, 1, STAT08)) %>% 
    ungroup %>% 
    filter(!is.na(STAT08)) 
  
  # temp %>% select(Population, Accumulated7) %>% 
  #   mutate(Population = as.numeric(Population)) %>%  
  #   filter(Population > 2000) %>% 
  #   mutate(Accumulated8 = ifelse(is.na(Accumulated7), NA, Accumulated7 / Population * 10000)) %>% 
  #   pull(Accumulated8) %>% 
  #   quantile(na.rm = T)
  
  
  CovidGeo <- CovidGeo %>% select(-town) %>% 
    full_join(population2000 %>% rename(town = town_name)) %>% 
    mutate(
      Accu7Scaled = Accumulated7 / as.numeric(Population) * 10000,
      G_accumulated = case_when(
        Under2000 ~ "Population Under 2000",
        Special == "<15" ~ "<15",
        Special == "First week above 15" ~ "First week above 15",
        as.numeric(Accu7Scaled) == 0 ~ "0",
        as.numeric(Accu7Scaled) <= 3 ~ "1-3",
        as.numeric(Accu7Scaled) <= 12 ~ "4-12",
        as.numeric(Accu7Scaled) <= 30 ~ "13-30",
        as.numeric(Accu7Scaled) > 30 ~ ">30"
      ),
      
      G_Color = case_when(
        G_accumulated == "Population Under 2000" ~ "grey",
        G_accumulated == "<15"                   ~ "white",
        G_accumulated == "First week above 15"   ~ "yellow",
        G_accumulated == "0"                     ~ "white",
        G_accumulated == "1-3"                   ~ "yellow",
        G_accumulated == "4-12"                   ~ "orange",
        G_accumulated == "13-30"                 ~ "red",
        G_accumulated == ">30"                  ~ "black",
      )
    ) %>% 
    mutate(
      AccuScaleLabel = case_when(
        Under2000 ~ "",
        Special == "<15" ~ "",
        Special == "First week above 15" ~ "",
        TRUE ~ paste0("Cases per 10,000: ", round(Accu7Scaled, 1))),
      Accumulated7 = case_when(
        Under2000 ~ "Population Under 2000<br/>MHO does not report cases",
        Special == "<15" ~ "Healthy Area<br/><15 cases accumulated<br/>since March 2020",
        Special == "First week above 15" ~ "First week above 15<br/>(from March 2020)<br/>No data on exact cases this week",
        TRUE ~ paste0(Accumulated7)
      )) %>% 
    mutate(
      Label7 = sprintf(
        "New cases in the week<br/>ending %s: <strong>%s</strong><br/>%s<br/><br/>Town: %s Statistical area: %g<br/>Population: %s<br/>Neighborhoods: %s<br/>Streets: %s<br/><br/>Voting Characteristics: %s<br/>",
        paste0(Month, " ", Day),  Accumulated7, AccuScaleLabel, town, STAT08, comma(parse_number(Population), accuracy = 1), Neighborhoods, Streets, VoteCluster
      ),
      GName = paste0("Week ending ", Month, " ", Day)
    ) %>% 
    select(town_code, STAT08, Population, G_Color, Label7, GName)


  
  names(CovidGeo)[4:6] <- paste0(names(CovidGeo)[4:6], "_", Month, "_", Day)
  
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
