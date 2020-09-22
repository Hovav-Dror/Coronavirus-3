CovidAccumulated <- function(CovidGeo, population2000 = population_by_statistical_area_, StatisticalAreas = StatisticalAreas ) {
  # a function that calculate for each statistical area how many cases accumulated
  
  
  CovidGeo <- CovidGeo %>% 
    select(town, town_code, STAT08, date, accumulated_cases) %>% 
    arrange(desc(date))
  
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

   # temp <- CovidGeo %>% select(-town) %>%
   #   full_join(population2000 %>% rename(town = town_name))
   #   temp %>% select(Population, Accumulated_all) %>%
   #     mutate(Population = as.numeric(Population)) %>%
   #     filter(Population > 2000) %>%
   #     mutate(Accumulated8 = ifelse(is.na(Accumulated_all), NA, Accumulated_all / Population * 10000)) %>%
   #     pull(Accumulated8) %>%
   #     quantile(na.rm = T, probs = (0:20)/20)

  
  CovidGeo <- CovidGeo %>% select(-town) %>% 
    full_join(population2000 %>% rename(town = town_name)) %>% 
    mutate(
      Accu7Scaled = Accumulated_all / as.numeric(Population) * 10000,
      G_accumulated = case_when(
        Under2000 ~ "Population Under 2000",
        Special == "<15" ~ "<15",
        Special == "First week above 15" ~ "First week above 15",
        as.numeric(Accu7Scaled) == 0 ~ "0",
        as.numeric(Accu7Scaled) <= 65 ~ "1-65", # 25% at Aug30th
        as.numeric(Accu7Scaled) <= 135 ~ "66-135", # 75% at Aug30th
        as.numeric(Accu7Scaled) <= 370 ~ "136-370", # 95% at Aug30th
        as.numeric(Accu7Scaled) > 370 ~ ">370"
      ),
      G_Color = case_when(
        G_accumulated == "Population Under 2000" ~ "grey",
        G_accumulated == "<15"                   ~ "white",
        #G_accumulated == "First week above 15"   ~ "yellow",
        #G_accumulated == "0"                     ~ "white",
        G_accumulated == "1-65"                   ~ "yellow",
        G_accumulated == "66-135"                   ~ "orange",
        G_accumulated == "136-370"                 ~ "red",
        G_accumulated == ">370"                  ~ "black",
      )
    ) %>% 
    mutate(
      AccuScaleLabel = case_when(
        Under2000 ~ "",
        Special == "<15" ~ "",
        Special == "First week above 15" ~ "",
        TRUE ~ paste0("Cases per 10,000: ", round(Accu7Scaled, 1))),
      Accumulated_all = case_when(
        Under2000 ~ "Population Under 2000<br/>MHO does not report cases",
        Special == "<15" ~ "Healthy Area<br/><15 cases accumulated<br/>since March 2020",
        #Special == "First week above 15" ~ "First week above 15<br/>(from March 2020)<br/>No data on exact cases this week",
        TRUE ~ paste0(Accumulated_all)
      )) %>% 
    mutate(
      Label7 = sprintf(
        "Accumulated cases since March: <strong>%s</strong><br/>%s<br/><br/>Town: %s Statistical area: %g<br/>Population: %s<br/>Neighborhoods: %s<br/>Streets: %s<br/><br/>Voting Characteristics: %s<br/>",
        Accumulated_all, AccuScaleLabel, town, STAT08, comma(parse_number(Population), accuracy = 1), Neighborhoods, Streets, VoteCluster
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
