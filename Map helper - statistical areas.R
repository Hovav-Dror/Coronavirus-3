# Map helper - statistical areas
#   tibbles:
#   - StatisticalAreas: has the geometry of the statistical areas
#   - population_by_statistical_area_: has the population (and also town name in Hebrew)


StatisticalAreas <- sf::read_sf(dsn = "C:/Users/owner/OneDrive/R projects/Israel Statistical Area Maps/stat_2008_NEW_04Nov_1335.shp") %>%
  sf::st_transform('+proj=longlat +datum=WGS84')
StatisticalAreas <- StatisticalAreas %>% rename(town_code = SEMEL_YISH)

# Temp1 <- StatisticalAreas %>% pull(town_code)
# Temp2 <- StatisticalAreas %>% pull(STAT08)
# Temp3 <- StatisticalAreas %>% pull(STAT08)
# 
# tibble(town_code = StatisticalAreas %>% pull(town_code), 
#        STAT08 = StatisticalAreas %>% pull(STAT08),
#        Shem_Yis_1 = StatisticalAreas %>% pull(Shem_Yis_1  )
#        ) %>% group_by(town_code) %>% mutate(N = n_distinct(STAT08)) %>% filter(town_code == 2710)
#StatisticalAreas <- StatisticalAreas %>% group_by(town_code) %>% mutate(STAT08 = ifelse(n_distinct(STAT08) == 1, 1, STAT08)) %>% ungroup
names(StatisticalAreas)
StatisticalAreas


population_by_statistical_area_ <- read_excel("C:/Users/owner/OneDrive/R projects/Elections/COVID by election cluster/population_madaf_2018_3 - population by statistical area .xlsx", 
                                              sheet = "total population", col_names = FALSE, 
                                              skip = 15)

names(population_by_statistical_area_)[1:5] <- c("Town_shape", "town_code", "town_name", "STAT08", "Population")

population_by_statistical_area_ <- population_by_statistical_area_ %>%
  select(1:5) %>% 
  filter(!is.na(STAT08)) %>% 
  group_by(town_code, town_name) %>% 
  mutate(STAT08 = ifelse(n() == 1 & STAT08 == "סה\"כ",
                         "1",
                         STAT08)) %>% 
  mutate(S8 = as.numeric(STAT08)) %>% 
  mutate(S8b = case_when(
    !is.na(S8) ~ paste(S8),
    is.na(S8) & n() == 1 ~ "1",
    is.na(S8) ~ "NA",
    TRUE ~ "oops"
  )) %>% 
  filter(S8b != "NA") %>% 
  mutate(STAT08 = as.numeric(S8b)) %>% 
  ungroup %>% 
  select(town_code, town_name, STAT08, Population) %>% 
  mutate(Under2000 = (as.numeric(Population) < 2000)) %>% 
  filter(!is.na(as.numeric(Population)))

Streets_by_STAT08 <- read_excel("Streets by STAT08.xls", 
                                sheet = "Streets and Neighborhoods")

population_by_statistical_area_ <- left_join(
  population_by_statistical_area_,
  Streets_by_STAT08 %>% select(-town)
  )

#population_by_statistical_area_ <- population_by_statistical_area_ %>% group_by(town_code) %>% mutate(STAT08 = ifelse(n_distinct(STAT08) == 1, 1, STAT08)) %>% ungroup



# MissingTowns = anti_join(
#   geographic_summary_per_day %>% count(town_code, STAT08),  
#   StatisticalAreas %>% ungroup %>% select(town_code, STAT08) %>% as_tibble()
#   ) %>% pull(town_code)
# 
# StatisticalAreas %>% filter(town_code %in% MissingTowns) 
