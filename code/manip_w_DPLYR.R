filter(gapminder, lifeExp < 29)
filter(gapminder, country == "Rwanda", year > 1979)
filter(gapminder, country %in% c("Rwanda", "Afghanistan"))
select(gapminder, year, lifeExp)
gapminder %>%
  filter(country == "Cambodia") %>%
  select(year, lifeExp)
gapminder %>%
  filter(country == "United States") %>%
  select(year, lifeExp)
gapminder %>%
  mutate(gdp = pop * gdpPercap)
gapminder %>%
  mutate(pop * gdpPercap)
gapminder %>%
  mutate(gdp = pop * gdpPercap)
gapminder %>%
  mutate(gdp = pop * gdpPercap, ln_gdp=log(gdp))

ctib <- gapminder %>%
  filter(country == "Canada") %>% 
  select(year, BasegdpPercap=gdpPercap)
ctib

my_gap <- gapminder %>%
  inner_join(ctib, by="year") %>% 
  mutate(gdpPercapRel = gdpPercap / BasegdpPercap,
         BasegdpPercap = NULL)
my_gap

my_gap %>%
  rename(life_exp = lifeExp,
         gdp_percap = gdpPercap,
         gdp_percap_rel = gdpPercapRel)
my_gap %>%
  rename(life_exp = lifeExp,
         gdp_percap = gdpPercap,
         gdp_percap_rel = gdpPercapRel)
my_gap %>%
  group_by(continent) %>%
  summarize(n = n())
my_gap %>%
  group_by(continent, year) %>%
  summarize(n = n())

my_gap %>%
  summarize(n = n())


gapminder%>%
  tally()

gapminder %>%
  group_by(continent) %>%
  summarize(n_country=n_distinct(country))

my_gap %>%
  filter(continent == "Asia") %>%
  group_by(year) %>%
  summarize(min_lifeExp = min(lifeExp), max_lifeExp = max(lifeExp))

filter(bikecounts, total == 0) %>%
  count("total")

read_csv("data/NCDC-CDO-USC00356750.csv")

filter(`historical weather`, is.na(TOBS)) %>%
  tally()

"dd" <- read_csv("data/NHTS2009_dd.csv")
dd %>% mutate(hh_race_str=recode(HH_RACE, 
                                 "01"="White",
                                 "02"="African American, Black",
                                 "03"="Asian Only",
                                 "04"="American Indian, Alaskan Native",
                                 "05"="Native Hawaiian, other Pacific",
                                 "06"="Multiracial",
                                 "07"="Hispanic/Mexican",
                                 "97"="Other specify",
                                 .default = as.character(NA) # any unspecified values would be assgined NA
)) %>% 
  select(HH_RACE, hh_race_str)

dd %>% mutate(driving=ifelse(TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07"), 1, 0),
              driving=ifelse(TRPTRANS %in% c("-1", "-7", "-8", "-9"), NA, driving) # retain missing values as NA
) %>% 
  select(TRPTRANS, driving)

dd %>% mutate(driving = case_when(
  TRPTRANS %in% c("01", "02", "03", "04", "05", "06", "07") ~ 1,
  TRPTRANS %in% c("-1", "-7", "-8", "-9") ~ as.double(NA),
  # retain missing values as NA
  TRUE ~ 0
)) %>%
  select(TRPTRANS, driving)

dd <- dd %>% mutate(income_cat=case_when(
  HHFAMINC %in% c("01", "02", "03", "04", "05", "06") ~ "low income",
  HHFAMINC %in% c("07", "08", "09", "10", "11", "12") ~ "med income",
  HHFAMINC %in% c("13", "14", "15", "16", "17", "18") ~ "high income",
  TRUE ~ as.character(NA) # retain missing values as NA
))

dd %>% group_by(HHFAMINC, income_cat) %>% 
  tally()

bikecounts %>% 
  filter(is.na(westbound)| is.na(eastbound)) %>% 
  group_by(date) %>% 
  tally

bikecounts %>% 
  mutate(year=year(date)) %>% 
  group_by(year) %>% 
  summarize(annual_total <- sum(total))

bikecounts %>% 
  group_by(name) %>% 
  top_n(3, total)

bikecounts %>% 
  mutate(DATE-as_date(date)) %>% 
  left_join(weather)
  
