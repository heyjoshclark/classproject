library(gapminder)
library(tidyverse)
library(broom)

canada_fit <-
  lm(lifeExp ~ year, data = gapminder, subset = country == "Canada")

canada_fit

oceania_ttest <- gapminder %>%
  filter(continent == "Asia") %>% 
  t.test(gdpPercap ~ country, data = .)

gapminder %>%
  group_by(continent) %>%
  summarize(range = range(lifeExp))

(gap_nested <- gapminder %>% 
    group_by(continent, country) %>% 
    nest())
(fit <- lm(lifeExp ~ I(year - 1950), data = gap_nested[[1, "data"]]))

le_vs_yr <- function(df) {
  lm(lifeExp ~ I(year - 1950), data = df)
}
le_vs_yr(gap_nested[[1, "data"]])

fits <- purrr::map(gap_nested$data[1:2], le_vs_yr)

(gap_nested <- gap_nested %>% 
    mutate(fit = purrr::map(data, le_vs_yr)))

tidy(gap_nested$fit[[1]])

(gap_nested <- gap_nested %>% 
    mutate(tidy = purrr::map(fit, tidy)))

(gap_coefs <- gap_nested %>% 
    select(continent, country, tidy) %>% 
    unnest(tidy))

bikecounts_weather %>%
  group_by(name) %>%
  summarize_at(funs(min, max), name)

(bw_lcdf <- bikecounts_weather %>% 
    group_by(name) %>% 
    nest())

lm(total ~ TMIN + TMAX, bikecounts_weather) %>%
  summary()

(bw_lcdf <- bikecounts_weather %>% 
    group_by(name) %>% 
    nest())

library(purrr)
bw_lcdf %>%
    mutate(fit-map(data, ~lm(total ~ TMIN + TMAX + PRCP, data-.)),
           tidy-map(fit, tidy),
           glance=map
            )


  