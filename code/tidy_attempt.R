library(readxl)
library(lubridate)
library(tidyr)
library(magrittr)
library(tidyverse)
library(readr)
library(dplyr)

input_file <- "data/Hawthorne Tilikum Steel daily bike counts 073118.xlsx"
bridge_name <- "Hawthorne"

# define a funtion that load bike counts data
load_data <- function(input_file, bridge_name) {
  bikecounts <- read_excel(input_file,
                           sheet = bridge_name,
                           skip = 1)
  bikecounts$name <- bridge_name
  bikecounts
}

Tilikum <- load_data(input_file, "Tilikum")
Hawthorne <- load_data(input_file, "Hawthorne")

# use the column names of Tilikum for Hawthorne
names(Hawthorne) <- names(Tilikum)

Steel <- load_data(input_file, "Steel")
names(Steel) <- c("date", "lower", "westbound", "eastbound", "total", "name")

# combine all three data frame for all three bridges
bikecounts <- bind_rows(Hawthorne, 
                        Tilikum, 
                        Steel %>% select(-lower)) # exclude the `lower` col in Steel data frame

tibble("Steel", "Hawthorne", "Tilikum")
tibble("Steel")

gather_("Hawthorne")

tibble_("bikecounts")

two_bridges <- Hawthorne %>% 
  left_join(Tilikum, by="name")

library(lubridate)

wday(two_bridges, abbr = TRUE)

gather(bikecounts, 
       `westbound`:`eastbound`, 
       key='direction', value="counts")

bikecounts <- bikecounts %>% mutate(dow=wday(date, label=TRUE))

spread(bikecounts, key="dow", value="total")
bikecounts <- bikecounts <- bikecounts %>% mutate(dow=wday(date, label=TRUE))

spread(bikecounts, key="dow", value="total")

bikecounts_dow <- bikecounts %>% group_by(name, dow) %>% 
  summarize(Sun:Sat, mean(total))         
