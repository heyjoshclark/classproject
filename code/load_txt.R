library(tidyverse)
library(readr)
library(haven)

read_dta("data/iris.dta")

read_table("data/NCDC-CDO-USC00356750.txt", comment = "-")

cols_df <- read_table("data/NCDC-CDO-USC00356750.txt")
(data_df <- read_table("data/NCDC-CDO-USC00356750.txt", comment = "-", skip=2, col_names = FALSE))
names(data_df) <- names(cols_df)

weather <- read_csv("data/NCDC-CDO-USC00356750.csv")

