## code to prepare `MigrationDistancesLit` dataset goes here
library(tidyverse)

MigrationDistancesLit <- read_csv("data-raw/MigrationDistancesLit.csv")

usethis::use_data(MigrationDistancesLit, overwrite = TRUE)
