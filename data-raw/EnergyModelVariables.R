## code to prepare `EnergyModelVariables` dataset goes here
library(tidyverse)

EnergyModelVariables <- read_csv("data-raw/EnergyModelVariables.csv")

usethis::use_data(EnergyModelVariables, overwrite = TRUE)
