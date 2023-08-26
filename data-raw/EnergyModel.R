## code to prepare `SpeedSeries` dataset goes here
library(tidyverse)

EnergyModel <- read_csv("data-raw/EnergyModel.csv")

usethis::use_data(EnergyModel, overwrite = TRUE)
