library(lubridate)
library(ggplot2)
library(purrr)
library(hrbrthemes)

dataset <- read.csv("south-korean-pollution-data.csv")
columns <- c("pm25", "pm10", "o3", "no2", "so2", "co", "Lat", "Long", "City", "District")
