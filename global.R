library(lubridate)
library(ggplot2)
library(purrr)
library(dplyr)
library(hrbrthemes)
library(RColorBrewer)

dataset <- read.csv("south-korean-pollution-data.csv")
columns <- c("pm25", "pm10", "o3", "no2", "so2", "co", "Lat", "Long", "City", "District", "Country")
cities <- c(
  "Bangsan-Myeon", "Cheongnim-Dong", "Gwanak-Gu", "Hwasun-Eup", "Hyeoksin-Dong",
  "Nowon-Gu", "Onui-Dong", "Pado-Ri", "Paju", "Saemangeum",
  "Saesol-Dong", "Sangjusi", "Seolseong-Myeon", "Seonggeoeup", "Soi-Myeon",
  "Taeha-Ri"
)

dados_filtrados <- dataset %>%
  filter(Country %in% c(" South Korea"))

dataset <- dados_filtrados
