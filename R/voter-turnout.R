# Voter turnout
library(BSol.mapR)
library(dplyr)
library(readxl)

data <- read_excel("data/Birmingham LE 2022 Ward Turnout .xlsx")

brum_wards <- Ward@data %>%
  filter(Area == "Birmingham") %>%
  pull(Ward)

all(data$Ward %in% brum_wards)
all(brum_wards %in% data$Ward)

map <- plot_map(
  data,
  area_name = "Birmingham",
  map_type = "Ward",
  value_header = "Turnout (%)",
  map_title = "2020 Local Election Voter Turnout (%)",
  style = "cont"
)

save_map(
  map,
  "output/voter-turnout-2022.png"
)