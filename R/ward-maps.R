library(dplyr)
library(stringr)
library(BSol.mapR)

Brum_wards <- Ward@data %>%
  filter(Area == "Birmingham") %>%
  pull(Ward)

data <- read.csv("data/data_ward.csv") %>%
  filter(
    AggregationLabel %in% Brum_wards
  ) %>%
  rename(
    Ward = AggregationLabel
  )

indicators <- unique(data$IndicatorName)

# Loop over all indicators
for (indicator_i in indicators) {
  
  # Filter data for indicator_i
  data_i <- data %>%
    filter(
      IndicatorName == indicator_i,
      IMD == "NULL"
    ) %>%
    # Separate filter to get max year of *filtered* data
    filter(
      Year == max(Year)
    )

  # Create title and save name strings
  title_i <- paste0(
    str_to_title(
      strsplit(data_i$IndicatorName[1], ": ")[[1]][2]
    ),
    " (", data_i$Year[[1]], ")"
  )
  
  save_name_i <- paste0(
    "output/",
    str_replace_all(
      strsplit(data_i$IndicatorName[1], ": ")[[1]][2]
      , " ", "_"),
    ".png"
  )
  
  # Plot map
  map_i <- plot_map(
    data_i,
    value_header = "Sum.of.IndicatorValue",
    map_type = "Ward",
    area_name = "Birmingham",
    map_title = title_i
  )
  
  # Save map
  save_map(map_i, save_name_i)
}



