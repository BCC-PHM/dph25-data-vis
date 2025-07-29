  library(dplyr)
  library(stringr)
  library(ggplot2)
  
  fill_col <- "#79ABE1"
  
  data <- read.csv("data/data_Bham.csv") %>%
    mutate(
      Ethnicity = str_replace(
        Ethnicity, " ", "\n"
      )
    )
  
  # Get data for ethnicity plotting
  eth_data <- data %>%
    filter(
      IMD == "NULL",
      Ethnicity != "NULL"
    )
  
  # Get data for IMD plotting
  imd_data <- data %>%
    filter(
      IMD != "NULL",
      Ethnicity == "NULL"
    )
  
  eth_indicators <- unique(eth_data$IndicatorName)
  imd_indicators <- unique(imd_data$IndicatorName)
  
  
  # Loop over all indicators
  for (indicator_i in eth_indicators) {
    
    # Get ethnicity data
    eth_data_i <- eth_data %>%
      filter(
        IndicatorName == indicator_i
      ) %>%
      # Separate filter to get max year of *filtered* data
      filter(
        Year == max(Year)
      )
    # Create title and save name strings
    eth_title_i <- paste0(
      str_to_title(
        strsplit(indicator_i, ": ")[[1]][2]
      ),
      " (", eth_data_i$Year[[1]], ")"
    )
  
    eth_save_name_i <- paste0(
      "output/ethnicity-plots/",
      str_replace_all(
        strsplit(indicator_i, ": ")[[1]][2], " ", "_"),
      "_ETH.png"
    )
  
    # Plot ethnicity data
    eth_plot_i <- ggplot(
      eth_data_i,
      aes(
        x = Ethnicity,
        y = Sum.of.IndicatorValue
      )
    ) +
      geom_col(fill = fill_col) +
      theme_bw() +
      geom_errorbar(
        aes(x = Ethnicity,
            ymin = Sum.of.LowerCI95,
            ymax = Sum.of.UpperCI95),
        width = 0.5
      ) +
      scale_y_continuous(
        limits = c(0, 1.1*max(eth_data_i$Sum.of.UpperCI95)),
        expand = c(0,0)
      ) +
      labs(
        y = str_wrap(eth_title_i, 30)
      )
  
    # Save ethnicity plot
    ggsave(
      eth_save_name_i,
      plot = eth_plot_i,
      width = 5,
      height = 3
    )
  }
  
  
  
  # Loop over all IMD indicators
  for (indicator_i in imd_indicators) {
    
    # Get ethnicity data
    imd_data_i <- imd_data %>%
      filter(
        IndicatorName == indicator_i
      ) %>%
      # Separate filter to get max year of *filtered* data
      filter(
        Year == max(Year)
      )
    # Create title and save name strings
    imd_title_i <- paste0(
      str_to_title(
        strsplit(indicator_i, ": ")[[1]][2]
      ),
      " (", imd_data_i$Year[[1]], ")"
    )
    
    imd_save_name_i <- paste0(
      "output/imd-plots/",
      str_replace_all(
        strsplit(indicator_i, ": ")[[1]][2], " ", "_"),
      "_IMD.png"
    )
    
    # Plot IMD data
    imd_plot_i <- ggplot(
      imd_data_i,
      aes(
        x = IMD,
        y = Sum.of.IndicatorValue
      )
    ) +
      geom_col(fill = fill_col) +
      theme_bw() +
      geom_errorbar(
        aes(x = IMD,
            ymin = Sum.of.LowerCI95,
            ymax = Sum.of.UpperCI95),
        width = 0.5
      ) +
      scale_y_continuous(
        limits = c(0, 1.1*max(imd_data_i$Sum.of.UpperCI95)),
        expand = c(0,0)
      ) +
      labs(
        y = str_wrap(imd_title_i, 30),
        x = "Index of Multiple Deprivation Quntile"
      )
    
    # Save ethnicity plot
    ggsave(
      imd_save_name_i,
      plot = imd_plot_i,
      width = 5,
      height = 3
    )
  }
