library(dplyr) # data formatting
library(ggplot2) # plotting
library(tidyr) # gathering

# Retrieve data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Calculate the needed variables
full_data <- full_data %>% mutate(
  "PE" = P / E,
  "PB" = 1 / as.numeric(bm),
  "PD" = P / D
)

# Make a data frame containing only the new valuation variables
ranked <- as.data.frame(cbind(full_data$dates,
                              percent_rank(full_data$CAPE),
                              percent_rank(full_data$PE),
                              percent_rank(full_data$PB),
                              percent_rank(full_data$PD)))

colnames(ranked) <- c("dates", "CAPE", "PE", "PB", "PD")

# Gather for plotting
to_plot <- gather(ranked, variable, value, -dates)

# Convert to numeric and tibble
to_plot <- to_plot %>%
  mutate(value = as.numeric(value)) %>%
  as_tibble()

ggplot(data = to_plot, aes(x = as.Date(to_plot$dates, "%Y-%M"),
                           y = value, color = variable)) +
  facet_wrap(~variable) +
  geom_line(size = 1.1) +
  labs(x = "Date", y = "Percentile of valuation")
