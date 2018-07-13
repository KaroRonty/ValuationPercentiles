library(dplyr) # data formatting
library(ggplot2) # plotting
library(reshape2) # melting data for plotting

# Retrieve data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Calculate the needed variables
full_data <- full_data %>% mutate(
  "PE" = P/E,
  "PB" = 1/as.numeric(bm),
  "PD" = P/D
)

# Make a data frame containing only the new valuation variables
ranked <- as.data.frame(cbind(full_data$dates,
                 percent_rank(full_data$CAPE),
                 percent_rank(full_data$PE),
                 percent_rank(full_data$PB),
                 percent_rank(full_data$PD)))
colnames(ranked) <- c("dates", "CAPE", "PE", "PB", "PD")

melted <- melt(ranked, id.vars = "dates")
melted$value <- as.numeric(melted$value)
melted <- as.data.frame(melted)

ggplot(data = melted, aes(x = as.Date(melted$dates, "%Y-%M"),y = value, color = variable)) +
  facet_wrap(~variable) + geom_line(size = 1.1) + labs(x = "Date", y = "Percentile of valuation")
