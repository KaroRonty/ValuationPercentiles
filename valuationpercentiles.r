library(httr) # downloading the xls(x) files
library(readxl) # reading xls(x) files
library(dplyr) # data formatting
library(ggplot2) # plotting
library(reshape2) # melting data for plotting

# Get CAPE, P/E and P/D data from Shiller
GET("http://www.econ.yale.edu/~shiller/data/ie_data.xls", write_disk(temp <- tempfile(fileext = ".xls")))
shillerdata <- read_xls(temp, sheet = 3, skip = 7)

# Format the years and months correctly
corrected_dates <- expand.grid(1:12, 1871:2018)
last_month <- length(grep("2018", shillerdata$Date))
months_to_be_cut_off <- 12 - last_month
corrected_dates <- head(corrected_dates, nrow(corrected_dates) - months_to_be_cut_off)
# Add leading zeros
corrected_dates$Var1 <- sprintf("%02d", as.numeric(corrected_dates$Var1))
dates <- as.data.frame(paste(corrected_dates$Var2, corrected_dates$Var1, sep = "-"))
names(dates) <- "dates"

# Remove possible excess rows & add corrected dates back
shillerdata <- head(shillerdata, nrow(dates))
shillerdata <- cbind(dates, shillerdata)
shillerdata$Date <- NULL

# Get P/B data from Goyal
GET("http://www.hec.unil.ch/agoyal/docs/PredictorData2017.xlsx", write_disk(temp <- tempfile(fileext = ".xls")))
goyaldata <- read_xlsx(temp, sheet = 1)
goyaldata <- select(goyaldata, c("yyyymm", "b/m"))

# Make dates into same format as above and prepare names for joining
goyaldata$yyyymm <- paste(substr(goyaldata$yyyymm, 1, 4), substr(goyaldata$yyyymm, 5, 6), sep = "-")
names(goyaldata) <- c("dates", "bm")

full_data <- full_join(shillerdata, goyaldata, by = "dates")

# Replace written NAs with real NAs
full_data$bm[full_data$bm == "NaN"] <- NA
full_data$CAPE[full_data$CAPE == "NA"] <- NA
full_data$CAPE <- as.numeric(full_data$CAPE)

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
