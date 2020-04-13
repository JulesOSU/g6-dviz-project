library(readr)

Master_Gene_Dataset_ <- read_csv("~/Spring Term 2020/Work/Master Gene Dataset .csv", 
                                 col_types = cols(F17 = col_number(), 
                                                  F18 = col_number(), F19 = col_number(), 
                                                  S18 = col_number(), S19 = col_number()))

data <- Master_Gene_Dataset_

data[is.na(data)] = 0

data$yr_code <- ifelse(data$F19 > 0 & data$S19 > 0 & data$F18 > 0 & data$S18 > 0 & data$F17 > 0, "Year Around Carryover", "No")
data$fall_code <- ifelse(data$F19 > 0 & data$F18 > 0 & data$F17 > 0 & data$S19 < 1 & data$S18 < 1, "Fall Seasonal Carryover", "No")
data$new_code <- ifelse(data$F19 > 0 & data$S19 < 1 & data$F18 < 1 & data$S18 < 1 & data$F17 < 1, "Fall New Style", "No")

sum(data$F19)
sum(data$F18)
sum(data$F17)

sum(data$S19)
sum(data$S18)


setwd('/Users/JOlsen/OneDrive - Columbia Sportswear Company/Documents/Spring Term 2020/Work')
write.csv(data,'Gene_dataset.csv')