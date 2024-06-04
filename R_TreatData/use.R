# © Copyright 2013 by LR Construction Technologies Limited.
#  All rights Reserved.  No part of this document may be reproduced or
#  transmitted in any form or by any means, electronic, mechanical,
#  photocopying, recording, or otherwise, without prior written permission of
#  LR Construction Technologies Limited.
#
#
#  @author    patty  (patty.hao@lrtcl.com)

library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(xlsx)
#library(rapp)

#read data
device1_data <- read_excel("1.xlsx")
device2_data <- read_excel("2.xlsx", skip = 4)

#read time
start_time <- readLines("Start_time.txt")
start_time <- ymd_hms(start_time)

#change time format
device1_data <- device1_data %>% 
  mutate(Time = seconds_to_period(Time) + start_time)

# every row in sheet3
for (i in 1:nrow(device2_data)) {
  # find the time 
  current_time <- device2_data$Time[i]
  current_time_seconds <- as.integer(floor(as.numeric(current_time)))
  # find the same time in sheet2
  matching_rows <- device1_data[as.integer(floor(as.numeric(device1_data$Time))) == current_time_seconds, ]
  if (nrow(matching_rows) > 0) {
    # calculate averange of load data of sheet2
    load_average <- mean(matching_rows$Load)
    # add into sheet3
    device2_data$Load_Average[i] <- load_average
  } else {    
    device2_data$Load_Average[i] <- NA
  } 
}

#visulization
plot_data <- data.frame(Time = device2_data$Time, Load_Average = device2_data$Load_Average)
ggplot(plot_data, aes(x = Time, y = Load_Average)) +
  geom_line() +
  xlab("Time") +
  ylab("Load Average") +
  ggtitle("Load Average over Time")

#output
header <- read_excel("2.xlsx", range = "A1:A3", col_names = TRUE)
# Create a data frame with empty rows
num_empty_rows <- 7  # Number of empty rows to add
empty_rows <- data.frame(matrix("", nrow = num_empty_rows, ncol = ncol(header)))
colnames(empty_rows) <- colnames(header)
# Combine the empty rows with the header data frame
output_header <- rbind(header, empty_rows)
# Write device1_data to the CSV file
write.table(output_header, "output.csv", append = FALSE, sep = ",", row.names = FALSE)
# Write device2_data to the CSV file, appending to the existing file
write.table(device2_data, "output.csv", append = TRUE, sep = ",", row.names = FALSE)



