library(ggplot2)
library(reshape2)

my_data <- read.csv(file="~/datasci_course_materials/assignment6/sanfrancisco_incidents_summer_2014.csv",head=TRUE,sep=",")
summary(my_data)
colnames(my_data)

table(my_data$PdDistrict)
table(my_data$Category)

get_time_of_day <- function(record) {
  full_time <- record['Time']
  time_hour <- ''
  if (!is.null(full_time) & full_time != '') {
    time_split <- unlist(strsplit(full_time,split = ':'))
    time_hour <- time_split[1]
  }
  return(time_hour)
}

# BY HOUR OF DAY
my_data$Hour <- apply(my_data, 1, get_time_of_day)

ggplot(my_data, aes(x=Hour)) +
  geom_bar(stat="count", fill="#7b97c4") +
  # coord_flip() +
  theme_light(base_size=16) +
  xlab("Time of Day") +
  ylab("Crime Count") +
  ggtitle("San Francisco: Crimes by Hour") +
  theme(plot.title=element_text(size=16))

# BY TYPE
types <- table(my_data$Category)
types_names <- names(types)
types_counts <- as.vector(types)
types_frame <- data.frame(CrimeType=types_names, CrimeCount=types_counts)
types_frame <- types_frame[which(as.integer(types_frame$CrimeCount) >= 100),]

ggplot(types_frame, aes(x=reorder(CrimeType,CrimeCount), y=CrimeCount)) +
  geom_bar(stat="identity", fill="#7b97c4") +
  coord_flip() +
  theme_light(base_size=16) +
  xlab("Crime Type") +
  ylab("Crime Count") +
  ggtitle("San Francisco: Crimes by Type") +
  theme(plot.title=element_text(size=16))





