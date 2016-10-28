library(ggplot2)
library(reshape2)

my_data <- read.csv(file="~/datasci_course_materials/assignment6/seattle_incidents_summer_2014.csv",head=TRUE,sep=",")
summary(my_data)
colnames(my_data)
# table(my_data$District.Sector)
# table(my_data$Offense.Type)
# table(my_data$Occurred.Date.or.Date.Range.Start)

get_time_of_day <- function(record) {
  full_time <- record['Occurred.Date.or.Date.Range.Start']
  time_hour <- ''
  if (!is.null(full_time) & full_time != '') {
    time_split <- unlist(strsplit(full_time,split = ' '))
    time <- time_split[2]
    time_am_pm <- time_split[3]
    time_hour <- unlist(strsplit(time,split = ':'))[1]
    if (time_am_pm == 'PM' && time_hour != '12') {
      time_hour <- as.integer(time_hour) + 12
    }
    if (time_am_pm == 'AM' && time_hour == '12') {
      time_hour <- '00'
    }
  }
  return(time_hour)
}

get_precinct <- function(record) {
  sector <- record['District.Sector']
  precinct <- 'N'
  if (sector %in% c('B','J','N','L','U')) {
    precinct <- 'N'
  } else if (sector %in% c('Q','D','M','K')) {
    precinct <- 'W'
  } else if (sector %in% c('C','E','G')) {
    precinct <- 'E'
  } else if (sector %in% c('W','F')) {
    precinct <- 'SW'
  } else if (sector %in% c('O','R','S')) {
    precinct <- 'S'
  } 
  return(precinct)
}

get_type <- function(record) {
  offense_type <- record['Offense.Type']
  type <- unlist(strsplit(offense_type,split = '-'))[1]
  return(type)
}

get_type_common <- function(record) {
  offense_type <- record['Offense.Type']
  type <- unlist(strsplit(offense_type,split = '-'))[1]
  if (!(type %in% c('THEFT','BURGLARY','VEH','PROPERTY DAMAGE','ASSLT','FRAUD','ROBBERY','HARASSMENT'))) {
    type <- 'OTHER'
  }
  return(type)
}

# ADD NEW COLUMNS
my_data$Hour <- apply(my_data, 1, get_time_of_day)
my_data$Precinct <- apply(my_data, 1, get_precinct)
my_data$Type <- apply(my_data, 1, get_type)
my_data$TypeCommon <- apply(my_data, 1, get_type_common)


# SIMPLE BY HOUR
ggplot(my_data, aes(x=Hour)) +
  geom_bar(stat="count", fill="#7b97c4") +
  theme_light(base_size=16) +
  xlab("Hour of Day") +
  ylab("Crime Count") +
  ggtitle("Seattle: Crimes by Hour") +
  theme(plot.title=element_text(size=16))


# COMMON TYPES ORDERED
types <- table(my_data$Type)
types_frame <- data.frame(Type=rownames(types), Count=as.vector(types))
types_frame <- types_frame[which(types_frame$Count > 500),]

ggplot(types_frame, aes(x=reorder(Type,Count), y=Count)) +
  geom_bar(stat="identity", fill="#7b97c4") +
  coord_flip() +
  theme_light(base_size=16) +
  xlab("Crime Type") +
  ylab("Crime Count") +
  ggtitle("Seattle: Crimes by Type") +
  theme(plot.title=element_text(size=16))



# CRIMES BY HOUR STACKED BY TYPE
library(dplyr)
ggplot(my_data %>% count(Hour, TypeCommon) %>%    # Group by Hour and Disctrict, then count number in each group
         mutate(pct=n,                   # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),      # Calculate label positions
       aes(Hour, n, fill=TypeCommon)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=pct, y=ypos), size=2.5) +
  # coord_flip() +
  theme_light(base_size=16) +
  xlab("Time of Day") +
  ylab("Crime Count") +
  ggtitle("Seattle: Crimes by Hour and Type") +
  theme(plot.title=element_text(size=16)) +
  scale_fill_brewer(palette = "Set1")


# CRIMES BY HOUR STACKED BY DISTRICT
ggplot(my_data %>% count(Hour, Precinct) %>%    # Group by Hour and Disctrict, then count number in each group
         mutate(pct=n/sum(n),                   # Calculate percent within each region
         ypos = cumsum(n) - 0.5*n),             # Calculate label positions
       aes(Hour, n, fill=Precinct)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.0f", pct*100),"%"), y=ypos), size=2.5) +
  # coord_flip() +
  theme_light(base_size=16) +
  xlab("Time of Day") +
  ylab("Crime Count") +
  ggtitle("Seattle: Crimes by Hour and Precinct") +
  theme(plot.title=element_text(size=16)) +
  scale_fill_brewer(palette = "Set1")
  # scale_fill_manual(values=c('#CF894C','#4CCFCB','#92CF4C','#CF4C51','#894CCF'))




# CRIMES BY PRECINCT STACKED BY COMMON TYPES
ggplot(my_data %>% count(Precinct, TypeCommon) %>%    # Group by Hour and Disctrict, then count number in each group
         mutate(pct=n,                          # Calculate percent within each region
         ypos = cumsum(n) - 0.5*n),             # Calculate label positions
         aes(Precinct, n, fill=TypeCommon)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=sprintf("%1.0f", pct), y=ypos), size=2.5) +
  theme_light(base_size=16) +
  xlab("Seattle Precinct") +
  ylab("Crime Count") +
  ggtitle("Seattle: Crimes by Precinct") +
  theme(plot.title=element_text(size=16)) +
  scale_fill_brewer(palette = "Set1")



data_1 <- my_data[which(my_data$TypeCommon %in% c('ASSLT', 'ROBBERY')),]
data_2 <- my_data[which(my_data$TypeCommon %in% c('THEFT', 'BURGLARY')),]
map_data <- data_1

# GEOGRAPHICAL MAPS
library(ggmap)
qmap(location = "Seattle,WA", zoom=11) +
  geom_point(data=map_data, aes(x=Longitude, y=Latitude), col="orange", alpha=0.3, size=0.5)
  # scale_size_continuous(range=0.5)


map1 <- get_map(location="Seattle,WA", zoom=12, maptype='roadmap', color='bw')
ggmap(map1, extent = "device") +
  geom_density2d(data = map_data, aes(x=Longitude, y=Latitude), size = 0.3) +
  stat_density2d(data = map_data, aes(x=Longitude, y=Latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") +
  scale_fill_gradient(low = "green", high = "red") +
  scale_alpha(range = c(0, 0.8), guide = FALSE)
