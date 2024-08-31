library(ggplot2)

US_Carrier_2018 <- read.csv("US_Carrier_PROJECT_2018.csv")
US_Carrier_2018_Subset <- subset(US_Carrier_2018, US_Carrier_2018$SEATS != 0)
US_Carrier_2018_Subset$SEATS_FILLED <- (US_Carrier_2018_Subset$PASSENGERS / US_Carrier_2018_Subset$SEATS) * 100
US_Carrier_2018_Subset$YEAR <- 2018


US_Carrier_2019 <- read.csv("US_Carrier_PROJECT_2019.csv")
US_Carrier_2019_Subset <- subset(US_Carrier_2019, US_Carrier_2019$SEATS != 0)
US_Carrier_2019_Subset$SEATS_FILLED <- (US_Carrier_2019_Subset$PASSENGERS / US_Carrier_2019_Subset$SEATS) * 100
US_Carrier_2019_Subset$YEAR <- 2019


US_Carrier_2020 <- read.csv("US_Carrier_PROJECT_2020.csv")
US_Carrier_2020_Subset <- subset(US_Carrier_2020, US_Carrier_2020$SEATS != 0)
US_Carrier_2020_Subset$SEATS_FILLED <- (US_Carrier_2020_Subset$PASSENGERS / US_Carrier_2020_Subset$SEATS) * 100
US_Carrier_2020_Subset$YEAR <- 2020

US_Carrier_2021 <- read.csv("US_Carrier_PROJECT_2021.csv")
US_Carrier_2021_Subset <- subset(US_Carrier_2021, US_Carrier_2021$SEATS != 0)
US_Carrier_2021_Subset$SEATS_FILLED <- (US_Carrier_2021_Subset$PASSENGERS / US_Carrier_2021_Subset$SEATS) * 100
US_Carrier_2021_Subset$YEAR <- 2021

US_Carrier_All_Years <- rbind(US_Carrier_2018_Subset, US_Carrier_2019_Subset, US_Carrier_2020_Subset, US_Carrier_2021_Subset)

# Does the month affect the air time based on distance of certain types of plane
# Are distance and aircraft type the only factor in determining the air time

# we can do facet on months and have air time as y axis and type of plane as x axis
# we can do facet on carrier and have air time as y axis and months in the x axis

# we will randomly choose 5 carriers to test this theory on
# sample(US_Carrier_All_Years$AIRCRAFT_TYPE, size = 5)
# 
# data_UA <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER == "UA")
# data_27Q <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER == "27Q")
# data_NK <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER == "NK")
# data_AA <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER == "AA")
# data_F9 <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER == "F9")

frame <- sample(US_Carrier_All_Years$AIRCRAFT_TYPE, size = 10)

ggplot(data = subset(US_Carrier_All_Years, US_Carrier_All_Years$AIRCRAFT_TYPE == frame)) +
  geom_point(mapping = aes(x = DISTANCE, y = AIR_TIME, color = as.factor(AIRCRAFT_TYPE))) +
  facet_grid(. ~ YEAR) 

mean(US_Carrier_All_Years$AIR_TIME[US_Carrier_All_Years$AIRCRAFT_TYPE == 515])

# Think about more questions
# Are there more flights travelling international or more flight travelling domestic

US_Carrier_region_d <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "D")

# Which airlines are preferred for international travel
US_International2018 <- subset(US_Carrier_2018_Subset, US_Carrier_2018_Subset$REGION == "I")
US_International2019 <- subset(US_Carrier_2019_Subset, US_Carrier_2019_Subset$REGION == "I")
US_International2020 <- subset(US_Carrier_2020_Subset, US_Carrier_2020_Subset$REGION == "I")
US_International2021 <- subset(US_Carrier_2021_Subset, US_Carrier_2021_Subset$REGION == "I")

US_International <- rbind(US_International2018, US_International2019, US_International2020,
                          US_International2021)

ggplot(data = US_International) +
  geom_tile(mapping = aes(x = YEAR, y = CARRIER_NAME, fill = SEATS_FILLED)) +
  labs(x = "years", y = "Carrier Name", fill = "Percentage of seats filled")

US_Carrier_region_d <- subset(US_Carrier_region_d, US_Carrier_region_d$SEATS_FILLED <= 100)

US_CAr <- subset(US_Carrier_region_d, US_Carrier_region_d$SEATS_FILLED 
                 > quantile(US_Carrier_region_d$SEATS_FILLED))

ggplot(data = US_Carrier_region_d) +
  geom_tile(mapping = aes(x = YEAR, y = DEST, fill = SEATS_FILLED))

# US_Carrier_2021_Subset$DEST <- as.factor(US_Carrier_2021_Subset$DEST)

# ggplot(data = US_Carrier_All_Years) +

US <- subset(US_Carrier_All_Years, US_Carrier_All_Years$SEATS_FILLED <= 100)

ggplot(data = US) +
  geom_tile(mapping = aes(x = YEAR, y = CARRIER_NAME, fill = as.factor(log10(DISTANCE))))

unique(US_Carrier_All_Years$AIRCRAFT_TYPE)

# Is there a relation between aircraft type and number of seats filled

ggplot(data = US_Carrier_All_Years) +
  geom_tile(mapping = aes(x = YEAR, y = factor(AIRCRAFT_TYPE), fill = SEATS_FILLED))

install.packages("ggthemes") # need this ofr tufte boxplot
library(ggthemes)

g <- ggplot(data = US_Carrier_All_Years) +
  geom_tufteboxplot(mapping = aes(x = factor(AIRCRAFT_TYPE), y = SEATS_FILLED))

g + theme(axis.title.x = element_text(angle = 90))

# Which aircraft type are preferred to go to different regions
d <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "D")
i <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "I")
a <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "A") 
l <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "L")
p <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "P")
s <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "S")

countd <- 0

for (i in US_Carrier_All_Years$REGION) {
  if (i == "I") {
    countd <- countd + 1
  }
}

countd

# Were domestic flights impacted more in Covid times or the international flights

# sub-setting based on region first
d <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "D")

ggplot(data = d) +
  geom_tile(mapping = aes(x = YEAR, y = CARRIER_NAME, fill = PASSENGERS)) +
  scale_fill_distiller(palette = "Reds", direction = 1)

i <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION == "I")
ggplot(data = i) +
  geom_tile(mapping = aes(x = YEAR, y = CARRIER_NAME, fill = PASSENGERS)) +
  scale_fill_distiller(palette = "Reds", direction = 1)
  

# calculate the mean of all passengers based on the dest_city_name 
# if the num of passengers is below the mean then we can kick those cities out

mean(US_Carrier_All_Years$PASSENGERS) # we can create a new subset > mean

median(US_Carrier_All_Years$PASSENGERS)

# this idea is not working
US <- subset(US_Carrier_All_Years, US_Carrier_All_Years$CARRIER_NAME
             [US_Carrier_All_Years$PASSENGERS > 3571])

quantile(US_Carrier_All_Years$PASSENGERS)

ggplot(data = US) +
  geom_tile(mapping = aes(x = YEAR, y = DEST_CITY_NAME))

# make sure that you change this to (1, 1)
par(mfrow = c(1, 2))
par(mfrow = c(1,1))

ggplot(data = US_Carrier_All_Years) +
  geom_line(mapping = aes(x = SEATS, y = PASSENGERS, color = as.factor(YEAR))) +
  facet_grid(. ~ YEAR)

ggplot(data = US_Carrier_All_Years) +
  geom_boxplot(mapping = aes(x = REGION, y = PASSENGERS, color = as.factor(REGION)))

# what trends can we see in different regions before and after covid, 
# which region flights travel more distance

US_NO_SYS <- subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION != "S")

ggplot(data = subset(US_Carrier_All_Years, US_Carrier_All_Years$REGION != "S")) +
  geom_jitter(mapping = aes(x = REGION, y = PASSENGERS, 
                           color = DISTANCE)) +
  facet_grid(. ~ YEAR) +
  labs(x = "Region", y = "number of passengers", color = "Distance") +
  scale_color_distiller(palette = "Reds", direction = 1)

ggplot(data = US_Carrier_All_Years) +
  geom_tile(mapping = aes(x = YEAR, y = DEST))

quantile(US_Carrier_All_Years$SEATS_FILLED)

dest <- subset(US_Carrier_All_Years, US_Carrier_All_Years$SEATS_FILLED > 88.31)
unique(dest$DEST)




