library(dplyr)
library(ggplot2)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Q1.
# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
# make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

#1. Load and divide the data
totalyearEM <- NEI %>% group_by(year) %>% summarise(total = sum(Emissions))

#2. Create a bar plot and transfer ton to kilotons
P1 <- barplot(totalyearEM$total/1000, main = "Total PM2.5 Emissions Each Year", 
              xlab = "Year", ylab = "PM2.5 Emissions in Kilotons", ylim = c(0,8000), 
              names.arg = totalyearEM$year, col = "red")
#3. Add labels to each bar
text(P1, round(totalyearEM$total/1000), labels = round(totalyearEM$total/1000), pos = 3)

#Q2
# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") 
# from 1999 to 2008? Use the base plotting system to make a plot answering this question.

#1. Load and divide the data
BCMtotalyearEM <- filter(NEI, fips == "24510") %>% group_by(year) %>% summarise(total = sum(Emissions))

#2. Create a bar plot
P2 <- barplot(BCMtotalyearEM$total, main = "Each Year Total PM2.5 Emissions in BCM",
              xlab = "Year", ylab = "PM2.5 Emissions in Tons",
              ylim = c(0,3500), names.arg = BCMtotalyearEM$year, col = "blue")

#3. Add labels to each bar
text(P2, round(BCMtotalyearEM$total), labels = round(BCMtotalyearEM$total), pos = 3)

#Q3
# Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases in emissions from 1999?C2008 for Baltimore City? 
# Which have seen increases in emissions from 1999?C2008? 
# Use the ggplot2 plotting system to make a plot answer this question.

#1. Load and divide the data
BCMtotalyeartEM <- filter(NEI, fips == "24510") %>% group_by(year, type) %>% summarise(total = sum(Emissions))

#2. Use ggplot2 to create a bar plot
ggplot(BCMtotalyeartEM, aes(x = factor(year), y = total, fill = type, label = round(total)))+
  geom_bar(stat = "identity")+facet_grid(. ~ type) +
  ggtitle("Total PM2.5 Emissions in BCM in Different Types") +
  xlab("Year") + ylab("PM2.5 Emissions in Tons") +
  theme_dark()

#Q4
# Across the United States, how have emissions from coal combustion-related sources changed from 1999?C2008?

#1. Pick up those data which is from coal combustion-related sources
coal <- SCC[grepl("Coal", SCC$EI.Sector), ]
EMIcoal <- NEI[(NEI$SCC %in% unique(coal$SCC)), ]
YEARcoal <- EMIcoal %>% group_by(year) %>% summarise(total = sum(Emissions))

#2. Use ggplot2 to create a bar plot
ggplot(YEARcoal, aes(factor(year), y = round(total/1000), label = round(total/1000))) + 
  geom_bar(stat = "identity", fill = "pink") + 
  ggtitle("Total coal combustion related PM2.5 Emissions") + 
  xlab("Year") + ylab("PM2.5 Emissions in Kilotons") +
  ylim(c(0, 620)) + geom_text(size = 5, vjust = -0.5) +theme_classic()

#Q5
# How have emissions from motor vehicle sources changed from 1999 to 2008 in Baltimore City?

#1.Pick up those data which is from motor vehicle sources 
mo <- SCC[grepl("Vehicle", SCC$SCC.Level.Two), ]
EMImo <- NEI[(NEI$SCC %in% unique(mo$SCC)), ]
Yearmo <- EMImo %>% group_by(year) %>% summarise(total = sum(Emissions))

#2. Use ggplot2 to create a bar plot
ggplot(Yearmo, aes(y = round(total/1000), factor(year), label = round(total/1000))) +
  geom_bar(stat = "identity", fill = "darkgreen") +
  ggtitle("Total motor vehicle combustion related PM2.5 Emissions") + 
  xlab("Year") + ylab("PM2.5 Emissions in Kilotons") +
  ylim(c(0,400)) + theme_classic() + geom_text(size = 5, vjust = -0.5)

#Q6
# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County,
# California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

#1. Pick up those data which is from motor vehicle sources in Baltimore City and Los Angeles County
yearblmo <- filter(EMImo, fips == "24510" | fips == "06037") %>% group_by(fips, year) %>%
  summarise(total = sum(Emissions))

#2. Reshape and combine data
yearblmo <- mutate(yearblmo, Unit = ifelse(fips == "24510", "Baltimore City",
                                           ifelse(fips == "06037", "Los Angeles County")))

#3. Use ggplot2 to create a bar plot
ggplot(yearblmo, aes(y = round(total), factor(year), label = round(total), fill = Unit))+
  geom_bar(stat = "identity") + facet_grid(.~Unit) +
  ggtitle("TOtal Motor Vehicle Emissions in Each Year") +
  xlab("Year") + ylab("PM2.5 Emissions in Tons") +
  theme_classic() + ylim(c(0,7500)) +geom_text(size = 5, vjust = -0.5) +
  scale_fill_manual(values = c("darkred","darkblue"))

