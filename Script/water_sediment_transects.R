library(tidyverse)
library(gridExtra)

data <- read.csv("Data/Transects.csv") #took the average of every pond measurement

glimpse(data)
colnames(data)
unique(data$Time)
unique(data$Station)


#### Glyphosate in Water ####

### just pre-treatment data ####

base <- subset(data, Time == "Pre-treatment", select = Station:AMPsed)

glimpse(base)
unique(base$Time)
unique(base$Station)

base$Station <- factor(base$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphWatPre <- ggplot(data = base, aes(x = Station, y = GlyphWat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate]in water (mg/L)") +
  ylim(0.00, 0.3) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphWatPre


### AMPA in pre-treatment
colnames(base)

AMPAWatPre <- ggplot(data = base, aes(x = Station, y = AMPwat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[AMPA] in water (mg/L)") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPAWatPre


#### within 24 hours #######

Hour24 <- subset(data, Time == "24 hour", select = Station:AMPsed)

glimpse(Hour24)
unique(Hour24$Time)
unique(Hour24$Station)

Hour24$Station <- factor(Hour24$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphWat24 <- ggplot(data = Hour24, aes(x = Station, y = GlyphWat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.3) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphWat24

## AMPA 

AMPAWat24 <- ggplot(data = Hour24, aes(x = Station, y = AMPwat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPAWat24


#### >20 Days after ####

Day20 <- subset(data, Time == ">20 day", select = Station:AMPsed)

glimpse(Day20)
unique(Day20$Time)
unique(Day20$Station)

Day20$Station <- factor(Day20$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphWat20 <- ggplot(data = Day20, aes(x = Station, y = GlyphWat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.3) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphWat20

## AMPA > 20 days

AMPAWat20 <- ggplot(data = Day20, aes(x = Station, y = AMPwat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPAWat20

#### One year after ####

OneYr <- subset(data, Time == "One year", select = Station:AMPsed)

glimpse(OneYr)
unique(OneYr$Time)
unique(OneYr$Station)

OneYr$Station <- factor(OneYr$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphWatOneyr <- ggplot(data = OneYr, aes(x = Station, y = GlyphWat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.3) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphWatOneyr


### AMPA one year

AMPAWatOneyr <- ggplot(data = OneYr, aes(x = Station, y = AMPwat, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPAWatOneyr


##### Glyphosate in Sediment #####

### just pre-treatment data ####

base <- subset(data, Time == "Pre-treatment", select = Station:AMPsed)

glimpse(base)
unique(base$Time)
unique(base$Station)

base$Station <- factor(base$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphSedPre <- ggplot(data = base, aes(x = Station, y = GlyphSed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate]in sediment (mg/kg)") +
  ylim(0.00, 0.2) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphSedPre

## AMPA baseline

AMPASedPre <- ggplot(data = base, aes(x = Station, y = AMPsed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[AMPA]in sediment (mg/kg)") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPASedPre

#### within 24 hours ####

Hour24 <- subset(data, Time == "24 hour", select = Station:AMPsed)

glimpse(Hour24)
unique(Hour24$Time)
unique(Hour24$Station)

Hour24$Station <- factor(Hour24$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphSed24 <- ggplot(data = Hour24, aes(x = Station, y = GlyphSed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.2) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphSed24

## AMPA 24 hours

AMPASed24 <- ggplot(data = Hour24, aes(x = Station, y = AMPsed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPASed24

### >20 Days after ####

Day20 <- subset(data, Time == ">20 day", select = Station:AMPsed)

glimpse(Day20)
unique(Day20$Time)
unique(Day20$Station)

Day20$Station <- factor(Day20$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphSed20 <- ggplot(data = Day20, aes(x = Station, y = GlyphSed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.2) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

GlyphSed20

# AMPA 20 days

AMPASed20 <- ggplot(data = Day20, aes(x = Station, y = AMPsed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

AMPASed20

### One year after ####

OneYr <- subset(data, Time == "One year", select = Station:AMPsed)

glimpse(OneYr)
unique(OneYr$Time)
unique(OneYr$Station)

OneYr$Station <- factor(OneYr$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "50 m", "100 m", "150 m"))

GlyphSedOneyr <- ggplot(data = OneYr, aes(x = Station, y = GlyphSed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.2) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))


GlyphSedOneyr

## AMPA one year

AMPASedOneyr <- ggplot(data = OneYr, aes(x = Station, y = AMPsed, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.05) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))


AMPASedOneyr

##### panel #####

# Glyphosate panel

grid.arrange(arrangeGrob(GlyphWatPre, GlyphSedPre , top = "Pre-treatment"),
             arrangeGrob(GlyphWat24, GlyphSed24, top = "24 hours"),
             arrangeGrob(GlyphWat20, GlyphSed20, top = ">20 Days"),
             arrangeGrob(GlyphWatOneyr, GlyphSedOneyr, top = "One Year"),
             ncol = 4)

# AMPA panel

grid.arrange(arrangeGrob(AMPAWatPre, AMPASedPre , top = "Pre-treatment"),
             arrangeGrob(AMPAWat24, AMPASed24, top = "24 hours"),
             arrangeGrob(AMPAWat20, AMPASed20, top = ">20 Days"),
             arrangeGrob(AMPAWatOneyr, AMPASedOneyr, top = "One Year"),
             ncol = 4)



###### weird AEH panel


data1 <- read.csv("AEH_weird.csv")

glimpse(data1)

data1$Year <- as.factor(data1$Year)
colnames(data1)

### pre-treatment

Pre2018 <- subset(data1, Timepoint == "Pre-treatment", select = Marsh:AEH)
Pre2018 <- subset(Pre2018, Year == "2018", select = Marsh:AEH)


glimpse(Pre2018)
unique(Pre2018$Station)


Pre2018$Station <- factor(Pre2018$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "25 m", "50 m", "100 m"))

preAEH <- ggplot(data = Pre2018, aes(x = Station, y = AEH, shape = Treatment)) +
  geom_point(size = 3) +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[AEH] in Water") +
  ylim(0, 2.5) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")


preAEH


#### 24 hours

max2018 <- subset(data1, Timepoint == "24 hour", select = Marsh:AEH)
max2018 <- subset(max2018, Year == "2018", select = Marsh:AEH)


glimpse(Pre2018)
unique(Pre2018$Station)


max2018$Station <- factor(max2018$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "25 m", "50 m", "100 m"))

AEHmax <- ggplot(data = max2018, aes(x = Station, y = AEH, shape = Treatment)) +
  geom_point(size = 3) +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  ylim(0.00, 2.5) +
  xlab(" ") +
  ylab("  ") +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16)) +
  theme(legend.position = "none")

AEHmax

#### >20 days


month2018 <- subset(data1, Timepoint == ">20 day", select = Marsh:AEH)
month2018 <- subset(month2018, Year == "2018", select = Marsh:AEH)


month2018$Station <- factor(month2018$Station, levels = c("Phragmites", "Pond", "0 m", "10 m", "25 m", "50 m", "100 m"))

AEHmonth <- ggplot(data = month2018, aes(x = Station, y = AEH, shape = Treatment)) +
  geom_point(size = 3) +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0, 2.5) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16)) 

AEHmonth


grid.arrange(arrangeGrob(preAEH , top = "Pre-treatment"),
             arrangeGrob(AEHmax, top = "24 hours"),
             arrangeGrob(AEHmonth, top = ">20 Days"),
             ncol = 3)

