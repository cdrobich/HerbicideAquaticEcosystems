

data <- read.csv("Data/2018_CrownMarsh.csv") #took the average of every pond measurement

library(tidyverse)
library(gridExtra)
library(ggpubr)


glimpse(data)
colnames(data)
unique(data$Time)

data$Year <- as.factor(data$Year)


#### Glyphosate in Water ####

### just pre-treatment data

base <- subset(data, Time == "Baseline", select = Treatment:Glyphosate)

glimpse(base)
unique(base$Time)

base$Transect <- factor(base$Transect, levels = c("Phragmites", "0 m", "10 m", "25 m", "50 m", "100 m"))

SedPre <- ggplot(data = base, aes(x = Transect, y = Glyphosate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate]in sediment (mg/kg)") +
  ylim(0.00, 0.1) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 14))

SedPre

#### within 24 hours

hour24 <- subset(data, Time == "24 hour", select = Treatment:Glyphosate)

glimpse(hour24)
unique(hour24$Time)

hour24$Transect <- factor(hour24$Transect, levels = c("Phragmites", "0 m", "10 m", "25 m", "50 m", "100 m"))

Sed24 <- ggplot(data = hour24, aes(x = Transect, y = Glyphosate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.1) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

Sed24


### >20 days ###

day20 <- subset(data, Time == ">20 day", select = Treatment:Glyphosate)

glimpse(day20)
unique(day20$Time)

day20$Transect <- factor(day20$Transect, levels = c("Phragmites", "0 m", "10 m", "25 m", "50 m", "100 m"))

Sed20 <- ggplot(data = day20, aes(x = Transect, y = Glyphosate, group = 1)) +
  geom_line() +
  geom_point() +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 0.1) +
  theme(axis.text = element_text(size = 13),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        axis.title.y = element_text(size = 16))

Sed20



##### panel #####

# Glyphosate panel

crown.marsh <- ggarrange(SedPre, Sed24, Sed20,
          ncol = 3,
          labels = c("Pre-2018 Treatment", "Within 24 hours", "> 20 Days"),
          font.label = list(size = 13, face = "plain"),
          hjust = c(-0.7,-0.9, -1.3),
          vjust = 2)
  
crown.marsh

ggsave("Figures/Crown_Sedimet_transect.tiff", crown.marsh,
       dpi = 300)
