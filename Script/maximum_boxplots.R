
library(vegan)
library(agricolae) #skewness, kurtosis, Tukeys
library(tidyverse)
library(car)
library(Hmisc)
library(gridExtra)

#### Glyphosate in Sediment boxplots ####

sediment <- read.csv("Data/Sediment_maxexp.csv")

## Examine Data

colnames(sediment)
glimpse(sediment)

sediment$Year <- as.factor(sediment$Year)

sediment <- subset(sediment, Treatment == "Herbicide", select = Marsh:AEH)

unique(sediment$Treatment) 

# Exploring the Data; response variables 

par(mfrow = c(2,2))

hist(sediment$Glyphosate, 
     xlab = "Glyphosate (mg/L)", main = " ",
     border = "black",
     col = "white")

hist(sediment$AMPA, 
     xlab = "AMPA (mg/L)", main = " ",
     border = "black",
     col = "white")


hist(sediment$AEH, 
     xlab = "AEH (mg/L)", main = " ",
     border = "black",
     col = "white")

# sample size

colnames(sediment)
sediment %>% group_by(Time, Year) %>% summarise(n = length(Treatment))


# Average and standard deviation among groups

sediment %>% group_by(Treatment, Year) %>% summarise(Glyph.avg = mean(Glyphosate),
                                                     Glyph.sd = sd(Glyphosate),
                                                     AMPA.avg = mean(AMPA),
                                                     AMPA.sd = sd(AMPA),
                                                     AEH.avg = mean(AEH),
                                                     AEH.sd = sd(AEH)) 



# Treatment Year  Glyph.avg Glyph.sd AMPA.avg AMPA.sd AEH.avg AEH.sd

#1 Control   2017     0.0100   0.0142  0       0         0      0    
#2 Herbicide 2016     0.0222   0.0498  0.00062 0.00237   0.247  1.12 
#3 Herbicide 2017     0.0083   0.0151  0       0         0      0    
#4 Herbicide 2018     0.0251   0.0639  0.00167 0.00440   0.08   0.310


## Make figures

## All sediment boxplot ##

sed <- ggplot(sediment, aes(x = Year, y = Glyphosate, fill = Year)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))+
  scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme_classic(base_size = 18) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("Glyphosate concentration in sediment (mg/L)")

sed

#### Baseline #####
unique(sediment$Time)

base <- subset(sediment, Time == "Pre-treatment", select = Marsh:AEH)
glimpse(base)

unique(base$Time)

## baseline glyphosate in sediment 

basesed <- ggplot(base, aes(x = Year, y = Glyphosate)) +
  geom_boxplot()

Sedb <- basesed + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate]in sediment (mg/L)") +
  ylim(-0.01, 0.3)

Sedb

## baseline AMPA in sediment 

baseAMPA <- ggplot(base, aes(x = Year, y = AMPA)) +
  geom_boxplot()

AMPAb <- baseAMPA + theme_classic(base_size = 16) +
  ylim(0.00, 0.3) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[AMPA] in sediment (mg/L)") 

AMPAb


## AEH baseline



baseAEH <- ggplot(base, aes(x = Year, y = AMPA)) +
  geom_boxplot()+
  theme_classic(base_size = 16) +
  ylim(0.00, 2.0) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Aquasurf] in sediment (mg/L)") 

baseAEH






calc <-subset(base, Treatment == "Herbicide", select = Marsh:AEH )
unique(calc$Treatment)

calc %>% group_by(Year, Transect) %>% summarise(Glyph.avg = mean(Glyphosate),
                                                Glyph.max = max(Glyphosate),
                                                Glyph.sd = sd(Glyphosate),
                                                N = length(Glyphosate),
                                                SE = Glyph.sd/(sqrt(N)))

#Year  Transect   Glyph.avg Glyph.max Glyph.sd     N       SE
#1 2016  0            0          0       0           2  0      
#2 2016  Phragmites   0          0       0           2  0      
#3 2016  Pond         0.00233    0.014   0.00572     6  0.00233

#4 2017  0            0          0       0           3  0      
#5 2017  Phragmites   0.00517    0.0155  0.00895     3  0.00517

#6 2018  0            0.00235    0.0047  0.00332     2  0.00235
#7 2018  Phragmites   0.0192     0.0383  0.0271      2  0.0192 
#8 2018  Pond         0          0      NA           1 NA    

calc %>% group_by(Year, Transect) %>% summarise(AMPA.avg = mean(AMPA),
                                                AMPA.max = max(AMPA),
                                                AMPA.sd = sd(AMPA),
                                                N = length(AMPA),
                                                SE = AMPA.sd/(sqrt(N)))
#Year  Transect   AMPA.avg AMPA.max AMPA.sd     N    SE
#<fct> <chr>         <dbl>    <dbl>   <dbl> <int> <dbl>
#  1 2016  0                 0        0       0     2     0
#2 2016  Phragmites        0        0       0     2     0
#3 2016  Pond              0        0       0     6     0
#4 2017  0                 0        0       0     3     0
#5 2017  Phragmites        0        0       0     3     0
#6 2018  0                 0        0       0     2     0
#7 2018  Phragmites        0        0       0     2     0
#8 2018  Pond              0        0      NA     1    NA

calc %>% group_by(Year, Transect) %>% summarise(AEH.avg = mean(AEH),
                                                AEH.max = max(AEH),
                                                AEH.sd = sd(AEH),
                                                N = length(AEH),
                                                SE = AEH.sd/(sqrt(N)))
#Year  Transect   AEH.avg AEH.max AEH.sd     N    SE
#<fct> <chr>        <dbl>   <dbl>  <dbl> <int> <dbl>
# 1 2016  0                0       0      0     2     0
#2 2016  Phragmites       0       0      0     2     0
#3 2016  Pond             0       0      0     6     0

#4 2017  0                0       0      0     3     0
#5 2017  Phragmites       0       0      0     3     0

#6 2018  0                0       0      0     2     0
#7 2018  Phragmites       0       0      0     2     0
#8 2018  Pond             0       0     NA     1    NA

#### 24-hours #########

unique(sediment$Time)

maximum <- subset(sediment, Time == "24 hour", select = Marsh:AEH)
glimpse(maximum)

unique(maximum$Time)

## 24 hour glyphosate in sediment 

maxsed <- ggplot(maximum, aes(x = Year, y = Glyphosate)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

Sed24 <- maxsed + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(-0.01, 0.3)

Sed24


maximum %>% group_by(Year, Transect) %>% summarise(Glyph.avg = mean(Glyphosate),
                                                   Glyph.max = max(Glyphosate),
                                                   Glyph.sd = sd(Glyphosate),
                                                   N = length(Glyphosate),
                                                   SE = Glyph.sd/(sqrt(N)))


# Year  Transect   Glyph.avg Glyph.sd     N       SE
# 1 2016  0            0.0055   0.00778     2  0.00550
#2 2016  Phragmites   0.06     0.0849      2  0.06   
#3 2016  Pond         0.0363   0.0436      6  0.0178 
#4 2017  0            0.00917  0.00843     3  0.00487
#5 2017  Phragmites   0.0177   0.0134      4  0.00668
#6 2018  0            0.0083   0.0117      2  0.0083 
#7 2018  Phragmites   0.0121   0.0171      2  0.0121 
#8 2018  Pond         0       NA           1 NA 


## AMPA 24 hours ###

AMPAs24 <- ggplot(maximum, aes(x = Year, y = AMPA)) +
  geom_boxplot()

AMPA24 <- AMPAs24 + theme_classic(base_size = 16) +
  ylim(0.00, 0.3) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ")

AMPA24


### AEH 24 hours

AEH24 <- ggplot(maximum, aes(x = Year, y = AEH)) +
  geom_boxplot()

AEH24S <- AEH24 + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0.00, 2.0)

AEH24S


###### >20 days #####

unique(sediment$Time)

month<- subset(sediment, Time == ">20 day", select = Marsh:AEH)
glimpse(month)

unique(month$Time)

## >20 days glyphosate in sediment 

monthsed <- ggplot(month, aes(x = Year, y = Glyphosate)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

Sed30 <- monthsed + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("") +
  ylim(-0.01, 0.3)

Sed30

## AMPA >20 days ###

AMPAs30 <- ggplot(month, aes(x = Year, y = AMPA)) +
  geom_boxplot() + theme_classic(base_size = 16) +
  ylim(0.00, 0.3) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ")

AMPAs30

## AEH >20 days

AEH30 <- ggplot(month, aes(x = Year, y = AEH)) +
  geom_boxplot() + theme_classic(base_size = 16) +
  ylim(0.00, 7.00) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ")

AEH30

####### Glyphosate in water boxplots ##########

water <- read.csv("Data/Water_maxexp.csv")

colnames(water)
glimpse(water)

water$Year <- as.factor(water$Year)
water <- subset(water, Treatment == "Herbicide", select = Marsh:AEH)

# summary stats
colnames(sediment)
water%>% group_by(Time, Year) %>% summarise(n = length(Treatment))


# Average and standard deviation among groups

water %>% group_by(Treatment, Year) %>% summarise(Glyph.avg = mean(Glyphosate),
                                                  Glyph.sd = sd(Glyphosate),
                                                  AMPA.avg = mean(AMPA),
                                                  AMPA.sd = sd(AMPA),
                                                  AEH.avg = mean(AEH),
                                                  AEH.sd = sd(AEH)) 

calc2 <-subset(basew, Treatment == "Herbicide", select = Marsh:AEH )
unique(calc2$Treatment)

calc2 %>% group_by(Year, Transect) %>% summarise(Glyph.avg = mean(Glyphosate),
                                                 Glyph.max = max(Glyphosate),
                                                 Glyph.sd = sd(Glyphosate),
                                                 N = length(Glyphosate),
                                                 SE = Glyph.sd/(sqrt(N)))



calc2 %>% group_by(Year, Transect) %>% summarise(AMPA.avg = mean(AMPA),
                                                 AMPA.max = max(AMPA),
                                                 AMPA.sd = sd(AMPA),
                                                 N = length(AMPA),
                                                 SE = AMPA.sd/(sqrt(N)))


calc2 %>% group_by(Year, Transect) %>% summarise(AEH.avg = mean(AEH),
                                                 AEH.max = max(AEH),
                                                 AEH.sd = sd(AEH),
                                                 N = length(AEH),
                                                 SE = AEH.sd/(sqrt(N)))


## Total Glyph in water boxplot

wat <- ggplot(water, aes(x = Year, y = Glyphosate, fill = Year)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

wat + scale_color_manual(values = c("#d8b365","#5ab4ac")) +
  theme_classic(base_size = 18) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate] in water (mg/L)")


#### Baseline #####

unique(water$Time)

basew <- subset(water, Time == "Pre-treatment", select = Marsh:AEH)

glimpse(basew)
basew$Year <- as.factor(basew$Year)
unique(basew$Time)


## baseline glyphosate in sediment 

basewat <- ggplot(basew, aes(x = Year, y = Glyphosate)) +
  geom_boxplot()

Watb <- basewat + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Glyphosate] in water (mg/L)") +
  ylim(-0.01, 0.4)

Watb

# AMPA in water baseline

baseAMPw <- ggplot(basew, aes(x = Year, y = AMPA)) +
  geom_boxplot() + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[AMPA] in water (mg/L)") +
  ylim(0.00, 0.3)

baseAMPw 

# AEH in water baseline

baseAEHw <- ggplot(basew, aes(x = Year, y = AEH)) +
  geom_boxplot() + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("[Aquasurf] in water (mg/L)") +
  ylim(0.00, 0.3)

baseAEHw 
#### 24-hours #########

unique(water$Time)

maximumw <- subset(water, Time == "24 hours", select = Marsh:AEH)
glimpse(maximumw)

unique(maximumw$Time)

## glyphosate in water

maxwat <- ggplot(maximumw, aes(x = Year, y = Glyphosate)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(0.2))

Wat24 <- maxwat + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("") +
  ylim(-0.01, 0.4)

Wat24

## 24 hours AMPA

AMPw24 <- ggplot(maximumw, aes(x = Year, y = AMPA)) +
  geom_boxplot()+
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("") +
  ylim(0, 0.3)

AMPw24

# 24 hours AEH

AEHw24 <- ggplot(maximumw, aes(x = Year, y = AEH)) +
  geom_boxplot()+
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("") +
  ylim(0, 0.3)

AEHw24



###### >20 days #####

unique(water$Time)

monthw <- subset(water, Time == ">20 days", select = Marsh:AEH)
monthw

unique(monthw$Time)

## 24 hourglyphosate in sediment 

monthwat <- ggplot(monthw, aes(x = Year, y = Glyphosate)) +
  geom_boxplot()

Wat30 <- monthwat + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab("") +
  ylim(-0.01, 0.4)

Wat30

## AMPA >20 days

AMPw30 <- ggplot(monthw, aes(x = Year, y = AMPA)) +
  geom_boxplot() +
  ylim(0, 0.050)

AMPAwat30 <- AMPw30 + theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0, 0.3)

AMPAwat30

## AEH month

AEHw30 <- ggplot(monthw, aes(x = Year, y = AEH)) +
  geom_boxplot() +
  ylim(0, 0.050) +
  theme_classic(base_size = 16) +
  theme(panel.border = element_rect(fill = NA)) +
  xlab(" ") +
  ylab(" ") +
  ylim(0, 0.3)

AEHw30


# save panels 

##### panel #####

# Glyphosate panel

grid.arrange(arrangeGrob(Watb, Sedb, top = "Pre-treatment"),
             arrangeGrob(Wat24, Sed24, top = "24 hours"),
             arrangeGrob(Wat30, Sed30, top = "> 20 Days"),
                                    ncol = 3)



glyphosate.boxplots <- ggarrange(Watb, Wat24, Wat30,
          Sedb, Sed24, Sed30,
          nrow = 2,
          ncol = 3,
          labels = c("Pre-2018 Treatment", "Within 24 hours", "> 20 Days"),
          font.label = list(size = 13, face = "plain"),
          hjust = c(-0.6,-0.7, -1),
          vjust = 2)
          
          
ggsave("Glyphosate_max_boxplots.tiff", glyphosate.boxplots,
       dpi = 300)

## AMPA panel (SM 1)
AMPA.max.boxplots <- grid.arrange(arrangeGrob(baseAMPw, AMPAb, top = "Pre-treatment"),
                                  arrangeGrob(AMPw24, AMPA24, top = "24 hours"),
                                  arrangeGrob(AMPAwat30, AMPAs30, top = ">20 Days"),
                                  ncol = 3)

## Aquasurf panel
AEH.max.boxplots <- grid.arrange(arrangeGrob(baseAEHw, baseAEH, top = "Pre-treatment"),
                                 arrangeGrob(AEHw24, AEH24S, top = "24 hours"),
                                 arrangeGrob(AEHw30, AEH30, top = ">20 Days"),
                                 ncol = 3)

