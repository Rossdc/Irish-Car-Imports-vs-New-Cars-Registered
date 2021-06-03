library(dplyr)
library(tidyverse)
library(ggfortify)
library(tidyr)
library(ggplot2)
library(GGally)
library(hrbrthemes)
library(broom)
library(reshape2)
library(ggpubr)
library(zoo)

Df1 <- read.csv("RTA01.20210402T170456.csv", header = TRUE, sep = ",")
Df2 <- read.csv("RTA02.20210402T170450.csv", header = TRUE, sep = ",")

Df1[!complete.cases(Df1),]
Df1 <- na.omit(Df1)

Df2[!complete.cases(Df2),]

#Getting rid of exempt and net registrations column
Df1<-Df1[!(Df1$Type.of.Registration =="Net registrations" | Df1$Type.of.Registration=="Exempt registrations"),]

Df2<-Df2[!(Df2$Type.of.Registration =="Net registrations" | Df2$Type.of.Registration=="Exempt registrations"),]

#Getting rid of statistic, type of registration and UNIT columns

Df1 <- select(Df1, -c(ï..Statistic, Type.of.Registration, UNIT))
Df2 <- select(Df2, -c(ï..Statistic, Type.of.Registration, UNIT))

#Making Value column a factor

Df1$Value <- factor(Df1$Value, levels = c("0 - 7000","8,001 - 13,000", "13,001 - 14,000", 
                                             "14,001 - 15,000", "15,001 - 20,000","20,001 - 25,000", "25,001 - 30,000",
                                             "30,001 - 40,000", "40,001 - 50,000", "50,001 - 60,000",
                                             "60,001 - 70,000",  "70,001 - 80,000", "80,001 - 90,000",
                                             "90,001 - 100,000", "100,001 - 150,000", 
                                          "150,001 - 200,000", "200,001 - 250,000", "Over 250,000"))

#Removing what was previously 'all values' rows

Df1 <- na.omit(Df1)

#Making Age column an ordered factor

Df2$Age <- factor(Df2$Age, levels = c("1 year", "2 years", "3 years", "4 years", "5 years",
                                      "6 years", "7 years", "8 years", "9 years", "10 years", 
                                      "11 years", "12 years", "13 years and over"))

#Removing what was previously 'All agea' rows

Df2 <- na.omit(Df2)

install.packages("RColorBrewer")

library(RColorBrewer)

g1 <- ggplot(Df1) + geom_col(aes(Year, VALUE), alpha = 0.6) +
  geom_smooth(data= Df1 %>% group_by(Year) %>% summarise(VALUE=sum(VALUE)),
              aes(x = Year, y = VALUE, group=1),
              method = "loess", se= FALSE, color = "firebrick1", size = 1) +
  theme_bw()

ggplot(Df1) + geom_col(aes(Year, VALUE, fill = Value)) + 
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
  facet_wrap(~Value, scales='free') +
  theme_bw()

g2 <- ggplot(Df2) + geom_col(aes(Year, VALUE), alpha= 0.6) +
  geom_smooth(data= Df2 %>% group_by(Year) %>% summarise(VALUE=sum(VALUE)),
              aes(x = Year, y = VALUE, group=1),
              method = "loess", se= FALSE, color = "firebrick1", size = 1) +
  theme_bw()

ggplot(Df2) + geom_col(aes(Year, VALUE, fill = Age)) + 
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    facet_wrap(~Age, scales='free') +
  theme_bw()

#Combined 

#Arranged
ggarrange(g1, g2)

#Getting Difference between years using Pipes - Df1 (Dylpr)
DfSummary <- Df1 %>% 
  group_by(Year) %>% 
  summarise(VALUE=sum(VALUE)) %>%
            mutate(difference = VALUE - lag(VALUE),
                   pct_change = (VALUE/lag(VALUE) - 1) * 100)

#Graphing Differences and %change

g1summaryDf <- ggplot(DfSummary) + geom_col(aes(Year, difference), alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

g2summaryDf <- ggplot(DfSummary) + geom_col(aes(Year, pct_change), alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Getting Difference between years using Pipes - Df2
DfSummary2 <- Df2 %>% 
  group_by(Year) %>% 
  summarise(VALUE=sum(VALUE)) %>%
  mutate(difference = VALUE - lag(VALUE),
         pct_change = (VALUE/lag(VALUE) - 1) * 100)

#Graphing Differences and %change

g1summaryDf2 <- ggplot(DfSummary2) + geom_col(aes(Year, difference), alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

g2summaryDf2 <- ggplot(DfSummary2) + geom_col(aes(Year, pct_change), alpha = 0.6) +
  theme_bw() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))

#Arranged - %change & Numerical Difference
ggarrange(g1summaryDf, g2summaryDf, g1summaryDf2, g2summaryDf2)

#Calculating Moving Average % change - 2 & 3 years - New Cars
DfSummary <- DfSummary %>%
  mutate(Moving_Average_2_years = rollapply(VALUE,3,mean,align='right',fill=NA),
         Moving_Average_3_years = rollapply(VALUE,4,mean,align='right',fill=NA))

#Calculating Moving Average % change - 2 & 3 years - Imported Cars
DfSummary2 <- DfSummary2 %>%
  mutate(Moving_Average_2_years = rollapply(VALUE,3,mean,align='right',fill=NA),
         Moving_Average_3_years = rollapply(VALUE,4,mean,align='right',fill=NA))

#Graphing actual vs 2 and 3 year Moving Average - Imported Cars

g3summaryDf2 <- ggplot(DfSummary2) + 
  geom_line(aes(Year, VALUE), group = 1, size = 2) + 
  geom_line(aes(Year, Moving_Average_2_years), group = 1, colour = "red", size = 2) + 
  geom_line(aes(Year, Moving_Average_3_years), colour = "blue", group = 1, size = 2) +
  theme_bw() +
  scale_y_continuous(labels = function(x) format(x, scientific = FALSE))
       
#Graphing actual vs 2 and 3 year Moving Average - New Cars

g3summaryDf3 <- ggplot(DfSummary) + 
  geom_line(aes(Year, VALUE), group = 1, size = 2) + 
  geom_line(aes(Year, Moving_Average_2_years), group = 1, colour = "red", size = 2) + 
  geom_line(aes(Year, Moving_Average_3_years), colour = "blue", group = 1, size = 2) +
  theme_bw()

#combined
ggarrange(g3summaryDf2, g3summaryDf3)

#Ratio Analysis

snippedDf1 <- Df1 %>% 
  group_by(Year) %>% 
  summarise(VALUE=sum(VALUE))

snippedDf2 <- Df2 %>% 
  group_by(Year) %>% 
  summarise(VALUE=sum(VALUE))

SnippedDf <- merge(snippedDf1, snippedDf2, by = "Year")

#Creating Ration Column
SnippedDf$Ratio <- SnippedDf$VALUE.y/(SnippedDf$VALUE.x)

#Creating difference & % change columns

SnippedDf <- SnippedDf %>%
  mutate(ratio_diff = Ratio - lag(Ratio),
         diff = VALUE.x - VALUE.y,
         ratio_pct_change = (Ratio/lag(Ratio) - 1) * 100,
         value_pct_change = (VALUE.x/lag(VALUE.y) - 1) * 100)

#Graphing Ratio
ggplot(SnippedDf)+ geom_col(aes(Year, Ratio), alpha = 0.6) + 
  geom_smooth(aes(Year, Ratio), se = F, color = "firebrick1") +
  theme_bw()

#Graphing difference in Ratio's
ggplot(SnippedDf)+ geom_col(aes(Year, difference), alpha = 0.6) + 
  geom_smooth(aes(Year, difference), se = F, color = "firebrick1") +
  theme_bw()

#Graphing differences - %change

ggplot(SnippedDf)+ geom_col(aes(Year, pct_change), alpha = 0.6) + 
  geom_smooth(aes(Year, pct_change), se = F, color = "firebrick1") +
  theme_bw()