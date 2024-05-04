library(tidyverse)
library(ggplot2)
library(plotly)

bigPapa <- read.csv('../data/CleanedData/bigPapa.csv')
head(bigPapa)

unique(bigPapa$Item)

ofInterest <- c("Average dietary energy supply adequacy (percent) (3-year average)", 
                "Average protein supply (g/cap/day) (3-year average)", 
                "Gross domestic product per capita, PPP, (constant 2017 international $)",
                "Prevalence of undernourishment (percent) (3-year average)", 
                "Prevalence of severe food insecurity in the male adult population (percent) (3-year average)", 
                "Prevalence of severe food insecurity in the female adult population (percent) (3-year average)", 
                "Political stability and absence of violence/terrorism (index)", 
                "Prevalence of obesity in the adult population (18 years and older)", 
                "Average dietary energy requirement (kcal/cap/day)")

dtPapa <- bigPapa %>%
  filter(Item %in% ofInterest)

unique(dtPapa$Item)

head(dtPapa)

write.csv(dtPapa, "../data/CleanedData/datatableversion.csv")
