library(ggplot2)
library(visdat)
library(plotly)
library(scales)
library(crosstalk)

# ------------------------------
# BASIC THEMEING (not final!!)
# ------------------------------

#themeing

pointsPal <- c("#248232","#4B3F72","#DDC4DD","#3E92CC","#A3320B","#D2BF55","#B33C86", "#2CF6B3") #reodering slightly + new colors
background <- "#FCFFFC"
accent <- "#F3F4EC"
text <- "#040F0F"
borders <- "#2d2a3a"
highMedLow <- c("#3E92CC", "#B33C86", "#DDC4DD")
gender <- c("#B33C86","#3E92CC")

IrnTheme <- theme_minimal() + theme(
  text = element_text(family = "Palatino", colour = text), #sets the color and family of all text
  line = element_line(color = borders),
  panel.background = element_rect(fill = background), #sets the background fill to that eggshell that sets of the points
  panel.border = element_rect(color = borders, fill = NA), #makes sure there's a gunmetal border
  panel.grid.major = element_line(color = accent), #fills in the major lines with parchment
  panel.grid.minor = element_line(color = accent, linetype = "dashed"), #again parchment but this time dashed
  plot.title = element_text(size = 22, hjust = 0, lineheight = 1.2, face = "bold"), #setting size and face of title
  plot.subtitle = element_text(size = 18, hjust = 0, lineheight = 1), #subtitle
  axis.title.x = element_text(size = 16, vjust = 0), #trying to drop the x axis label slightly
  axis.title.y = element_text(size = 16, angle = 90), #rotating y axis label
  axis.text = element_text(family = "Palatino"), #I got the idea of using different font families from Nathan Yau
  legend.title = element_text(size = 18), #set legend title size
  legend.key = element_rect(fill = background), #key has the same fill as the background
  legend.background = element_rect(fill = accent), #not sure I like how dark this is but the eggshell was washed out
  legend.position = "right", #defaults legend to the right side
  axis.ticks = element_blank(), #default to no axis ticks! Just a bit cleaner,
  strip.text = element_text(size = 16), #for if multiple facets, controls title size
  strip.background = element_rect(fill = accent), #provides a fill for facet backgrounds (I think?????)
  #aspect.ratio = 9/16 #I prefer the 16:9 ratio over 4:3, because its a bit wider and more rectangular rather than being just a slightly off square
)

theme_set(IrnTheme)

# ------------------------------
# VISUALIZING MISSING DATA
# ------------------------------

cerealImport <- read.csv('data/CleanedData/cerealimportratio.csv')

head(cerealImport)

vis_miss(cerealImport)

#cereal import measurement stops at 2020 (so year period 2018-2020), 
#but appears to have data for most all countries

kids <- read.csv("data/CleanedData/childrenunderfiveoverweightstunted.csv")

head(kids)

vis_miss(kids)

#missing overweight numbers for cabo verde, underweight numbers from Russia and UK

energy <- read.csv("data/CleanedData/energyadequacy.csv")

head(energy)

vis_miss(energy)

#really clean, only missing south sudan

energyCereal <- read.csv("data/CleanedData/energyfromcereal.csv")

head(energyCereal)

vis_miss(energyCereal)

#same as before, no numbers after 2020

fat <- read.csv("data/CleanedData/fatsupply.csv")

head(fat)

vis_miss(fat)

listMissingFat <- fat %>%
  filter(is.na(AVG_g.cap.day)) %>%
  distinct(Area, YearPeriod) %>%
  filter(YearPeriod != "Y20192021") %>%
  filter(YearPeriod != "Y20202022")

print(listMissingFat)

#once again, nothing after 2020, plus some missing countries...
#this is seeming much finickier
#So, we are missing up to 2005/2008 from Montenegro and Serbia
#everything pre 2014/2016 from Burundi
#everything pre 2010/2012 from Comoros, DR of the Congo, Libya, and the Seychelles
#and everything pre 2011/2013 from Sudan
#that makes sense, there was a fair bit of conflict in those areas during those years...
#but, if the time is from 2014/2016 to 2018/2020, got everything on everywhere

insecurityGender <- read.csv("data/CleanedData/foodinsecuritybygender.csv")

head(insecurityGender)

vis_miss(insecurityGender)

#ok, thats wrong. Let's take another look

insecurityGender <- insecurityGender %>%
  mutate(AVG_PercentFemale = na_if(AVG_PercentFemale, "")) %>%
  mutate(AVG_PercentMale = na_if(AVG_PercentMale, ""))

head(insecurityGender)

vis_miss(insecurityGender)

#ok, thats a TON missing. Let's look at the years...

listMissingGender <- insecurityGender %>%
  filter(is.na(AVG_PercentFemale)) %>%
  distinct(YearPeriod)

print(listMissingGender)

#great, we have stuff missing from literally every year
#but, the upside seems to be if there is data for one gender, there is data from both

insecurityClean <- insecurityGender %>%
  filter(!is.na(AVG_PercentFemale))

vis_miss(insecurityClean)

yearsWith <- insecurityClean %>%
  distinct(YearPeriod) %>%
  pull()

print(yearsWith)
#k, looks like a few that have data

insecuritySubset <- insecurityGender %>%
  filter(YearPeriod %in% yearsWith)

head(insecuritySubset)

vis_miss(insecuritySubset)

#ok, still not great! Let's look at the countries...

insecurityCountries <- insecuritySubset %>%
  filter(is.na(AVG_PercentFemale)) %>%
  filter(YearPeriod == 'Y20192021') %>%
  distinct(Area, YearPeriod)

print(insecurityCountries)

#this is so baaaaad ok we are just gonna throw away gender!!
#only really useful for 2019, and even then you miss about a dozen countries
#only use as a sprinkling and on specific country comparisons


# ------------------------------
# Random EDAs
# ------------------------------

head(bigPapa)

smallPapa <- bigPapa %>%
  filter(Region == 'Africa') %>%
  select(Area, PrimaryColonizer)

smallPapa <- unique(smallPapa)

smallPapa$PrimaryColonizer <- replace(smallPapa$PrimaryColonizer, smallPapa$PrimaryColonizer == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
smallPapa$PrimaryColonizer <- replace(smallPapa$PrimaryColonizer, smallPapa$PrimaryColonizer == 'Netherlands (Kingdom of the)', 'The Netherlands')

totalColonies <- subset(colony, select = c(Empire1,Empire2,Empire3))

totalColonies <- totalColonies %>%
  mutate(Empire1 = na_if(Empire1, "")) %>%
  mutate(Empire2 = na_if(Empire2, "")) %>%
  mutate(Empire3 = na_if(Empire3, ""))

emp1 <- as.vector(unlist(totalColonies$Empire1))
emp2 <- as.vector(na.omit(unlist(totalColonies$Empire2)))
emp3 <- as.vector(na.omit(unlist(totalColonies$Empire3)))

tot <- c(emp1,emp2,emp3)
tot <- replace(tot, tot == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
tot <- replace(tot, tot == 'Netherlands (Kingdom of the)', 'Netherlands')
tot <- (table(tot))

print(tot)
tot <- as.data.frame(tot)

colonizers <- ggplot(
  data = tot,
  aes(x = reorder(tot, -Freq), y = Freq)
) +
  geom_bar(stat = 'identity')

(colonizers)

politicalPapa$Index <- as.numeric(politicalPapa$Index)
politicalPapa$Year <- as.numeric(politicalPapa$Year)

polStab <- ggplot(
  data = politicalPapa %>% filter(Region == 'Africa') %>% filter(Year != 2000 & Year != 2001),
  aes(
    x = Year,
    y = Index
    )
) +
  geom_point()+
  geom_line(aes(group = Area)) +
  facet_wrap(~ PrimaryColonizer, nrow = 3, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

polStab

#bleck!! try something else

#manually added "core" year to following energy adequacy csv 
#in order to link to political data and gdp, since they are not 3 year periods

eneAdMan <- read.csv("data/CleanedData/energyadequacy_manualedit.csv")

head(eneAdMan)

eneAdMan <- subset(eneAdMan, select = -c(X, YearPeriod))

colnames(eneAdMan) <- c("Area", "Region", "PrimaryColonizer", "AVG_Percent", "Year")

polPlusGDP <- merge(
  x = politicalPapa,
  y = gdPapa,
  by = c('Area', 'Region', 'PrimaryColonizer','Year')
)

polPlusGDP <- merge(
  x = polPlusGDP,
  y = eneAdMan,
  by = c('Area', 'Region', 'PrimaryColonizer', 'Year')
)
polPlusGDP <- polPlusGDP %>% filter(Year != 2000 & Year != 2001 & Year != 2022)

head(polPlusGDP)

polPlusGDP$GDP_PerCapita = as.numeric(polPlusGDP$GDP_PerCapita)

avgPolGDP <- polPlusGDP %>%
  filter(!is.na(GDP_PerCapita)) %>%
  filter(!is.na(Index)) %>%
  group_by(Region, Year) %>%
  summarise(AVG_Percent = mean(GDP_PerCapita),
            AVG_Index = mean(Index)) %>%
  mutate('Area' = Region) %>%
  mutate(Region = paste("Continent of\n", Region))

print(avgPolGDP)

avgPolGDP <- as.data.frame(avgPolGDP)

polGDP <- ggplot(
  data = polPlusGDP,
  aes(x = GDP_PerCapita, 
      y = Index, 
      color = Region, 
      text = Area,
      frame = Year)
) +
  geom_point(size = 3, alpha = 0.7) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = 'Political Stability Index vs GDP Per Capita',
    subtitle = 'In Europe and Africa',
    x = 'GDP Per Capita',
    y = 'Political Stability Index',
    caption = 'Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nGDP per capita is calculated in constant international 2017 dollars,\nand is based on purchasing power parity (PPP).'
  ) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(labels = label_dollar()) +
  geom_hline(yintercept = 0, color = 'grey')+
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f")) +
  geom_point(data = avgPolGDP,
             aes(x = AVG_Percent, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  )

(polGDP)

plotlyPolGDP <- ggplotly(polGDP, tooltip = "text")

(plotlyPolGDP)

## political stability and energy adequacy

yearKey <- data.frame(
  Year = c(2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
           2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,
           2020,2021,2022),
  YearPeriod = c(NA, "Y20002002", "Y20012003", 'Y20022004', "Y20032005", "Y20042006", "Y20052007", "Y20062008",
                 "Y20072009", "Y20082010", "Y20092011", "Y20102012", "Y20112013", "Y20122014", "Y20132015", 
                 "Y20142016", "Y20152017", "Y20162018", "Y20172019", "Y20182020", "Y20192021", "Y20202022", NA)
)

head(yearKey)


polLag <- politicalPapa %>%
  group_by(Area) %>%
  mutate(AVG_Index = (lag(Index) + Index + lead(Index)) / 3.0) %>%
  filter(!is.na(AVG_Index))

head(polLag)

polLag <- merge(
  x = polLag,
  y = yearKey,
  by = 'Year',
  all.x = TRUE
)

yearper <- c("2000-2002", "2001-2003", '2002-2004', "2003-2005", "Y20042006", "Y20052007", "Y20062008",
             "2007-2009", "2008-2010", "2009-2011", "2010-2012", "Y20112013", "Y20122014", "Y20132015", 
             "2014-2016", "2015-2017", "2016-2018", "2017-2019", "Y20182020", "Y20192021", "Y20202022", NA)

polLag <- polLag %>% arrange(Area)

head(polLag)

polLag <- subset(polLag, select = -c(Index, Year))

head(polLag)

head(energyPapa)

polEn <- merge(
  x = polLag,
  y = energyPapa,
  by = c("Area", "Region", "PrimaryColonizer", "YearPeriod"),
  all.x = TRUE
)

polEn <- polEn %>%
  mutate(YearPeriod = )

head(polEn)

polEn$AVG_Percent <- as.numeric(polEn$AVG_Percent)

avg_data <- bigMergePapa %>%
  filter(!is.na(AVG_Percent)) %>%
  filter(!is.na(AVG_Index)) %>%
  group_by(Region, YearPeriod) %>%
  summarise(AVG_Percent = mean(AVG_Percent),
            AVG_Index = mean(AVG_Index)) %>%
  mutate('Area' = Region) %>%
  mutate(Region = paste("Continent of\n", Region))

print(avg_data)

avg_data <- as.data.frame(avg_data)

head(polEn)
head(proteinPapa)

polEn$PrimaryColonizer <- replace(polEn$PrimaryColonizer, polEn$PrimaryColonizer == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
polEn$PrimaryColonizer <- replace(polEn$PrimaryColonizer, polEn$PrimaryColonizer == 'Netherlands (Kingdom of the)', 'The Netherlands')

polEn$Area <- replace(polEn$Area, polEn$Area == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
polEn$Area <- replace(polEn$Area, polEn$Area == 'Netherlands (Kingdom of the)', 'The Netherlands')

proteinPapa$PrimaryColonizer <- replace(proteinPapa$PrimaryColonizer, proteinPapa$PrimaryColonizer == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
proteinPapa$PrimaryColonizer <- replace(proteinPapa$PrimaryColonizer, proteinPapa$PrimaryColonizer == 'Netherlands (Kingdom of the)', 'The Netherlands')

proteinPapa$Area <- replace(proteinPapa$Area, proteinPapa$Area == 'United Kingdom of Great Britain and Northern Ireland', 'UK')
proteinPapa$Area <- replace(proteinPapa$Area, proteinPapa$Area == 'Netherlands (Kingdom of the)', 'The Netherlands')

head(polEn)

head(proteinPapa)
colnames(proteinPapa) <- c('X', 'Area', 'Region', 'YearPeriod', 'PrimaryColonizer', 'TotalProtein', 'AnimalProtein', 'NonAnimalProtein')

proteinPapa <- proteinPapa %>%
  mutate(NonAnimalProtein = TotalProtein - AnimalProtein)

bigMergePapa <- merge(
  x = polEn, 
  y = proteinPapa,
  by = c('Area', 'Region', 'PrimaryColonizer', 'YearPeriod'),
  all = TRUE
)

bigMergePapa <- subset(bigMergePapa, select = -c(PrimaryColonizer, X))

head(bigMergePapa)
bigMergePapa$AVG_Percent <- as.numeric(bigMergePapa$AVG_Percent)

summary(bigMergePapa)

bigMergePapa <- na.omit(bigMergePapa)

stabilityVsEnergy <- ggplot(
  data = bigMergePapa,
  aes(x = (AVG_Percent)/100, 
      y = AVG_Index, 
      color = Region, 
      text = Area,
      frame = YearPeriod)
) +
  geom_vline(xintercept = 1, color = 'grey') +
  geom_hline(yintercept = 0, color = 'grey') +
  geom_point(size = 3, alpha = 0.7) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(.40, 1.60), labels = label_percent()) +
  labs(
    title = "Political Stability Index vs Protein Supply and Adequacy of Energy Supply",
    subtitle = "Europe vs Africa (3 Year Average)",
    y = "Political Stability Index",
    x = "Dietary Energy Supply Adequacy",
    caption = "Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nDietary energy supply adequacy of 100% indicates that the average person in the country receives exactly the necessary energy from their diet."
  )+
  geom_point(data = avg_data,
             aes(x = (AVG_Percent)/100, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  ) + 
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f"))

(stabilityVsEnergy)

plotlypolEn<- ggplotly(stabilityVsEnergy, tooltip = "text")

#axis shifting so it's not so scrunched.
plotlypolEn <- plotlypolEn %>%
  layout(xaxis = list(
    title = list(text = "Dietary Energy Supply Adequacy",
                 standoff = 10)
  )) %>%
  layout(yaxis = list(
    title = list(text = "Political Stability Index",
                 standoff = 5)
  ))

(plotlypolEn)

head(bigMergePapa)

avgProtein <- bigMergePapa %>%
  filter(!is.na(TotalProtein)) %>%
  group_by(Region, YearPeriod) %>%
  summarise(AVG_Protein = mean(TotalProtein),
            AVG_Index = mean(AVG_Index)) %>%
  mutate('Area' = Region) %>%
  mutate(Region = paste("Continent of\n", Region))

print(avgProtein)

avgProtein <- as.data.frame(avgProtein)


proteinScatter <- ggplot(
  data = bigMergePapa,
  aes(x = TotalProtein, #AnimalProtein, 
      y = AVG_Index, #NonAnimalProtein,
      color = Region, 
      text = Area,
      frame = YearPeriod)
) + 
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = avgProtein,
             aes(x = AVG_Protein, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  ) + 
  scale_y_continuous(limits = c(-2.5,2.5)) +
  theme(legend.position = "none") + 
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f"))+
  labs(
    title = "Political Stability Index vs Protein Supply and Adequacy of Energy Supply",
    subtitle = "Europe vs Africa (3 Year Average)",
    y = "Political Stability Index",
    x = "Protein Supply (Grams/Capita/Day)",
    caption = "Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nDietary energy supply adequacy of 100% indicates that the average person in the country receives exactly the necessary energy from their diet."
  ) +
  geom_hline(yintercept = 0, color = 'grey')
  

(proteinScatter)

plotlyProtein <- ggplotly(proteinScatter, tooltip = "text", show_legend = FALSE)

(plotlyProtein)

hopefulLinked <- subplot(
  plotlyProtein,
  plotlypolEn,
  nrows = 1
)

hopefulLinked <- hopefulLinked %>%
  highlight(
    on = "plotly_selecting",
    selectize = TRUE,
    opacityDim = 0.1,
  )


(hopefulLinked)

#colonial <- ggplot(
#  data = polEn %>% 
#    filter(!is.na(PrimaryColonizer)) %>% 
#    distinct(Area, Region, PrimaryColonizer),
#  aes(x = PrimaryColonizer)
#) +
#  geom_bar() +
#  coord_flip() +
#  labs(
#    title = "Primary Colonizers of African Nations",
#    subtitle = "",
#    y = "Number of Colonies",
#    x = "European Colonial Powers"
#  )

#(colonial)

#plotlyColonial <- ggplotly(colonial)

#(plotlyColonial)

#failedlinked <- subplot(
#  plotlyColonial,
#  plotlypolEn,
#  nrows = 1)

#(linked)
