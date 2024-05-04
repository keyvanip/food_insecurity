library(tidyverse)

#initial read in
europeNA <- read.csv('data/RawData/Europe+NA/Food_Security_Data_E_Northern_America_and_Europe_NOFLAG.csv')
africa <- read.csv('data/RawData/Africa/Food_Security_Data_E_Africa_NOFLAG.csv')

colony <- read.csv('data/colonydata.csv')

#add region code for ease of comparison later
europeNA <- europeNA %>% 
  mutate('Region' = 'Europe')
africa <- africa %>%
  mutate('Region' = 'Africa' )

#filter out North American countries
noThanks <- c("Canada", "Bermuda", "Greenland", "United States of America")
europe <- europeNA %>%
  filter(!(Area %in% noThanks))

#merge em for pulling out metrics
bigPapa <- rbind(europe, africa)

#add colonial info to African countries
bigPapa <- merge(
  x = bigPapa, 
  y = colony,
  by.x = 'Area',
  by.y = 'AfricanCountry',
  all.x = TRUE)

#drop unnecessary cols after merge
bigPapa <- subset(bigPapa, select = -c(Empire1, Empire2, Empire3, NotesOnDecisionOfPrimary, X))

print(unique(bigPapa$Item))

#lists that make life easier
#for dropping unneccessary cols
excludeInThreeYearAvg <- c("Area.Code", "Area.Code..M49.", "Item.Code", "Item", "Element.Code", 
                  "Element", "Unit", "Y2000", "Y2001", "Y2002", "Y2003", "Y2004", "Y2005",
                  "Y2006", "Y2007", "Y2008", "Y2009", "Y2010", "Y2011", "Y2012", "Y2013", "Y2014",
                  "Y2015", "Y2016", "Y2017", "Y2018", "Y2019", "Y2020", "Y2021", "Y2022")
excludeInYearly <- c("Area.Code", "Area.Code..M49.", "Item.Code", "Item", "Element.Code", 
                    "Element", "Unit", "Y20002002", "Y20012003", 'Y20022004', "Y20032005", "Y20042006", "Y20052007", "Y20062008",
                    "Y20072009", "Y20082010", "Y20092011", "Y20102012", "Y20112013", "Y20122014", "Y20132015", 
                    "Y20142016", "Y20152017", "Y20162018", "Y20172019", "Y20182020", "Y20192021", "Y20202022")

write.csv(bigPapa, "data/CleanedData/bigPapa.csv")

# ------------------------------
# DIETARY ENERGY ADEQUACY
# ------------------------------

#initial filter
energyAdequacy <- bigPapa %>%
  filter(Item == "Average dietary energy supply adequacy (percent) (3-year average)")  

head(energyAdequacy)

#get rid of unnecessary columns
energyAdequacy <- energyAdequacy %>%
  select (-all_of(excludeInThreeYearAvg))

head(energyAdequacy)

#pivot
energyPapa <- energyAdequacy %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_Percent"
  )

head(energyPapa)

#export
write.csv(energyPapa, "data/CleanedData/energyadequacy.csv")

# ------------------------------
# RAIL DENSITY
# ------------------------------

#initial filter
railLine <- bigPapa %>%
  filter(Item == "Rail lines density (total route in km per 100 square km of land area)")  

head(railLine)

#drop extra cols
railLine <- railLine %>%
  select (-all_of(excludeInYearly))

head(railLine)

#pivot
railPapa <- railLine %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "RailDensity(kmTrack/100sqkmLand)"
  )

head(railPapa)

#export
write.csv(railPapa, "data/CleanedData/raildensity.csv")

# ------------------------------
# GDP PER CAPITA
# ------------------------------

#initial filter
gdp <- bigPapa %>%
  filter(Item == "Gross domestic product per capita, PPP, (constant 2017 international $)")  

head(gdp)

#drop extra cols
gdp <- gdp %>%
  select (-all_of(excludeInYearly))

head(gdp)

#pivot
gdPapa <- gdp %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "GDP_PerCapita"
  )

head(gdPapa)

#export
write.csv(gdPapa, "data/CleanedData/gdp.csv")


# ------------------------------
# UNDERNOURISHMENT
# ------------------------------

#initial filter
undernourish <- bigPapa %>%
  filter(Item == "Prevalence of undernourishment (percent) (3-year average)")  

head(undernourish)

#get rid of unnecessary columns
undernourish <- undernourish %>%
  select (-all_of(excludeInThreeYearAvg))

head(undernourish)

#pivot
underPapa <- undernourish %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_Percent"
  )

head(underPapa)

#export
write.csv(underPapa, "data/CleanedData/undernourishment.csv")

# ------------------------------
# CEREAL IMPORT RATIO
# ------------------------------

#initial filter
cerealImport <- bigPapa %>%
  filter(Item == "Cereal import dependency ratio (percent) (3-year average)")  

head(cerealImport)

#get rid of unnecessary columns
cerealImport <- cerealImport %>%
  select (-all_of(excludeInThreeYearAvg))

head(cerealImport)

#pivot
cerealPapa <- cerealImport %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_ImportDependencyRatio"
  )

head(cerealPapa)

#export
write.csv(cerealPapa, "data/CleanedData/cerealimportratio.csv")


# ------------------------------
# FOOD INSECURITY
# ------------------------------

#initial filter
femaleInsecurity <- bigPapa %>%
  filter(Item == "Prevalence of severe food insecurity in the female adult population (percent) (3-year average)") %>%
  filter(Element == 'Value')
maleInsecurity <- bigPapa %>%
  filter(Item == "Prevalence of severe food insecurity in the male adult population (percent) (3-year average)") %>%
  filter(Element == 'Value')

head(femaleInsecurity)
head(maleInsecurity)

#get rid of unnecessary columns
femaleInsecurity <- femaleInsecurity %>%
  select (-all_of(excludeInThreeYearAvg))
maleInsecurity <- maleInsecurity %>%
  select (-all_of(excludeInThreeYearAvg))

head(femaleInsecurity)
head(maleInsecurity)

#pivot
femalePapa <- femaleInsecurity %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_PercentFemale"
  )
malePapa <- maleInsecurity %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_PercentMale"
  )

head(femalePapa)
head(malePapa)

#bind
insecurityPapa <- merge(
  x = femalePapa,
  y = malePapa,
  by = c("Area","Region","YearPeriod", "PrimaryColonizer"),
  all = TRUE
)

head(insecurityPapa)

#export
write.csv(insecurityPapa, "data/CleanedData/foodinsecuritybygender.csv")

# ------------------------------
# OBESITY IN ADULTS
# ------------------------------

#intial filter
obesity <- bigPapa %>%
  filter(Item == "Prevalence of obesity in the adult population (18 years and older)")  

head(obesity)

#drop extra cols
obesity <- obesity %>%
  select (-all_of(excludeInYearly))

#pivot
obesityPapa <- obesity %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "Percent"
  )

head(obesityPapa)

#export
write.csv(obesityPapa, "data/CleanedData/obesityadults.csv")

# ------------------------------
# POLITICAL STABILITY
# ------------------------------

#initial filter
political <- bigPapa %>%
  filter(Item == "Political stability and absence of violence/terrorism (index)")  

head(political)

#drop extra cols
political <- political %>%
  select (-all_of(excludeInYearly))

#pivot
politicalPapa <- political %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'Year', 
    names_prefix = 'Y',
    values_to = "Index"
  )

head(politicalPapa)

#export
write.csv(politicalPapa, "data/CleanedData/politicalstability.csv")

# ------------------------------
# PERCENT CHILDREN UNDER 5 STUNTED VS OVERWEIGHT
# ------------------------------

#initial filter
stuntedChildren <- bigPapa %>%
  filter(Item == "Percentage of children under 5 years of age who are stunted (modelled estimates) (percent)")
overweightChildren <- bigPapa %>%
  filter(Item == "Percentage of children under 5 years of age who are overweight (modelled estimates) (percent)")

head(stuntedChildren)
head(overweightChildren)

#get rid of unnecessary columns
stuntedChildren <- stuntedChildren %>%
  select (-all_of(excludeInYearly))
overweightChildren <- overweightChildren %>%
  select (-all_of(excludeInYearly))

head(stuntedChildren)
head(overweightChildren)

#pivot
stuntedPapa <- stuntedChildren %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_PercentStunted"
  )
overweightPapa <- overweightChildren %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_PercentOverweight"
  )

head(stuntedPapa)
head(overweightPapa)

#bind
underFivePapa <- merge(
  x = stuntedPapa,
  y = overweightPapa,
  by = c("Area","Region","YearPeriod", "PrimaryColonizer"),
  all = TRUE
)

head(underFivePapa)

#export
write.csv(underFivePapa, "data/CleanedData/childrenunderfiveoverweightstunted.csv")

# ------------------------------
# ENERGY FROM CEREALS
# ------------------------------

#initial filter
cerealEnergy <- bigPapa %>%
  filter(Item == "Share of dietary energy supply derived from cereals, roots and tubers (kcal/cap/day) (3-year average)")  

head(cerealEnergy)

#get rid of unnecessary columns
cerealEnergy <- cerealEnergy %>%
  select (-all_of(excludeInThreeYearAvg))

head(cerealEnergy)

#pivot
cerealPapaTwo <- cerealEnergy %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_kcal/cap/day"
  )

head(cerealPapaTwo)

#export
write.csv(cerealPapaTwo, "data/CleanedData/energyfromcereal.csv")

# ------------------------------
# PROTEIN
# ------------------------------

#initial filter
proteinSupply <- bigPapa %>%
  filter(Item == "Average protein supply (g/cap/day) (3-year average)")
proteinAnimal <- bigPapa %>%
  filter(Item == "Average supply of protein of animal origin (g/cap/day) (3-year average)")

head(proteinSupply)
head(proteinAnimal)

#get rid of unnecessary columns
proteinSupply <- proteinSupply %>%
  select (-all_of(excludeInThreeYearAvg))
proteinAnimal <- proteinAnimal %>%
  select (-all_of(excludeInThreeYearAvg))

head(proteinSupply)
head(proteinAnimal)

#pivot
supplyPapa <- proteinSupply %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "TotalProtein(AVG_g/cap/day)"
  )
animalPapa <- proteinAnimal %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AnimalProtein(AVG_g/cap/day)"
  )

proteinPapa <- merge(
  x = supplyPapa,
  y = animalPapa,
  by = c("Area","Region","YearPeriod", "PrimaryColonizer"),
  all = TRUE
)

head(proteinPapa)

#export
write.csv(proteinPapa, "data/CleanedData/proteinsupply.csv")

# ------------------------------
# FAT SUPPLY
# ------------------------------

#initial filter
fatSupply <- bigPapa %>%
  filter(Item == "Average fat supply (g/cap/day) (3-year average)")  

head(fatSupply)

#get rid of unnecessary columns
fatSupply <- fatSupply %>%
  select (-all_of(excludeInThreeYearAvg))

head(fatSupply)

#pivot
fatPapa <- fatSupply %>%
  pivot_longer(
    cols = starts_with("Y"),
    names_to = 'YearPeriod', 
    values_to = "AVG_g/cap/day"
  )

head(fatPapa)

#export
write.csv(fatPapa, "data/CleanedData/fatsupply.csv")


