library(ggplot2)
library(visdat)
library(plotly)
library(scales)
library(crosstalk)

##THEMEING
background <- "#FCFFFC"
accent <- "#F0F8FF"
text <- "#1c0b19"
borders <- "#11270b"

#adapted heavily from Irn theme
vizProjTheme <- theme_minimal() + theme(
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
  axis.text = element_text(family = "Palatino"),
  legend.title = element_text(size = 18), #set legend title size
  legend.key = element_rect(fill = background), #key has the same fill as the background
  legend.background = element_rect(fill = background), #not sure I like how dark this is but the eggshell was washed out
  legend.position = "right", #defaults legend to the right side
  axis.ticks = element_blank(), #default to no axis ticks! Just a bit cleaner,
  strip.text = element_text(size = 16), #for if multiple facets, controls title size
  strip.background = element_rect(fill = accent), #provides a fill for facet backgrounds (I think?????)
  #aspect.ratio = 9/16 #I prefer the 16:9 ratio over 4:3, because its a bit wider and more rectangular rather than being just a slightly off square
)

theme_set(vizProjTheme)

##Linked plot: stability vs energy against stability vs protein supply

#bigMergePapa <- bigMergePapa %>%
#  rename("Year Period (3 Year Average)" = YearPeriod)

head(bigMergePapa)

#colnames(bigMergePapa) <- c("Area", "Region", "YearPeriod", "AVG_Index", "AVG_Percent", "TotalProtein", "AnimalProtein", "NonAnimalProtein")

bigMergePapa$YearPeriod <- sub("^Y", "", bigMergePapa$YearPeriod)
bigMergePapa$YearPeriod <- sub("(\\d{4})(\\d{4})", "\\1-\\2", bigMergePapa$YearPeriod)

avg_data$YearPeriod <- sub("^Y", "", avg_data$YearPeriod)
avg_data$YearPeriod <- sub("(\\d{4})(\\d{4})", "\\1-\\2", avg_data$YearPeriod)

head(avg_data)

stabilityVsEnergy <- ggplot(
  data = bigMergePapa,
  aes(x = (AVG_Percent)/100, 
      y = AVG_Index, 
      color = Region, 
      text = Area,
      frame = YearPeriod)
) +
  geom_vline(xintercept = 1, color = 'darkgrey') +
  geom_hline(yintercept = 0, color = 'darkgrey') +
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
    title = list(text = "",
                 standoff = 5)
  ))

(plotlypolEn)

avgProtein <- bigMergePapa %>%
  filter(!is.na(TotalProtein)) %>%
  group_by(Region, YearPeriod) %>%
  summarise(AVG_Protein = mean(TotalProtein),
            AVG_Index = mean(AVG_Index)) %>%
  mutate('Area' = Region) %>%
  mutate(Region = paste("Continent of\n", Region))

print(avgProtein)

avgProtein <- as.data.frame(avgProtein)


head(bigMergePapa)

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
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f"))+
  labs(
    title = "Political Stability Index vs Protein Supply and Adequacy of Energy Supply",
    subtitle = "Europe vs Africa (3 Year Average)",
    y = "Political Stability Index",
    x = "Protein Supply (Grams/Capita/Day)",
    caption = "Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nDietary energy supply adequacy of 100% indicates that the average person in the country receives exactly the necessary energy from their diet."
  ) +
  geom_hline(yintercept = 0, color = 'darkgrey')


(proteinScatter)

plotlyProtein <- ggplotly(proteinScatter, tooltip = "text", show_legend = FALSE)

#axis shifting so it's not so scrunched.
plotlyProtein <- plotlyProtein %>%
  layout(xaxis = list(
    title = list(text = "Protein Supply (Grams/Capita/Day)",
                 standoff = 10)
  )) %>%
  layout(yaxis = list(
    title = list(text = "Political Stability Index",
                 standoff = 5)
  ))

(plotlyProtein)

#plotlyProtein <-plotlyProtein %>% layout(showlegend = FALSE)


hopefulLinked <- subplot(
  plotlyProtein,
  plotlypolEn,
  titleX = TRUE,
  #shareY = TRUE, #I prefer having doubled marks for ease of readability
  titleY = TRUE,
  nrows = 1
)

hopefulLinked <- hopefulLinked %>%
  highlight(
    on = "plotly_selecting",
    selectize = TRUE,
    opacityDim = 0.1,
  )
  

(hopefulLinked)

#version with ONLY countries with full years present

unique(bigMergePapa$Area)

incomplete <- c("Comoros", "Democratic Republic of the Congo", "Libya", "Montenegro", 
                "Serbia", "Seychelles", "South Sudan", "Sudan")

selectPapa <- bigMergePapa %>%
  filter(!Area %in% incomplete)

unique(selectPapa$Area)

selectPapa <- selectPapa %>%
  rename('ThreeYearAveragePeriod' = YearPeriod)

head(selectPapa)

selectAvgProtein <- avgProtein %>%
  rename('ThreeYearAveragePeriod' = YearPeriod)

selectAvgEn <- avg_data %>%
  rename('ThreeYearAveragePeriod' = YearPeriod)

stabilityVsEnergySelect <- ggplot(
  data = selectPapa,
  aes(x = (AVG_Percent)/100, 
      y = AVG_Index, 
      color = Region, 
      text = Area,
      frame = ThreeYearAveragePeriod)
) +
  geom_vline(xintercept = 1, color = 'darkgrey') +
  geom_hline(yintercept = 0, color = 'darkgrey') +
  geom_point(size = 3, alpha = 0.7) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(.40, 1.60), labels = label_percent()) +
  labs(
    title = "Political Stability Index vs Per Capita Food Supply",
    subtitle = "Europe vs Africa (3 Year Moving Average)",
    y = "Political Stability Index",
    x = "Dietary Energy Adequacy",
    caption = "Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nDietary energy supply adequacy of 100% indicates that the average person in the country receives exactly the necessary energy from their diet."
  )+
  geom_point(data = selectAvgEn,
             aes(x = (AVG_Percent)/100, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  ) + 
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f"))

(stabilityVsEnergySelect)

plotlypolEnSelect<- ggplotly(stabilityVsEnergySelect, tooltip = "text")

#axis shifting so it's not so scrunched.
plotlypolEnSelect <- plotlypolEnSelect %>%
  layout(xaxis = list(
    title = list(text = "Dietary Energy Adequacy",
                 standoff = 10)
  )) %>%
  layout(yaxis = list(
    title = list(text = "",
                 standoff = 5)
  ))

energyAno <- list(
  list(
    xref = 'x',
    yref = 'y',
    x = 0.98,
    y = 2.5,
    text = "<b><----</b> \t Energy from diet\n<i>insufficient</i> for average person",
    xanchor = "right",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  ),
  list(
    xref = 'x',
    yref = 'y',
    x = 1.02,
    y = 2.5,
    text = "Energy from diet \t <b>----></b>\n<i>sufficient</i> for average person",
    xanchor = "left",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  ),
  list(
    #xref = 'x',
    #yref = 'y',
    x = .4,
    y = 0.75,
    text = "Perceived as\n<i>politically stable</i>",
    xanchor = "left",
    yanchor = "bottom",
    font = list(color = "#11270b", size = 8),
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = 30
  )
  ,
  list(
    x = .4,
    y = -0.75,
    text = "Perceived as\n<i>politically unstable</i>",
    xanchor = "left",
    yanchor = "top",
    font = list(color = "#11270b", size = 8),
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = -30
  )
)

plotlypolEnSelect <- plotlypolEnSelect %>%
  layout(
    annotations = energyAno
  )

(plotlypolEnSelect)

proteinScatterSelect <- ggplot(
  data = selectPapa,
  aes(x = TotalProtein, #AnimalProtein, 
      y = AVG_Index, #NonAnimalProtein,
      color = Region, 
      text = Area,
      frame = ThreeYearAveragePeriod)
) + 
  geom_point(size = 3, alpha = 0.7) +
  geom_point(data = selectAvgProtein,
             aes(x = AVG_Protein, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  ) + 
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(limits = c(30, 150)) +
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f"))+
  labs(
    title = "Political Stability Index vs Per Capita Food Supply",
    subtitle = "Europe vs Africa (3 Year Moving Average)",
    y = "Political Stability Index",
    x = "Protein (g/day)",
    caption = "Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nDietary energy supply adequacy of 100% indicates that the average person in the country receives exactly the necessary energy from their diet."
  ) +
  geom_hline(yintercept = 0, color = 'darkgrey')


(proteinScatterSelect)

plotlyProteinSelect <- ggplotly(proteinScatterSelect, tooltip = "text", show_legend = FALSE)

#axis shifting so it's not so scrunched.
plotlyProteinSelect <- plotlyProteinSelect %>%
  layout(xaxis = list(
    title = list(text = "Protein (g/day)",
                 standoff = 10)
  )) %>%
  layout(yaxis = list(
    title = list(text = "Political Stability Index",
                 standoff = 5)
  ))

proteinAnno <- list(
  list(
    #xref = 'x',
    #yref = 'y',
    x = 150,
    y = 0.75,
    #text = "Perceived as\n<i>politically stable</i>",
    xanchor = "left",
    yanchor = "top",
    #font = list(color = "#11270b", size = 8),
    #showarrow = FALSE
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = 30
  )
  ,
  list(
    x = 150,
    y = -0.75,
    #text = "Perceived as\n<i>politically unstable</i>",
    xanchor = "left",
    yanchor = "top",
    #font = list(color = "#11270b", size = 8),
    #showarrow = FALSE
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = -30
  ),
  list(
    x = 120,
    y = 0.7,
    xanchor = 'left',
    yanchor = 'top',
    text = "Perceived as\n<i>politically stable</i>",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  ),
  list(
    x = 115,
    y = -0.7,
    xanchor = 'left',
    yanchor = 'bottom',
    text = "Perceived as\n<i>politically unstable</i>",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  )
)

plotlyProteinSelect <- plotlyProteinSelect %>%
  layout(
    annotations = proteinAnno
  )

(plotlyProteinSelect)

linkedSelect <- subplot(
  plotlyProteinSelect,
  plotlypolEnSelect,
  titleX = TRUE,
  #shareY = TRUE, #I prefer having doubled marks for ease of readability
  titleY = TRUE,
  nrows = 1
)

#linkedSelect <- linkedSelect %>%
#  highlight(
#    on = "plotly_selecting",
#    selectize = TRUE,
#    opacityDim = 0.1,
#  )


(linkedSelect)


#political stability vs gdp

polGDP <- ggplot(
  data = polPlusGDP,
  aes(x = GDP_PerCapita, 
      y = Index, 
      color = Region, 
      text = Area,
      frame = Year)
) +
  geom_point(size = 3, alpha = 0.7) +
  #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = 'Political Stability Index vs GDP Per Capita',
    subtitle = 'In Europe and Africa',
    x = 'GDP Per Capita',
    y = 'Political Stability Index',
    caption = 'Lower/negative political stability index values indicate a high perception of likelihood of political violence,\nwith higher/positive values indicating the opposite.\nGDP per capita is calculated in constant international 2017 dollars,\nand is based on purchasing power parity (PPP).'
  ) +
  scale_y_continuous(limits = c(-2.5,2.5)) +
  scale_x_continuous(labels = label_dollar()) +
  geom_hline(yintercept = 0, color = 'darkgrey')+
  scale_color_manual(values = c("#598b2c", "#ffa400", "#42Cafd", "#140d4f")) +
  geom_point(data = avgPolGDP,
             aes(x = AVG_Percent, 
                 y = AVG_Index),
             size = 10,
             alpha = 0.5
  )

(polGDP)

plotlyPolGDP <- ggplotly(polGDP, tooltip = "text")

plotlyPolGDP <- plotlyPolGDP %>%
  layout(xaxis = list(
    title = list(text = "GDP Per Capita (International 2017 Dollars)",
                 standoff = 15)
  )) %>%
  layout(yaxis = list(
    title = list(text = "Political Stability Index",
                 standoff = 5)
  ))

(plotlyPolGDP)

polAnno <- list(
  list(
    #xref = 'x',
    #yref = 'y',
    x = 125000,
    y = 0.75,
    #text = "Perceived as\n<i>politically stable</i>",
    xanchor = "left",
    yanchor = "top",
    #font = list(color = "#11270b", size = 8),
    #showarrow = FALSE
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = 30
  )
  ,
  list(
    x = 125000,
    y = -0.75,
    #text = "Perceived as\n<i>politically unstable</i>",
    xanchor = "left",
    yanchor = "top",
    #font = list(color = "#11270b", size = 8),
    #showarrow = FALSE
    showarrow = TRUE,
    arrowhead = 4,
    arrowwidth = 2,
    arrowcolor = "#11270b",
    ax = 0,
    ay = -30
  ),
  list(
    x = 108000,
    y = 0.7,
    xanchor = 'left',
    yanchor = 'top',
    text = "Perceived as\n<i>politically stable</i>",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  ),
  list(
    x = 107500,
    y = -0.7,
    xanchor = 'left',
    yanchor = 'bottom',
    text = "Perceived as\n<i>politically unstable</i>",
    font = list(color = "#11270b", size = 8),
    showarrow = FALSE
  )
)

plotlyPolGDP <- plotlyPolGDP %>%
  layout(
    annotations = polAnno
  )
