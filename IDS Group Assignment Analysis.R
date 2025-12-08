# Target 2: By 2020, substantially reduce the proportion of youth not in employment, education or 
# training. 

# Assignment brief 
# Perform exploratory data analysis to assess how 6 continents in the world 
# (all continents except Antarctica) are faring with respect to the following specific targets 
# set out by the UN:

# You may wish to compare across the continents, as well as assess their individual 
# progress across time. You may also wish to further divide up a continent 
# (e.g., into more developed vs. less developed countries). 


# Loading R Packages Required
library(tidyverse)
library(ggplot2)
library(dplyr)


# Loading Datasets Required for Target 2
continents_data <- read.csv("data sets/continents-according-to-our-world-in-data.csv")

youth_neet <- read.csv("data sets/youth-not-in-education-employment-training.csv")

income_groups <- read.csv("data sets/CLASS_2025_10_07.csv")


# Data Joining and Cleaning

# Selecting Specific Data Columns
income_groups_essential <- income_groups %>% select(Economy, Code, Income.group)

continents_data_essential <- continents_data %>% select(Entity, Code, Continent)


# Checking for and Identifying Non-Corresponding Primary Keys between Data Frames
setequal(youth_neet$Code, income_groups_essential$Code)

anti_join(youth_neet, income_groups_essential, by = "Code")


# Replacing Identified Non-Corresponding Primary Key (Country Name Corresponds, Code does not)
# Since left_join will be utilised for merging youth_neet and income_groups_essential, Kosovo's Country Code
# needs to be adjusted, and accordingly the one in the continents_data_essential Data Frame
youth_neet$Code <- recode(youth_neet$Code, 'OWID_KOS' = 'XKX')

continents_data_essential$Code <- recode(continents_data_essential$Code, 'OWID_KOS' = 'XKX')


# Joining Datasets (youth_neet and income_groups_essential)
youth_neet_classification <- youth_neet %>% left_join(income_groups_essential)

# Checking for and Identifying Non-Corresponding Primary Keys between youth_neet_classification
# and continents_data_essential Data Frame
anti_join(youth_neet_classification, continents_data_essential, by = "Code")

# Selecting Required Column Variables
youth_neet_essential <- youth_neet_classification %>% select(
  Entity, Code, Year, Share.of.youth.not.in.education..employment.or.training..total....of.youth.population., Income.group
  )

# Joining Datasets (youth_essential and continents_data_essential), Removing Missing-Variable Observations
# and Renaming Column Variables
youth_neet_complete <- youth_neet_essential %>% 
  left_join(continents_data_essential) %>% 
  filter(Income.group != '') %>% 
  rename(
    Youth_NEET_Pc = Share.of.youth.not.in.education..employment.or.training..total....of.youth.population., 
    Income_Group = Income.group, 
    Country = Entity
  )


# Checking for Appearance of Antarctica in observations
unique(youth_neet_complete$Continent)
unique(youth_neet_complete$Country)


# Data Visualisation

# 1) Annual Average Continental Youth NEET % by Year
# Creating a new Data Frame to derive the average Youth NEET percentage
# for each continent for the available years using the youth_neet_complete Data Set
average_continental_youth_neet <- youth_neet_complete %>%
 group_by(Continent, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>%
  ungroup()

# Utilising ggplot to visualise the data derived through respective average 
# continental Youth NEET % by year data points connected by lines
p1 <- average_continental_youth_neet %>%
  
  ggplot() +
  
  geom_point(
    aes(
      x = Year, 
      y = avg_neet, 
      colour = Continent,
      ), 
    size = 0.7
    ) +
  
  geom_line(
    aes(
      x = Year, 
      y = avg_neet, 
      colour = Continent
      ),
    size = 0.3
    ) +

# Formatting and Modifying the data visualisation  
  labs(
    x = 'Year',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Continental Youth NEET Percentage by Year', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training'
  ) +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 15, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      hjust = 1
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 13, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p1)

# 2) Annual Average Continental Youth NEET % by Year (Event Accentuation)
# Creating a new Data Frame to derive the average Youth NEET percentage
# for each continent for the available years using the youth_neet_complete Data Set
# and ensuring all continents' Average Youth NEET Rate begin from the same year
average_continental_youth_neet_2 <- youth_neet_complete %>%
  filter(Year >= 2000) %>%
  group_by(Continent, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>%
  ungroup()

# Utilising ggplot to visualise the data derived through respective average 
# continental Youth NEET % by year data points connected by lines
p2 <- average_continental_youth_neet_2 %>%
  
  ggplot() +
  
  geom_point(
    aes(
      x = Year, 
      y = avg_neet,
    ), 
    size = 0.9, 
    colour = 'black'
  ) +
  
  geom_line(
    aes(
      x = Year, 
      y = avg_neet, 
      colour = Continent
    ),
    size = 0.7
  ) +
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
  aes(
    label = 'Imposition of SGD 8 
    by UN (2016)', 
    x = 2015, 
    y = 33
  ), 
    colour = 'black', 
    size = 2.5, 
  hjust = 0.75
) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  # Formatting and Modifying the data visualisation  
  labs(
    x = 'Year',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Continental Youth NEET Percentage by Year', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training'
  ) +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 15, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      hjust = 1
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p2)

# 3) Intra-Continental Observation of Youth NEET Rates (Africa)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

Af <- intra_continental_income_group %>% filter(Continent == 'Africa') %>% 
  filter(Year >= 2003)

Af_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'Africa') %>% filter(Year >= 2003)

p3 <- ggplot() + geom_point(data = Af, aes(
      x = Year, 
      y = avg_neet, 
      colour = Income_Group
        )
  ) + 
  
  geom_line(data = Af, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
    )
    ) +
  
  geom_point(data = Af_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
    )
  ) +
  
  geom_line(data = Af_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (2003-2022)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (Africa)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and few data available for HICs in Africa in observed time period', 
    colour = 'Income group') +

  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p3)


# 4) Intra-Continental Observation of Youth NEET Rates (Europe)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

Eu <- intra_continental_income_group %>% filter(Continent == 'Europe') %>% 
  filter(Year >= 2000)

Eu_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'Europe') %>% filter(Year >= 2000)

p4 <- ggplot() + geom_point(data = Eu, aes(
  x = Year, 
  y = avg_neet, 
  colour = Income_Group
)
) + 
  
  geom_line(data = Eu, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
  )
  ) +
  
  geom_point(data = Eu_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  )
  ) +
  
  geom_line(data = Eu_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (2000-2022)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (Europe)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and no data available for LICs & LMICs in Europe in observed time period', 
    colour = 'Income group') +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p4)



# 5) Intra-Continental Observation of Youth NEET Rates (NA)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

Na <- intra_continental_income_group %>% filter(Continent == 'North America') %>% 
  filter(Year >= 2001)

Na_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'North America') %>% filter(Year >= 2001)

p5 <- ggplot() + geom_point(data = Na, aes(
  x = Year, 
  y = avg_neet, 
  colour = Income_Group
)
) + 
  
  geom_line(data = Na, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
  )
  ) +
  
  geom_point(data = Na_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  )
  ) +
  
  geom_line(data = Na_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (2001-2022)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (North America)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and no data available for LICs in North America in observed time period', 
    colour = 'Income group') +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p5)


# 6) Intra-Continental Observation of Youth NEET Rates (SA)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

Sa <- intra_continental_income_group %>% filter(Continent == 'South America') %>% 
  filter(Year >= 1999)

Sa_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'South America') %>% filter(Year >= 1999)

p6 <- ggplot() + geom_point(data = Sa, aes(
  x = Year, 
  y = avg_neet, 
  colour = Income_Group
)
) + 
  
  geom_line(data = Sa, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
  )
  ) +
  
  geom_point(data = Sa_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  )
  ) +
  
  geom_line(data = Sa_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (1999-2022)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (South America)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and no data available for LICs in South America in observed time period', 
    colour = 'Income group') +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p6)



# 7) Intra-Continental Observation of Youth NEET Rates (Asia)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

As <- intra_continental_income_group %>% filter(Continent == 'Asia') %>% 
  filter(Year >= 2009)

As_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'Asia') %>% filter(Year >= 2009)

p7 <- ggplot() + geom_point(data = As, aes(
  x = Year, 
  y = avg_neet, 
  colour = Income_Group
)
) + 
  
  geom_line(data = As, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
  )
  ) +
  
  geom_point(data = As_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  )
  ) +
  
  geom_line(data = As_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (2009-2022)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (Asia)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and few data available for LICs in Asia in observed time period', 
    colour = 'Income group') +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p7)


# 8) Intra-Continental Observation of Youth NEET Rates (Oceania)
# Comparison between every intra-continental income group's average 
# Youth NEET Rate and the continental rate

intra_continental_income_group <- youth_neet_complete %>% 
  group_by(Continent, Income_Group, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc)) %>% ungroup()

Oce <- intra_continental_income_group %>% filter(Continent == 'Oceania') %>% 
  filter(Year >= 2005)

Oce_avg_neet <- average_continental_youth_neet %>% 
  filter(Continent == 'Oceania') %>% filter(Year >= 2005)

p8 <- ggplot() + geom_point(data = Oce, aes(
  x = Year, 
  y = avg_neet, 
  colour = Income_Group
)
) + 
  
  geom_line(data = Oce, aes(
    x = Year, 
    y = avg_neet, 
    colour = Income_Group
  )
  ) +
  
  geom_point(data = Oce_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  )
  ) +
  
  geom_line(data = Oce_avg_neet, aes(
    x = Year, 
    y = avg_neet, 
    colour = Continent
  ), 
  size = 1.2
  ) + 
  
  
  # Formatting and Modifying the data visualisation  
  
  geom_vline(
    xintercept = 2016,
    linetype = 'dotted') +
  
  geom_vline(
    xintercept = 2020,
    linetype = 'dashed', 
    colour = 'red') + 
  
  geom_text(
    aes(
      label = 'Imposition of SGD 8 
    by UN (2016)', 
      x = 2015, 
      y = 33
    ), 
    colour = 'black', 
    size = 2.5, 
    hjust = 0.75
  ) + 
  
  geom_text(
    aes(
      label = 'UN SDG Target 
      Year (2020)',
      x = 2020, 
      y = 15
    ), 
    colour = 'red', 
    size = 2.5, 
    hjust = 1
  ) +
  
  labs(
    x = 'Year (2005-2021)',
    y = 'Average Youth NEET Rate (%)',
    title = 'Average Annual Youth NEET Percentage by Income Group (Oceania)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training, and no data available for LICs in Oceania in observed time period', 
    colour = 'Income group') +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p8)


# 9) Youth NEET Percentage World Map (2016-2020 Average)

install.packages('sf')
install.packages('rnaturalearthdata')
library(sf)
library(rnaturalearthdata)

youth_neet_world <- youth_neet_complete %>% filter(Year >= 2016 & Year <= 2020) %>% 
  group_by(Country) %>%
  summarise(avg_youth_neet = mean(Youth_NEET_Pc, na.rm = TRUE)) %>% ungroup()

world <- ne_countries(scale = "medium", returnclass = "sf") %>% filter(name != 'Antarctica')

youth_neet_world_2016_2022 <- world %>%
  left_join(youth_neet_complete, by = c("iso_a3" = "Code"))

p9 <- youth_neet_world_2019_2022 %>% ggplot() +
  geom_sf(aes(fill = Youth_NEET_Pc)) +
  scale_fill_viridis_c(option = 'plasma', na.value = 'grey90') + 
  
  labs(
    title = 'Youth NEET % (2016-2020 Average)'
    ) +
  
  theme_minimal(13) +
  
  labs(fill = 'Youth NEET %') +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p9)

# 10) Box Plot of Youth NEET by Continent

youth_neet_box_2016_2022 <- youth_neet_world %>% inner_join(youth_neet_complete) %>%
  select(Country, avg_youth_neet, Continent) %>% distinct(Country, avg_youth_neet, Continent)

p10 <- youth_neet_box_2016_2022 %>% ggplot() + 
  geom_boxplot(aes(x = Continent, y = avg_youth_neet)) + 
  
  
  labs(
    x = 'Continent',
    y = 'Average Youth NEET Rate (%)',
    title = 'Youth NEET Rate Distribution by Continent (2016-2020 Average)'
    ) +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )

print(p10)

# 11) Heatmap of Annual Youth NEET Rate by Continent

youth_neet_heatmap <- youth_neet_complete %>%
  filter(Year >= 2004 & Year <= 2021) %>%
  group_by(Continent, Year) %>%
  summarise(avg_neet = mean(Youth_NEET_Pc, na.rm = TRUE)) %>%
  ungroup()

p11 <- youth_neet_heatmap %>% ggplot() + 
  geom_tile(aes(x = Year, y = Continent, fill = avg_neet)) + 

  labs(
    x = 'Year',
    y = 'Continent',
    title = 'Heatmap of Continental Youth NEET Rate by Year (2005-2021)', 
    caption = '*Note Youth NEET: Youth Not in Employment, Education, or Training',
    fill = 'Avg. Youth NEET (%)'
  ) +
  
  theme_minimal(13) +
  
  theme(
    plot.background = element_rect(
      fill = "#F9F9F9", color = NA
    ), 
    panel.background = element_rect(
      fill = "#FFFFFF", color = NA
    ),
    panel.grid.major = element_line(
      color = "#D9D9D9", size = 0.5
    ),
    panel.grid.minor = element_line(
      color = "#EFEFEF", size = 0.25
    ),
    plot.title = element_text(
      size = 12, face = "bold", hjust = 0.5
    ),
    plot.caption = element_text(
      size = 7, hjust = 0
    ),
    axis.title.y = element_text(
      size = 11, face = "bold"
    ),
    axis.title.x = element_text(
      size = 11, face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      size = 11, face = "bold"
    ),
    legend.text = element_text(
      size = 11
    ) 
  )


print(p11)

