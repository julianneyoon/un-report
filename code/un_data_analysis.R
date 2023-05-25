# Load necessary library
library(tidyverse)

# Read in data
gapminder_data <- read_csv("data/gapminder_data.csv")

# What is the mean life expectancy?
  # summarize()
  summarize(gapminder_data, average_LifeExp = mean(lifeExp))
  # mean life expectancy = 59.5 years

gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

gapminder_data_summary <- gapminder_data %>% 
  summarize(averageLifeExp = mean(lifeExp))

# What is the mean population?
  gapminder_data %>% 
    summarize(average_pop = mean(pop))
  # mean population = 29,601,212
  
# What is the mean life expectancy AND the mean population?
  gapminder_data %>% 
    summarize(averageLifeExp = mean(lifeExp),average_pop = mean(pop))

# What is the mean life expectancy for the most recent year?
  # filter()
  # max()
  gapminder_data %>% 
    summarize(maxYear = max(year))
  # max year = 2007
  
  gapminder_data %>% 
    filter(year == 2007) %>%
    summarize(meanLifeExp = mean(lifeExp))
  # meanLifeExp_2007 = 67
  
# What is the mean of the most recent year?
  gapminder_data %>% 
    filter(year == max(year)) %>% 
    summarize(meanLifeExp = mean(lifeExp))
  # meanLifeExp_2007 = 67
  
# What is the mean GDP per capita for the first/earliest year?
  gapminder_data %>% 
    filter(year == min(year)) %>% 
    summarize(meanGDP = mean(gdpPercap))
  # meanGDP_firstyear = 3725
  
# < > !=(excluding)
  
# What is the mean life expectancy for each year?
  # group_by()
  gapminder_data %>% 
    group_by(year) %>% 
    summarize(meanLifeExp = mean(lifeExp))
  
# What is the mean life expectancy for each continent?
  gapminder_data %>% 
    group_by(continent) %>% 
    summarize(meanLifeExp = mean(lifeExp))

# What is the mean life expectancy AND mean GDP per capita
  # for each continent in a single result tibble?
  gapminder_data %>% 
    group_by(continent) %>% 
    summarize(meanLifeExp = mean(lifeExp), meanGDP = mean(gdpPercap))

# What is the GDP (not per capita)?
  # mutate()
  gapminder_data %>% 
    mutate(gdp = gdpPercap * pop)
  colnames(gapminder_data)
  
# Make a new column for population in millions
  gapminder_data %>% 
    mutate(popinMillions = pop / 1000000, gdp = gdpPercap * pop)

# Saving columns
  gapminder_data_popmill <-  gapminder_data %>% 
    mutate(popinMillions = pop / 1000000, gdp = gdpPercap * pop)

# select(): chooses a subset of columns from the dataset
  gapminder_data %>% 
    select(year, pop)
  # select all but : use -
  gapminder_data %>% 
    select(-continent)
  
# Create a tibble with only country, continent, year, lifeExp
  gapminder_data %>% 
    select(country, continent, year, lifeExp)
  # OR
  gapminder_data %>% 
    select(-pop, -gdpPercap)
  
# Select helper functions: starts_with(), ends_with(), contains()
  gapminder_data %>% 
    select(year, starts_with("c")) 
    
# Vectors
  # c()
  # my_vec <- c() : make a vector in r
  
# Reshaping functions: pivot_longer(), pivot_wider()
  gapminder_data %>% 
    select(country, continent, year, lifeExp) %>% 
    pivot_wider(names_from = year, values_from = lifeExp)

# Pivot wider, but populate values with gdpPercap  
  gapminder_data %>% 
    select(country, continent, year, lifeExp, gdpPercap) %>% 
    pivot_wider(names_from = year, values_from = gdpPercap)
  
# pivot_longer
  gapminder_data %>% 
    pivot_longer(cols = c(pop, lifeExp, gdpPercap), names_to = "measurement_type", 
                 values_to = "measurement" )
  
# Is there a relationship between GDP and CO2 emissions?
  
  # gapminder_data_2007
    gapminder_data_2007 <- gapminder_data %>% 
      filter(year == 2007, continent == "Americas") %>% 
        select(-year, -continent)
  # Read in CO2 data and assign to object
    co2_emissions_dirty <- read_csv("data/co2-un-data.csv", skip = 2,
             col_names = c("region", "country", "year", "series",
                           "value", "footnotes", "source"))
    
  # Cleaning
    co2_emissions <- co2_emissions_dirty %>% 
      select(country, year, series, value) %>% 
        mutate(series = recode(series, "Emissions (thousand metric tons of carbon dioxide)" = "total emissions",
              "Emissions per capita (metric tons of carbon dioxide)" = "per_capita_emissions")) %>% 
      # Matching co2_emissions_dirty and gapminder_data_2007
        pivot_wider(names_from = series, values_from = value) %>% 
        filter(year == 2005) %>% 
        select(-year)

  # Join datasets (co2_emissions and gapminder_data_2007)    
    inner_join(gapminder_data_2007, co2_emissions, by = "country")

    