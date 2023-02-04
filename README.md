---
title: "Analysis of High Death Rate from 2020 to 2021 in Toronto"
author: "Faustine Fan"
thanks: "Code and data are available at:https://github.com/Faustine123/Death_Rate_trt/edit/main/README.md."
date: "`r format(Sys.Date(), '%d %B %Y')`"
abstract: "The death rate is an effective way to supervise the health of a particular population like Toronto. There could be various reasons leading to a rise in the death rate. This report explores the influence of the Covid-19 pandemic, aging, and motor vehicle collisions on the death rate, aiming to identify the main reason of the arisen in the death rate in Toronto from 2020 to 2021. I analyze death rate data from Open Data Toronto to gain insight into death trends during this period. My results show that death rates have risen significantly since the start of the covid-19 pandemic, particularly for older age groups. Motor vehicle collisions did not play much of a role during this period. These findings suggest that older adults are more vulnerable to serious health conditions than younger adults, and there may also be differences in access to health care. Those inequalities may contribute to increased mortality in the older population during Covid-19. "
output:
  pdf_document:
    toc: true
    toc_depth: 2
bibliography: references_datasheet.bib

---

```{r setup, include=FALSE, warning=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


# Introduction

The death rate in Toronto fluctuated significantly between 2020 and 2021. Considering the extraordinary events that occurred during this period, the COVID-19 pandemic is the most significant. As of July 6, 2021, in Toronto, Canada's largest city, there were 170,023 cases of COVID-19 and 3570 deaths [@covid-19]. The risk of COVID-19 may vary by age, biological, socioeconomic, behavioural and logistical reasons differences, these differences may be attributable to these differences. In addition, the increasing aging of the population is also a necessary influencing factor for the surge in mortality. Nushrat Nazia assessed changes in neighbourhood risk among different age groups by calculating the Kohn Kappa coefficient. Findings indicate that knowledge of health risks and health behaviours varies across Toronto communities by age[@covid_map]. At the same time, the risk of epidemic infection of the elderly is different from that of the young. In Toronto, Canada, the aging population has been severely affected, accounting for 92% of all COVID-19 deaths[@covid_map]. Another set of data shows that more than 80% of COVID-19 deaths in Canada are residents of nursing homes [@covid]. In addition, considering the impact of motor vehicle collisions, poverty alleviation policies, environment, economy, etc. on the mortality rate, the fluctuations are relatively small. In this report, only injury data from motor vehicle collisions in Toronto from 2005 to 2020 are visualized.

There may be errors caused by a series of inequalities in the collection of death data. Different racial and ethnic groups have unequal access to quality health care. Socioeconomic and geographic biases can affect access to health care, health behaviours, and health outcomes, leading to disparities in mortality between different socioeconomic groups and between urban and rural areas, or across regions. Gender bias can lead to differences in death rates between men and women. On the other hand, intentional and accidental underreporting, misclassification, population bias, sampling selection bias, and data quality all have an impact on the accuracy of mortality data.

This report was produced using knitr [@citeknitr], and bookdown [@citebookdown].



```{r, message=FALSE, warning=FALSE}
library(tidyverse)
library(opendatatoronto)
library(knitr)
library(janitor)
library(lubridate)
library(palmerpenguins)
library(ggmap)
library(modelsummary)
library(kableExtra)
library(reprex)
library(tinytex)

```

# Data

My primary datasets are Death Rate from 2016 to Present， COVID19 cases, Motor Vehicle Collision, and Neighbourhood Profiles in 2016. The data is obtained from the Open Data Toronto portal, retrieved using the opendatatoronto package [@citeopt] and saved and retrieved using readr [@citereadr].For my data analysis, we will be using R [@citeR] and the tidyverse package [@citetidyverse] to perform the data manipulations and ggplot2 [@citeggplot] to generate plots and figures.For my data analysis, we will be using R [@citeR] and the tidyverse package [@citetidyverse] to perform the data manipulations and ggplot2 [@citeggplot] to generate plots and figures.

The Death Rate dataset is a summary of the number of deaths tracked in Toronto and outside Toronto each month from 2016 to 2020, which includes those located in four civic centers (Scarborough, North York, Toronto, and Etobicoke). I filtered the local death toll in Toronto and found that the death rate in Toronto has a surge in 2020-2021, but the strange thing is that most of the increased deaths came from Etobicoke. Meanwhile, data for Scarborough is missing from 2020 onwards.

The COVID19 cases dataset is a survey by Toronto Public Health of the number of people infected by geography and severity in Toronto since the start of the pandemic. I filtered the two variables of confirmed and death outcomes, and arranged the data by age group to find that Toronto had x% of the total population's deaths during COVID-19, see Figure \ref{fig:Covid-19}. At the same time, the death rate increases gradually with age, especially among those over 80 years old.

The Neighborhood Profiles in 2016 dataset is a census table of 140 blocks in Toronto released by Statistics Canada, which provides detailed data on block names and different age groups of the population. I analyzed this set of data by filtering three variables, namely topic, characteristic, city_of_toronto, which were then renamed for presentation in tables and graphs, like Figure \ref{fig:aging}. I found that the two age groups 65+ and 85+ had the largest number of people in 2016, which means that by 2020, these people may die of natural causes or increase the possibility of contracting the epidemic when they are older.

The Motor Vehicle Collision dataset includes all traffic collisions with deaths or serious injuries from 2006 to 2021, which can reflect the tracking of the number of deaths due to motor vehicle collisions. I present this set of data by presenting different levels of damage. At the same time, the official classification of accidents is displayed in different colors in Figure \ref{fig:MVC}. There is a bit of ambiguity here that accidents officially classified as fatal are shown as minor injuries or none in the injury classification. Overall, I found that the fatality rate for motor vehicle crashes was relatively flat between 2016 and 2020.

A small subset of before six lines of the death rate data is shown below, formatted using kableExtra [@citekable].


## Death Rate

```{r, results='hide'}

# get package
package <- show_package("cba07a90-984b-42d2-9131-701c8c2a9788")
package

# get all resources for this package
resources <- list_package_resources("cba07a90-984b-42d2-9131-701c8c2a9788")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
raw_data <- filter(datastore_resources, row_number()==1) %>% get_resource()
raw_data

write_csv(
  x = raw_data,
  file = "Deathrate_Toronto.csv"
)
```

```{r, results='hide'}
raw_call_data <-
  read_csv(
    file = "Deathrate_Toronto.csv",
    show_col_types = FALSE
)
```

```{r, results='hide'}
cleaned_call_data <-
  clean_names(raw_call_data)
head(cleaned_call_data)
```

```{r, results='hide'}
cleaned_call_data <-
  cleaned_call_data |>
  select(
   death_licenses,
   place_of_death,
   time_period,
   civic_centre
  )

head(cleaned_call_data)
```

Table \ref{tab:cdl}

```{r,results='hide'}
cleaned_call_data <-
  cleaned_call_data |>
  rename(
    Year = time_period,
    Death_Place = place_of_death,
    Amount = death_licenses,
    Home_City = civic_centre
  )

head(cleaned_call_data)

```

```{r}
cleaned_call_data |>
  slice(1:6) |>
  kable(caption = "Cleaned Dataset Preview", label = "cdl", align = "c")
```

```{r, fig.show='hide'}
cleaned_call_data |>
  filter(Death_Place == "Toronto") |>
  ggplot(mapping = aes(x = Year, y = as.numeric(Amount), group = Home_City, color = Home_City)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Amount",
    title = "The Death Rate in Toronto from 2011 to Present",
    caption = "Data source: Toronto Government.",
    color = "Home City"
  ) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Etobicoke", "North York","Scarborough", "Toronto")
  ) +
  theme(legend.position = "bottom") 
```

```{r, fig.align='center', fig.pos="!htbp", fig.dim=c(8.46, 4.64), out.width="70%", fig.cap="\\label{fig:death_rate}The Death Rate in Toronto from 2011 to Present", warning=FALSE, message=FALSE}
cleaned_call_data %>%
  mutate(Year_single = str_sub(Year, 1, 4)) %>%
  group_by(Home_City, Year_single) %>%
  summarise(Amount_total = sum(Amount)) %>%
  mutate(Year = as.numeric(Year_single)) %>%
  ggplot(mapping = aes(x = Year, y = Amount_total)) +
    geom_line(aes(color = Home_City)) +
    scale_x_continuous(breaks = seq(2011, 2023, 1),
                       labels = seq(2011, 2023, 1)) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Etobicoke", "North York","Scarborough", "Toronto")
  ) +
  theme(legend.position = "bottom") +
  theme_bw() +
  labs(title = "The Death Rate in Toronto from 2011 to Present",)
```

## Covid-19

```{r,results='hide'}
# get package
package <- show_package("64b54586-6180-4485-83eb-81e8fae3b8fe")
package

# get all resources for this package
resources <- list_package_resources("64b54586-6180-4485-83eb-81e8fae3b8fe")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

write_csv(
  x = data,
  file = "COVID-19.csv"
)
```

```{r,results='hide'}
raw_19_data <-
  read_csv(
    file = "COVID-19.csv",
    show_col_types = FALSE
)
```

```{r,results='hide'}
cleaned_19_data <-
  clean_names(raw_19_data)

cleaned_19_data <-
  cleaned_19_data |>
  select(
   classification,
   episode_date,
   client_gender,
   age_group,
   outcome,
  )

head(cleaned_19_data)
```

```{r}
cleaned_19_data |>
  slice(1:6) |>
  kable(caption = "COVID-19", label = "covid19", align = "c")
```


```{r,results='hide'}

cleaned_19_data <-
  cleaned_19_data |>
  rename(
    Diagnosed = classification,
    Date = episode_date,
    Gender = client_gender,
    Age = age_group,
    Outcome = outcome
  )

head(cleaned_19_data)

```

```{r, fig.align='center', fig.pos="!htbp", fig.dim=c(8.46, 4.64), out.width="70%", fig.cap="\\label{fig:Covid-19}Mortality Rate of Different Age Group during the Covid-19", warning=FALSE, message=FALSE}
cleaned_19_data %>%
  filter(Outcome == "FATAL",
         Diagnosed == "CONFIRMED"
         ) %>%
  ggplot(mapping = aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge2") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Amount",
    title = "Mortality Rate of Different Age Group during the Covid-19",
    caption = "Data source: Toronto Government.",
    color = "Gender") +
  coord_flip() +
  scale_fill_brewer(
    palette = "Set1",
    type = "qual"
  )

```

## Aging

```{r,results='hide'}
# get package
package <- show_package("6e19a90f-971c-46b3-852c-0c48c436d1fc")
package

# get all resources for this package
resources <- list_package_resources("6e19a90f-971c-46b3-852c-0c48c436d1fc")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
data <- filter(datastore_resources, row_number()==1) %>% get_resource()
data

write_csv(
  x = data,
  file = "Age.csv"
)
```

```{r,results='hide'}
raw_age_data <-
  read_csv(
    file = "Age.csv",
    show_col_types = FALSE
)

cleaned_age_data <-
  clean_names(raw_age_data)

head(cleaned_age_data)
```

```{r}

cleaned_age_data <-
  cleaned_age_data |>
  select(
    topic,
    characteristic,
    city_of_toronto
  )

cleaned_age_data |>
  slice(1:6) |>
  kable(caption = "Aging", label = "aging", align = "c")
```

```{r, fig.align='center', fig.pos="!htbp", fig.dim=c(8.46, 4.64), out.width="70%", fig.cap="\\label{fig:aging}The Age of Neighborhood at Toront in 2016", warning=FALSE, message=FALSE}
cleaned_age_data %>%
  filter(topic == "Age characteristics") %>%
  filter(str_starts(characteristic, "(Male)|(Female)", negate = T))  %>%
  rename(`Age Group` = characteristic) %>%
  ggplot(mapping = aes(x = `Age Group`, y = city_of_toronto)) +
  geom_col(aes(fill = `Age Group`)) +
  theme_minimal() +
  coord_flip() +
  labs(
    x = "Age Group",
    y = "Amount",
    title = "The Age of Neighborhood at Toront in 2016",
    caption = "Data source: Toronto Government.",
  ) +
  scale_fill_brewer(
    palette = "Set1") +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.key.width = unit(0.5, "cm"), legend.margin = margin(l = -120))
```

## Motor Vehicle Collision

```{r, results='hide'}

# get package
package <- show_package("0b6d3a00-7de1-440b-b47c-7252fd13929f")
package

# get all resources for this package
resources <- list_package_resources("0b6d3a00-7de1-440b-b47c-7252fd13929f")

# identify datastore resources; by default, Toronto Open Data sets datastore resource format to CSV for non-geospatial and GeoJSON for geospatial resources
datastore_resources <- filter(resources, tolower(format) %in% c('csv', 'geojson'))

# load the first datastore resource as a sample
raw_MVC_data <- filter(datastore_resources, row_number()==1) %>% get_resource()
raw_MVC_data

write_csv(
  x = raw_MVC_data,
  file = "Motor_Vehicle_Collision.csv"
)
```

```{r,results='hide'}

raw_MVC_data <-
  read_csv(
    file = "Motor_Vehicle_Collision.csv",
    show_col_types = FALSE
)

cleaned_MVC_data <-
  clean_names(raw_MVC_data)

head(cleaned_MVC_data)

```

```{r,results='hide'}

cleaned_MVC_data <-
  cleaned_MVC_data |>
  select(
    injury,
    year,
    acclass
  )

cleaned_MVC_data <-
  cleaned_MVC_data |>
  rename(
    Injury = injury,
    Year = year,
    Accident_Class = acclass
  )

cleaned_MVC_data |>
  slice(1:6) |>
  kable(caption = "MVC", label = "mvc", align = "c")

```

```{r, fig.align='center', fig.pos="!htbp", fig.dim=c(8.46, 4.64), out.width="70%",fig.cap="\\label{fig:MVC}Motor Vehicle Collision in Toronto", warning=FALSE, message=FALSE}
cleaned_MVC_data %>%
  ggplot(mapping = aes(x = Year)) +
  geom_bar(aes(fill = Accident_Class)) +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Amount",
    title = "Motor Vehicle Collision in Toronto",
    caption = "Data source: Toronto Government.",
    color = "Accident_Class"
  ) +
  facet_wrap(~Injury, scale = "free_y") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(
    palette ="Set1")
  
```



# Dicussion


# References
