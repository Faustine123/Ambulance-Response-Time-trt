---
title: "Analysis of High Death Rate from 2020 to 2021 in Toronto"
author: "Faustine Fan"
thanks: "Code and data are available at: LINK."
date: "`r format(Sys.time(), '%2 %Feburary %2022')`"
abstract: "The death rate is an effective way to supervise the health of a particular population like Toronto. There could be various reasons leading to a rise in the death rate. This report explores the influence of the Covid-19 pandemic, aging, and motor vehicle collisions on the death rate, aiming to identify the main reason of the arisen in the death rate in Toronto from 2020 to 2021. I analyze death rate data from Open Data Toronto to gain insight into death trends during this period. My results show that death rates have risen significantly since the start of the covid-19 pandemic, particularly for older age groups. Motor vehicle collisions did not play much of a role during this period. These findings suggest that older adults are more vulnerable to serious health conditions than younger adults, and there may also be differences in access to health care. Those inequalities may contribute to increased mortality in the older population during Covid-19. From the perspective of mortality data, intentional and accidental underreporting, misclassification, population bias, sampling selection bias, and data quality all have an impact on the accuracy of mortality data."
output:
  bookdown::pdf_document2:
  toc: true
  toc_depth: 2
bibliography: references_datasheet.bib
quote_footer: ["\\begin{https://github.com/Faustine123/Death_Rate_trt}", "\\end{flushright}"]
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```

# Content


# Introduction

We examine the xxx using Faustine. Another way to do a citation is this[Faustine].We use "opendatatoronto" to gather data[citesharla].

Add a new line of text

```{r}
library(tidyverse)
library(opendatatoronto)
library(dplyr)
library(ggplot2)
library(knitr)
library(janitor)
library(lubridate)
library(palmerpenguins)
library(ggmap)
library(modelsummary)
library(kableExtra)
library(reprex)

install.packages("LaTeX")
```

# Data

Our data is of (@fig-).

## Death Rate

```{r}

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

```{r}
raw_call_data <-
  read_csv(
    file = "Deathrate_Toronto.csv",
    show_col_types = FALSE
)
```

```{r}
cleaned_call_data <-
  clean_names(raw_call_data)
head(cleaned_call_data)
```

```{r}
cleaned_call_data <-
  cleaned_call_data |>
  select(
   death_licenses,
   place_of_death,
   time_period,
   civic_centre
  )

head(cleaned_call_data)

cleaned_call_data |>
  slice(1:10) |>
  kable()

```

```{r}

tail(cleaned_call_data) |>
  slice(1:10) |>
  kable()

```


```{r}
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
  filter(Death_Place == "Toronto") |>
  ggplot(mapping = aes(x = Year, y = as.numeric(Amount), group = Home_City, color = Home_City)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Amount",
    title = "The Death Rate in Toronto from 2011 to Present",
    caption = "Data source: Toronto Government.",
    color = "Death Rate"
  ) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Etobicoke", "North York","Scarborough", "Toronto")
  ) +
  theme(legend.position = "bottom")
```

## Covid-19

```{r}
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

```{r}
raw_19_data <-
  read_csv(
    file = "COVID-19.csv",
    show_col_types = FALSE
)
```

```{r}
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

cleaned_19_data |>
  slice(1:10) |>
  kable()

```

```{r}

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

```{r}
cleaned_19_data |>
  filter(Outcome == "FATAL",
         Diagnosed == "CONFIRMED"
         ) |>
  ggplot(mapping = aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge2") +
  theme_bw() +
  labs(
    x = "Age",
    y = "Amount",
    title = "Death Amount of Different Age Group during the Covid-19",
    caption = "Data source: Toronto Government.",
    color = "Gender") +
  theme(legend.position = "right") +
  scale_color_brewer(
    palette = "Blues",
    labels = c("Female", "Male")) 

```



## Motor Vehicle Collision

```{r}

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

```{r}

raw_MVC_data <-
  read_csv(
    file = "Motor_Vehicle_Collision.csv",
    show_col_types = FALSE
)

cleaned_MVC_data <-
  clean_names(raw_MVC_data)

head(cleaned_MVC_data)

```

```{r}

cleaned_MVC_data <-
  cleaned_MVC_data |>
  select(
    injury,
    year,
    acclass
  )

cleaned_MVC_data |>
  slice(1:10) |>
  kable()

```

```{r}

cleaned_MVC_data |>
  ggplot(mapping = aes(x = year)) +
  geom_bar() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Amount",
    title = "Motor Vehicle Collision in Toronto",
    caption = "Data source: Toronto Government.",
    color = "acclass"
  ) +
  facet_wrap(vars(injury)) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Fatal", "Non-Fatal Injury")) +
  theme(legend.position = "bottom")
  
```
## Aging

```{r}
library(opendatatoronto)
library(dplyr)

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

```{r}
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
  slice(1:10) |>
  kable()
```

```{r}
cleaned_age_data |>
  filter(topic == "Age characteristics") |>
  ggplot(mapping = aes(x = characteristic)) +
  geom_bar() +
  theme_minimal() +
  labs(
    x = "Age Group",
    y = "Amount",
    title = "The Age of Neighborhood at Toront in 2016",
    caption = "Data source: Toronto Government.",
  ) +
  scale_color_brewer(
    palette = "Set2",
    labels = c("Fatal", "Non-Fatal Injury")) +
  theme(legend.position = "bottom")
```


# Dicussion


# References

