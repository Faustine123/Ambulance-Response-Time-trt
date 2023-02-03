# Death_Rate_trt
---
title: "Analysis of Hight Death Rate from 2020 to 2021 in Toronto"
author: "Faustine Fan"
thanks: "Code and data are available at: LINK."
date: "`r format(Sys.time(), '%2 %Feburary %2022')`"
abstract: "First sentence. Second sentence. Third sentence. Fourth sentence."
output:
  bookdown::pdf_document2
toc: FALSE
bibliography: references_datasheet.bib
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE)
```


# Introduction

We examine the xxx using @Faustine. Another way to do a citation is this[@Faustine].We use "opendatatoronto" to gather data[@citesharla].

```{r}
library(tidyverse)
library(opendatatoronto)
library(dplyr)
library(ggplot2)
library(knitr)
library(janitor)
library(lubridate)
library(palmerpenguins)
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
  ggplot(aes(x = Year, y = as.numeric(Amount), group = Home_City, color = Home_City)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Year",
    y = "Amount",
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
```

```{r}

```



# Dicussion
# References
