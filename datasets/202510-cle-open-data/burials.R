# Dirty items:

# - Some ages > 100 are suspect.
# - Not all burial times are valid data. When burial time is '00:00:00', '23:00:00', '01:00:00', '23:03:58', '00:03:58', these have been identified as placeholders.
#   - Perhaps time should be a different field so that proper NA's can fill the placeholder values.
# - One burial date record can probably be corrected "0199-12-24 08:58:30"

{
  library(arcgis)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
}

# connect to data and download
url = "https://services3.arcgis.com/dty2kHktVXHrqO8i/arcgis/rest/services/Cemeteries_public_65acc8aeda5349ee82d4d534f6be584a/FeatureServer"
con = arcgislayers::arc_open(url)
con

{
  cemeteries_lyr = arcgislayers::get_layer(con, 0)
  cemeteries = arcgislayers::arc_select(cemeteries_lyr)

  sections_lyr = arcgislayers::get_layer(con, 1)
  sections = arcgislayers::arc_select(sections_lyr)

  lots_lyr = arcgislayers::get_layer(con, 4)
  lots = arcgislayers::arc_select(lots_lyr)

  burials_lyr = arcgislayers::get_layer(con, 5)
  burials = arcgislayers::arc_select(burials_lyr)
}

# Data overview
cemeteries |> glimpse()
sections |> glimpse()
lots |> glimpse()
burials |> glimpse()

# There are people exceptionally long-lived. Look at this individuals.
burials$age |> summary()
burials |> filter(age > 100) |> select(age, birthdate, deathdate, burialdate) |>
  arrange(desc(age)) |>
  head()

# 10 most common names! Not very surprising here
burials |> 
  filter(!is.na(firstname)) |>
  count(fullname) |>
  arrange(desc(n)) |>
  head(10)

# All people with surname SMITH, but without a first name -- burials tend to be long ago.
burials |> 
  filter(fullname == 'SMITH,') |>
  pull(burialdate) |> 
  summary()

# What is the distribution of burial date?
burials |>
  pull(burialdate) |>
  summary()

# What is this particular record? Based on the death date, the correct burial year is likely 1999.
burials |>
  filter(burialdate != "0199-12-24 08:58:30") |>
  pull(burialdate) |>
  summary()

# Burials are decreasing over time, somewhat -- could this be due to cremations?
ggplot(subset(burials, ! burialdate %in% "0199-12-24 08:58:30")) + 
    geom_histogram(aes(x = lubridate::as_date(burialdate)), fill = "powderblue", color = "black", bins = 50) + 
    labs(x = 'date', y = 'burials') + 
    xlim(c(1813, 2025))  

# Burials within the past week
burials |>
  filter(burialdate %>% between(as.Date('2025-10-20'), as.Date('2025-10-25')))

# Distribution of burial *times*, removing errant data. 
# Many people buried in the morning, less over lunch, picks up in the afternoon
# again.
burials |>
  mutate(
    burialtime = format(burialdate, "%H:%M:%S") |> hms::as.hms(),
    burialyear = year(burialdate)
  ) |>
  filter(!burialtime %in% c('00:00:00', '23:00:00', '01:00:00', '23:03:58', '00:03:58')) |>
  # count(burialyear, burialtime) |> 
  ggplot() +
  geom_histogram(aes(x = burialtime), fill = "powderblue", color = "black")
