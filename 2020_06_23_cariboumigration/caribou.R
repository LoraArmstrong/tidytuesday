library(tidytuesdayR)
library(tidyverse)
library(ggmap)
library(maps)
library(lubridate)
library(broom)

tt <- tidytuesdayR::tt_load("2020-06-23")

loc <- tt$locations
ind <- tt$individuals

# Number of locations (nloc) per individual caribou
nloc_per_caribou <- loc %>%
  group_by(animal_id) %>%
  summarize(n = n()) %>%
  arrange(n)

# Number of caribou with each nloc
nloc_hist <- nloc_per_caribou %>%
  count(nloc = n)

ggplot() +
  geom_histogram(data = nloc_hist, aes(n), bins = 100) +
  xlab("Number of locations") +
  ylab("Number of caribou") +
  ggtitle("Histogram: Number of locations per caribou")

# Smaller dataset for testing
ids <- ind$animal_id
loc <- loc %>%
  filter(animal_id %in% ids) %>%
  mutate(
    year = lubridate::year(timestamp),
    month = lubridate::month(timestamp),
    day = lubridate::day(timestamp),
    date = lubridate::date(timestamp)
  ) %>%
  group_by(animal_id, date) %>%
  slice(1) %>%
  ungroup()

# Plot map of Canada in ggplot2
# Based on https://tinyurl.com/ya2m9m4r
worldmap <- map_data("world", country = "Canada")
wrld <- c(geom_polygon(aes(long, lat, group = group),
  size = 0.1,
  # colour= "#090D2A",#090D2A",
  colour = "#151e5e", # 151e5e",
  fill = "#151e5e",
  alpha = 0.8,
  data = worldmap
))

plot <- ggplot() +
  wrld +
  theme(
    panel.background =
      element_rect(fill = "#00001C", colour = "#00001C"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  # coord_cartesian(xlim = c(-166, -53), ylim = c(23,84)) +
  coord_cartesian(xlim = c(-124, -120), ylim = c(53, 57)) +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.position = "none"
  ) +
  geom_path(
    data = loc,
    aes(
      x = longitude,
      y = latitude,
      group = animal_id,
      color = animal_id
    ),
    size = 0.3
  )
show(plot)

# TODO: Make inset with map of N. America
