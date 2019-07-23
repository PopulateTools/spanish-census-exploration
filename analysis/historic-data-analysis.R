library(tidyverse)
library(ggplot2)
library(ggalt)
library(httr)
library(bbplot)
library(sf)
library(tmap)
library(leaflet)
library(rmapshaper)
library(geojsonsf)

set.seed(14723)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dataset <- "ds-censo-municipal-poblacion-derecho"
account <- Sys.getenv('POPULATE_DATA_ACCOUNT')
token <- Sys.getenv('POPULATE_DATA_TOKEN')
origin <- Sys.getenv('POPULATE_DATA_ORIGIN')

df <- NULL
years <- c(1877,1887,1897,1900,1910,1920,1930,1940,1950,1960,1970,1981,1991,2001,2011)

for(year in years){
  url <- paste0("https://data.populate.tools/", account , "/datasets/", dataset, ".csv?include=province,municipality&filter_by_year=", year)
  r <- GET(url, add_headers(authorization = paste0("Bearer ", token), origin = origin))
  partial_df <- content(r, "parsed")
  if(nrow(partial_df) == 0) { break }
  
  df <- rbind(df, partial_df)
}
  
df <- df %>% group_by(location_id) %>% mutate(avg = mean(value)) %>% ungroup()
df <- df %>% group_by(location_id) %>% mutate(delta_pct = (last(value) / first(value)*100)) %>% ungroup()
df <- df %>% group_by(location_id) %>% mutate(delta_pct_5 = (last(value) / nth(value, 11)*100)) %>% ungroup()
df <- df %>% group_by(location_id) %>% mutate(delta_pct_3 = (last(value) / nth(value, 14)*100)) %>% ungroup()
df <- df %>% group_by(location_id) %>% mutate(current_value = last(value)) %>% ungroup()
df <- df %>%
  group_by(location_id) %>% 
  mutate(base_value = first(value), last_value = last(value)) %>% 
  ungroup() %>% 
  mutate(value_idx = ((last_value - base_value)/base_value)*100) %>% 
  mutate(value_idx = replace(value_idx, value_idx == Inf, 100))


## Valor inicial y final en dotplot para provincias

df %>% group_by(province_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% select(province_name, total0, total1) %>% 
  distinct(province_name, total0, total1) %>% 
  ggplot(aes(y=reorder(province_name, total0), x=total0, xend=total1)) + 
    bbc_style() +
    geom_dumbbell(colour = "#dddddd",
                  size = 3,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1"
                  ) +
    scale_y_discrete(labels=function(label) substring(label, 0, 20)) +
    scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000),
                     labels = c(0, "1M", "2M", "3M", "4M", "5M", "6M"),
                     position = "top" )  +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_line(color="#eaeaea"),
      panel.grid.major.y = element_blank()
    )

ggsave(file="../output/barras_horiz_evolucion_poblacion_provincias_1877.svg", width = 24, height = 30, units = "cm")


# Versión responsive
df %>% group_by(province_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% select(province_name, total0, total1) %>% 
  distinct(province_name, total0, total1) %>% 
  ggplot(aes(y=reorder(province_name, total0), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_y_discrete(labels=function(label) substring(label, 0, 20)) +
  scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000),
                     labels = c(0, "1M", "2M", "3M", "4M", "5M", "6M"),
                     position = "top" )  +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color="#eaeaea"),
    panel.grid.major.y = element_blank()
  )

ggsave(file="../output/barras_horiz_evolucion_poblacion_provincias_1877_small.svg", width = 16, height = 30, units = "cm")
    
 
df %>% group_by(province_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% select(province_name, total0, total1) %>% 
  distinct(province_name, total0, total1) %>% 
  ggplot(aes(y=reorder(province_name, total1), x=total0, xend=total1)) + 
    bbc_style() +
    geom_dumbbell(colour = "#dddddd",
                  size = 3,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1"
                  ) +
    scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000),
                     labels = c(0, "1M", "2M", "3M", "4M", "5M", "6M"),
                     position = "top")  +
    scale_y_discrete(labels=function(label) substring(label, 0, 20)) +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_line(color="#eaeaea"),
      panel.grid.major.y = element_blank()
    )

ggsave(file="../output/barras_horiz_evolucion_poblacion_provincias_2011.svg", width = 24, height = 30, units = "cm")


df %>% group_by(province_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% select(province_name, total0, total1) %>% 
  distinct(province_name, total0, total1) %>% 
  ggplot(aes(y=reorder(province_name, total1), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_y_discrete(labels=function(label) substring(label, 0, 20)) +
  scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000, 4000000, 5000000, 6000000),
                     labels = c(0, "1M", "2M", "3M", "4M", "5M", "6M"),
                     position = "top")  +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color="#eaeaea"),
    panel.grid.major.y = element_blank()
  )

ggsave(file="../output/barras_horiz_evolucion_poblacion_provincias_2011_small.svg", width = 16, height = 30, units = "cm")


## Comparar rankings provincias

temp <- df %>% group_by(province_name, date) %>% 
  summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(province_name, total0, total1) %>% 
  distinct(province_name, total0, total1)
  
v1 <- (temp %>% arrange(desc(total0)))$province_name
v2 <- (temp %>% arrange(desc(total1)))$province_name

o <- 0.05
df_rankings <- data.frame(x = c(rep(1, length(v1)), rep(2, length(v2))),
                 x1 = c(rep(1 + o, length(v1)), rep(2 - o, length(v2))),
                 y = c(rev(seq_along(v1)), rev(seq_along(v2))),
                 g = c(v1, v2))

df_rankings %>% 
ggplot(aes(x=x, y=y, group=g, label=g)) +
  geom_path(aes(x=x1),
            arrow = arrow(length = unit(0.02,"npc")),
            size=0.5, color="#1380A1") +
  geom_text(size=4.5) +
  bbc_style() +
  theme(
      axis.title = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
    
ggsave(file="../output/diff_provincias_1877_2011.svg", width = 24, height = 30, units = "cm")

## Valor inicial y final en dotplot para municipios

df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  top_n(50, total0) %>% 
  ggplot(aes(y=reorder(municipality_name, total0), x=total0, xend=total1)) + 
    bbc_style() +
    geom_dumbbell(colour = "#dddddd",
                  size = 3,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1"
                  ) +
    scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c(0, "1M", "2M", "3M"),
                     position="top")  +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color="#eaeaea")
    ) 

ggsave(file="../output/barras_horiz_evolucion_poblacion_municipios_1877.svg", width = 24, height = 30, units = "cm")

df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  top_n(50, total0) %>% 
  ggplot(aes(y=reorder(municipality_name, total0), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c(0, "1M", "2M", "3M"),
                     position="top")  +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="#eaeaea")
  ) 

ggsave(file="../output/barras_horiz_evolucion_poblacion_municipios_1877_small.svg", width = 16, height = 30, units = "cm")


df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  top_n(50, total1) %>% 
  ggplot(aes(y=reorder(municipality_name, total1), x=total0, xend=total1)) + 
    bbc_style() +
    geom_dumbbell(colour = "#dddddd",
                  size = 3,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1"
                  ) +
    scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c(0, "1M", "2M", "3M"), position = "top")  +
    theme(
      axis.text.x = element_text(size = 11),
      axis.text.y = element_text(size = 10),
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color="#eaeaea")
    )

ggsave(file="../output/barras_horiz_evolucion_poblacion_municipios_2011.svg", width = 24, height = 30, units = "cm")

df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  top_n(50, total1) %>% 
  ggplot(aes(y=reorder(municipality_name, total1), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_x_continuous(breaks = c(0, 1000000, 2000000, 3000000),
                     labels = c(0, "1M", "2M", "3M"), position = "top")  +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color="#eaeaea")
  )

ggsave(file="../output/barras_horiz_evolucion_poblacion_municipios_2011_small.svg", width = 16, height = 30, units = "cm")

## Como eran las ciudades en esa epoca

summary_df <- df %>%  
  group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() 

# Densidad 1877
cities_2011 <- summary_df %>% arrange(desc(total0)) %>% filter(total1 >= 100000)

# Valor mediou ciudades en 1877
print(summary(cities_2011$total0)['Mean'])
# Valor mediou ciudades en 2011
print(summary(cities_2011$total1)['Mean'])

## Ciudades > 100k como eran antes
cities_2011 %>% 
  top_n(30, total0) %>% 
  ggplot(aes(y=reorder(municipality_name, total0), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_x_continuous(breaks = c(0, 20000, 50000, 100000, 1000000, 2000000, 3000000),
                     trans='log10',
                     labels = c(0, "20k", "50k", "100k", "1M", "2M", "3M"))  +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    plot.title = element_text(size=16),
    plot.subtitle = element_text(size=14),
    axis.ticks.x = element_line(colour = "#333333"), 
    axis.ticks.length =  unit(0.26, "cm")
  ) +
  labs(title="Ciudades de hoy en 1877 (log)",
       subtitle = "1877 - 2011. Fuente: Censo INE")


ggsave(file="../output/barras_horiz_evolucion_poblacion_ciudades_log.svg", width = 24, height = 30, units = "cm")


## Top ciudades que menos han crecido

df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  mutate(diff = ((total1 - total0) / total0)*100) %>% 
  filter(total0 > 21000) %>% 
  top_n(-25, diff) %>% 
  arrange(diff) %>% 
  ggplot(aes(y=reorder(municipality_name, -diff), x=total0, xend=total1)) + 
    bbc_style() +
    geom_dumbbell(colour = "#dddddd",
                  size = 3,
                  colour_x = "#FAAB18",
                  colour_xend = "#1380A1"
                  ) +
    scale_x_continuous(breaks = c(0, 25000, 50000, 75000, 100000, 150000, 250000),
                     labels = c(0, "25k", "50k", "75k", "100k", "150k", "200k"),
                     position = "top")  +
    theme(
      axis.text.x = element_text(size = 10),
      axis.text.y = element_text(size = 10),
      panel.grid.major.x = element_line(color="#eaeaea"),
      panel.grid.major.y = element_blank()
    ) 

ggsave(file="../output/barras_horiz_top_municipios_menos_crecimiento.svg", width = 24, height = 30, units = "cm")


df %>% group_by(municipality_name, date) %>% summarize(total = sum(value)) %>% 
  mutate(total0 = first(total), total1 = last(total)) %>% 
  select(municipality_name, total0, total1) %>% 
  distinct(municipality_name, total0, total1) %>% 
  ungroup() %>% 
  mutate(diff = ((total1 - total0) / total0)*100) %>% 
  filter(total0 > 21000) %>% 
  arrange(total0) %>% 
  top_n(-25, diff) %>% 
  ggplot(aes(y=reorder(municipality_name, total0), x=total0, xend=total1)) + 
  bbc_style() +
  geom_dumbbell(colour = "#dddddd",
                size = 2,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1"
  ) +
  scale_x_continuous(breaks = c(0, 25000, 50000, 75000, 100000, 150000, 250000),
                     labels = c(0, "25k", "50k", "75k", "100k", "150k", "200k"),
                     position = "top")  +
  theme(
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 12),
    panel.grid.major.x = element_line(color="#eaeaea"),
    panel.grid.major.y = element_blank()
  ) 

ggsave(file="../output/barras_horiz_top_municipios_menos_crecimiento_small.svg", width = 16, height = 30, units = "cm")

###### 
###### 
## Como te influye la proximidad a una ciudad
###### 
###### 

earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

df_places <- read_csv("../data/places.csv", col_types = "iiccccdd")
df_top_cities <- read_csv("../data/top_cities.csv", col_types = "cii") %>% 
  inner_join(df_places, by = c("location_id" = "location_id")) %>% 
  select(municipality_name, location_id, province_id.x, lat, lon)

df_distances <-  NULL
for(j in 1:nrow(df_places)) {
  shortest_distance <- 99999999999
  shortest_location_id <- NULL
  shortest_province_id <- NULL
  for(i in 1:nrow(df_top_cities)){
    dist <- earth.dist(df_top_cities[i, ]$lon, df_top_cities[i, ]$lat, df_places[j, ]$lon, df_places[j, ]$lat)
    if(dist < shortest_distance) {
      shortest_distance <- dist
      shortest_location_id <- df_top_cities[i, ]$location_id
      shortest_province_id <- df_top_cities[i, ]$province_id.x
    }
  }
  location_id <- df_places[j, ]$location_id
  df_distances <-  rbind(df_distances, data.frame(location_id, shortest_distance, shortest_location_id, shortest_province_id))
}

# Summarize df with delta_pct, delta_pct_3, delta_pct_5

df_summary <- df %>% 
  select(location_id, delta_pct, delta_pct_3, delta_pct_5, current_value) %>% 
  distinct(location_id, delta_pct, delta_pct_3, delta_pct_5, current_value)

df_places_with_distance <- df_places %>% 
  inner_join(df_distances, by = c("location_id" = "location_id")) %>% 
  inner_join(df_summary, by = c("location_id" = "location_id"))

# Top growth muncipalities
df_places_with_distance %>% 
  filter(shortest_distance > 1 & shortest_distance < 100) %>% 
  arrange(desc(delta_pct)) %>% 
  ggplot(aes(shortest_distance, log10(delta_pct))) + 
    geom_point() + geom_smooth(method="loess")

ggsave(file="../output/raw_relacion_dist_pct.svg", width = 24, height = 30, units = "cm")

# Top growth municipalities small multiples
df_places_with_distance %>% 
  filter(shortest_distance > 1 & shortest_distance < 100) %>% 
  arrange(desc(delta_pct)) %>% 
  ggplot(aes(shortest_distance, log10(delta_pct))) + 
    geom_smooth() +
    bbc_style() +
    facet_wrap( ~ province_name, ncol = 10) +
    theme(
        axis.text.y = element_blank(),
        strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.ticks.x = element_line(colour = "#333333"), 
        axis.ticks = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        panel.spacing.x = unit(1, "lines")
    ) + labs(title="")

ggsave(file="../output/raw_relacion_dist_pct_small_multiples.svg", width = 30, height = 20, units = "cm")


###
## Compare evolution of population with working by sectors
###
df_employment_sectors <- read_csv("../data/empleo_sectores.csv", col_types = "idddd")
colnames(df_employment_sectors) <- c("date", "agricultura", "industria", "construccion", "servicios")

df_employment_sectors <- df_employment_sectors %>% 
  gather(var, value, industria, agricultura, construccion, servicios, -date)

  
df_employment_sectors %>% 
  ggplot(aes(x=date, y=value, colour=var)) + 
  bbc_style() +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = c(1877, 1900, 1920, 1940, 1960, 1981, 2011)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), labels=c("0%", "20%", "40%", "60%", "80%")) +
  theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = element_line(colour = "#333333"), 
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size=16),
      plot.subtitle = element_text(size=14),
      axis.ticks.length =  unit(0.26, "cm")) 
  
ggsave(file="../output/aporte_empleo_por_sector.svg", width = 30, height = 20, units = "cm")


df_employment_sectors %>% 
  ggplot(aes(x=date, y=value, colour=var)) + 
  bbc_style() +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = c(1877, 1900, 1920, 1940, 1960, 1980, 1995, 2011)) +
  scale_y_continuous(breaks = c(0, 20, 40, 60, 80), labels=c("0%", "20%", "40%", "60%", "80%")) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.ticks.x = element_line(colour = "#333333"), 
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size=16),
    plot.subtitle = element_text(size=14),
    axis.ticks.length =  unit(0.26, "cm")) 

ggsave(file="../output/aporte_empleo_por_sector_small.svg", width = 20, height = 16, units = "cm")
####

df_employment_people_sectors <- read_csv("../data/empleo_personas.csv", col_types = "iddddd") %>% 
  gather(var, value, industria, agricultura, construccion, servicios, total, -year) %>% 
  mutate(
    value = value * 1000000
  )

df_employment_people_sectors %>% 
  ggplot(aes(x=year, y=value, colour=var)) + 
  bbc_style() +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = c(1877, 1900, 1920, 1940, 1960, 1980, 1995, 2011)) +
  scale_y_continuous(breaks = c(0, 2500000, 5000000, 10000000, 15000000, 20000000),
                     labels = c(0, "2.5M",  "5M",    "10M",   "15M",    "20M"))  +
  theme(
      legend.position = "top",
      legend.direction = "horizontal",
      axis.ticks.x = element_line(colour = "#333333"), 
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      plot.title = element_text(size=16),
      plot.subtitle = element_text(size=14),
      axis.ticks.length =  unit(0.26, "cm")) 
  
ggsave(file="../output/evolucion_pb_aporte_empleo_por_sector.svg", width = 30, height = 20, units = "cm")

df_employment_people_sectors %>% 
  ggplot(aes(x=year, y=value, colour=var)) + 
  bbc_style() +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, size = 1, colour="#333333") +
  scale_x_continuous(breaks = c(1877, 1900, 1920, 1940, 1960, 1980, 1995, 2011)) +
  scale_y_continuous(breaks = c(0, 2500000, 5000000, 10000000, 15000000, 20000000),
                     labels = c(0, "2.5M",  "5M",    "10M",   "15M",    "20M"))  +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.ticks.x = element_line(colour = "#333333"), 
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    plot.title = element_text(size=16),
    plot.subtitle = element_text(size=14),
    axis.ticks.length =  unit(0.26, "cm")) 

ggsave(file="../output/evolucion_pb_aporte_empleo_por_sector_small.svg", width = 20, height = 16, units = "cm")

## 
## Mapas
##

pal <- c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84")

# ---- Read the shapefiles
p_canarias <- '../data/ign_spain/recintos_municipales_inspire_canarias_wgs84/'
p_peninsula <- '../data/ign_spain/recintos_municipales_inspire_peninbal_etrs89/'


peninsula <- sf::read_sf(p_peninsula, options = "ENCODING=UTF-8")
peninsula_s <- rmapshaper::ms_simplify(input = as(peninsula, 'Spatial'), keep = 0.1) %>% st_as_sf()

canarias <- sf::read_sf(p_canarias, options = "ENCODING=UTF-8")
canarias_s <- rmapshaper::ms_simplify(input = as(canarias, 'Spatial'), keep = 0.1) %>% st_as_sf()

ign_spain <- peninsula_s %>% 
  st_transform(4326) %>%
  rbind(canarias_s) %>% 
  mutate(MUNICODE_INE = as.numeric(str_sub(NATCODE, start = 7, end = 11))) %>% 
  select(MUNICODE_INE, NAMEUNIT) %>% 
  inner_join(df, by = c("MUNICODE_INE" = "location_id")) %>% 
  filter(date == 2011) 

st_write(ign_spain, "../output/spain.shp", layer_options = "ENCODING=UTF-8")

tmap_mode("view")
# tmap_mode("plot")

tm_shape(ign_spain) +
  tm_polygons("value_idx", id="municipality_name", 
    palette = pal, 
    # palette="-RdYlBu", contrast=1, 
    title = "% Incremento Población", contrast = 0.3, border.col = "#cccccc",
    border.lwd = 0.5,
    popup.vars=c("Población (1877)"="base_value", "Población (2011)"="value", "Incremento desde 1877 (%)"="value_idx"),
    popup.format=list(value_idx=list(digits=2)),
    # breaks = c(0, 5000, 10000, 50000, 200000, 500000, 1000000, Inf)
    breaks = c(-Inf, 0, 100, 500, 1000, 5000, 10000, Inf)
  ) +
  tm_borders() +
  tm_view(set.view=c(-3.68041, 40.4449045, 7))
