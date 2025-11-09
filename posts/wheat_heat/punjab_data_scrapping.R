library(lubridate)
library(glue)
library(tidyverse)
library(janitor)
library(ncdf4)
library(data.table)
library(ggrepel)
library(ggtext)
library(showtext)




# Import fonts
# First argument = google name, 
# Secont name = font name in R
font_add_google('Roboto Mono', 'r_m')
font_add_google('Noto Sans', 'noto')
font_add_google('Bangers', 'Bangers')

showtext_auto()

#Patiala, Ludhiana, Bathinda, Ferozepur


# UK MET source
inventory_url <- "https://www.metoffice.gov.uk/hadobs/hadisd/v331_202303p/files/hadisd_station_info_v331_202303p.txt"

inventory <- read_table(inventory_url, 
                        col_names = c("station", "lat", "lon", "a")) %>% 
  mutate(lat_r = lat *2 *pi/360, long_r = lon *2 *pi/360) %>% 
  select(station,lat, lon, lat_r, long_r)

# Bathinda (30.206802, 74.944951) ---------
bat_lat <- 30.206802 *2 *pi/360 #converting longitude and latitude to radians
bat_long <- 74.944951 *2 *pi/360

bathinda_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * 
                                                                  sin(bat_lat) + 
                                                                  cos(lat_r) * cos(bat_lat) * 
                                                                  cos(bat_long - long_r)))) %>% 
  arrange(d) %>% slice_head(n = 1) %>% select(station) %>% pull()

view(bathinda_station)

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_421310-99999.nc")

head(w_da)
print(w_da)
attributes(w_da$var)
attributes(w_da$dim)
lat <- ncvar_get(w_da, "latitude")
lon <- ncvar_get(w_da, "longitude")
time <- ncvar_get(w_da, "time")
tunits <- ncatt_get(w_da, "time", "units")
nt <- dim(time)
temp_array <- ncvar_get(w_da, "temperatures")
fillvalue <- ncatt_get(w_da, 
                       "temperatures", 
                       "_FillValue")
the_date <- ymd_hm("1931-01-01 00:00")

# fill nc FillValues with NAs
temp_array[temp_array == fillvalue$value] <- NA
temp_array

time_obs <- as.POSIXct(time*3600, origin = the_date)
tail(time_obs)

data_bathinda <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


bathinda <- data_bathinda %>% filter(year(time_s) %in% c(2020:2023))

bathinda_max_min <- setDT(bathinda)[, 
                                list(MAX = max(temp_array),
                                     Min = min(temp_array)), by = list(time_s)]
#Bathinda's station is in Hisar

# # Ferozpur (30.930019, 74.613274) -------------------
# fer_lat <- 30.930019 *2 *pi/360 #converting longitude and latitude to radians
# fer_long <- 74.613274 *2 *pi/360
# 
# ferozepur_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(fer_lat) + 
#                                                                        cos(lat_r) * cos(fer_lat) * cos(fer_long - long_r)))) %>% 
#   arrange(d) %>% 
#   slice_head(n = 1) %>% 
#   select(station) %>% 
#   pull()
# view(ferozepur_station)
# # Ferozepur station is in Lahore
# 
# w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_416410-99999.nc")
# 
# head(w_da)
# print(w_da)
# attributes(w_da$var)
# attributes(w_da$dim)
# lat <- ncvar_get(w_da, "latitude")
# lon <- ncvar_get(w_da, "longitude")
# time <- ncvar_get(w_da, "time")
# tunits <- ncatt_get(w_da, "time", "units")
# nt <- dim(time)
# temp_array <- ncvar_get(w_da, "temperatures")
# fillvalue <- ncatt_get(w_da, 
#                        "temperatures", 
#                        "_FillValue")
# the_date <- ymd_hm("1931-01-01 00:00")
# 
# # fill nc FillValues with NAs
# temp_array[temp_array == fillvalue$value] <- NA
# temp_array
# 
# time_obs <- as.POSIXct(time*3600, origin = the_date)
# tail(time_obs)
# 
# data_ferozepur <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)
# 
# 
# ferozepur <- data_ferozepur %>% filter(year(time_s) %in% c(2020:2023))
# 
# ferozepur_max_min <- setDT(ferozepur)[, list(MAX = max(temp_array),
#                                                  Min = min(temp_array)), by = list(time_s)]
# continue for Patiala and Ludhiana
# Patiala (30.330320, 76.386381) -----
pat_lat <- 30.330320 *2 *pi/360 #converting longitude and latitude to radians
pat_long <- 76.386381 *2 *pi/360

patiala_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(pat_lat) + 
                                                                  cos(lat_r) * cos(pat_lat) * 
                                                                  cos(pat_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(patiala_station)

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_421010-99999.nc")

head(w_da)
print(w_da)
attributes(w_da$var)
attributes(w_da$dim)
lat <- ncvar_get(w_da, "latitude")
lon <- ncvar_get(w_da, "longitude")
time <- ncvar_get(w_da, "time")
tunits <- ncatt_get(w_da, "time", "units")
nt <- dim(time)
temp_array <- ncvar_get(w_da, "temperatures")
fillvalue <- ncatt_get(w_da, 
                       "temperatures", 
                       "_FillValue")
the_date <- ymd_hm("1931-01-01 00:00")

# fill nc FillValues with NAs
temp_array[temp_array == fillvalue$value] <- NA
temp_array

time_obs <- as.POSIXct(time*3600, origin = the_date)
tail(time_obs)

data_patiala<- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


patiala <- data_patiala %>% filter(year(time_s) %in% c(2020:2023))

patiala_max_min <- setDT(patiala)[, list(MAX = max(temp_array),
                                       Min = min(temp_array)), by = list(time_s)]

# Data cleaning -----

ba <- tibble(bathinda_max_min %>% filter(Min > 0))
# fer <- tibble(ferozepur_max_min %>% filter(Min > 0))
pat <- tibble(patiala_max_min %>% filter(Min > 0))


ba %>% full_join(pat, by = 'time_s', suffix = c("_ba", "_pat"))%>% 
  write_csv(".\\posts\\wheat_heat\\punjab.csv")
check <- read_csv(".\\posts\\wheat_heat\\punjab.csv")
head(check)

# up_long <- check %>% pivot_longer(c("MAX_br":"_go"), names_to = "variable", values_to = "temp")

punjab_summary <- check %>% group_by(time_s) %>% mutate(mean_max = mean(c(MAX_ba, MAX_pat)),
                                                    mean_min = mean(c(Min_ba, Min_pat))) %>% 
  select(time_s, mean_max, mean_min)

# danger_days <- up_summary %>% filter(mean_max >= 30) %>% 
#   filter(lubridate::month(time_s) %in% c(2,3)) %>% mutate(year = year(time_s)) %>% 
#   group_by(year) %>% summarise(n = n())
max_days <-  punjab_summary %>% filter(mean_max >= 30) %>% 
  filter(lubridate::month(time_s) %in% c(2,3))

# Graphing -------
# # for highlighting the march and april months

p2 <- punjab_summary %>% ggplot(aes(x=time_s, y=mean_max)) +
  geom_area(fill="#69b3a2", alpha= 0.7) +
  geom_line(color="#69b3a2") + 
  geom_area(inherit.aes = FALSE, aes(x=time_s, y=mean_min), fill = "#FAF9C7")

p2 +  geom_abline(intercept = 18, colour = "#003300", slope = 0, linetype="dashed") +
  geom_abline(intercept = 24, colour = "#003300", slope = 0, linetype = "dashed") + theme_classic() +
  labs(x = "Years", y = "Temperature",
       title = "DAILY TEMPERATURE",
       subtitle = "seasonal fluctuations of the minimum and maximum dry-bulb temperature in the wheat growing belt of Punjab",
       caption = "DATA: Met Office Hadley Centre, UK; graphing by Ankur Jamwal",
       coord_cartesian(clip = "off")) +
  geom_point(inherit.aes = FALSE, data = max_days, 
             aes(x = time_s, y = mean_max), 
             color = "firebrick", shape = "diamond", size = 2) +
  theme(axis.text = element_text(colour = "black", size = 25, face = "bold"),
        axis.title = element_text(color = "sienna", size = 30, face = "bold"),
        #plot.title = element_text(face = "bold", margin = margin(10, 0, 10, 0), size = 14),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.title = element_text(family = "Bangers", size = 50),
        plot.caption = element_text(size = 30),
        plot.subtitle = element_text(size = 30, family = "Instrument")) +
  scale_y_continuous(label = function(x) {return(paste(x, "°C"))}) + 
  geom_abline(slope = 0, intercept = 30, colour = "red", size = 0.9)
ggsave("punjab.jpg", path = ".\\posts\\wheat_heat\\graphics", width = 25, height = 12.5, units = "cm")
