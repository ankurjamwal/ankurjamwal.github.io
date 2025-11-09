
# New -----
#This is for scrapping the weather data from the web. 
#Tutorial is here https://www.youtube.com/watch?v=V5Df6vw4-e8&t=29s

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

#Sehore, Narshinghpur, Ratlam (Indore)


# UK MET source
inventory_url <- "https://www.metoffice.gov.uk/hadobs/hadisd/v331_202303p/files/hadisd_station_info_v331_202303p.txt"

inventory <- read_table(inventory_url, 
                        col_names = c("station", "lat", "lon", "a")) %>% 
  mutate(lat_r = lat *2 *pi/360, long_r = lon *2 *pi/360) %>% 
  select(station,lat, lon, lat_r, long_r)

# Sehore (23.203328, 77.086740) ---------
seh_lat <- 23.203328 *2 *pi/360 #converting longitude and latitude to radians
seh_long <- 77.086740 *2 *pi/360

sehore_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * 
                                                                sin(seh_lat) + 
                                                                cos(lat_r) * cos(seh_lat) * 
                                                                cos(seh_long - long_r)))) %>% 
                  arrange(d) %>% slice_head(n = 1) %>% select(station) %>% pull()

view(sehore_station)

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_426670-99999.nc")

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

data_sehore <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


sehore <- data_sehore %>% filter(year(time_s) %in% c(2020:2023))

sehore_max_min <- setDT(sehore)[, 
                                      list(MAX = max(temp_array),
                                           Min = min(temp_array)), by = list(time_s)]

# Narsinghpur -------------------
# Narsinghpur (22.947729, 79.174425)
nar_lat <- 22.947729 *2 *pi/360 #converting longitude and latitude to radians
nar_long <- 79.174425 *2 *pi/360

narsinghpur_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(nar_lat) + 
                                                                  cos(lat_r) * cos(nar_lat) * cos(nar_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(narsinghpur_station)
# narsinghpur station is in Jabalpur

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_426750-99999.nc")

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

data_narsinghpur <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


narsinghpur <- data_narsinghpur %>% filter(year(time_s) %in% c(2020:2023))

narsinghpur_max_min <- setDT(narsinghpur)[, list(MAX = max(temp_array),
                                       Min = min(temp_array)), by = list(time_s)]

# Ratlam ------
# Ratlam (23.336821916079668, 75.03695084872753)
rat_lat <- 23.336821916079668 *2 *pi/360 #converting longitude and latitude to radians
rat_long <- 75.03695084872753 *2 *pi/360

ratlam_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(rat_lat) + 
                                                                  cos(lat_r) * cos(rat_lat) * 
                                                                  cos(rat_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(ratlam_station)
# Ratlam station is in Indore
w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_427540-99999.nc")

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

data_ratlam <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


ratlam <- data_ratlam %>% filter(year(time_s) %in% c(2020:2023))

ratlam_max_min <- setDT(ratlam)[, list(MAX = max(temp_array),
                                       Min = min(temp_array)), by = list(time_s)]


# Data cleaning -----

se <- tibble(sehore_max_min %>% filter(Min > 0))
nar <- tibble(narsinghpur_max_min %>% filter(Min > 0))
rat <- tibble(ratlam_max_min %>% filter(Min > 0))


se %>% full_join(nar, by = 'time_s', suffix = c("_se", "_nar"))%>% 
  full_join(rat, by = 'time_s') %>% 
  rename(MAX_rat = MAX, Min_rat = Min) %>% remove_empty() %>% 
  write_csv(".\\posts\\wheat_heat\\M_P.csv")
check <- read_csv(".\\posts\\wheat_heat\\M_P.csv")
head(check)

# up_long <- check %>% pivot_longer(c("MAX_br":"_go"), names_to = "variable", values_to = "temp")

mp_summary <- check %>% group_by(time_s) %>% mutate(mean_max = mean(c(MAX_se, MAX_nar, MAX_rat)),
                                                    mean_min = mean(c(Min_se, Min_nar, Min_rat))) %>% 
                                      select(time_s, mean_max, mean_min)

# danger_days <- up_summary %>% filter(mean_max >= 30) %>% 
#   filter(lubridate::month(time_s) %in% c(2,3)) %>% mutate(year = year(time_s)) %>% 
#   group_by(year) %>% summarise(n = n())
max_days <-  up_summary %>% filter(mean_max >= 30) %>% 
   filter(lubridate::month(time_s) %in% c(2,3))

# Graphing -------
# # for highlighting the march and april months

p2 <- mp_summary %>% ggplot(aes(x=time_s, y=mean_max)) +
  geom_area(fill="#69b3a2", alpha= 0.7) +
  geom_line(color="#69b3a2") + 
  geom_area(inherit.aes = FALSE, aes(x=time_s, y=mean_min), fill = "#FAF9C7")

p2 +  geom_abline(intercept = 18, colour = "#003300", slope = 0, linetype="dashed") +
  geom_abline(intercept = 24, colour = "#003300", slope = 0, linetype = "dashed") + theme_classic() +
  labs(x = "Years", y = "Temperature",
       title = "DAILY TEMPERATURE",
       subtitle = "seasonal fluctuations of the minimum and maximum dry-bulb temperature in the wheat growing belt of Madhya Pradesh",
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

  # annotate("text", label = "TERMINAL HEAT STRESS (35 °C): CAUSES FLOWER STERLITY AND IMPAIRS GRAIN FILLING", y = 42, x = as_date("2021-08-01"), colour = 'red',
  #          family = "r_m", size = 12) +
  # annotate("curve", x = as_date("2021-08-01"), y = 41, xend = as_date("2021-10-01"), yend = 30,
  #          colour = "red", arrow = arrow(length = unit(0.03, "npc"))) +
  # annotate("text", label = "Days in Feb and March when Tmax > 30 °C", x = as_date("2020-06-12"), y = 38, colour = "firebrick", size = 12) +
  # annotate("curve", x = as_date("2020-10-15"), xend = as_date("2021-03-1"), y = 37, yend = 32, curvature = 0.3,
  #          size = 0.8, arrow = arrow(length = unit(0.03, "npc")), colour = "firebrick") +
  # annotate("curve", x = as_date("2021-09-25"), y = 21, xend = as_date("2021-11-15"), yend = 24, size = 0.8,
  #          arrow = arrow(length = unit(0.02, "npc")), colour = "#024604", curvature = 0.3) +
  # annotate("curve", x = as_date("2021-09-25"), y = 21, xend = as_date("2021-11-15"), yend = 18, size = 0.8,
  #          arrow = arrow(length = unit(0.02, "npc")), colour = "#024604", curvature = -0.3)
ggsave("madhya_pradesh.jpg", path = ".\\posts\\wheat_heat\\graphics", width = 25, height = 12.5, units = "cm")

