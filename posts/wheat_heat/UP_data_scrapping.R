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

# Gorakhpur ----------------------------------
# UK MET source
inventory_url <- "https://www.metoffice.gov.uk/hadobs/hadisd/v331_202303p/files/hadisd_station_info_v331_202303p.txt"

inventory <- read_table(inventory_url, 
                        col_names = c("station", "lat", "lon", "a")) %>% 
                        mutate(lat_r = lat *2 *pi/360, long_r = lon *2 *pi/360) %>% 
                        select(station,lat, lon, lat_r, long_r)

# For Gorakhpur (26.759265, 83.381055)
gor_lat <- 26.759265 *2 *pi/360 #converting longitude and latitude to radians
gor_long <- 83.381055 *2 *pi/360

gorakhpur_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * 
                                                                     sin(gor_lat) + 
                                                                     cos(lat_r) * cos(gor_lat) * 
                                                                     cos(gor_long - long_r)))) %>% 
                    arrange(d) %>% slice_head(n = 1) %>% select(station) %>% pull()

view(gorakhpur_station)

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_423790-99999.nc")

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

data_gorakhpur <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


gorakhpur <- data_gorakhpur %>% filter(year(time_s) %in% c(2020:2023))

gorakhpur_max_min <- setDT(gorakhpur)[, 
                                      list(MAX = max(temp_array),
                                           Min = min(temp_array)), by = list(time_s)]

# Meerut -------------------
# Meerut (28.981601, 77.695477)
mee_lat <- 28.981601 *2 *pi/360 #converting longitude and latitude to radians
mee_long <- 77.695477 *2 *pi/360

meerut_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(mee_lat) + 
                                                                  cos(lat_r) * cos(mee_lat) * cos(mee_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(meerut_station)
# meerut station is in Safdarjung airport

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_421820-99999.nc")

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

data_meerut <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


meerut <- data_meerut %>% filter(year(time_s) %in% c(2020:2023))

meerut_max_min <- setDT(meerut)[, list(MAX = max(temp_array),
                                           Min = min(temp_array)), by = list(time_s)]

# Etawah ------
# Etawah (26.802740, 79.011997)
eta_lat <- 26.802740 *2 *pi/360 #converting longitude and latitude to radians
eta_long <- 79.011997 *2 *pi/360

etawah_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(eta_lat) + 
                                                                  cos(lat_r) * cos(eta_lat) * 
                                                                  cos(eta_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(etawah_station)
# Etawah station is in Gwalior
w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_423610-99999.nc")

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

data_etawah <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


etawah <- data_etawah %>% filter(year(time_s) %in% c(2020:2023))

etawah_max_min <- setDT(etawah)[, list(MAX = max(temp_array),
                                       Min = min(temp_array)), by = list(time_s)]


# Bareilly -------------
# Bareilly (28.366350, 79.439787)
bar_lat <- 28.366350 *2 *pi/360 #converting longitude and latitude to radians
bar_long <- 79.439787 *2 *pi/360

bareilly_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(bar_lat) + 
                                                                    cos(lat_r) * cos(bar_lat) * cos(bar_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(bareilly_station)

w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_421890-99999.nc")

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

data_bareilly <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


bareilly <- data_bareilly %>% filter(year(time_s) %in% c(2020:2023))

bareilly_max_min <- setDT(bareilly)[, list(MAX = max(temp_array),
                                       Min = min(temp_array)), by = list(time_s)]

# Saharanpur ---------
#Saharanpur (29.964303, 77.555998)
sah_lat <- 29.964303 *2 *pi/360 #converting longitude and latitude to radians
sah_long <- 77.555998 *2 *pi/360

saharanpur_station <- inventory %>% mutate(d = 1.609 * 3963 * acos((sin(lat_r) * sin(sah_lat) + 
                                                                      cos(lat_r) * cos(sah_lat) * cos(sah_long - long_r)))) %>% 
  arrange(d) %>% 
  slice_head(n = 1) %>% 
  select(station) %>% 
  pull()
view(saharanpur_station)
# the saharanpur station is in Dehradun
w_da <- nc_open(".\\posts\\wheat_heat\\hadisd.3.3.1.202303p_19310101-20230401_421110-99999.nc")

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

data_saharanpur <- tibble(lat, lon, time_s = ymd(as_date(time_obs)), temp_array)


saharanpur <- data_saharanpur %>% filter(year(time_s) %in% c(2020:2023))

saharanpur_max_min <- setDT(saharanpur)[, list(MAX = max(temp_array),
                                           Min = min(temp_array)), by = list(time_s)]

br <- tibble(bareilly_max_min %>% filter(Min > 0))
et <- tibble(etawah_max_min %>% filter(Min > 0))
me <- tibble(meerut_max_min %>% filter(Min > 0))
sa <- tibble(saharanpur_max_min %>% filter(Min > 0))
go <- tibble(gorakhpur_max_min %>% filter(Min > 0))


br %>% full_join(et, by = 'time_s', suffix = c("_br", "_et"))%>% 
      full_join(me, by = 'time_s', suffix = c( " ", "_me")) %>% 
      full_join(sa, by = 'time_s', suffix = c("_me", "_sa")) %>% 
      full_join(go, by = 'time_s', suffix = c("", "_go")) %>% 
      rename(MAX_go = MAX, Min_go = Min) %>% remove_empty() %>% write_csv(".\\posts\\wheat_heat\\U_P.csv")
check <- read_csv(".\\posts\\wheat_heat\\U_P.csv")
head(check)

# up_long <- check %>% pivot_longer(c("MAX_br":"_go"), names_to = "variable", values_to = "temp")

up_summary <- check %>% group_by(time_s) %>% mutate(mean_max = mean(c(MAX_br, MAX_et, MAX_me, MAX_sa, MAX_go)),
                                      mean_min = mean(c(Min_br, Min_et, Min_me, Min_sa, Min_go))) %>% 
  select(time_s, mean_max, mean_min)

danger_days <- up_summary %>% filter(mean_max >= 30) %>% 
  filter(lubridate::month(time_s) %in% c(2,3)) %>% mutate(year = year(time_s)) %>% 
  group_by(year) %>% summarise(n = n())
max_days <-  up_summary %>% filter(mean_max >= 30) %>% 
  filter(lubridate::month(time_s) %in% c(2,3))

# Graphing -------
# # for highlighting the march and april months
# start <- as_date(c("2020-02-01", "2021-02-01", "2022-02-01", "2023-02-01"))
# end <- as_date(c("2020-03-31", "2021-03-31", "2022-03-31", "2023-03-31"))
# group <- c(1:4)
# rect <- data.frame(start, end, group)


p1 <- up_summary %>% ggplot(aes(x=time_s, y=mean_max)) +
  geom_area(fill="#69b3a2", alpha= 0.7) +
  geom_line(color="#69b3a2") + 
  geom_area(inherit.aes = FALSE, aes(x=time_s, y=mean_min), fill = "#FAF9C7")
  
p1 + 
  # geom_rect(data = rect, inherit.aes = FALSE, aes(xmin=start, xmax=end, ymin=0, ymax=Inf, group = group),
  #             color= "black", fill = "skyblue", alpha=0.25) +
  geom_abline(intercept = 18, colour = "#003300", slope = 0, linetype="dashed") +
  geom_abline(intercept = 24, colour = "#003300", slope = 0, linetype = "dashed") + theme_classic() +
  annotate("text", label = "THERMAL OPTIMA", y = 21, x = as_date("2021-06-28"), colour = "#024604",
           family = "r_m", face = "bold", size = 12) +
  labs(x = "Years", y = "Temperature",
          title = "DAILY TEMPERATURE",
          subtitle = "seasonal fluctuations of the minimum and maximum dry-bulb temperature in the wheat growing belt of Uttar Pradesh",
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
        plot.subtitle = element_text(size = 30, family = "noto")) +
  scale_y_continuous(label = function(x) {return(paste(x, "°C"))}) +
    annotate("text", label = "minimum temperature", y = 10, x = as_date("2020-07-01"), colour = "blue",
            family = "r_m", size = 12) + 
    annotate("curve", x = as_date("2020-05-01"), y = 10, xend = as_date("2020-04-12"), yend = 20, color = "black", linewidth = 0.5, 
             curvature =  -1, arrow = arrow(length = unit(0.03, "npc"))) +
    annotate("text", label = "maximum temperature", y = 15, x = as_date("2020-08-01"), colour = '#ec5f2b',
           family = "r_m", size = 12) +
    annotate("curve", x = as_date("2020-08-01"), y = 15.2, xend = as_date("2020-02-17"), yend = 25.5, colour = "black", linewidth = 0.5,
             curvature = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
    # annotate("text", label = "terminal heat stress: causes flower sterlity and reduces grain filling", y = 30, x = as_date("2020-08-25"), colour = "black",
    #          family = "r_m") + 
    # annotate("curve", x = as_date("2020-04-25"), y = 30, xend = as_date("2020-01-01"), yend = 30, curvature = -0.3,
    #          colour = "black", arrow = arrow(length = unit(0.03, "npc"))) +
    geom_abline(slope = 0, intercept = 30, colour = "red", size = 0.9) +
    annotate("text", label = "TERMINAL HEAT STRESS (30 °C): CAUSES FLOWER STERLITY AND IMPAIRS GRAIN FILLING", y = 42, x = as_date("2021-08-01"), colour = 'red',
           family = "r_m", size = 12) +
    annotate("curve", x = as_date("2021-08-01"), y = 41, xend = as_date("2021-10-01"), yend = 30,
           colour = "red", arrow = arrow(length = unit(0.03, "npc"))) +
    annotate("text", label = "Days in Feb and March when Tmax > 30 °C", x = as_date("2020-06-12"), y = 38, colour = "firebrick", size = 12) +
    annotate("curve", x = as_date("2020-10-15"), xend = as_date("2021-03-1"), y = 37, yend = 32, curvature = 0.3,
             size = 0.8, arrow = arrow(length = unit(0.03, "npc")), colour = "firebrick") +
    annotate("curve", x = as_date("2021-09-25"), y = 21, xend = as_date("2021-11-15"), yend = 24, size = 0.8,
           arrow = arrow(length = unit(0.02, "npc")), colour = "#024604", curvature = 0.3) +
    annotate("curve", x = as_date("2021-09-25"), y = 21, xend = as_date("2021-11-15"), yend = 18, size = 0.8,
           arrow = arrow(length = unit(0.02, "npc")), colour = "#024604", curvature = -0.3)
ggsave("uttar_pradesh.jpg", path = ".\\posts\\wheat_heat\\graphics", width = 25, height = 12.5, units = "cm")

