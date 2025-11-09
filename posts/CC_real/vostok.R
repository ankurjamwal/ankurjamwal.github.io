vostok_temp <- nc_open("C:\\Users\\AnkurJamwal\\Downloads\\data.nc")
head(vostok_temp)
str(vostok_temp)
temp <- ncvar_get(vostok_temp, "temp")
year <- ncvar_get(vostok_temp, "T")
vostok_bind_t <- as_tibble(cbind(temp, year))
head(vostok_bind_t)
t <- ggplot(data = vostok_bind_t, aes(x = year, y = temp)) + geom_point() +
  geom_path()

DD <- read_tsv("C:\\Users\\AnkurJamwal\\Downloads\\deltaD.txt")
DD <- DD |> 
  mutate(temp = (deltaD + 440)/6.2, ice_ageBP = ice_ageBP/1000)

ggplot(DD) + geom_path(aes(x = ice_ageBP, y = temp))
