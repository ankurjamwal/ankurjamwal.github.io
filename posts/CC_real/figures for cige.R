library(tidyverse)
library(patchwork)
setwd('.//posts//CC_real')
getwd()

complete <- carbon_temp |> drop_na()
segment <- complete |> filter(Year > 1976)

e <- ggplot(complete) + geom_point(aes(x = Year, y = emission))


t <- ggplot(data = complete) + 
  geom_point(aes(Year, temperature))+
  geom_path(aes(x = Year, y = temperature), show.legend = FALSE) + 
  geom_hline(yintercept = 0.0, colour = "red", 
             linetype = "dashed", linewidth = 1) + theme_classic() +
  theme(axis.text = element_text(colour = "black", size = 55, face = "bold"),
        axis.title = element_text(color = "sienna", size = 50, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 35),
        plot.background = element_rect(fill = "#ECEBBD")) +
  labs(x = "Year", y = "Temperature anomaly (°C)", 
       colour = "Agency",
      illustration = "Ankur Jamwal", caption = "Data: UK-Met Office's HadCRUT5 dataset & Our World In Data
      Illustration: Ankur Jamwal") +
  geom_path(data = segment, aes(x = Year, y = temperature), colour = 'red',
            size = 2, alpha = 0.45) +
  annotate("text", label = "Mean Annual Global Surface Temperature consistently above the IPCC's reference point",
           x = 1920, y = 0.75, family = "jost", size = 19) +
  annotate("curve", x = 1920, y = 0.6, xend = 1976, yend = 0.2, colour = "purple", linewidth = 1,
           curvature = 0.25, arrow = arrow(length = unit(0.05, "npc")))

carbon_emission <- ggplot(data = complete) + 
  geom_point(aes(Year, emission)) +
  geom_path(aes(x = Year, y = emission), show.legend = FALSE) + 
  theme_classic() +
  theme(axis.text = element_text(colour = "black", size = 55, face = "bold"),
        axis.title = element_text(color = "sienna", size = 50, face = "bold"),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 25),
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(),
        plot.background = element_rect(fill = '#E5ECF8')) +
  scale_y_continuous(label = function(x) {return(paste(x, "billion t"))}) +
  annotate("text", label = "Global carbon dioxide emissions consistently rising post Industrial Revolution",
           x = 1920, y = 25, family = "jost", size = 19) +
  labs(y = "CO2 emission (Mean Annual)")

carbon_emission/t

ggsave(filename = "save_plot.png", path = 'C:\\Users\\AnkurJamwal\\OneDrive - Azim Premji Foundation\\Desktop',
       scale = 2)
