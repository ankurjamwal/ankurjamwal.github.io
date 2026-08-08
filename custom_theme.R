library(ggplot2)
library(ggtext)
library(showtext)




# Import fonts
# First argument = google name, 
# Secont name = font name in R
font_add_google('Roboto Mono', 'r_m')
font_add_google('Noto Sans', 'noto')
font_add_google('Bangers', 'Bangers')
font_add_google('Instrument Serif', 'Instrument')

showtext_auto()

theme_custom_styled <- function(base_size = 11, base_family = "sans") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # Axis Text & Title Styling
      axis.text = element_text(colour = "black", size = 25, face = "bold"),
      axis.title = element_text(color = "sienna", size = 30, face = "bold"),
      
      # Title & Caption Alignments
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Text & Custom Font Styling
      plot.title = element_text(family = "Bangers", size = 45),
      plot.subtitle = element_text(size = 20, family = "Instrument"),
      plot.caption = element_text(
        family = "mono",
        size = 15,
        hjust = 1,                                # right-aligned
        colour = scales::alpha("black", 0.75)      # 0.75 alpha via colour, not element_text directly
      )
    )
}
