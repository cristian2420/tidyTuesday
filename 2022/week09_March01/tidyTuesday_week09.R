suppressWarnings( library(tidytuesdayR))
suppressWarnings( library(tidyverse))
suppressWarnings( library(wesanderson))
suppressWarnings( library(geofacet))

stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')


stateDF <- stations %>% count(STATE, FUEL_TYPE_CODE) %>% filter(STATE != "ON")
labels.vec <- c("Biodiesel", "Compressed Natural Gas", "Electric", "Ethanol", "Hydrogen", "Liquefied Natural Gas", "Propane")
f1 <- "#93b5ab"


ggp <- ggplot(stateDF, aes(x = FUEL_TYPE_CODE, y = n, fill = FUEL_TYPE_CODE )) +
  geom_bar(stat = "identity") +
  geom_col() +
  coord_flip() +
  scale_y_continuous(trans='log10') +
  geom_text(aes(4, 3.5, label = STATE), lineheight = 0.8, size = 4, stat = "unique", alpha = 0.15) +
  labs(title = "Alternative Fueling Stations", 
       subtitle = "Number of stations per US state by fuel type", 
       caption = "Source US DOT | Graphic: Cristian Gonzalez-Colin", 
       x = "", 
       y = "") +
  scale_fill_manual(name = "Type of fueling", labels = labels.vec, values = wes_palette("Moonrise3", 7, type = c("continuous")))+
  facet_geo(~ STATE, grid = "us_state_with_DC_PR_grid1") + 
  #guides(fill=guide_legend(ncol=2)) +
  theme_minimal() +
  theme(plot.background = element_rect(colour = f1, fill = f1),
        panel.background = element_rect(colour = f1, fill = f1), 
        plot.title = element_text(colour = "black", size=46, face = "bold", hjust = 0.5), 
        plot.subtitle = element_text(colour = "black", size=30, hjust = 0.5), 
        plot.caption = element_text(colour = "black", size=15, hjust = 1), 
        strip.text.x = element_blank(),
        strip.background = element_blank(), 
        axis.text.y=element_blank(),
        axis.text.x=element_text(size = 12, angle = 50, vjust = 1, hjust=1),
        legend.background = element_rect(colour = f1, fill = f1), 
        legend.key = element_rect(colour = f1, fill = f1),
        legend.position = c(0.85, 0.25),
        legend.text = element_text(colour = "black", size=20, hjust = 0), 
        legend.title = element_text(colour = "black", size=30, hjust = 0))


plot.nam <- paste0("tidyTuesday_week09.png")
ggsave(here::here(plot.nam), ggp, dpi = 320, height = 15, width = 25)




