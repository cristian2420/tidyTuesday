library(tidyverse)
library(countrycode)
library(rworldmap)
library(rgeos)
library(MetBrewer)
library(ggpubr)
library(patchwork)

erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

########DATA countries coordinates
#Get World map
worldMap <- getMap(resolution =  "low")
mapworld_df <- fortify( worldMap)
mapworld_df <- worldMap@data %>% select(NAME_SORT, ISO_A2) %>%
  mutate(continent = countrycode(sourcevar = worldMap@data$ISO_A2,
                                 origin = "iso2c", destination = "continent")) %>%
  filter(continent %in% c("Americas", "Europe") & ISO_A2 != "RU") %>%
  left_join(mapworld_df, by = c("NAME_SORT" = "id")) 

# get centroids
centroids <- gCentroid(worldMap, byid=TRUE)
centroids <- as.data.frame(centroids)
centroids$country <- rownames(centroids) 
centroids["United States of America",]$country <- "United States"
rownames(centroids["United States of America",]) <- "United States"
centroids["Czech Republic",]$country <- "Czechia"
rownames(centroids["Czech Republic",]) <- "Czechia"
erasmusCountries <- countrycode::codelist %>% 
  select( country_name = country.name.en, continent, iso2c) %>%
  left_join(centroids, by = c("country_name" = "country")) %>%
  filter( iso2c %in% mapworld_df$ISO_A2)
##############
###### Filter erasmus data
##
erasmus <- erasmus %>% group_by(participant_nationality, receiving_country_code) %>% 
  dplyr::summarize(n = sum(participants)) %>%
  mutate(continent = countrycode(sourcevar = participant_nationality,
                                 origin = "iso2c", destination = "continent")) %>%
  dplyr::filter(continent == "Americas" ) %>%
  right_join(erasmusCountries, by = c("participant_nationality" = "iso2c")) %>%
  right_join(erasmusCountries, by = c("receiving_country_code" = "iso2c")) %>%
  drop_na(c("participant_nationality", "receiving_country_code","n"))
##############
## Creating Categories. 
#TODO: find a way to make the next block in a tidyverse-like way
erasmus$sz <- ""
erasmus[erasmus$n == 1 ,]$sz <- "1"
erasmus[erasmus$n > 1 & erasmus$n <= 5,]$sz <- "< 5"
erasmus[erasmus$n > 5 & erasmus$n <= 10,]$sz <- "< 10"
erasmus[erasmus$n > 10 & erasmus$n <= 20,]$sz <- "< 20"
erasmus[erasmus$n > 20 & erasmus$n <= 30,]$sz <- "< 30"
erasmus[erasmus$n > 30 ,]$sz <- "> 30"
erasmus$sz <- factor(erasmus$sz, levels = c("1", "< 5", "< 10", "< 20", "< 30", "> 30"))
####
cols.use <-met.brewer("Hiroshige", 5)
#####PLOTTING
###MAP
ggmap_erasmus <- ggplot() + 
  geom_polygon(data= mapworld_df, aes(long,lat, group=group), fill="#a9845b" ) +
  geom_curve(data = erasmus, aes(x = x.x, y = y.x, xend = x.y, yend = y.y, color=sz),
             curvature = -0.2, arrow = arrow(length = unit(0.01, "npc")), size = 0.2) +
  scale_color_manual(values=rev(cols.use)) +
  coord_equal()+
  labs(title = "American participants in the\nErasmus program going to Europe",
       subtitle = "Mobility of American\nparticipants to Europe\nfrom 2014 to 2020 of\nErasmus program",
       caption = "Source Data.Europa | Graphic: Cristian Gonzalez-Colin")+
  xlim(-180,50) +
  theme_void() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -8.5, size = 25),
        plot.subtitle = element_text(face = "bold", hjust = 0.25, vjust = -110, size = 15),
        plot.caption=element_text( hjust=0.9),
        legend.position = "none", 
        panel.background = element_rect(fill = "transparent", colour = "transparent"))

#####
#legend map
ggfoo <- ggplot(data = erasmus, aes(x = x.x, y = y.x, color=sz))+
geom_point(size = 5) +
  scale_color_manual(values=rev(cols.use))+
  labs(color = "Number of\nparticipants")+
  theme_classic()+
  theme(legend.position = "top", 
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_text(size = 10, face = "bold"))
gglegend <- as_ggplot( get_legend(ggfoo))

###Top participant nationalities
ggnatio <- erasmus %>% select(participant_nationality, n) %>%
  dplyr::group_by(participant_nationality) %>%  # group data by country name
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%   # arrange data by descending order
  dplyr::ungroup() %>%  # ungroup data
  dplyr::mutate(percent = 100 * total / sum(total))  %>% # calculate ratio
  mutate(participant_nationality = factor(participant_nationality, levels = rev(participant_nationality))) %>%  # set levels %>% 
  head(10) %>%
  mutate( country_name = countrycode(participant_nationality, 
                                     origin = "iso2c", destination = "country.name") ) %>%
  ggplot(aes(participant_nationality, total, fill = participant_nationality)) +
  geom_bar(width = 0.9, stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Manet", 10)) +
  coord_polar(theta = "y", start = 0)+
  labs(title = "Top 10 of American\ncontries from\nErasmus program") +
  ylim(c(0, 95)) +
  geom_text(aes(x = participant_nationality, y = 0, 
                label = paste0(country_name, " - ", round(percent, digits = 2), " %")),
                hjust = 1.05, size = 2, colour = met.brewer("Manet", 10)) +
  theme_void() +
  theme( plot.title = element_text(face = "bold", hjust = 0.38, vjust = -55, size =8),
         legend.background = element_rect(fill = "transparent", colour = "transparent"))

###Top receiving countries
ggrecei <- erasmus %>% select(receiving_country_code, n) %>%
  dplyr::group_by(receiving_country_code) %>%  # group data by country name
  summarise(total = sum(n)) %>%
  arrange(desc(total)) %>%   # arrange data by descending order
  dplyr::ungroup() %>%  # ungroup data
  dplyr::mutate(percent = 100 * total / sum(total))  %>% # calculate ratio
  mutate(receiving_country_code = factor(receiving_country_code, levels = rev(receiving_country_code))) %>%  # set levels %>% 
  head(10) %>%
  mutate( country_name = countrycode(receiving_country_code, 
                                     origin = "iso2c", destination = "country.name") ) %>%
  ggplot(aes(receiving_country_code, total, fill = receiving_country_code)) +
  geom_bar(width = 0.9, stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Manet", 10)) +
  coord_polar(theta = "y", start = 0)+
  labs(title = "Top 10 European\ndestinations for\n American students") +
  ylim(c(0, 260)) +
  geom_text(aes(x = receiving_country_code, y = 0, 
                label = paste0(country_name, " - ", round(percent, digits = 2), " %")),
                hjust = 1.05, size = 2, colour = met.brewer("Manet", 10)) +
  theme_void() +
  theme( plot.title = element_text(face = "bold", hjust = 0.5, vjust = -55, size =8),
         legend.background = element_rect(fill = "transparent", colour = "transparent"))
##########
#Plotting together
ggf <- ggmap_erasmus + 
  inset_element(gglegend, left = 0.2, bottom = 0.8, right = 0.8, top = 1.2) +
  inset_element(ggrecei + ggnatio, left = 0.6, bottom = 0.1, right = 0.95, top = 0.7)

plot.nam <- paste0("tidyTuesday_week10.png")
ggsave(plot.nam, ggf, dpi = 320, height = 8.6, width = 13)
