install.packages("tidyverse", "ggplot2", "mapproj", "purrr", "cowplot")
devtools::install_github("dkahle/ggmap")

library(tidyverse)
library(ggmap)
library(ggplot2)
library(maps)
library(mapproj)
library(dplyr)
library(cowplot)

europe.map <- get_stamenmap(c(left = -25.410724, bottom = 35.479777,
                              right = 35.618354, top = 71.280356), zoom = 5)

ggmap(europe.map)

sweden.map <- get_stamenmap(c(left =  8.606408, bottom = 54.308350,
                              right = 24.714435, top = 70.136434), zoom = 6)

ggmap(sweden.map)


europe.map.complete <- ggmap(europe.map, extent="device") +
  annotate('rect', xmin=14, ymin=61.5, xmax=16, ymax=62.5, col='white', fill='white') +
  annotate('rect', xmin=2, ymin=47.5, xmax=4, ymax=48.5, col='white', fill='white') +
  annotate('rect', xmin=26, ymin=64.5, xmax=28, ymax=65.5, col='white', fill='white') +
  annotate('rect', xmin=8.5, ymin=55.5, xmax=10.5, ymax=56.5, col='white', fill='white') +
  annotate('text', x=15, y= 62, label = 'A', colour='black', size= 4) +
  annotate('rect', xmin=4, ymin=50.5, xmax=6, ymax=51.5, col='white', fill='white') +
  annotate('text', x=3, y= 48, label = 'B', colour='black', size= 4) +
  annotate('text', x=27, y= 65, label = 'C', colour='black', size= 4) +
  annotate('text', x=9.5, y= 56, label = 'D', colour='black', size= 4) +
  annotate('text', x=5, y= 51, label = 'E', colour='black', size= 4) 
  
 europe.map.complete


breeds.country <- data_raw@phdata %>%
  group_by(Breed, Country) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  arrange(desc(Country))

sweden <- subset(breeds.country, breeds.country$Country == 'Swe')
france <- subset(breeds.country, breeds.country$Country == 'Fra')
finland <- subset(breeds.country, breeds.country$Country == 'Fin')
denmark <- subset(breeds.country, breeds.country$Country == 'Den')
belgium <- subset(breeds.country, breeds.country$Country == 'Bel')


sweden.plot <- ggplot(breeds.sweden, aes(x=Breed, y=n)) +
  geom_col() +
  labs(title="Sweden") +
  theme_bw(base_family="Gidole")+ 
  xlab("Breed") +
  ylab("Number of individuals") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))

france.plot <- ggplot(breeds.france, aes(x=Breed, y=n)) +
  geom_col() +
  labs(title="France") +
  theme_bw(base_family="Gidole")+ 
  xlab("Breed") +
  ylab("Number of individuals") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))

finland.plot <- ggplot(breeds.finland, aes(x=Breed, y=n)) +
  geom_col() +
  labs(title="Finland") +
  theme_bw(base_family="Gidole")+ 
  xlab("Breed") +
  ylab("Number of individuals") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))

denmark.plot <- ggplot(breeds.denmark, aes(x=Breed, y=n)) +
  geom_col() +
  labs(title="Denmark") +
  theme_bw(base_family="Gidole")+ 
  xlab("Breed") +
  ylab("Number of individuals") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))

belgium.plot <- ggplot(breeds.belgium, aes(x=Breed, y=n)) +
  geom_col() +
  labs(title="Belgium") +
  theme_bw(base_family="Gidole")+ 
  xlab("Breed") +
  ylab("Number of individuals") +
  theme(panel.grid.minor=element_blank(),
        panel.grid.major.x=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        legend.title=element_blank(),
        axis.title=element_text(face="italic"),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_line(color="grey60"),
        plot.title=element_text(face="bold"),
        plot.caption=element_text(hjust=0,size=8))
      
breed.country.plot <- plot_grid(sweden.plot, france.plot, finland.plot, denmark.plot, belgium.plot, labels = c("A", "B", "C", "D", "E"))

breed.country.plot

ggdraw(xlim=c(0,2.2), ylim = c(0,1)) +
  draw_plot(europe.map.complete, x=0.05, y=0) +
  draw_plot(breed.country.plot, x=1.1, y=0, scale=0.75) 

