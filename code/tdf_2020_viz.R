#Created by: Daniel Kuhman
#Last Updated: 2020-09-21

#LOAD LIBRARIES
library('reshape2')
library('tidyverse')
library("ggplot2")
library('viridis')
library('ggjoy')

#CLEAR WORKSPACE
rm(list=ls())

#SELECT AND LOAD DATA
mydata_path <- file.choose(new = FALSE)
mydata <- read.csv(mydata_path)
rm(mydata_path)

#---BAR PLOT OF DISTANCE PER STAGE
#Prep Data
bar_data <- aggregate(mydata[, c(9,11)],
                       list(mydata$Stage),
                       mean)
bar_data$Group.1 <- factor(bar_data$Group.1,
                            levels = c("Stage 1",
                                       "Stage 2",
                                       "Stage 3",
                                       "Stage 4",
                                       "Stage 5",
                                       "Stage 6",
                                       "Stage 7",
                                       "Stage 8",
                                       "Stage 9",
                                       "Stage 10",
                                       "Stage 11",
                                       "Stage 12",
                                       "Stage 13",
                                       "Stage 14",
                                       "Stage 15",
                                       "Stage 16",
                                       "Stage 17",
                                       "Stage 18",
                                       "Stage 19",
                                       "Stage 20",
                                       "Stage 21"))
#Plot
ggplot(bar_data, aes(x=Group.1, group=1)) +
  geom_bar(aes(y=Stage_Distance),
           stat = 'identity',
           color='black',
           fill='#3C3B6E',
           alpha=0.8)+
  xlab('Stage')+
  ylab('Distance (km)')+
  theme_classic()+
  theme(
    axis.title.x = element_text(size=20,
                                color='black',
                                face='bold',
                                margin = margin(l=0,t=20,r=0,b=0)),
    axis.title.y.left = element_text(size=20,
                                     color='#3C3B6E',
                                     face='bold',
                                     margin = margin(l=0,t=0,r=20,b=0)),
    axis.text.y.left = element_text(size=16,
                                    color='#3C3B6E'),
    axis.text.x.bottom = element_text(color='black',
                                      size=16,
                                      angle = 45,
                                      hjust = 1),
    axis.line = element_line(size=2)
  )

#VARIOUS VELOCITY CHARTS
#---Ridgeline Plot of Velocity by Stage
ridge_data <- mydata %>%
  mutate(Stage = fct_relevel(Stage,
                             levels = "Stage 1",
                             "Stage 2",
                             "Stage 3",
                             "Stage 4",
                             "Stage 5",
                             "Stage 6",
                             "Stage 7",
                             "Stage 8",
                             "Stage 9",
                             "Stage 10",
                             "Stage 11",
                             "Stage 12",
                             "Stage 13",
                             "Stage 14",
                             "Stage 15",
                             "Stage 16",
                             "Stage 17",
                             "Stage 18",
                             "Stage 19",
                             "Stage 20",
                             "Stage 21"))

ggplot(ridge_data,aes(x=Velocity_kmh,y=Stage, fill=..x..))+
  geom_density_ridges_gradient(rel_min_height = 0.01,
                               color='white',
                               lwd=1)+
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "Velocity [km/h]", option = "C") +
  ylab('Stage')+
  xlab('Avg Velocity (km/h)')+
  theme_classic()+
  theme_ridges(font_size = 15) + 
  theme(
    axis.title.y = element_text(size=20,
                                face='bold',
                                margin = margin(l=0,t=0,r=20,b=0)),
    axis.title.x = element_text(size=20,
                                face='bold',
                                margin = margin(l=0,t=20,r=0,b=0)),
    axis.text = element_text(size=12, color='black')
  )

#---Box Plot of Velocity by Country
ggplot(mydata, aes(x=reorder(Country,
                             Velocity_kmh,
                             FUN = median),
                   y=Velocity_kmh,
                   fill=Country))+
  geom_boxplot(color='black',lwd=1)+
  coord_flip()+
  xlab('Country')+
  ylab('Avg. Velocity (km/h)')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size=20,
                                  color='black',
                                  face='bold'),
        axis.text = element_text(size = 16,
                                 color = 'black'),
        axis.line = element_line(size=1.5),
        axis.ticks = element_blank()
          )

#---Box Plot of Velocity By Team
ggplot(mydata, aes(x=reorder(Team,
                             Velocity_kmh,
                             FUN = median),
                    y=Velocity_kmh,
                   fill=Team))+
  geom_boxplot(color='black',lwd=1)+
  coord_flip()+
  ylim(30, 50) +
  xlab('Team')+
  ylab('Avg. Velocity (km/h)')+
  theme_classic()+
  theme(legend.position = 'none',
        axis.title = element_text(size=20,
                                  color='black',
                                  face='bold'),
        axis.text = element_text(size = 12,
                                 color = 'black'),
        axis.line = element_line(size=1.5),
        axis.ticks = element_blank()
  )

#Box Plot of Velocity by Stage
ggplot(mydata, aes(x=Stage_Type,
                   y=Velocity_kmh,
                   group=Stage_Type)) +
  geom_jitter(shape=16,size=0.9,
              position = position_jitter(0.3))+
  geom_boxplot(aes(fill=Stage_Type),
               color='black',lwd=2,
               outlier.color = 'white',
               alpha=0.5)+
  xlab('Stage Classification')+
  ylab('Velocity (km/h)')+
  theme_classic()+
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size=20,
                                color='black',
                                face='bold',
                                margin = margin(l=0,t=20,r=0,b=0)),
    axis.title.y.left = element_text(size=20,
                                     color='black',
                                     face='bold',
                                     margin = margin(l=0,t=0,r=20,b=0)),
    axis.text.y.left = element_text(size=16,
                                    color='black'),
    axis.text.x.bottom = element_text(color='black',
                                      size=16,
                                      angle = 20,
                                      hjust = 1),
    axis.line = element_line(size=2)
  )
