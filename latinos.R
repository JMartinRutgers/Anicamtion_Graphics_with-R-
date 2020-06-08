
library(av)
library(gganimate)
library(dplyr)
library(tidyverse) 
library(reshape2) 
library(ggthemes)
library(gifski) 
# library(av) 
library(readr) 
library(ggplot2)   
dea <- read_csv("Desktop/Deaths.csv") 

str(dea) 
head(dea)
ncolnames(dea)
names(dea) 
class(dea$`COVID-19 Deaths`)  


#change column to Date
dea$`Start week` <- as.Date(dea$`Start week` , format = "%m/%d/%y")
dea$`Start week` <- as.Date(dea$`Start week` , anytime("20130501000000") )
dea$`End Week` <- as.Date(dea$`End Week` , format = "%m/%d/%y")
dea$`End Week` <- as.Date(dea$`End Week` , anytime("20130501000000"))
str(dea)


#change to numeric
dea$`Start week` <- as.numeric(dea$`Start week`)
dea$`Race and Hispanic Origin Group` <- as.numeric(dea$`Race and Hispanic Origin Group`)

str(dea)
#change all the data set to factor 
dea <- mutate_if(dea, is.character, as.factor)

#Show the columns values like date, dhr,dbl
str(dea)

# dea$`Start week` <-mdy(dea$`Start week`)
str(dea)

  data<-dea %>% select( "End Week","COVID-19 Deaths","Race and Hispanic Origin Group") %>%
  group_by("Non-Hispanic Black","Non-Hispanic White","Hispanic or Latino") %>%
   summarise(race=n())

data

 data$`Start week` <- rep(date$`Start week`, 40)

 
 p <- ggplot(
   dea, 
   aes(x = `COVID-19 Deaths`, 
       y=`Race and Hispanic Origin Group`, 
       # size=`Race and Hispanic Origin Group`, 
       colour = State)
 ) +
   geom_point(show.legend = FALSE, alpha = 0.7) +
   scale_color_viridis_d() +
   scale_size(range = c(2, 12)) +
   scale_x_log10() +
   labs(x = "COVID HOSPITALIZATION RATE", y = "RACE")
 p
 
 p + transition_time("COVID-19 Deaths") +
   labs(title = "COVID-19 Deaths frame_time}")


White<-filter(dea,`COVID-19 Deaths`, `Total Deaths`,`Race and Hispanic Origin Group`=="Non-Hispanic White") 
Black<-filter(dea,`COVID-19 Deaths`, `Total Deaths`,`Race and Hispanic Origin Group`=="Non-Hispanic Black") 
Latino<-filter(dea,`COVID-19 Deaths`,`Total Deaths`,`Race and Hispanic Origin Group`=="Hispanic or Latino") 

date<-dea %>% select( "End Week","COVID-19 Deaths","Race and Hispanic Origin Group") %>% 
  group_by("Non-Hispanic Black","Non-Hispanic White","Hispanic or Latino") %>% 
  summarise(race=n())  
date


dea$`Race and Hispanic Origin Group` <- rep(dea$`Race and Hispanic Origin Group`, 40)


date

data$date <- as.Date(data$date)  



p <- ggplot(dea, mapping=aes(y=`Race and Hispanic Origin Group`, x=`COVID-19 Deaths`, label= `Race and Hispanic Origin Group`, group= `Race and Hispanic Origin Group`, color = `Race and Hispanic Origin Group`, fill=`Race and Hispanic Origin Group`))+ 
  geom_point(stat='identity', size = 5) + 
  geom_segment(aes(
    y = 10, 
    x = `COVID-19 Deaths`, 
    yend = `Race and Hispanic Origin Group`, 
    xend = `COVID-19 Deaths`) 
    ) + 
    geom_text(color="blue", size=3) + 
    coord_flip()+ 
    theme(legend.position = "none")  
    transition_reveal(`End Week`)
    P
    animate(p, fps = 5, end_pause = 10)
    p
  
  dea
    
 anim_save("Latinos.gif")

# create animation gif file
animate(r, fps=5,renderer = gifski_renderer("lat.gif"))



 