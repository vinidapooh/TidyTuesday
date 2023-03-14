library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(ggstatsplot)
library(plotly)
library(tidytext)
library(ggthemes)
library(patchwork)
library(ggtext)
library(tidyr)
library(showtext)
library(RColorBrewer)

#https://www.youtube.com/watch?v=IKgVPXuuOnY

# Get the Data

# Read in with tidytuesdayR package 
# Install from CRAN via: install.packages("tidytuesdayR")
# This loads the readme and all the datasets for the week of interest

# Either ISO-8601 date or year/week works!

tuesdata <- tidytuesdayR::tt_load('2023-02-28')
tuesdata <- tidytuesdayR::tt_load(2023, week = 9)

afrisenti <- tuesdata$afrisenti
languages <- tuesdata$languages
language_scripts <- tuesdata$language_scripts
language_countries <- tuesdata$language_countries
country_regions <- tuesdata$country_regions


afrosenti = merge(afrisenti, languages, by = "language_iso_code")
afrosenti = afrosenti%>%left_join(language_countries, by = "language_iso_code") %>%
  left_join(country_regions, by = "country")%>%filter(region != "Southeastern Africa")



theme_set(theme_fivethirtyeight())
theme_update(plot.title.position = "plot",
             plot.title = element_text(size = 35,
                                       hjust = 0.6,vjust = 1),
             plot.subtitle = element_text(size = 12,
                                          hjust = 0.6, vjust = 1.6))


#distribution of label by country
df3 = afrosenti%>%select(country, label)%>%group_by(country,label)%>%count()%>%ungroup()%>%
  group_by(country)%>%mutate(percent = round(n/sum(n)*100.0,1),xpos = cumsum(percent))

p1 = ggplot(df3, aes(x = percent, y = reorder(country, desc(country)),fill = label))+geom_col(colour = "black")+
  geom_text(position = position_stack(vjust = 0.5),size = 3,fontface = "bold",label = paste0(df3$percent,"%"))+theme_minimal()+
  labs(x = paste0("Percentage"," ","%"), y = " ",title = "Sentiment Percentage of Tweets by Country")+
  scale_fill_manual(values = c('#d95f0e','#fec44f','#fff7bc'))+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face = "bold",size = 10),axis.text.x = element_text(face = "bold"),
        legend.position = "bottom",axis.text.y = element_text(size = 9, face = "bold",vjust = 0.7,hjust = 0.4),axis.title.x = element_text(face = "bold"),
        legend.text=element_text(size=7,face = "bold"),
        legend.key.size = unit(1.5, "lines"),
        legend.title = element_text(size = 7,face = "bold"),
        legend.key.height = unit(0.3,"lines"),legend.key.width = unit(0.6,"lines"))




my_palette = c(brewer.pal(9, "Pastel1"), brewer.pal(8, "YlGn"))

df9 = afrosenti%>%select(country,language,tweet)%>%group_by(language, country)%>%count()%>%ungroup()%>%
  group_by(country)%>%mutate(percent = round(n/sum(n)*100.0,2),xpos = cumsum(percent))%>%arrange(country)



p2 = ggplot(df9, aes(x = percent, y = reorder(country, desc(country)),fill = factor(language)))+geom_col(colour = "black")+
  geom_text(position = position_stack(vjust = 0.5),size = 3,fontface = "bold",label = paste0(df9$language))+theme_minimal()+
  labs(y = "",x = paste0("Percentage"," ","%"),title = "Language Distribution of Tweets by Country")+
  scale_y_reordered()+scale_fill_manual(values = my_palette)+
  theme(plot.title = element_text(hjust = 0.5,vjust = 0.9,face = "bold",size = 10),axis.title.x = element_text(face = "bold"),
        legend.position = "none",axis.text.y = element_text(size = 9, face = "bold",vjust = 0.7,hjust = 0.4),axis.text.x = element_text(face = "bold"))


p = p1+p2
p+plot_annotation(title = "African Tweets Sentiment",subtitle = "Analyzing over 111k tweets from 14 African Countries by their languages and the sentiment")+
  labs(caption = paste0("Source: AfriSenti: Sentiment Analysis dataset for 14 African languages | ","Graphic: Vinit Poojari"))+
  theme(plot.caption.position = "plot",plot.caption = element_text(face = "bold",hjust = 0.5,vjust = 0.5, margin = margin(t = 10, r = 0, b = -40, l = 0)))

ggsave("Afroplot.png", dpi = 400, width = 14, height =9)

