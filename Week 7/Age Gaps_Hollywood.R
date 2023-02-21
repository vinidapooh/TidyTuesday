library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(ggridges)
library(ggstatsplot)
library(plotly)
library(tidytext)
library(ggthemes)
library(patchwork)
library(showtext)

tuesdata <- tidytuesdayR::tt_load('2023-02-14')
tuesdata <- tidytuesdayR::tt_load(2023, week = 7)

age_gaps <- tuesdata$age_gaps
age_gaps = age_gaps%>%arrange(release_year)
age_gaps$release_year = as.Date(age_gaps$release_year)

#checking for NAs
age_gaps%>%summarise_all(~sum(is.na(.)))

age_gaps$decade = paste0(floor(age_gaps$release_year/10)*10,"s")


theme_set(theme_wsj())
theme_update(plot.title.position = "plot",
             plot.title = element_text(size = 30,
                                       hjust = 0.5,vjust = 1),
             plot.subtitle = element_text(size = 16,
                                          hjust = 0.5, vjust = 1),
             
)


#PLOT1 Checking range of age difference and mean using a boxplot
(p1 = ggplot(age_gaps,aes(x = factor(decade),y = age_difference,fill = decade))+geom_boxplot()+
    facet_wrap(~character_1_gender,nrow = 2)+labs(title = "Age Diff.Range by Decade and Older Person Gender",y = "Age Difference")+
    theme(plot.title = element_text(hjust = 0.7,vjust = 1.1,
                                    face = "bold",size = 12),strip.text = element_text(color = "black",size = 9,face = "bold"),
          axis.text.x = element_text(color = "black", size = rel(0.6)),legend.position = "none",
          axis.text.y = element_text(color = "black",size = 9))
  
)

df_1 = age_gaps%>%select(release_year,movie_name,character_1_gender, director,age_difference,decade,release_year,couple_number)%>%
  filter(age_difference >=10 & couple_number == 1)%>%
  group_by(release_year,movie_name,director,character_1_gender,decade)%>%arrange(desc(age_difference))

df = df_1%>%group_by(decade)%>%mutate(decade = as.factor(decade),y = reorder_within(movie_name, age_difference,decade)[movie_name])%>%slice(1:5)

(
  p2 = df%>%ggplot(aes(x = age_difference, y = reorder_within(movie_name, age_difference, decade),fill = character_1_gender))+
    geom_bar(stat = "identity")+scale_y_reordered()+
    facet_wrap(~decade, ncol= 2, scales ="free")+labs(title = "Top 5 Movies by Age Difference for each decade")+
    theme(plot.title = element_text(hjust = 0.7,vjust = 1.1,face = "bold",size = 12),
          strip.text = element_text(color = "black",size = 9,face = "bold"),
          axis.text.x = element_text(color = "black", size = rel(0.7)),
          legend.position = "top",
          legend.text=element_text(size=5,face = "bold"),
          legend.key.size = unit(1.2, "lines"),
          legend.title = element_text(size = 5,face = "bold"),
          legend.key.height = unit(0.4,"lines"),legend.key.width = unit(0.7,"lines"),
          axis.text.y = element_text(size = 9, face = "bold"))
)

p = p1+p2
p+plot_annotation(title = "HOLLYWOOD AGE GAP",subtitle = "Looking at the range of Age Differences Among Romantic Couples in Hollywood Movies")

ggsave("finalplot3.png", dpi = 400, width = 12, height =9)







