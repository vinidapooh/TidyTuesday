library(tidytuesdayR)
library(ggplot2)
library(tidyverse)
library(tidytext)
library(ggthemes)

tuesdata <- tidytuesdayR::tt_load('2023-04-25')
tuesdata <- tidytuesdayR::tt_load(2023, week = 17)

tuesdata$winners
tuesdata$london_marathon


#Set the theme
theme_set(theme_wsj(color = "brown",))
theme_update(plot.title.position = "plot",
             plot.title = element_text(size = 35,
                                       hjust = 0.6,vjust = 1),
             plot.subtitle = element_text(size = 12,
                                          hjust = 0.6, vjust = 1.6),
             plot.caption = element_text(size = 8,hjust = 0.6, vjust = 1.6))



data_marathon = merge(tuesdata$winners,tuesdata$london_marathon,by = "Year")

head(data_marathon)

#showing number of applicatns, accepted, starters and finishers
dm_app = data_marathon|>group_by(Year)|>summarise_at(c("Applicants","Accepted","Starters","Finishers"),sum)


dm_nationality =data_marathon|>group_by(Nationality,Category)|>summarise(count = n())|>arrange(desc(count))

data_marathon|>group_by(Category)|>summarise(count = n())|>arrange(desc(count))

data_marathon|>group_by(Category)|>summarise(mean = mean(Time))


#Ordering the Area Graph, from Applicants to Finishers
custom_order <- c("Applicants","Accepted", "Starters", "Finishers")
dm_app_new = dm_app|>pivot_longer(cols = -Year, names_to = "Variable", values_to = "Count")
dm_app_new$Variable <- factor(dm_app_new$Variable, levels = custom_order)

#PLOT1
p1=ggplot(dm_app_new|>filter(Year < 2020), aes(x=Year,y = Count,fill = Variable))+
  geom_area(alpha=0.6 , linewidth=1, colour="black")+labs(subtitle = "Trend Of Participants")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.key.size = unit(0.7,"cm"),
        panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
        axis.text.y = element_text(size = 9),axis.title = element_text(size = 3,face = "bold"),axis.title.y = element_blank(),
        axis.title.x = element_blank(),axis.text.x = element_text(size = 9))

#PLOT2

#setting order for the category facet grid
category_order = c("Men","Women","Wheelchair Men","Wheelchair Women")
dm_nationality$Category <- factor(dm_nationality$Category, levels = category_order)

p2 = ggplot(dm_nationality, aes(x = count, y = reorder(Nationality,Nationality)))+
  geom_segment(aes(x = 0, xend = count,yend = Nationality),linewidth = 1.4)+
  geom_point(color = "#B59410",size = 4, alpha = 2)+labs(x = "Number of Marathon Wins",subtitle = "Number of Marathon wins by Country and Event")+
  facet_grid(~Category,scales = "free")+
  theme(axis.text.y = element_text(size = 9,face = "bold"),strip.text = element_text(size = 8, vjust = 0.5,face = "bold"),
        axis.text.x = element_text(size = 8,face = "bold"))
  

p = p1+p2+plot_annotation(title = "LONDON MARATHON",subtitle = "Plots showing the trend of participants finishers by year and number of winners from each country",caption = "Data Source:LondonMarathonRPackage by @nrennie Graphic: Vinit Poojari")+theme(plot.caption = element_text(size = 1, face = "bold"))
p

ggsave("test_marathon.png", dpi = 500, width = 14, height =9)
