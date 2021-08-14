library(readr)
library(ggplot2)
tips <- read_csv("tips.csv")

ggplot(tips)

ggplot(tips, aes(x=total_bill, y=tip))

ggplot(tips,aes(x=total_bill, y=tip)) + geom_point()

ggplot(tips, aes(x=total_bill, y=tip, color=time)) + geom_point()

ggplot(tips, aes(x=total_bill, y=tip, color=time, shape=smoker)) + geom_point()

ggplot(tips, aes(x=total_bill, y=tip, color=time, shape=smoker,)) + 
  geom_point() + facet_wrap(~day)


ggplot(tips, aes(x=day)) + geom_bar()

ggplot(tips, aes(x=day, fill=smoker)) + geom_bar()

ggplot(tips, aes(x=day, fill = smoker)) + geom_bar(position = "dodge")

ggplot(tips, aes(x=day, fill = smoker)) + geom_bar(position = "fill")

ggplot(tips, aes(x=day, fill = smoker)) + geom_bar(position = "dodge") +
  facet_wrap(~time)





ggplot(tips, aes(x=day, fill = smoker)) + geom_bar(position = "dodge") +
  facet_wrap(~time) +
  labs(title = "Number of Patrons by Day and Meal", 
       subtitle = "Position by Smoker",
       x="Day", y="Count")






tips %>%
  select(smoker,tip,day,time) %>%
  mutate(gbp_tip = tip*0.82) %>%
  filter(gbp_tip>5) %>%
  ggplot(aes(x=day,fill=smoker)) + geom_bar(position="dodge") +
  facet_wrap(~time) +
  labs(title="Number of Patrons by Day and Meal",
       subtitle = "Position by Smoker",
       x="Day",y="Count")






tips %>%
  select(smoker, tip, day, time) %>%
  mutate(gbp_tip = tip * .82) %>%
  filter(gbp_tip >= 5) %>%
  ggplot(aes(x = day, fill= smoker)) +
  geom_bar(position = "dodge") + 
  facet_wrap(~time) +
  labs(title = "Number of Patrons By Day and Meal",
       subtitle = "Position by Smoker",
       x =  "Day", y = "Count")




library(dplyr)




tips %>%
  select(smoker,tip,day,time) %>%
  mutate(gbp_tip = tip*0.82) %>%
  filter(gbp_tip>5) %>%
  ggplot(aes(x=day,fill=smoker)) + geom_bar(position="dodge") +
  facet_wrap(~time) +
  labs(title="Number of Patrons by Day and Meal",
       subtitle = "Position by Smoker",
       x="Day",y="Count")












library(tidyverse)
library(dplyr)

population <- read_csv("population.csv")
population

population %>%
  group_by(`Country Name`)

population %>%
  select(`Country Name`, Year, Value)

pop_by_country <- population %>%
                    select(`Country Name`, Year, Value)
pop_by_country


pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" )

pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  ggplot(aes(x=Year,y=Value,color=`Country Name`))+geom_line()



pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  ggplot(aes(x=Year,y=Value,fill=`Country Name`))+
  geom_bar(stat="identity")



pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  mutate(decade=Year-Year%%10)


pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  mutate(decade=Year-Year%%10) %>%
  ggplot(aes(x=decade,y=Value,fill=`Country Name`))+
  geom_bar(stat="identity")



pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  mutate(decade=Year-Year%%10) %>%
  group_by(`Country Name`,decade)



pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  mutate(decade=Year-Year%%10) %>%
  group_by(`Country Name`,decade)%>%
  ggplot(aes(x=decade,y=Value,fill=`Country Name`))+
  geom_bar(stat="identity")




pop_by_country %>%
  filter(`Country Name`=="Arab World" | `Country Name`=="Euro area"
         | `Country Name`=="Afghanistan" ) %>%
  ggplot(aes(x=Year,y=Value,color=`Country Name`)) +
  geom_point() + geom_smooth(method=lm) +
  scale_color_manual(values=c('#30D5C8','#008080','#000080')) +
  theme(legend.position='bottom')
