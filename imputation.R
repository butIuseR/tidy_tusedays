require(tidyverse)
require(mdthemes)
theme_set(md_theme_ipsum_ps())

african_names=read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-16/african_names.csv")

african_names %>% 
  ggplot(aes(year_arrival))+
  geom_histogram(bins=20,color='white')+
  labs(title='**Arrival Year**')

african_names %>% 
  filter(year_arrival<1850) %>% 
  group_by(year_arrival) %>% 
  summarise(age=mean(age,na.rm = T)) %>% 
  ggplot(aes(year_arrival,age))+
  geom_line(alpha=0.6,size=1.3)+
  scale_y_continuous(limits=c(0,NA))+
  geom_smooth(method = 'lm')

african_names %>% 
  group_by(name) %>% 
  summarise(n=n(),
            age=mean(age,na.rm = T),
            year_arrival=mean(year_arrival,na.rm = T)) %>% 
  ungroup() %>% 
  filter(n>30) %>% 
  ggplot(aes(year_arrival,age))+
  geom_point(aes(size=n),alpha=0.6)+
  ggrepel::geom_text_repel(aes(label=name),size=3)+
  labs(size='No. of people')


liberated_df=african_names %>% 
  mutate(gender=case_when(gender=='Boy'~'Man',
                          gender=='Girl'~'Woman',
                          TRUE~gender)) %>% 
  mutate(across(is.character,factor))

require(naniar)

african_names %>% 
  select(gender,height,age) %>% 
  gg_miss_upset()



##missing imputation----------

require(recipes)




























































































