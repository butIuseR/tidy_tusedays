require(tidyverse)
require(tidytuesdayR)
theme_set(mdthemes::md_theme_ipsum_ps())

tt=tt_load('2020-07-28')

penguins=tt$penguins

penguins %>% 
  count(species,sort=T)

pivoted_penguins = penguins %>% 
                   pivot_longer(cols=bill_length_mm:body_mass_g,
                   names_to='metric',
                   values_to='value')

 
pivoted_penguins %>% 
  ggplot(aes(value,fill=species))+
  geom_histogram(color='white')+
  facet_wrap(~metric,scales='free_x')

pivoted_penguins %>% 
  ggplot(aes(value,fill=species))+
  geom_density(alpha=0.7)+
  facet_wrap(~metric,scales='free')

pivoted_penguins %>% 
  ggplot(aes(y=value,x=species))+
  geom_boxplot()+
  facet_wrap(~metric,scales='free')

penguins %>% 
  filter(!is.na(sex)) %>% 
  ggplot(aes(flipper_length_mm,bill_length_mm,color=sex))+
  geom_point(aes(size=body_mass_g),alpha=0.7)+
  facet_wrap(~species)

penguins_df=penguins %>% 
  select(-year,-island) %>% 
  mutate(sex=factor(sex)) %>% 
  na.omit()

require(tidymodels)
  
split=initial_split(penguins_df,strata=sex)
train=training(split)
test=testing(split)


boot_samples=bootstraps(train)


##model building------------

log_spec=logistic_reg() %>% 
  set_engine('glm')

rf_spec=rand_forest(mode='classification') %>% 
          set_engine('ranger')

penguin_wf=workflow() %>% 
  add_formula(sex~.)

log_rs=penguin_wf %>% 
  add_model(log_spec) %>% 
  fit_resamples(
    resamples=boot_samples,
    control=control_resamples(save_pred = T,verbose = T)
  )

rf_rs=penguin_wf %>% 
  add_model(rf_spec) %>% 
  fit_resamples(
    resamples=boot_samples,
    control=control_resamples(save_pred = T,verbose = T)
  )

collect_metrics(log_rs)
collect_metrics(rf_rs)


log_rs %>% 
  collect_predictions() %>% 
  group_by(id) %>% 
  roc_curve(sex,.pred_female) %>% 
  autoplot()

penguin_wf %>% 
  add_model(log_spec) %>% 
  last_fit(split)
 
penguin_final=.Last.value

penguin_final %>% 
  collect_metrics()

penguin_final %>% 
  collect_predictions() %>% 
  conf_mat(.pred_class,sex)

penguin_final$.workflow[[1]] %>% 
  broom::tidy(exponentiate=T)
