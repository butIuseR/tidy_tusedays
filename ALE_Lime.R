require(tidyverse)
require(tidytuesdayR)
require(iml)
require(AmesHousing)
require(tidymodels)


##Data--------------
ames_df <- make_ames()
ames_df <- ames_df %>% 
  select_if(is.numeric) %>% 
  select(Sale_Price, everything())

##Making some non-interpretable models--------------

rand_forest=rand_forest() %>% 
  set_mode('regression') %>% 
  set_engine('randomForest')

xg_boost=boost_tree() %>% 
  set_mode('regression') %>% 
  set_engine('xgboost')

##Training models-----------

rf_model=rand_forest %>% 
  fit(Sale_Price~.,data=ames_df)

xgb_model=xg_boost %>% 
  fit(Sale_Price~.,data=ames_df)

##feature imp----------------

x_data=ames_df %>% select(-Sale_Price) %>% as.data.frame()

rf_predictor=Predictor$new(rf_model,data=x_data,y=ames_df$Sale_Price)
xgb_predictor=Predictor$new(xgb_model,data=x_data,y=ames_df$Sale_Price)

rf_feature_imp=FeatureImp$new(rf_predictor,loss='mae')
xgb_feature_imp=FeatureImp$new(xgb_predictor,loss='mae')

rf_feature_imp %>% plot()
xgb_feature_imp %>% plot()

##ALE(accumulated local Effects)--------------

rf_ale=FeatureEffects$new(rf_predictor)
xgb_ale=FeatureEffects$new(xgb_predictor)

rf_ale %>% plot()
xgb_ale %>% plot()

##Lime models (Local Surrogate Model)---------------
#for analyzing how models make individual predictions and what would change after
#changing feature

rf_lime=LocalModel$new(rf_predictor,x.interest = x_data[1,])
rf_lime %>% plot()

xgb_lime=LocalModel$new(xgb_predictor,x.interest = x_data[1,])
xgb_lime %>% plot()




















