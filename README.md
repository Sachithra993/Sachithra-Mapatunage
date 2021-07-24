# Sachithra-Mapatunage
Internship tasks
---
title: "Iteration"
author: "Sachi Mapatunage"
date: "02/06/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
#install.packages("readr")
#install.packages("tidyverse")
library(readr)
library(tidyverse)
spotify_songs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')
write_csv(spotify_songs,"/cloud/project/Research report/data.csv")
data<-read_csv("/cloud/project/Research report/data.csv")

#Selecting the relevent parameters
data2<-data %>%
  select(track_popularity,danceability,energy,key,loudness,mode,speechiness,acousticness,instrumentalness,liveness,valence,tempo,duration_ms)%>%filter(loudness<0)

#install.packages("devtools")
devtools::install_github("ropensci/skimr")
library(skimr)
skim(data2)  
#All the values under var in the selected data set are numeric

#Catergorizing popularity 
datafinal<-data2 %>%
  mutate(track_popularity=case_when(
      track_popularity>50 ~ "Popular", TRUE ~ "Not popular"))
skim(datafinal)

#Choosing random sample with 5000 obs
set.seed(123456789)
datarev<-sample_n(datafinal, size = 17000, replace = FALSE)
datarev2<-sample_n(data2, size = 17000, replace = FALSE)

write_csv(datarev,"/cloud/project/Research report/datarev.csv")

library(ggplot2)

set.seed(123456789)
dataplot<-sample_n(data2, size = 15, replace = FALSE)
ggplot(data=dataplot, aes(x=track_popularity, y=danceability, group=1))+geom_line()

#Running the linear models on parameters

lmdanceability<-lm(track_popularity~danceability, data=datarev2)
summary(lmdanceability)
#Significant

set.seed(123456789)
dataplot<-sample_n(data2, size = 15, replace = FALSE)
ggplot(data=dataplot, aes(x=track_popularity, y=energy, group=1))+geom_line()

lmenergy<-lm(track_popularity~energy, data=datarev2)
summary(lmenergy)
#Significant

set.seed(123456789)
dataplot<-sample_n(data2, size = 15, replace = FALSE)
ggplot(data=dataplot, aes(x=track_popularity, y=key, group=1))+geom_line()

lmkey<-lm(track_popularity~key, data=datarev2)
summary(lmkey)
#Not Significant

set.seed(123456789)
dataplot<-sample_n(data2, size = 15, replace = FALSE)
ggplot(data=dataplot, aes(x=track_popularity, y=loudness, group=1))+geom_line()


lmloudness<-lm(track_popularity~loudness, data=datarev2)
summary(lmloudness)
#Significant

lmmode<-lm(track_popularity~mode, data=datarev2)
summary(lmmode)
#Not Significant

lmspeechiness<-lm(track_popularity~speechiness, data=datarev2)
summary(lmspeechiness)
#Not Significant

lmacousticness<-lm(track_popularity~acousticness, data=datarev2)
summary(lmacousticness)
#Significant

lminstrumentalness<-lm(track_popularity~instrumentalness, data=datarev2)
summary(lminstrumentalness)
#Significant

lmliveness<-lm(track_popularity~liveness, data=datarev2)
summary(lmliveness)
#Significant

lmvalence<-lm(track_popularity~valence, data=datarev2)
summary(lmvalence)
#Not significant

lmtempo<-lm(track_popularity~tempo, data=datarev2)
summary(lmtempo)
#Not significant

lmduration_ms<-lm(track_popularity~duration_ms, data=datarev2)
summary(lmduration_ms)
#Significant

#comparing energy and acoustic models
#install.packages("lmtest")
library(lmtest)
compar<-lrtest(lmenergy,lmacousticness)
compar

#Selecting significant parameters
dataclean<-datarev %>% select(-key,-mode,-tempo,-speechiness,-valence)
dataclean

```
```{r}
#install.packages("tidymodels")
library(tidymodels)

#Spliting the data set into training and testing sets
set.seed(123456789)
datasplit<-initial_split(dataclean ,prop = 0.90)
datasplit

data_training<-training(datasplit)
data_crossvalidation<-vfold_cv(data_training)

data_testing<-testing(datasplit)
```


```{R}
#install.packages("randomForest")
library(randomForest)

#Applying recipe
data_recipe<-recipe(track_popularity ~ ., data=dataclean) %>%
  step_corr(all_predictors())%>%    
  step_center(all_predictors(), -all_outcomes()) %>%
  step_scale(all_predictors(),-all_outcomes()) %>%
  step_knnimpute(all_predictors(),-all_outcomes())%>%
  prep()
data_recipe



#install.packages("randomForest")
library(randomForest)
#Defining model
data_rfmodel<-rand_forest()%>%
  set_args(mtry=tune())%>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") 
data_rfmodel

#Setting workflow
library(workflows)
data_workflow<-workflow() %>%
  add_recipe(data_recipe) %>%
  add_model(data_rfmodel)

#install.packages("ranger")
library(tune)
library(yardstick)
set.seed(123456789)

data_grid<-expand.grid(mtry = c(3,4,5))
data_tuneresults <- data_workflow %>%
  tune_grid(resamples = data_crossvalidation, 
            grid = data_grid ,
            metrics = metric_set(accuracy, roc_auc)
            ) %>%collect_metrics()
data_tuneresults

library(yardstick)
library(dplyr)
library(recipes)
library(rsample)
library(parsnip)


```

```{R}
final_para<-data_tuneresults[1,1]
final_para
data_workflow1<-data_workflow%>%
  finalize_workflow(final_para)

#Evaluate model
data_fit<-data_workflow1%>%last_fit(datasplit)
data_fit

#Testing performance
test_per<-data_fit %>%collect_metrics()
test_per

#Testing predictions
test_pred<-data_fit%>%collect_predictions()
test_pred

#Building confusion matrix
test_pred %>% 
  conf_mat(truth = track_popularity, estimate = .pred_class)%>% autoplot()


plot1<-test_pred %>%   ggplot() + geom_density(aes(x = .pred_Popular, fill = track_popularity), 
               alpha = 0.5)
plot1

test_pred<-data_fit %>% pull(.predictions)
test_pred

#Fitting using the final model
final_mod<-fit(data_workflow1, dataclean)
final_mod

#Checking variable importance
var_importance <- pull_workflow_fit(final_mod)$fit
var_importance

```

