### DISNEY STOCK FORECAST ###
#############################
# Levi C Nicklas
# 3/17/2020

## LIBRARIES
library(tidyverse)
library(lubridate)
library(here)

## READ IN DATA
DIS <- readr::read_csv(here::here("DisneyStock/DIS.csv")) %>% 
  select(-`Adj Close`)
indep_vars <- colnames(DIS)
indep_vars <- indep_vars[indep_vars %in% c("Open","High","Low","Close", "Volume")]


DIS %>% 
  pivot_longer(indep_vars) %>% 
  ggplot(aes(x = Date, y = value, color = as.factor(name)))+
  geom_line()+
  facet_wrap(~name, scales = "free", nrow = 5)+
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  labs(x = "Date", y = "Nominal Value",
       title = "Figure 1: DIS Values",
       color = "Variable")

# PRE PROCESS THE DATA
DIS.trans <- DIS %>% 
  mutate(d.Open = log(Open) - log(lag(Open,1))) %>% 
  mutate(d.Close = log(Close) - log(lag(Close,1))) %>% 
  mutate(d.Low = log(Low) - log(lag(Low,1))) %>% 
  mutate(d.High = log(High) - log(lag(High,1))) %>% 
  mutate(d.Volume = log(Volume) - log(lag(Volume,1))) %>% 
  drop_na()

DIS.trans <- DIS.trans %>% 
  mutate(month = lubridate::month(Date)) %>% 
  mutate(Jan = (month == 1),
         Feb = (month == 2),
         Mar = (month == 3),
         Apr = (month == 4),
         May = (month == 5),
         Jun = (month == 6),
         Jul = (month == 7),
         Aug = (month == 8),
         Sep = (month == 9),
         Oct = (month == 10),
         Nov = (month == 11),
         Dec = (month == 12))

indep_vars <- paste0("d.",indep_vars)

DIS.trans %>% 
  pivot_longer(indep_vars) %>% 
  ggplot(aes(x = Date, y = value, color = as.factor(name)))+
  geom_line()+
  facet_wrap(~name, scales = "free", nrow = 5)+
  scale_color_brewer(palette = "Set1") +
  theme_classic() +
  labs(x = "Date", y = "First Difference Log Value",
       title = "Figure 2: DIS Transformed Values",
       color = "Variable")

## GENERATE LAG AND SEASONAL VARIABLES
source(here::here("Functions/generate_lag_terms.R"))
lags_df <- generate_lag_terms(var_names = indep_vars, max_lags = 730)

source("Functions/build_seasonal_indicators.R")
seasons_df <- build_seasonal_indicators(seasonality = "weekly")

all_terms <- c(lags_df$d.Open,
               lags_df$d.High,
               lags_df$d.Low,
               lags_df$d.Close,
               lags_df$d.Volume,
               as.character(seasons_df$months))

lag_terms <- c(lags_df$d.Open,
               lags_df$d.High,
               lags_df$d.Low,
               lags_df$d.Close,
               lags_df$d.Volume)


## FIND MODELS WITH GENETIC ALG
library(GA)

source("DisneyStock/ga_fitness_dis.R")

# Run GA
GA_i1 <- GA::ga(type = "binary",
       upper = rep(1, length(lag_terms)),
       lower = rep(0, length(lag_terms)),
       nBits = length(lag_terms),
       fitness = ga_fitness,
       popSize = 10,
       maxiter = 5,
       pmutation = 0.5,
       pcrossover = 0.8,
       elitism = 0.5,
       optim = T,
       run = 25,
       keepBest = T
       )
saveRDS(GA_i1,"DisneyStock/genetic_alg_result.RDS")

## ASSESS BEST MODELS
source("Functions/AIC_BIC_BinaryRep.R")

population <- list()

for(i in 1:nrow(GA_i1@population)){
  population[[i]] <- GA_i1@population[i,]
}

# Allocate Storage
AIC_BIC_metrics <- data.frame(aic = rep(0,length(population)),
                              bic = rep(0,length(population)),
                              model = rep(0,length(population)))

for(i in 1:length(population)){
  metrics <- AIC_BIC_BinaryRep(as.vector(population[[i]]),
                               lag_term_vector =  lag_terms,
                               all_term_vector = all_terms,
                               train_df = DIS.trans[1:3650,],
                               dependent_var_name = "d.High")
  
  AIC_BIC_metrics$aic[i] <- metrics[1]
  AIC_BIC_metrics$bic[i] <- metrics[2]
  AIC_BIC_metrics$model[i] <- population[i]
}

AIC_BIC_metrics <- AIC_BIC_metrics %>% 
  mutate(id = row_number())


AIC_top_50 <- AIC_BIC_metrics %>% 
  arrange(aic) %>% 
  top_n(15)

BIC_top_50 <- AIC_BIC_metrics %>% 
  arrange(bic) %>% 
  top_n(15)

# Check that these are the same top 50 models.
# length(AIC_top_50$id %in% BIC_top_50$id)

best_models <- AIC_top_50 %>% 
  select(id, model)

knitr::kable(AIC_top_50 %>% 
               select(id, aic, bic) %>% 
               arrange(aic))

rm(AIC_BIC_metrics, AIC_top_50, BIC_top_50)
