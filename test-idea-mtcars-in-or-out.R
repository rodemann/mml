
# Loading package
library(caTools)
library(ROCR)
library(dplyr)


data = mtcars

# number of imprecise obs
n_imp = 1
which_flip = seq(n_imp)

data_null <- data[-c(1:n_imp),]

data_to_add <- data[1:n_imp,]
data_to_add[,8] <- !data_to_add[,8]

# backward to be done

#data_null_null <- data[-c(1:n_imp),]


for (i in seq(n_imp)) {
  if(i >= 2){
    which_flip <- which_flip[-(winner)]
  }
  data_sets = list()  
  for (flip_count in seq_along(which_flip)) {
    flip = which_flip[flip_count]
    new_data = rbind(data_null, data_to_add[flip_count,])
    data_sets[[flip_count]] = new_data 
  }
  # fit models
  BICs = list()
  AICs = list()
  models = list()
  for(flip_count in seq_along(which_flip)){
    logistic_model <- glm(vs ~ wt + mpg, 
                          data = data_sets[[flip_count]], 
                          family = "binomial")
    n <- data_sets[[flip_count]] %>% nrow()
    #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "both")
    BICs[[flip_count]] <- logistic_model %>% BIC()
    AICs[[flip_count]] <- logistic_model %>% AIC()
    models[[flip_count]] <- logistic_model
  }
  
  #marginal AICs/BICs
  logistic_model <- glm(vs ~ wt + mpg, 
                        data = data_null, 
                        family = "binomial")
  n <- data_null %>% nrow()
  #logistic_model <- step(logistic_model, k = log(n), trace = 0, direction = "both")
  BIC_null <- logistic_model %>% BIC()
  AIC_null <- logistic_model %>% AIC()
  
  delta_BIC = unlist(BICs) - BIC_null
  delta_AIC = unlist(AICs) - AIC_null
  
  # new base model
  
  # if(min(delta_BIC) > 0){stop(
  #   "converged"
  # )}
  
  winner <- which.min(delta_BIC)
  data_null <- data_sets[[winner]] 
  
  print(winner)
  print(BICs)
}


