library("tidyverse")
library("tidymodels")
library("glmnet")

# Problem 4
housing <- read_table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data", col_names = FALSE)
names(housing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", "dis", "rad", "tax", "ptratio", "b", "lstat", "medv")

# Problem 5
set.seed(123456)

# Problem 6
housing_split <- initial_split(housing, prop = 0.8)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

# Problem 7
housing_recipe <- recipe(medv ~ ., data = housing) %>% 
  step_log(all_outcomes()) %>% 
  step_bin2factor(chas) %>% 
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>% 
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree=6)

housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)



# Problem 8
tunespeclasso <- linear_reg(penalty = tune(), mixture = 1) %>% set_engine("glmnet") %>% set_mode("regression")
lambdagridlasso <- grid_regular(penalty(), levels = 50)
recfoldslasso <- vfold_cv(housing_train_prepped, v = 6)

recwflasso <- workflow() %>% add_formula(log(medv) ~ .) %>% add_model(tunespeclasso)
recreslasso <- recwflasso %>% tune_grid(resamples = recfoldslasso, grid = lambdagridlasso)

toprmselasso <- show_best(recreslasso, metric = "rmse")
print(toprmselasso)
bestrmselasso <- select_best(recreslasso, metric = "rmse")
print(bestrmselasso)

finallasso <- finalize_workflow(recwflasso, bestrmselasso)
last_fit(finallasso, split = housing_split) %>% collect_metrics() %>% print
toprmselasso %>% print(n = 1)



# Problem 9
tunespecridge <- linear_reg(penalty = tune(), mixture = 0) %>% set_engine("glmnet") %>% set_mode("regression")
lambdagridridge <- grid_regular(penalty(), levels = 50)
recfoldsridge <- vfold_cv(housing_train_prepped, v = 6)

recwfridge <- workflow() %>% add_formula(log(medv) ~ .) %>% add_model(tunespecridge)
recresridge <- recwfridge %>% tune_grid(resamples = recfoldsridge, grid = lambdagridridge)

toprmseridge <- show_best(recresridge, metric = "rmse")
print(toprmseridge)
bestrmseridge <- select_best(recresridge, metric = "rmse")
print(bestrmseridge)

finalridge <- finalize_workflow(recwfridge, bestrmseridge)
last_fit(finalridge, split = housing_split) %>% collect_metrics() %>% print
toprmseridge %>% print(n = 1)
