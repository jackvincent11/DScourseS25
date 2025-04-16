install.packages(c("tidymodels", "rpart", "e1071", "kknn", "nnet", "kernlab"))
library(tidymodels)
library(tidyverse)
library(rpart)    
library(e1071)     
library(kknn)      
library(nnet)       
library(kernlab)   


set.seed(123)


income <- read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")


colnames(income) <- c("age", "workclass", "fnlwgt", "education", "education_num",
                      "marital_status", "occupation", "relationship", "race", "sex",
                      "capital_gain", "capital_loss", "hours_per_week", "native_country",
                      "income")


income <- income %>%
  mutate(
    income = str_trim(income),
    high.earner = factor(ifelse(income == ">50K", "Yes", "No"))
  )


income <- income %>% select(-income)


data_split <- initial_split(income, prop = 0.8, strata = high.earner)
train_data <- training(data_split)
test_data <- testing(data_split)


all_levels <- union(levels(train_data$native_country), levels(test_data$native_country))
train_data$native_country <- factor(train_data$native_country, levels = all_levels)
test_data$native_country <- factor(test_data$native_country, levels = all_levels)


cv_folds <- vfold_cv(train_data, v = 3, strata = high.earner)


simple_rec <- recipe(high.earner ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors())


evaluate_model <- function(workflow, test_data) {
  predictions <- predict(workflow, test_data)
  conf_mat <- conf_mat(test_data, predictions$.pred_class, truth = high.earner)
  accuracy <- accuracy(test_data, predictions$.pred_class, truth = high.earner)
  return(list(conf_mat = conf_mat, accuracy = accuracy))
}


log_spec <- logistic_reg(mode = "classification", penalty = tune()) %>% 
  set_engine("glmnet")

log_grid <- grid_regular(penalty(), levels = 50)

log_wf <- workflow() %>%
  add_model(log_spec) %>%
  add_recipe(base_rec)

log_res <- tune_grid(
  log_wf,
  resamples = cv_folds,
  grid = log_grid,
  metrics = metric_set(accuracy)
)

log_best <- select_best(log_res, metric = "accuracy")
print("Best logistic regression parameters:")
print(log_best)

final_log_wf <- finalize_workflow(log_wf, log_best)
final_log_fit <- fit(final_log_wf, data = train_data)


tree_spec <- decision_tree(
  mode = "classification",
  min_n = tune(),
  tree_depth = tune(),
  cost_complexity = tune()
) %>% set_engine("rpart")


tree_grid <- grid_regular(
  min_n(range = c(10, 50)),
  tree_depth(range = c(5, 20)),
  cost_complexity(range = c(0.001, 0.2)),
  levels = 5
)

tree_wf <- workflow() %>%
  add_model(tree_spec) %>%
  add_recipe(base_rec)

tree_res <- tune_grid(
  tree_wf,
  resamples = cv_folds,
  grid = tree_grid,
  metrics = metric_set(accuracy)
)

tree_best <- select_best(tree_res, metric = "accuracy")
print("Best decision tree parameters:")
print(tree_best)

final_tree_wf <- finalize_workflow(tree_wf, tree_best)
final_tree_fit <- fit(final_tree_wf, data = train_data)


nn_spec <- mlp(
  mode = "classification",
  hidden_units = tune(),
  penalty = tune()
) %>% set_engine("nnet")


nn_grid <- grid_regular(
  hidden_units(range = c(1, 10)),
  penalty(range = c(0.0001, 0.1)),
  levels = 5
)

nn_wf <- workflow() %>%
  add_model(nn_spec) %>%
  add_recipe(base_rec)

nn_res <- tune_grid(
  nn_wf,
  resamples = cv_folds,
  grid = nn_grid,
  metrics = metric_set(accuracy)
)

nn_best <- select_best(nn_res, metric = "accuracy")
print("Best neural network parameters:")
print(nn_best)

final_nn_wf <- finalize_workflow(nn_wf, nn_best)
final_nn_fit <- fit(final_nn_wf, data = train_data)


knn_spec <- nearest_neighbor(
  mode = "classification",
  neighbors = tune()
) %>% set_engine("kknn")


knn_grid <- tibble(neighbors = seq(1, 30))

knn_wf <- workflow() %>%
  add_model(knn_spec) %>%
  add_recipe(base_rec)

knn_res <- tune_grid(
  knn_wf,
  resamples = cv_folds,
  grid = knn_grid,
  metrics = metric_set(accuracy)
)

knn_best <- select_best(knn_res, metric = "accuracy")
print("Best kNN parameters:")
print(knn_best)

final_knn_wf <- finalize_workflow(knn_wf, knn_best)
final_knn_fit <- fit(final_knn_wf, data = train_data)


svm_spec <- svm_rbf(
  mode = "classification",
  cost = tune(),
  rbf_sigma = tune()
) %>% set_engine("kernlab")


svm_grid <- expand.grid(
  cost = 2^c(-2, -1, 0, 1, 2),
  rbf_sigma = 2^c(-2, -1, 0, 1, 2)
)

svm_wf <- workflow() %>%
  add_model(svm_spec) %>%
  add_recipe(base_rec)

svm_res <- tune_grid(
  svm_wf,
  resamples = cv_folds,
  grid = svm_grid,
  metrics = metric_set(accuracy)
)

svm_best <- select_best(svm_res, metric = "accuracy")
print("Best SVM parameters:")
print(svm_best)

final_svm_wf <- finalize_workflow(svm_wf, svm_best)
final_svm_fit <- fit(final_svm_wf, data = train_data)


log_eval <- evaluate_model(final_log_fit, test_data)
tree_eval <- evaluate_model(final_tree_fit, test_data)
nn_eval <- evaluate_model(final_nn_fit, test_data)
knn_eval <- evaluate_model(final_knn_fit, test_data)
svm_eval <- evaluate_model(final_svm_fit, test_data)


accuracies <- tibble(
  Model = c("Logistic Regression", "Decision Tree", "Neural Network", "k-NN", "SVM"),
  Accuracy = c(
    log_eval$accuracy$.estimate,
    tree_eval$accuracy$.estimate,
    nn_eval$accuracy$.estimate,
    knn_eval$accuracy$.estimate,
    svm_eval$accuracy$.estimate
  )
)


print("Model Accuracies:")
print(accuracies)


parameter_table <- tibble(
  Model = c("Logistic Regression", "Decision Tree", "Neural Network", "k-NN", "SVM"),
  Parameters = c(
    paste("penalty =", round(log_best$penalty, 5)),
    paste("min_n =", tree_best$min_n, ", tree_depth =", tree_best$tree_depth, 
          ", cost_complexity =", round(tree_best$cost_complexity, 5)),
    paste("hidden_units =", nn_best$hidden_units, ", penalty =", round(nn_best$penalty, 5)),
    paste("neighbors =", knn_best$neighbors),
    paste("cost =", round(svm_best$cost, 2), ", rbf_sigma =", round(svm_best$rbf_sigma, 5))
  )
)


print("Best Parameters for Each Model:")
print(parameter_table)







