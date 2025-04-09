install.packages(c("yardstick", "tune", "workflowsets", "tidymodels"),
                 type = "source",
                 repos = "https://cloud.r-project.org")
install.packages("glmnet")
install.packages("mlbench")

library(tidymodels)
library(readr)
library(mlbench)
library(glmnet)

data("BostonHousing")
str(BostonHousing)
summary(BostonHousing)

sum(is.na(BostonHousing))

set.seed(123456)

split <- initial_split(BostonHousing, prop = 0.8)
housing_train <- training(split)
housing_test <- testing(split)

housing_recipe <- recipe(medv ~ ., data = housing_train) %>%
  step_log(all_outcomes()) %>%           
  step_interact(terms = ~ crim + zn + indus + rm + age + rad + tax + ptratio + b + lstat + dis + nox) %>% 
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6) 

housing_prep <- prep(housing_recipe, training = housing_train, retain = TRUE)

housing_train_prepped <- juice(housing_prep)
housing_test_prepped <- bake(housing_prep, new_data = housing_test)

housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

lasso_model <- glmnet(housing_train_x, housing_train_y$medv, alpha = 1)
housing_train_matrix <- model.matrix(~ . - 1, data = housing_train_x)
cv_lasso <- cv.glmnet(housing_train_matrix, housing_train_y$medv, alpha = 1, nfolds = 6)

plot(cv_lasso)

optimal_lambda_lasso <- cv_lasso$lambda.min
optimal_lambda_lasso


housing_test_prepped <- bake(housing_prep, new_data = housing_test)
housing_test_x <- model.matrix(~ . - 1, data = housing_test_prepped)
housing_test_x <- housing_test_x[, colnames(housing_train_matrix)]


lasso_train_pred <- predict(cv_lasso, s = "lambda.min", newx = housing_train_matrix)
lasso_train_rmse <- sqrt(mean((lasso_train_pred - housing_train_y$medv)^2))
lasso_train_rmse

lasso_test_pred <- predict(cv_lasso, s = "lambda.min", newx = housing_test_x)
lasso_test_rmse <- sqrt(mean((lasso_test_pred - housing_test_y$medv)^2))
lasso_test_rmse

ridge_model <- glmnet(housing_train_x, housing_train_y$medv, alpha = 0)
housing_train_x_numeric <- as.data.frame(lapply(housing_train_x, as.numeric))
housing_train_x_matrix <- as.matrix(housing_train_x_numeric)
cv_ridge <- cv.glmnet(housing_train_x_matrix, housing_train_y$medv, alpha = 0, nfolds = 6)
plot(cv_ridge)

optimal_lambda_ridge <- cv_ridge$lambda.min
optimal_lambda_ridge

housing_train_x_matrix <- apply(housing_train_x_matrix, 2, as.numeric)
is.numeric(housing_train_x_matrix)
ridge_train_pred <- predict(cv_ridge, s = "lambda.min", newx = housing_train_x_matrix)
ridge_train_rmse <- sqrt(mean((ridge_train_pred - housing_train_y$medv)^2))
ridge_train_rmse


housing_recipe_prep <- prep(housing_recipe, training = housing_train)
housing_test_prepped <- bake(housing_recipe_prep, new_data = housing_test)
housing_test_x_matrix <- model.matrix(~ . - 1, data = housing_test_prepped)
training_columns <- colnames(housing_train_x_matrix)
missing_cols <- setdiff(training_columns, colnames(housing_test_x_matrix))
for (col in missing_cols) {
  housing_test_x_matrix <- cbind(housing_test_x_matrix, setNames(data.frame(0), col))
}
housing_test_x_matrix <- housing_test_x_matrix[, training_columns]
housing_test_x_matrix <- as.matrix(housing_test_x_matrix)
ridge_test_pred <- predict(cv_ridge, s = "lambda.min", newx = housing_test_x_matrix)
ridge_test_rmse <- sqrt(mean((ridge_test_pred - housing_test_y$medv)^2))
ridge_test_rmse

elastic_net_model <- glmnet(housing_train_x, housing_train_y$medv, alpha = 0.5)
housing_train_x <- as.data.frame(lapply(housing_train_x, function(x) as.numeric(as.character(x))))
housing_train_x_matrix <- as.matrix(housing_train_x)
housing_train_y_vector <- as.numeric(housing_train_y$medv)  # Make sure the response variable is numeric
cv_elastic_net <- cv.glmnet(housing_train_x_matrix, housing_train_y_vector, alpha = 0.5, nfolds = 6)
plot(cv_elastic_net)

optimal_lambda_en <- cv_elastic_net$lambda.min
optimal_lambda_en

en_train_pred <- predict(cv_elastic_net, s = "lambda.min", newx = housing_train_x_matrix)
en_train_rmse <- sqrt(mean((en_train_pred - housing_train_y_vector)^2))
en_train_rmse


housing_test_x <- as.data.frame(lapply(housing_test_x, function(x) as.numeric(as.character(x))))
training_columns <- colnames(housing_train_x_matrix)
test_columns <- colnames(housing_test_x)
missing_cols <- setdiff(training_columns, test_columns)
for (col in missing_cols) {
  housing_test_x[[col]] <- 0
}
housing_test_x <- housing_test_x[, training_columns]
housing_test_x_matrix <- as.matrix(housing_test_x)
en_test_pred <- predict(cv_elastic_net, s = "lambda.min", newx = housing_test_x_matrix)
en_test_rmse <- sqrt(mean((as.vector(en_test_pred) - housing_test_y$medv)^2))
en_test_rmse

