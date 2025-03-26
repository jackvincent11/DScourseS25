install.packages("mice")
install.packages("modelsummary")
library(mice)
library(modelsummary)

wages <- read_csv("ProblemSets/PS7/wages.csv")

datasummary_skim(wages, output = "latex", file = "summary_table.tex")


missing_rate <- sum(is.na(wages$logwage)) / nrow(wages)
print(paste("Missing rate for logwage:", missing_rate))
# Log wages are missing at a rate of 0.2493321460374. I think the logwage variable is most 
# likely to be MAR.

complete_cases <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = na.omit(wages))


wages$logwage[is.na(wages$logwage)] <- mean(wages$logwage, na.rm = TRUE)
mean_imputation <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages)


wages$logwage[is.na(wages$logwage)] <- predict(complete_cases, newdata = wages)
predicted_imputation <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages)


imp <- mice(wages, method = "pmm", m = 5)
mice_fit <- with(imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
mice_pool <- pool(mice_fit)

modelsummary(list(Complete = complete_cases, MeanImpute = mean_imputation, Predicted = predicted_imputation, MICE = mice_pool),
             output = "latex", file = "regression_results.tex")

