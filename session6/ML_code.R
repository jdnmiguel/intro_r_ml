library(ISLR2)
library(glmnet)
library(ggplot2)

# LASSO Regression on Hitters Dataset ----



# Hitters dataset from ISLR2 package
df <- Hitters

# Display variable names
names(df)

# Count missing values
sum(is.na(df))

# Remove rows with missing values
df_na <- na.omit(df)

# Check again for missing values
sum(is.na(df_na))

## Create predictor matrix (x) and response variable (y) ----

# model.matrix():
# - automatically creates dummy variables for categorical predictors
# - removes the response variable (Salary)
# - [, -1] removes the intercept column

x <- model.matrix(Salary ~ ., df_na)[, -1]

# Response variable
y <- df_na$Salary

# Standardize predictors
x <- scale(x)


## Train / Test split ----

set.seed(060526)

# Create grid of lambda values
grid <- 10^seq(10, -2, length = 100)

# Randomly select half of observations for training
train <- sample(1:nrow(x), nrow(x) / 2)

# Remaining observations used for testing
test <- (-train)

# Test response values
y.test <- y[test]


## Fit LASSO model ----

# alpha = 1 -> LASSO regression
lasso.mod <- glmnet(
  x[train, ],
  y[train],
  alpha = 1,
  standardize = TRUE
)

# Plot coefficient paths as lambda changes
plot(lasso.mod)


### Cross-validation to choose best lambda ----


set.seed(1)

cv.out <- cv.glmnet(
  x[train, ],
  y[train],
  alpha = 1,
  standardize = TRUE
)

# Cross-validation plot
plot(cv.out)

# Best lambda minimizing CV error
bestlam <- cv.out$lambda.min

bestlam


## Predict on test set ----


lasso.pred <- predict(
  lasso.mod,
  s = bestlam,
  newx = x[test, ]
)

# Compute test Mean Squared Error (MSE)
mean((lasso.pred - y.test)^2)


## Extract LASSO coefficients ----


out <- glmnet(
  x,
  y,
  alpha = 1,
  lambda = grid,
  standardize = TRUE
)

# Coefficients at optimal lambda
lasso.coef <- predict(
  out,
  type = "coefficients",
  s = bestlam
)

# Display coefficients
lasso.coef

# Display only selected variables (non-zero coefficients)
lasso.coef[lasso.coef != 0]

# ---------------------------------------------------------
# Create dataframe for ggplot
# ---------------------------------------------------------

coef <- as.matrix(coef(cv.out, s = "lambda.min"))

df_plot <- data.frame(
  Variable = rownames(coef),
  Coefficient = coef[, 1]
)

# Keep only selected variables
df_plot <- subset(
  df_plot,
  Coefficient != 0 & Variable != "(Intercept)"
)


# Plot selected variables and coefficients


ggplot(
  df_plot,
  aes(
    x = reorder(Variable, Coefficient),
    y = Coefficient,
    fill = Coefficient > 0
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variables Selected by the LASSO Model",
    subtitle = "Non-zero coefficients at the optimal lambda value",
    x = "Predictor Variables",
    y = "Estimated Coefficient",
    fill = "Coefficient Sign"
  ) +
  theme_minimal(base_size = 13) +
  scale_fill_discrete(
    labels = c("Negative Effect", "Positive Effect")
  )
# Decision trees ----
library(randomForest)
Boston <- Boston

set.seed(1)
## Train / Test split ----
train <- sample(1:nrow(Boston), nrow(Boston) / 2)
boston.test <- Boston[-train, "medv"]

## Fit Random Forest model ----
## can do again the same and vary mtry to see how it improves
rf.boston <- randomForest(medv ∼ ., data = Boston,
  subset = train,  importance = TRUE) # bydefault mytry=p/3 = 4

rf.boston #Display model summary 

## Predict on test set ----
yhat.rf <- predict(rf.boston, newdata = Boston[-train, ])
mean((yhat.rf- boston.test)^2) 


## Actual vs Predicted plot ----
pred_df <- data.frame(
  Actual = boston.test,
  Predicted = yhat.rf
)

ggplot(pred_df, aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Random Forest Predictions vs Actual Values",
    subtitle = "Boston Housing Dataset",
    x = "Actual Median Home Value",
    y = "Predicted Median Home Value"
  ) +
  theme_minimal(base_size = 13)


# Extract importance measures
importance_df <- data.frame(
  Variable = rownames(importance(rf.boston)),
  Importance = importance(rf.boston)[, "%IncMSE"]
)

# Sort variables by importance
importance_df <- importance_df[
  order(importance_df$Importance),
]


## Plot variable importance ----


ggplot(
  importance_df,
  aes(x = reorder(Variable, Importance),y = Importance)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Variable Importance in Random Forest Model",
    subtitle = "Measured by Increase in MSE (%IncMSE)",
    x = "Predictor Variables",
    y = "Importance Score"
  ) +
  theme_minimal(base_size = 13)

#Two measures of variable importance are reported. The first is based upon
#the mean decrease of accuracy in predictions on the out of bag samples when
#a given variable is permuted. The second is a measure of the total decrease
#in node impurity that results from splits over that variable, averaged over
#all trees (this was plotted in Figure 8.9). In the case of regression trees,
#the node impurity is measured by the training RSS


# 3. PDS LASSO 


library(hdm)
data("GrowthData") # = use ?GrowthData for more information = #
GrowthData=GrowthData[,-2] # = The second column is just a vector of ones = #

y=as.vector(GrowthData$Outcome)
D=as.vector(GrowthData$gdpsh465)
Controls=as.matrix(GrowthData)[,-c(1,2,3)]


# = Naive OLS with all variables = #
OLS=lm(y~D+Controls)
OLS=summary(OLS)$coefficients[1,]

# = Single step selection LASSO and Post-OLS = #
# = I will select only the summary line that contains the initial log GDP = #
lasso = rlasso(y~., data = GrowthData, post = FALSE) # = Run the Rigorous LASSO = #
selected = which(coef(lasso)[-c(1:2)] !=0) # = Select relevant variables = #
SS = summary(lm(formula, data = dataset))$coefficients[1, ]

# = Double Selection = #
DS<-rlassoEffect(Controls,y,D,method="double selection")
summary(DS)
DS$selection.index

DS=summary(DS)$coefficients[1,]
(results=rbind(OLS,SS,DS))







