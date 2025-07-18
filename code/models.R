## prelim (loading libraries, read data, etc)
library(dplyr)
library(ROSE)
library(glmnet)
library(e1071)
library(xgboost)
library(pROC)
library(summarytools)
library(Matrix)
library(randomForest)
library(MLmetrics)


# read data
df <- readRDS('data/clean_data.rds')


# ---------- Analysis Starts Here -----------

# add binary variable for binary injury prediction
df$inj_severity_bin <- factor(if_else(df$inj_severity %in% c("Killed","Serious"), 1 , 0))

# make train/test split
set.seed(123)
train_ind <- sample(1:nrow(df), floor(0.85 * nrow(df)))

# --------- Binary without balancing ------------
df_bin <- df %>% select(-inj_severity)
y_bin <- df_bin$inj_severity_bin
mf <- model.frame(~ . - inj_severity_bin, data = df_bin, na.action = na.pass)
x_bin <- model.matrix(~ . - inj_severity_bin, data = mf)[, -1]


x_bin_train <- x_bin[train_ind, ]
y_bin_train <- y_bin[train_ind]
x_bin_test <- x_bin[-train_ind, ]
y_bin_test <- y_bin[-train_ind]

# --------- Binary with ROSE balancing ----------
# Use raw data (with factor variables) as input to ROSE before dummy encoding
df_train_raw <- df_bin[train_ind, ]
rose_data <- ROSE(inj_severity_bin ~ ., data = df_train_raw, seed = 123)$data

# now dummy encode the balanced data
x_bin_rose <- model.matrix(~ . - inj_severity_bin, data = rose_data)[, -1]
y_bin_rose <- rose_data$inj_severity_bin

# garbage collect (remove previous stuff that i dont need)
keep_vars <- c(
  "x_bin_train", "y_bin_train",
  "x_bin_test", "y_bin_test",
  "df_train_raw", "rose_data",
  "x_bin_rose", "y_bin_rose",
  'xbin'
)

rm(list = setdiff(ls(), keep_vars))
gc()

# --------- model without balancing ----------
set.seed(123)

# logistic
log_model <- glm(y_bin_train ~ ., data = as.data.frame(x_bin_train), family = "binomial")
saveRDS(log_model, 'models/logistic.rds')

# xgboost
xgb_model <- xgboost(data = x_bin_train, label = as.numeric(as.character(y_bin_train)),
                     objective = "binary:logistic", nrounds = 100, eval_metric = "auc", verbose = 0)
saveRDS(xgb_model, 'models/xgboost.rds')

# random forest (to see decision trees)
rf_model <- randomForest(x = x_bin_train, y = y_bin_train, ntree = 100)
saveRDS(rf_model, 'models/randomforest.rds')

# -------- Binary Model with ROSE ------
set.seed(123)

# logistic
log_model_rose <- glm(y_bin_rose ~ ., data = as.data.frame(x_bin_rose), family = "binomial")
saveRDS(log_model_rose, 'models/log_rose.rds')

# xgboost
xgb_model_rose <- xgboost(data = x_bin_rose, label = as.numeric(as.character(y_bin_rose)),
                           objective = "binary:logistic", nrounds = 100, eval_metric = "auc", verbose = 0)
saveRDS(xgb_model_rose, 'models/xgb_rose.rds')

# random forest
rf_model_rose <- randomForest(x = x_bin_rose, y = y_bin_rose, ntree = 100)
saveRDS(rf_model_rose, 'models/rf_rose.rds')

# -------- Regularized Logistic ----------

# no ROSE
cv_glm <- cv.glmnet(x_bin_train, y = as.numeric(as.character(y_bin_train)), family = "binomial", alpha = 1)
saveRDS(cv_glm, 'models/cv_log.rds')

# ROSE
cv_glm_rose <- cv.glmnet(x_bin_rose, y = as.numeric(as.character(y_bin_rose)), family = "binomial", alpha = 1)
saveRDS(cv_glm_rose, 'models/cv_log_rose.rds')

# -------- Logistic on full dataset (interpretability) --------
full_log_model <- glm(inj_severity_bin ~ ., data = as.data.frame(x_bin_train), family = "binomial")
saveRDS(full_log_model, 'models/full_log.rds')

cv_glm_full <- cv.glmnet(x_bin, y = as.numeric(as.character(y_bin)), family = "binomial", alpha = 1)
saveRDS(cv_glm_full, 'moedls/full_cv_log.rds')
# coef(cv_glm_full, s = "lambda.min")



