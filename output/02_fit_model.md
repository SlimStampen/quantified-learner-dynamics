Fit XGBoost model
================
Last updated: 2025-02-19

- [Load features data](#load-features-data)
- [Prepare model](#prepare-model)
- [Fit model](#fit-model)

``` r
library(here)
library(fst)
library(data.table)
library(forcats)
library(tidymodels)
library(themis)
library(vip)
library(purrr)
library(furrr)

plan(multisession, workers = 8)
```

## Load features data

``` r
df <- read_fst(here("data", "response_features.fst"))
setDT(df)

# Set numeric ID columns to character
df[, response_id := as.character(response_id)]
df[, user_id := as.character(user_id)]

# Set `correct` to integer
df[, correct := as.integer(correct)]

# Prepare column that will be used to indicate whether the session is from the target user
df[, session_from_user := 0]
df[, session_from_user := factor(session_from_user, levels = c(0, 1))]
df[, session_from_user := relevel(session_from_user, ref = "1")]
```

## Prepare model

Recipes for the three model variants: a full model with all available
features, a model without bigram-specifci features, and a model with
only performance features.

``` r
recipe_xgb <- recipe(session_from_user ~ ., data = df) |>
  update_role(response_id, user_id, session_id, new_role = "ID") |>
  step_downsample(session_from_user, under_ratio = 1) # Ensure training set has 1:1 ratio of same/different user

recipe_xgb_bigram_agnostic <- recipe_xgb |>
  step_rm(starts_with("bigram_"))

recipe_xgb_only_keystroke <- recipe_xgb |>
  step_rm(c("correct", "reaction_time", "alpha"))

recipe_xgb_only_performance <- recipe(session_from_user ~ correct + reaction_time + alpha, data = df) |>
  step_downsample(session_from_user, under_ratio = 1) # Ensure training set has 1:1 ratio of same/different user
```

Model specification:

``` r
# XGBoost
spec_xgb <- boost_tree(
  tree_depth = 4, # default: 6
  trees = 100, # default: 15
  learn_rate = .1, # default: .3
  mtry = 5, # default: NULL (all features)
  min_n = 1, # default: 1
  loss_reduction = 0.01, # default: 0
  sample_size = .8, # default: 1
  stop_iter = 10, # default: Inf
) |>
  set_mode("classification") |>
  set_engine("xgboost")
```

Workflows:

``` r
wf_xgb <- workflow() |>
  add_recipe(recipe_xgb) |>
  add_model(spec_xgb)

wf_xgb_bigram_agnostic <- workflow() |>
  add_recipe(recipe_xgb_bigram_agnostic) |>
  add_model(spec_xgb)

wf_xgb_only_keystroke <- workflow() |>
  add_recipe(recipe_xgb_only_keystroke) |>
  add_model(spec_xgb)

wf_xgb_only_performance <- workflow() |>
  add_recipe(recipe_xgb_only_performance) |>
  add_model(spec_xgb)
```

## Fit model

Fit the model per user:

``` r
user_ids <- unique(df$user_id)

fits <- map_dfr(user_ids, function (u) {
  
  # Fill in the session_from_user column
  df[, session_from_user := "0"]
  df[user_id == u, session_from_user := "1"]
  
  # Create a train/test split using stratified sampling
  # and ensuring that responses from the same session end up in the same set
  set.seed(42)
  df_minority <- df[session_from_user == "1"]
  df_majority <- df[session_from_user == "0"]
  
  # Split the minority class manually into training/testing
  minority_sessions <- df_minority[, unique(session_id)]
  test_minority_sessions <- sample(minority_sessions, size = ceiling(0.2 * length(minority_sessions)))
  df_minority_train <- df_minority[!session_id %in% test_minority_sessions]
  df_minority_test <- df_minority[session_id %in% test_minority_sessions]
  
  # Split the majority class in the same way
  majority_sessions <- df_majority[, unique(session_id)]
  test_majority_sessions <- sample(majority_sessions, size = ceiling(0.2 * length(majority_sessions)))
  df_majority_train <- df_majority[!session_id %in% test_majority_sessions]
  df_majority_test <- df_majority[session_id %in% test_majority_sessions]
  
  # Combine the training and testing sets
  df_train <- rbind(df_minority_train, df_majority_train)
  df_test <- rbind(df_minority_test, df_majority_test)
  split <- make_splits(x = df_train, assessment = df_test)

  # See the effect of preprocessing steps on the training data  
  # df_preproc <- prep(recipe_xgb, training = training(split), retain = TRUE) |>
  #   bake(new_data = NULL) |>
  #   setDT()

  # Fit each model to the training set and evaluate it on the test set
  fit_model <- function (wf, split, model_name) {
    
    # Fit the model
    fit <- last_fit(wf, split, metrics = metric_set(roc_auc, bal_accuracy, sens, spec))
    
    # Collect metrics
    metrics <- collect_metrics(fit) |>
      as.data.table()
    metrics <- dcast(metrics, .config ~ .metric, value.var = ".estimate")
    metrics[, .config := NULL]
    
    # Also make a classification at the session level, using majority voting
    trial_predictions <- collect_predictions(fit)
    setDT(trial_predictions)
    trial_predictions[, session_id := testing(split)$session_id] # Add session ID
    
    # Classify session based on a majority of trial-level predictions for that session
    # (within a session, count how often .pred_class == 1 relative to the total count of rows)
    session_predictions <- trial_predictions[, .(est_prob_session_from_user = sum(.pred_class == 1) / .N), by = .(session_id, session_from_user)]
    session_predictions[, pred_session_from_user := factor(ifelse(est_prob_session_from_user > 0.5, 1, 0), levels = c(0, 1))]
    session_predictions[, pred_session_from_user := relevel(pred_session_from_user, ref = "1")]
    
    roc_auc_session <- roc_auc(session_predictions, session_from_user, est_prob_session_from_user)$.estimate
    bal_accuracy_session <- bal_accuracy(session_predictions, session_from_user, pred_session_from_user)$.estimate
    sens_session <- sens(session_predictions, session_from_user, pred_session_from_user)$.estimate
    spec_session <- spec(session_predictions, session_from_user, pred_session_from_user)$.estimate
    
    metrics <- cbind(metrics, roc_auc_session, bal_accuracy_session, sens_session, spec_session)
    
    # Variable importance
    vimp <- extract_fit_parsnip(fit) |>
      vi(method = "model", scale = TRUE) |>
      as.data.table()
    vimp[, user_id := u]
    vimp_wide <- dcast(vimp, user_id ~ Variable, value.var = "Importance")
    
    return(cbind(model_name, vimp_wide, metrics))
  }
  
  # Fit the three variants of the model
  fit_xgb <- fit_model(wf_xgb, split, "full")
  fit_xgb_bigram_agnostic <- fit_model(wf_xgb_bigram_agnostic, split, "bigram_agnostic")
  fit_xgb_only_keystroke <- fit_model(wf_xgb_only_keystroke, split, "only_keystroke")
  fit_xgb_only_performance <- fit_model(wf_xgb_only_performance, split, "only_performance")

  user_fits <- rbind(fit_xgb, fit_xgb_bigram_agnostic, fit_xgb_only_keystroke, fit_xgb_only_performance, fill = TRUE)
  
  # Add information about training set size
  user_fits[, n_sessions_train := length(minority_sessions) - length(test_minority_sessions)]
  user_fits[, n_trials_train := nrow(df_minority_train)]
  
  return(user_fits)
}, .progress = interactive())
setDT(fits)

fwrite(fits, here("data", "model_fits.csv"))
```
