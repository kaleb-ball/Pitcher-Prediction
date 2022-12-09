library(tidymodels)
library(vip)
library(mgcv)

# Common
xgb <- function(data_model) {
  ind_vars <- c('total_bases', 'runs_scored', 'woba_value')
  ind_var <- ind_vars[3]

  ids <- c('game_pk', 'pitcher', 'batter')
  predictors <- data_model %>% select(-{{ind_vars}}) %>% colnames
  exclude <- ind_vars[!(ind_vars %in% ind_var)]

  formula_str <- formula(paste0(ind_var, ' ~ ',  '.'))

  data_split <- initial_split(data_model %>% select(-{{exclude}}))
  train <- training(data_split)
  test <- testing(data_split)

  rec <- recipe(formula = formula_str, data = train) %>%
    update_role(game_pk, pitcher, batter, new_role = "ID") %>%
    step_dummy(all_nominal_predictors())

  #XGBoost

  folds <- vfold_cv(train, v=3)
  xgb_model <- boost_tree(
    mode="regression",
    engine="xgboost",
    mtry = tune(),
    learn_rate = tune(),
    tree_depth = tune(),
    loss_reduction = tune(),
    trees = 20)

  xgb_params <- xgb_model %>%
    extract_parameter_set_dials() %>%
    finalize(x = train)

  xgb_grid <- xgb_params %>% grid_regular(levels = 5)

  xgb_wflow <-
    workflow() %>%
      add_recipe(rec) %>%
      add_model(xgb_model)

  xgb_res <- xgb_wflow %>%
    tune_grid(resamples = folds, grid = xgb_grid)

  xgb_final_wf <-
    xgb_wflow %>% finalize_workflow(xgb_res %>% select_best("rmse"))

  xgb_fit <- xgb_final_wf %>% last_fit(data_split) %>% extract_workflow()

  xgb_final_wf %>% fit(data=train) %>% extract_fit_parsnip() %>% vip(geom = 'point')

  xgb_test_pred <-
    predict(xgb_fit, test)%>%
      bind_cols(test %>% select(woba_value))

  rmse(data = xgb_test_pred, truth = woba_value, estimate = .pred)

  return(xgb_fit)
}
