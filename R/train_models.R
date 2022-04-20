train_models <- function(data, model_list, horizon) {

  # list of models to be trained
  model_list <- sort(model_list)

  results <- list()

  # load fit function
  source(here::here("R", "stacking_pred_function.R"))

  # fit and predict
  results[["stacking"]] <- stacking_pred(
    data = data,
    model_list = model_list,
    horizon = horizon
  )

  return(results)
}