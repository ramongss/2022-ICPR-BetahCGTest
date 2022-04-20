rm(list = ls(all.names = TRUE))

# load functions
source(here::here("R", "train_models.R"))

demand <- readr::read_rds(here::here("data", "demand_norm.rds"))

demand <-
  demand |>
  dplyr::select(date = day, target = mm) |>
  dplyr::filter(!is.na(target))

print("Tranining models...")
results <- train_models(
  data = demand,
  # model_list = sort(c("cubist", "svmLinear2", "glm", "gaussprLinear")),
  model_list = sort(c("ridge", "pls", "glm", "gaussprLinear")),
  horizon = c(1, 15, 30, 45, 60)
)
print("Models trained!")

print(results$stacking$Metrics)
print(results$stacking$Stack_Metrics)

results |>
  saveRDS(here::here("results", paste0(Sys.Date(), "_results.rds")))