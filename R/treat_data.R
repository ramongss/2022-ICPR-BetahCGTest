# load functions
source(here::here("R", "get_data.R"))

# get raw data from database
demand_raw <-
  readr::read_file(here::here("queries", "bhcg_consumption.txt")) |>
  get_data() |>
  dplyr::as_tibble()

# complete the missing days with 0
demand <-
  demand_raw |>
  dplyr::group_by(region, exam, index) |>
  dplyr::mutate(day = as.Date(day),
                n_exams = as.numeric(n_exams)) |>
  tidyr::complete(day = seq.Date(min(day), max(day), by = "day")) |>
  dplyr::mutate(n_exams = tidyr::replace_na(n_exams, 0)) |>
  dplyr::group_by(exam, index, day) |>
  dplyr::summarise(n_exams = sum(n_exams)) |>
  dplyr::ungroup() |>
  dplyr::mutate(mm = zoo::rollmean(n_exams, k = 7, fill = NA, align = "right"))

demand |>
  saveRDS(here::here("data", "demand.rds"))


# define Min-Max normalization function
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# normalize the dataset
demand_norm <-
  demand |>
  dplyr::mutate(n_exams = min_max_norm(n_exams)) |>
  dplyr::mutate(mm = zoo::rollmean(n_exams, k = 7, fill = NA, align = "right"))

# save normalized data as rds
demand_norm |>
  saveRDS(here::here("data", "demand_norm.rds"))

