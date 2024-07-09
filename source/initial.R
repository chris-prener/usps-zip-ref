library(dplyr)
library(janitor)
library(readr)
library(stringr)
library(tidyr)

zip3 <- read_csv("data/data_raw/DMM_L002_202408.csv", skip = 11) |>
  clean_names() |>
  select(
    zip3 = x3_digit_zip_code_prefix,
    destination = column_a_3_digit_destinations_label_to,
    scf = column_c_scf_destinations_label_to
  )

zip3 <- zip3 |>
  mutate(
    destination_area = word(destination, 1, -3),
    destination_state = word(destination, -2),
    .after = "destination"
  ) |>
  mutate(
    scf_name = word(scf, 1, -3),
    scf_state = word(scf, -2),
    scf_id = word(scf, -1),
    .after = "scf"
  ) |>
  select(-destination, -scf)

write_csv(zip3, "data/zip3_202408.csv")

meta <- data.frame(
  type = c("zip3"),
  date = c("202408")
)

write_csv(meta, "data/meta.csv")

split_range <- function(x){
  
  if (length(x) > 1){
    limits <- as.numeric(unlist(x))
    out <- seq(limits[1], limits[2])
  } else {
    out <- as.numeric(x)
  }
  
  return(out)
}

zip5 <- read_csv("data/data_raw/DMM_L606_202408.csv", skip = 7) |>
  clean_names() |>
  select(
    zip5_range = column_a_destination_zip_codes,
    delivery_unit = column_b_label_container_to
  ) |>
  mutate(
    delivery_unit_city = word(delivery_unit, 1, -3),
    delivery_unit_state = word(delivery_unit, -2),
    delivery_unit_zip5 = word(delivery_unit, -1),
    .after = "delivery_unit"
  ) |>
  select(-delivery_unit)

zip5_2 <- zip5 |>
  mutate(
    zip5_range_clean = strsplit(zip5_range, ", "), .after = "zip5_range"
  ) |>
  unnest(zip5_range_clean) |>
  mutate(
    zip5_range_clean = strsplit(zip5_range_clean, "-")
  ) |>
  rowwise() |>
  mutate(
    zip5_range_clean = list(split_range(zip5_range_clean))
  ) |>
  ungroup() |>
  unnest(zip5_range_clean)
