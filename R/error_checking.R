
# Put functions for error checking here

## Identify duplicates preventing pivoting wider
check_duplicates <- function(dataset_ids, path_data = "data") {

  schema <- get_schema()
  resource_metadata <- get_schema("config/metadata.yml",  "metadata")
  definitions <- get_schema("config/traits.yml", "traits")
  unit_conversions <- traits.build:::get_unit_conversions("config/unit_conversions.csv")
  taxon_list <- read_csv_char("config/taxon_list.csv")
  examples_dir <- "examples"

  # Build each dataset in `dataset_ids`
  for (dataset in dataset_ids) {
    build_config <- dataset_configure(
      file.path(path_data, dataset, "metadata.yml"), definitions)
    build_dataset_raw <- dataset_process(
      file.path(path_data, dataset, "data.csv"),
      build_config, schema, resource_metadata, unit_conversions)
    # Assign built dataset to the variable name from `dataset`
    assign(dataset, build_update_taxonomy(build_dataset_raw, taxon_list))
  }

  # Combine datasets
  traits <- dataset_ids %>%
    rlang::parse_exprs() %>%
    lapply(eval) %>%
    lapply("[[", "traits") %>%
    dplyr::bind_rows()

  # Check for duplicates
  traits %>%
    select(
      # `taxon_name` and `original_name` are not needed for pivoting but are included for informative purposes
      dplyr::all_of(
        c("dataset_id", "trait_name", "value", "taxon_name", "original_name", "observation_id",
        "value_type", "repeat_measurements_id", "method_id", "method_context_id"))
    ) %>%
    tidyr::pivot_wider(names_from = "trait_name", values_from = "value", values_fn = length) %>%
    tidyr::pivot_longer(cols = 9:ncol(.)) %>%
    dplyr::rename(dplyr::all_of(c("trait_name" = "name", "number_of_duplicates" = "value"))) %>%
    select(
      dplyr::all_of(c("dataset_id", "trait_name", "number_of_duplicates",
      "taxon_name", "original_name", "observation_id", "value_type")), everything()
    ) %>%
    filter(.data$number_of_duplicates > 1)

}
