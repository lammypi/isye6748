#_data.R for handling of data

# Create match to file targets
match_targets <- list(
  tar_target(s24_f24_matches_file, "data/raw/s24_f24_email_matches.csv"),
  tar_target(s24_s25_matches_file, "data/raw/s24_s25_email_matches.csv"),
  tar_target(f24_s25_matches_file, "data/raw/f24_s25_email_matches.csv"),
  tar_target(s24_f24_matches, readr::read_csv(s24_f24_matches_file, show_col_types = FALSE)),
  tar_target(s24_s25_matches, readr::read_csv(s24_s25_matches_file, show_col_types = FALSE)),
  tar_target(f24_s25_matches, readr::read_csv(f24_s25_matches_file, show_col_types = FALSE))
)

# Mapping of files
data_targets <- tar_map(
  values = tibble::tibble(
    wave = c("spring2024", "fall2024", "spring2025"),
    path = c("data/raw/new_spring2024.csv", "data/raw/fall2024.csv", "data/raw/new_spring2025.csv")
  ),
  names = wave,
  list(
    # Track the file as a dependency
    tar_target(file, path, format="file"),
    # Import it
    tar_target(raw, readr::read_csv(file, show_col_types=FALSE)),
    ### ADD ANY CLEANING FUNCTIONS - UNCOMMENT WHEN READY
    tar_target(
      clean,
      switch(
        wave,
        spring2024 = prepare_dataframe(raw, wave = NULL, ratings_original_names, s24f24_new_ratings_names, base_result_col, base_prod_cols),
        fall2024 = prepare_dataframe(raw, wave = "Fall 2024", ratings_original_names, s24f24_new_ratings_names, nonbase_result_col, base_prod_cols, s24_f24_matches = s24_f24_matches),
        spring2025 = prepare_dataframe(raw, wave = "Spring 2025", s25_original_names, s25_new_ratings_names, nonbase_result_col, s25_prod_cols, s24_s25_matches = s24_s25_matches, f24_s25_matches = f24_s25_matches)
      )
    )
  )
)

# Update data_targets
data_targets <- c(match_targets, data_targets)