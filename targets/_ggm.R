##### SPECIFIES THE GGM {targets} SETTINGS

library(targets)

# GGM Dataset Targets 
ggm_dataset_targets <- list(
  # ----- DATA SETS PER WAVE FOR GGM: OVERALL -----
  tar_target(ggm_spring2024, prepare_core_data(clean_spring2024, core_mental_model_cols)),
  tar_target(ggm_fall2024, prepare_core_data(clean_fall2024, core_mental_model_cols)),
  tar_target(ggm_spring2025, prepare_core_data(clean_spring2025, core_mental_model_cols)),
  # ----- DATA SETS PER WAVE FOR GGM: PERSONAS SPRING 2024 -----
  tar_target(ggm_s24p0, prepare_persona_data(clean_spring2024, 0, base_result_col, core_mental_model_cols)),
  tar_target(ggm_s24p1, prepare_persona_data(clean_spring2024, 1, base_result_col, core_mental_model_cols)),
  tar_target(ggm_s24p2, prepare_persona_data(clean_spring2024, 2, base_result_col, core_mental_model_cols)),
  # ----- DATA SETS PER WAVE FOR GGM: PERSONAS FALL 2024 -----
  tar_target(ggm_f24p0, prepare_persona_data(clean_fall2024, 0, nonbase_result_col, core_mental_model_cols)),
  tar_target(ggm_f24p1, prepare_persona_data(clean_fall2024, 1, nonbase_result_col, core_mental_model_cols)),
  tar_target(ggm_f24p2, prepare_persona_data(clean_fall2024, 2, nonbase_result_col, core_mental_model_cols)),
  # ----- DATA SETS PER WAVE FOR GGM: PERSONAS SPRING 2025 -----
  tar_target(ggm_s25p0, prepare_persona_data(clean_spring2025, 0, nonbase_result_col, core_mental_model_cols)),
  tar_target(ggm_s25p1, prepare_persona_data(clean_spring2025, 1, nonbase_result_col, core_mental_model_cols)),
  tar_target(ggm_s25p2, prepare_persona_data(clean_spring2025, 2, nonbase_result_col, core_mental_model_cols))
)



# ---------- Generator for analysis targets - eliminates copy and paste ----------
#' Constructs analysis targets
#' 
#' Parameters
#' - label
#' - data_target_name: the dataframe to use in an analysis.
make_ggm_analysis_targets <- function (
    label,
    data_target_name
) {
  # The dataframe object
  df_expr <- rlang::parse_expr(data_target_name)
  # Name of the GGM model
  model_name <- paste0("ggm_model_", label)
  # The model name as parsed out of a variable
  model_expr <- rlang::parse_expr(model_name)
  
  ### Received Gen AI help for cent_*, boot_* variables below because this is a less fragile way of specifying
  ### objects with accessible components than my original method. (Things were breaking my original way.)
  # For centrality
  cent_name <- paste0("ggm_cent_", label)
  cent_expr <- rlang::parse_expr(cent_name)
  
  # For bootstrapping centrality
  boot_cent_name <- paste0("ggm_bootstrap_cent_", label)
  boot_cent_expr <- rlang::parse_expr(boot_cent_name)
  
  # For bootstrapping expected influence
  boot_ei_name <- paste0("ggm_bootstrap_ei_", label)
  boot_ei_expr <- rlang::parse_expr(boot_ei_name)
  
  # Build the analysis targets
  list(
    # ----- 1. Estimate the network: Wrapper on bootnet::estimateNetwork() -----
    targets::tar_target_raw(
      name = model_name, # Name of the object
      command = rlang::expr(
        ggm_network_builder(
          !!df_expr
        ) # The function being called
      )
    ),
    # ----- 2. Bootstrapped edge weights (nonparametric) -----
    targets::tar_target_raw(
      name = paste0("ggm_bootstrap_edges_", label),
      command = rlang::expr(
        omnibus_bootstrap_analysis(
          !!model_expr,
          "edge",
          "nonparametric",
          1000
        )
      )
    ),
    # ----- 3. Centrality -----
    # Note that this uses qgraph::centrality(). So it will automatically calculate:
    # - strength centrality
    # - expected influence
    # - predictability, if R2 = TRUE (which it is in ggm_networks.R)
    
    # CENTRALITY OBJECT
    targets::tar_target_raw(
      name = paste0("ggm_cent_", label),
      command = rlang::expr(
        get_centrality(
          !!model_expr
        )
      )
    ),
    # EXTRACT STRENGTH
    targets::tar_target_raw(
      name = paste0("ggm_strength_", label),
      command = rlang::expr(
        (!!cent_expr)[["Strength"]]
      )
    ),
    # ----- 4. Bootstrapped strength centrality -----
    targets::tar_target_raw(
      name = boot_cent_name,
      command = rlang::expr(
        omnibus_bootstrap_analysis(
          !!model_expr,
          "strength",
          "case",
          1000
        )
      )
    ),
    # ----- 5. Strength corStability -----
    # EXTRACT CORSTABILITY
    targets::tar_target_raw(
      name = paste0("ggm_strength_cs_", label),
      command = rlang::expr(
        (!!boot_cent_expr)$cor_stability
      )
    ),
    # ----- 6. Expected Influence (EI) -----
    # EXTRACT EXPECTED INFLUENCE
    targets::tar_target_raw(
      name = paste0("ggm_ei_", label),
      command = rlang::expr(
        (!!cent_expr)$InExpectedInfluence # Undirected graph, prefer IN. OUT should be equal.
      )
    ),
    # ----- 7. Bootstrapped EI -----
    targets::tar_target_raw(
      name = boot_ei_name,
      command = rlang::expr(
        omnibus_bootstrap_analysis(
          !!model_expr,
          "expectedInfluence",
          "case",
          1000
        )
      )
    ),
    # ----- 8. EI corStability -----
    # EXTRACT CORSTABILITY
    targets::tar_target_raw(
      name = paste0("ggm_ei_cs_", label),
      command = rlang::expr(
        (!!boot_ei_expr)$cor_stability
      )
    ),
    # ----- 9. Predictability -----
    # EXTRACT R2
    targets::tar_target_raw(
      name = paste0("ggm_predictability_", label),
      command = rlang::expr(
        (!!cent_expr)$R2
      )
    )
  )
}



# ---------- NCT TARGET GENERATOR ----------
make_ggm_nct_targets <- function(
    label,
    df1_name,
    df2_name,
    p_method = "BH",
    alpha = 0.05
){
  # Parse out the dataframes
  df1 <- rlang::parse_expr(df1_name)
  df2 <- rlang::parse_expr(df2_name)
 
  # ---- Targets Definition -----
  targets::tar_target_raw(
    name = paste0("nct_", label),
    command = rlang::expr(
      compare_networks(
        df1 = !!df1,
        df2 = !!df2, 
        nw1_label = !!df1_name,
        nw2_label = !!df2_name,
        p_method = !!p_method,
        alpha = !!alpha
      )
    )
  )
}





######################## ANALYSIS CONSTRUCTION ########################
#
# Contains everything needed to contribute to a pipeline construction
# for GGM analysis.
#######################################################################

# Create a datasset registry
ggm_dataset_registry <- c(
  spring2024 = "ggm_spring2024",
  fall2024 = "ggm_fall2024",
  spring2025 = "ggm_spring2025",
  s24p0 = "ggm_s24p0",
  s24p1 = "ggm_s24p1",
  s24p2 = "ggm_s24p2",
  f24p0 = "ggm_f24p0",
  f24p1 = "ggm_f24p1",
  f24p2 = "ggm_f24p2",
  s25p0 = "ggm_s25p0",
  s25p1 = "ggm_s25p1",
  s25p2 = "ggm_s25p2"
)

# Within-wave alpha level
alpha_within_wave <- 0.05/3

# NCT within-wave registry
nct_within_wave_registry <- tibble::tribble(
  ~label,    ~df1,        ~df2,        ~alpha,
  "s24_p0_p1", "ggm_s24p0", "ggm_s24p1", alpha_within_wave,
  "s24_p0_p2", "ggm_s24p0", "ggm_s24p2", alpha_within_wave,
  "s24_p1_p2", "ggm_s24p1", "ggm_s24p2", alpha_within_wave,
  "f24_p0_p1", "ggm_f24p0", "ggm_f24p1", alpha_within_wave,
  "f24_p0_p2", "ggm_f24p0", "ggm_f24p2", alpha_within_wave,
  "f24_p1_p2", "ggm_f24p1", "ggm_f24p2", alpha_within_wave,
  "s25_p0_p1", "ggm_s25p0", "ggm_s25p1", alpha_within_wave,
  "s25_p0_p2", "ggm_s25p0", "ggm_s25p2", alpha_within_wave,
  "s25_p1_p2", "ggm_s25p1", "ggm_s25p2", alpha_within_wave
)



# NCT across wave overall registry
nct_across_wave_overall_registry <- tibble::tribble(
  ~label,                 ~df1,              ~df2,             ~alpha,
  "overall_s24_vs_f24",   "ggm_spring2024",  "ggm_fall2024",   0.05,
  "overall_s24_vs_s25",   "ggm_spring2024",  "ggm_spring2025", 0.05,
  "overall_f24_vs_s25",   "ggm_fall2024",    "ggm_spring2025", 0.05
)



# NCT across wave personas registry
nct_across_wave_personas_registry <- tibble::tribble(
  ~label,     ~df1,        ~df2,        ~alpha,
  "p0_s24_f24", "ggm_s24p0", "ggm_f24p0", 0.05,
  "p0_s24_s25", "ggm_s24p0", "ggm_s25p0", 0.05,
  "p0_f24_s25", "ggm_f24p0", "ggm_s25p0", 0.05,
  "p1_s24_f24", "ggm_s24p1", "ggm_f24p1", 0.05,
  "p1_s24_s25", "ggm_s24p1", "ggm_s25p1", 0.05,
  "p1_f24_s25", "ggm_f24p1", "ggm_s25p1", 0.05,
  "p2_s24_f24", "ggm_s24p2", "ggm_f24p2", 0.05,
  "p2_s24_s25", "ggm_s24p2", "ggm_s25p2", 0.05,
  "p2_f24_s25", "ggm_f24p2", "ggm_s25p2", 0.05
)



# Analysis targets
ggm_analysis_targets <- unlist(
  Map(make_ggm_analysis_targets, names(ggm_dataset_registry), ggm_dataset_registry),
  recursive = FALSE
)



# ---------- NCT targets ----------
ggm_nct_targets <- c(
  ### Gen AI help here because could not get the files to parse for the analysis correctly
  # ----- Within Wave Persona Comparisons -----
  unlist(
    purrr::pmap(
      nct_within_wave_registry,
      ~ make_ggm_nct_targets(..1, ..2, ..3, ..4, p_method = "BH")
    ),
    recursive = FALSE
  ),
  # ----- Across Wave Comparison: Overall Comparison -----
  unlist(
    purrr::pmap(
      nct_across_wave_overall_registry,
      ~ make_ggm_nct_targets(..1, ..2, ..3, ..4, p_method = "BH")
    ),
    recursive = FALSE
  ),
  # ----- Across Wave Comparison: Personas Comparisons -----
  unlist(
    purrr::pmap(
      nct_across_wave_personas_registry,
      ~ make_ggm_nct_targets(..1, ..2, ..3, ..4, p_method = "BH")
    ),
    recursive = FALSE
  )
)





# Notebook targets
ggm_notebook_targets <- list(
  # ----- GGM MODEL NOTEBOOKS BY SURVEY WAVE -----
  tar_target_raw(
    name="spring2024_mental_models_notebook", 
    command=bundle_ggm("spring2024", personas=spring2024_personas)
  ),
  tar_target_raw(
    name="fall2024_mental_models_notebook", 
    command=bundle_ggm("fall2024", personas=fall2024_personas)
  ),
  tar_target_raw(
    name="spring2025_mental_models_notebook", 
    command=bundle_ggm("spring2025", personas=spring2025_personas)
  ),
  # ----- OVERALL NETWORK MODEL COMPARISON -----
  tar_target(
    comparison_mental_models,
    list(
      spring2024 = ggm_model_spring2024,
      fall2024 = ggm_model_fall2024,
      spring2025 = ggm_model_spring2025
    )
  ),
  # ----- PERSONA NETWORK MODEL COMPARISONS -----
  tar_target(
    persona0_mental_models_comparison,
    list(
      spring2024 = ggm_model_s24p0,
      fall2024 = ggm_model_f24p0,
      spring2025 = ggm_model_s25p0
    )
  ),
  tar_target(
    persona1_mental_models_comparison,
    list(
      spring2024 = ggm_model_s24p1,
      fall2024 = ggm_model_f24p1,
      spring2025 = ggm_model_s25p1
    )
  ),
  tar_target(
    persona2_mental_models_comparison,
    list(
      spring2024 = ggm_model_s24p2,
      fall2024 = ggm_model_f24p2,
      spring2025 = ggm_model_s25p2
    )
  )  
)
  


# ---------- ASSEMBLE TARGETS ----------
ggm_targets <- c(
  ggm_dataset_targets,
  ggm_analysis_targets,
  ggm_nct_targets,
  ggm_notebook_targets
)