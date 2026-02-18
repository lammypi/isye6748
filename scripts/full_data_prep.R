########## DATA_PREP.R ##########
#### DESC: Data cleaning.
####       - For the GA Tech Applied Analytics Practicum (ISYE 6748)
#### AUTH: Leslie A. McFarlin, Principal UX Architect.

#########################################################################
#                            COLUMNS NAMES
#
# Tracks the target columns containing clustering results.
# Contains vectors of original column names and updated column names.
# Specifies out core mental model variable names.
#########################################################################
### COLUMNS CONTAINING CLUSTERING RESULTS
base_result_col <- "core_cluster"

nonbase_result_col <- "kmeans_cluster"

### ORIGINAL COLUMN NAMES
# Get the ratings original names - hold this if needed for Spring and Fall 2024
ratings_original_names <- c(
  "How satisfied are you with your company's fleet management program & policies?",                                                                                                         
  "How satisfied are you with the quality of your company vehicle?",   
  "Please rate your satisfaction with the below areas: - The ability to handle your requests in our online portal or mobile app",                                                           
  "Please rate your satisfaction with the below areas: - The ease of contacting Wheels",                                                                                                    
  "Please rate your satisfaction with the below areas: - The knowledge & professionalism of the representative",                                                                            
  "Please rate your satisfaction with the below areas: - The communication & follow up provided by the team handling your requests",                                                        
  "Please rate your satisfaction with the below areas: - The time it takes for your requests to be resolved",                                                                               
  "Please rate your satisfaction with the below areas: - Our ability to thoroughly & accurately resolve your requests", 
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Accident Management",                            
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Driver Call Center",                             
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Emergency Roadside services",                    
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Electric Vehicle Support & Charger Installation",
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - EV Charging Card",                               
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Fuel Card management",                           
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Glass Repair or Replacement",                    
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - License Registration & Renewal program",         
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Maintenance program",                            
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - New Vehicle Ordering & Delivery",                
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Personal Mileage Reporting",                     
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Rental Vehicle management",                      
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Safety & MVR program",              
  "Next, please rate your satisfaction with the products & services below: \r\n\r\nIf you have not utilized a service, please select N/A - Vehicle Turn In process")

# Names for Spring 2025 - New columns were added, existing elements were changed.
s25_original_names <- c(
  "How satisfied are you with your company's fleet management program & policies?",                                                                                                         
  "How satisfied are you with the quality of your company vehicle?", 
  "Please rate your satisfaction with the below areas: - The ability to handle your requests in our online portal or mobile app",                                                       
  "Please rate your satisfaction with the below areas: - The ease of contacting Wheels",                                                                                                
  "Please rate your satisfaction with the below areas: - The knowledge & professionalism of the representative",                                                                        
  "Please rate your satisfaction with the below areas: - The communication & follow up provided by the team handling your requests",                                                    
  "Please rate your satisfaction with the below areas: - The time it takes for your requests to be resolved",                                                                           
  "Please rate your satisfaction with the below areas: - Our ability to thoroughly & accurately resolve your requests",                                                                 
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Accident Management",                            
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Driver Call Center",                             
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Emergency Roadside services",                    
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Electric Vehicle Support & Charger Installation",
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - EV Charging Card",                               
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Fuel Card management",                           
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Glass Repair or Replacement",                    
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - License Registration & Renewal program",         
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Maintenance program",                            
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - New Vehicle Ordering & Delivery",                
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Personal Mileage Reporting",                     
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Rental Vehicle management",                      
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Safety & MVR program",                           
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Toll & Violation management",                    
  "Next, please rate your satisfaction with the products & services below: \n\nIf you have not utilized a service, please select N/A - Vehicle Turn In process")

### NEW COLUMN NAMES
# Spring and Fall 2024 - New column names
s24f24_new_ratings_names <- c(
  "company_fleet_mgmt_satisfaction",
  "company_vehicle_satisfaction",                     
  "ability_to_handle_requests_online_mobile",
  "ease_of_contacting_wheels",                        
  "rep_knowledge_professionalism",
  "communication_and_follow_up",
  "request_resolution_time",
  "ability_to_thoroughly_accurately_resolve_requests",
  "accident_management",
  "call_center",                                      
  "emergency_roadside",
  "ev_charger_installation",                          
  "fuel_card_services",
  "glass_repair",                                     
  "maintenance",
  "vehicle_ordering",                                 
  "mileage_reporting",
  "registration_and_renewals",                        
  "rental_vehicle_svcs",
  "safety",                                           
  "vehicle_turn_in")

# Spring 2025 new column names
s25_new_ratings_names <- c(
  "company_fleet_mgmt_satisfaction",
  "company_vehicle_satisfaction",                     
  "ability_to_handle_requests_online_mobile",         
  "ease_of_contacting_wheels",                        
  "rep_knowledge_professionalism",                    
  "communication_and_follow_up",                      
  "request_resolution_time",                          
  "ability_to_thoroughly_accurately_resolve_requests", 
  "accident_management",
  "call_center",
  "emergency_roadside",
  "ev_charger_support_installation",
  "ev_charger_card",
  "fuel_card_mgmt",
  "glass_repair_replacement",
  "license_registration_renewals",
  "maintenance_program",
  "new_vehicle_ordering_delivery",
  "personal_mileage_reporting",
  "rental_vehicle_mgmt",
  "safety_mvr_program",
  "toll_violation_mgmt",
  "vehicle_turn_in_process")

### VARIABLE SETS COLUMN NAMES
# Core mental model - same regardless of year
core_mental_model_cols <- c(
  "company_fleet_mgmt_satisfaction",
  "company_vehicle_satisfaction",                     
  "ability_to_handle_requests_online_mobile",         
  "ease_of_contacting_wheels",                        
  "rep_knowledge_professionalism",                    
  "communication_and_follow_up",                      
  "request_resolution_time",                          
  "ability_to_thoroughly_accurately_resolve_requests")

# Spring and Fall 2024 product column names
base_prod_cols <- c(
  "accident_management",
  "call_center",                                      
  "emergency_roadside",
  "ev_charger_installation",                          
  "fuel_card_services",
  "glass_repair",                                     
  "maintenance",
  "vehicle_ordering",                                 
  "mileage_reporting",
  "registration_and_renewals",                        
  "rental_vehicle_svcs",
  "safety",                                           
  "vehicle_turn_in")

# Spring 2025 column names
s25_prod_cols <- c(
  "accident_management",
  "call_center",
  "emergency_roadside",
  "ev_charger_support_installation",
  "ev_charger_card",
  "fuel_card_mgmt",
  "glass_repair_replacement",
  "license_registration_renewals",
  "maintenance_program",
  "new_vehicle_ordering_delivery",
  "personal_mileage_reporting",
  "rental_vehicle_mgmt",
  "safety_mvr_program",
  "toll_violation_mgmt",
  "vehicle_turn_in_process")





#########################################################################
#                   BASIC DATAFRAME FUNCTIONALITY
#
# Forms the dataframes for consistency in analysis.
#########################################################################

#' Function to subset columns within a data set and rename them.
#' 
#' @description
#' make_dataset_match subsets a dataframe via user-specified columns. 
#' It then renames those columns using a collection of names specified by a user.
#' Finally, it returns the data set as a dataframe.
#' 
#' @details
#' This is a function meant to be used as part of a broader data cleaning and preparation process.
#' It should be used with ratings_original_names and s25_original_names as the current_col_names arguments 
#' and s24f24_new_ratings_names and s25_new_ratings_names for new column names
#' 
#' @param df the dataframe meant to be subsetted according to current_col_names and then have its columns renamed via new_col_names.
#' @param current_col_names a vector or list of column names in df that df should be subsetted with. These will be renamed via new_col_names.
#' @param new_col_names a vector or list of new names that will replace the column names in the subsetted dataframe.
#' 
#' @returns A dataframe containing only the renamed columns.
make_dataset_match <- function(df, current_col_names, new_col_names) {
  # Subset the dataframe to the relevant columns
    df2 <- df[,current_col_names, drop = FALSE]
    # Rename all columns if same number of columns
    if (length(colnames(df2)) == length(new_col_names)) {
      colnames(df2) <- new_col_names
      # Return the dataframe - with new column names if successful, otherwise just truncated
      return (df2)
    }
    else {
      # Alert the user to an issue
      stop("There is a mismatch between the actual number of columns and the expected number of columns. Unable to return a final dataframe.")
    }
  }





#########################################################################
#                         FEATURE ENGINEERING

# Functions that create simple additional features for use in networks.
#########################################################################

#' Function to create response indicators per product - used specifically for the MGM networks
#' 
#' @description
#' A function to create 2-level response indicators for a specified set of variables.
#' 0 = no response
#' 1 = response
#' The new variables will also be prefixed with use_ to indicate their purpose.
#' This is meant for use with the Mixed Graphical Model networks to create use indicator variables.
#' 
#' @param df a dataframe containing variables from which the indicator variables will be derived.
#' @param col_names a vector or list of column names to derive the indicator variables from.
#' 
#' @returns an updated dataframe with the indicator columns attached at the end of it.
make_response_indicators <- function(
    df, 
    col_names
    ) {
  # Find missing col names
  missing <- setdiff(col_names, names(df))
  # If there is anything in missing, stop 
  if (length(missing > 0)) {
    stop("These columns are missing from df: ", paste(missing, collapse=", "))
  }
  # Iterate col_names
  for (col in col_names) {
    # Set the new column name
    new_col_name <- paste0("use_",col)
    # Create the new column
    df[[new_col_name]] <- as.integer(!is.na(df[[col]]))
  }
  # Return df
  return(df)
}





#########################################################################
#                       DEDUPLICATION, MATCHING, ETC
#
# Functions that find overlap and take those rows out of data sets.
#########################################################################

#' Removes repeat participants across later survey waves
#' 
#' @description
#' removes repeat participants based on preceding waves and then generates a
#' new dataframe.
#' 
#' @param data the dataframe to clean.
#' @param match_df the dataframe that contains the repeat participant list keyed by index.
#' @param row_idx_col the column to look at containing the matching row indices.
#' 
#' @returns a dataframe without the repeat participants.
#' 
find_matches <- function(
    data,
    match_df,
    row_idx_col
) {
  # Subset nrow based on row_idx_col
  clean_data <- data[-match_df[[row_idx_col]],]
  
  # Return
  return(clean_data)
}





#########################################################################
#                   WAVE DATA + PREPARATION FUNCTIONS
#
# Constructs the dataframes necessary for each part of an analysis.
# Different network models will use different sets of nodes.
# GGM networks only need to customers service satisfaction nodes.
# MGM networks will need customer service + service/product satisfaction 
# nodes.
# BGGM will need at least the customer service satisfaction nodes. 
#########################################################################

#' Function for preparing a dataframe
#' 
#' @description
#' prepare_data is a function for shaping out a dataframe as needed based on columns.
#' 
#' @details
#' This function is meant to be used as part of a broader data cleaning and preparation.
#' process.
#' 
#' @param df a dataframe to use in analysis.
#' @param wave a string indicating which survey wave is being prepared. Must choose from one of 3 options provided. 
#' @param base_col_names a vector or list of column names that might be the baseline.
#' @param new_col_names a vector or list of new column names.
#' @param cluster_col_name a name of the clustering results column.
#' @param prod_col_names a vector or list of service/product-only satisfaction columns.
#' @param s24_f24_matches a dataframe used to remove repeat participants between waves.
#' @param s24_s25_matches a dataframe used to remove repeat participants between waves.
#' @param f24_s25_matches a dataframe used to remove repeat participants between waves.
#' @param row_idx_cols the row indices to focus on during removal
#' 
#' @returns a dataframe
prepare_dataframe <- function(
    df,
    wave = NULL,
    base_col_names,
    new_col_names,
    cluster_col_name,
    prod_col_names,
    s24_f24_matches = NULL,
    s24_s25_matches = NULL,
    f24_s25_matches = NULL,
    row_idx_cols = c(f24="f24_row_idx", s25="s25_row_idx")
) {
  # Build the full list of column names for base and new
  full_base_list = append(base_col_names, cluster_col_name, after = length(base_col_names))
  full_new_list = append(new_col_names, cluster_col_name, after = length(new_col_names))
  
  # If columns are already in the new schema, do not rename
  if (all(full_new_list %in% names(df))) {
    new_df <-df
  }
  # Check column names in the dataframe
  else if (all(full_base_list %in% colnames(df))) {
    # Call make_dataset_match
    new_df <- make_dataset_match(
      df, 
      base_col_names,
      new_col_names
    )
    # Add back cluster_col_name 
    new_df[[cluster_col_name]] <- df[[cluster_col_name]]
  } 
  else {
    # There is likely an error. Do a hard stop and force the user to investigate.
    stop("There is no match in column names or any of the lists you provided. Check your dataframe to ensure you passed the correct one.")
  }
  
  # Call make_response_indicators
  prod_present <- intersect(prod_col_names, names(new_df))
  new_df <- make_response_indicators(new_df, prod_present)
  
  
  # ----- Repeat participant removal by wave - Debugging help from Gen AI -----
  if (!is.null(wave) && identical(wave, "Fall 2024")) {
    
    if (is.null(s24_f24_matches)) stop("Fall 2024 requires s24_f24_matches.")
    new_df <- find_matches(new_df, s24_f24_matches, row_idx_cols[["f24"]])
    
  } else if (!is.null(wave) && identical(wave, "Spring 2025")) {
    
    if (is.null(s24_s25_matches) || is.null(f24_s25_matches)) {
      stop("Spring 2025 requires s24_s25_matches and f24_s25_matches.")
    }
    new_df <- find_matches(new_df, s24_s25_matches, row_idx_cols[["s25"]])
    new_df <- find_matches(new_df, f24_s25_matches, row_idx_cols[["s25"]])
    
  }
  # Return the dataframe
  return(new_df)
}



#' Function for preparing a dataframe
#' 
#' @description
#' prepare_core_data is a function for shaping out a dataframe as needed based on core mental columns.
#' 
#' @details
#' This function is meant to be used as part of a broader data cleaning and preparation.
#' process. In particular, it filters a data set down to just the core mental model variables using core_mental_model_cols.
#' 
#' @param full_df the full dataframe for use in analysis. (The output of prepare_dataframe)
#' @param core_col_names a vector or list of service/product-only satisfaction columns.
#' 
#' @returns a dataframe
prepare_core_data <- function(
    full_df,
    core_col_names
  ) {
      # Subset the dataframe
      core_df <- full_df[,core_col_names]
      # Return
      return(core_df)
}



#' Function for preparing a dataframe
#' 
#' @description
#' prepare_persona_data is a function for shaping out a dataframe as needed based on persona group membership.
#' 
#' @details
#' This function is meant to be used as part of a broader data cleaning and preparation.
#' process. In particular, it filters a data set based on persona group membership.
#' 
#' @param full_df the full dataframe for use in analysis. (The output of prepare_dataframe.)
#' @param persona a value indicating a specific persona group membership.
#' @param persona_col_name name of column to filter on
#' @param subset_cols a vector or list of columns for further subsetting a dataframe.
#' 
#' @returns a dataframe
prepare_persona_data <- function(
    full_df,
    persona,
    persona_col_name,
    subset_cols
) {
  # Filter & subset the dataframe
  persona_df <- full_df %>% filter(
    .data[[persona_col_name]] == persona #uses tidy-eval
  ) %>% select(
    all_of(subset_cols)
  )
  # Return
  return(persona_df)
}



