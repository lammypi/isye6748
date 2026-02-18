########## NETWORKS.R ##########
#### DESC: Build the GGM networks for psychometric analysis.
####       - Includes metrics, bootstrapping, and plots.
####       - For the GA Tech Applied Analytics Practicum (ISYE 6748).
#### AUTH: Leslie A. McFarlin, Principal UX Architect.

# Load in setup.R
source("./setup.R")



#########################################################################
#                       WRAPPER FUNCTIONS

# These functions wrap around core functions for PNA: 
# - bootnet::estimateNetwork()
# - Assist with post-network analysis
#########################################################################

#' Build a GGM network
#' 
#' @description
#' Wrapper function on bootnet::estimateNetwork() to build a GGM
#' estimated via EBICglasso. It can accommodate custom node labels as needed.
#' 
#' @param data dataframe containing the variables for the network.
#' 
#' @returns a bootnet object containing a graph and its relevant details.
ggm_network_builder <- function (
    data
) {
  # Call bootnet::estimateNetwork()
  network <- estimateNetwork(
    data,
    default="EBICglasso",
    corMethod="cor_auto", # Will automatically use the best correlation 
    missing="listwise",   # Drop missing if full pair is empty.
  )
  
  # Return the network object
  return(network)
}



#' Plot a network
#' 
#' @description
#' Wrapper around qgraph::plot() to visualize a GGM network.
#' 
#' @param network the network object estimated from bootnet::estimateNetwork().
#' @param node_labels (optional) vector or list of user-specified node labels.
#' @param file_name string of file path + name.
#' 
#' @returns a file of the network visual.
ggm_plot_network <- function(
    network,
    node_labels = NULL,
    file
) {
  # Specifics of a PNG file
  grDevices::png(file_name, width=1400, height = 1000, res = 200)
  on.exit(grDevices::dev.off(), add = TRUE) # Closes out the renderer
  
  # Visualize
  if (length(node_labels) >0 ) {
    plot(network, 
         edge.labels=TRUE,
         labels=node_labels
         )
  } 
  else {
    plot(network, edge.labels=TRUE)
  }

  # Return
  file
}
 


# ---------- CENTRALITY, EI, PREDICTABILITY ----------
#' Gets the centrality values from a GGM network.
#' 
#' @description
#' Wrapper around qgraph::centrality.
#' 
#' @param network the network object from bootnet::estimateNetwork().
#' 
#' @return list of centrality and other relevant values as specified in arguments.
get_centrality <- function(
    network
) {
  # Get the table of centrality values
  cent <- qgraph::centrality(
    network$graph, # Isolate the graph object
    R2 = TRUE # Gets predictability. Access via cent$R2.
  )
}





# ---------- GGM BOOTSTRAPPING FUNCTIONS ----------
#' Perform bootstrapping with visualization
#' 
#' @description
#' A generic bootstrapping function as a wrapper on bootnet::bootnet() that is 
#' included as part of an omnibus bootstrapping evaluation function. It can be 
#' used for edge weights, strength centrality, and expected influence.
#' 
#' @details
#' When setting a statistic, be mindful of the type you've chosen.
#' - edge weights are type = "nonparametric".
#' - strength, expected influence = "case".
#' 
#' 
#' @param graph the network being evaluated.
#' @param statistic the network statistic to evaluate: edge, strength, or expectedInfluence
#' @param type method for bootstrapping.
#' @param nBoots number of bootstrapping samples.
#' 
#' @returns list of bootstrapped objects.
run_bootstrapping <- function(
    graph,
    statistic,
    type,
    nBoots=1000
  ) {
  # Edge weights
  boots <- bootnet::bootnet(graph, nBoots=nBoots, type=type, statistics=statistic, verbose=FALSE)
  
  # For strength or expected influence, also run corStability
  if ((statistic=="strength") | (statistic=="expectedInfluence")) {
    stability <- corStability(boots, statistics=statistic, verbose=FALSE)
    # Return
    return (
      # Return boots
      list(
        boots = boots,
        cor_stability = stability
      )
    )
  }
  # edge
  else if(statistic=="edge") {
    # Return
    return (
      # Return boots
      list(
        boots = boots
      )
    )
  }
  # Stop
  else {
    stop("Unrecognized statistic provided. Please use one of edge, strength, or expectedInfluence.")
  }
  
}

#' Get the bootstrapping results table
#' 
#' @description
#' Access the CI range columns to assist with interpretation of stability.
#' 
#' @details
#' Remember these are regularized edge weights if using statistics="edge". 
#' Don't interpret these as statistical significance indicators, rather as stability 
#' on a range of more to less stable. Sign can also be interpreted for directionality.
#' of relationship.
#' 
#' @param boots bootstrap object from run_bootstrapping.
#' @param level confidence interval range level
#' 
#' @returns the bootstrap summary dataframe filtered down to the CI range columns.
#' 
get_bootstrap_tbl <- function(boots, level = 0.95) {
  # Gen AI help - summary(boots) was triggering a bootnet bug
  # Grab the bootTable object
  bt <- boots$bootTable
  
  # Handle missing/empty - sometimes this happens
  if (is.null(bt) || nrow(bt) == 0L) {
    return(tibble::tibble(id = character(), CIlower = numeric(), CIupper = numeric(), prop0 = numeric()))
  }
  
  # Guard rails in case value is missing or not a number.
  if (!"value" %in% names(bt)) {
    stop("bootnet::bootTable() did not return a `value` column. Available: ",
         paste(names(bt), collapse = ", "))
  }
  
  # Manual calculation for CIs
  alpha <- 1 - level
  lo <- alpha / 2
  hi <- 1 - (alpha / 2)
  
  # Stable ID across edge/node stats
  if ("id" %in% names(bt)) {
    bt$id <- as.character(bt$id)
  } else if (all(c("node1", "node2") %in% names(bt))) {
    bt$id <- paste0(bt$node1, "--", bt$node2)
  } else if ("node" %in% names(bt)) {
    bt$id <- as.character(bt$node)
  } else if ("nNode" %in% names(bt)) {
    # common in bootnet centrality tables
    bt$id <- as.character(bt$nNode)
  } else {
    stop("Could not construct `id`. bootTable columns are: ", paste(names(bt), collapse = ", "))
  }
  
  bt$value <- suppressWarnings(as.numeric(bt$value))
  
  # Output to return via bt
  bt |>
    dplyr::filter(!is.na(.data$id)) |>
    dplyr::group_by(.data$id) |>
    dplyr::summarise(
      CIlower = if (all(is.na(.data$value))) NA_real_
      else stats::quantile(.data$value, probs = lo, na.rm = TRUE, names = FALSE),
      CIupper = if (all(is.na(.data$value))) NA_real_
      else stats::quantile(.data$value, probs = hi, na.rm = TRUE, names = FALSE),
      prop0   = if (all(is.na(.data$value))) NA_real_
      else mean(.data$value == 0, na.rm = TRUE),
      .groups = "drop"
    )
}



#' Omnibus bootstrapping function
#' 
#' @description
#' Returns a collection of relevant bootstrapping objects for interpretation.
#' 
#' @param graph the network being evaluated.
#' @param statistic the network statistic to evaluate: edge, strength, or expectedInfluence
#' @param type method for bootstrapping.
#' @param nBoots number of bootstrapping samples.
#' 
#' @returns all bootstrapping generated by bootnet::bootnet().
omnibus_bootstrap_analysis <- function(
    graph,
    statistic,
    type,
    nBoots=1000
    ) {
  # Conduct the bootstrap analysis - Bootstrapping, plot, centrality stability is optional
  results <- run_bootstrapping(graph, statistic, type, nBoots)
  
  # Returns everything to use it downstream as needed
  list(
    boots = results$boots,
    bootstrap_tbl = get_bootstrap_tbl(results$boots),
    cor_stability = results$cor_stability
  )
}





#########################################################################
#                       DIFFERENCE TESTS

# These functions perform calculations to: 
# - Detect differences within a graph
# - Determine if two graphs are different based on a specified feature
#########################################################################
#' Detect within-graph edge weight differences
#' 
#' @description
#' Conducts a difference test of edge weights within a graph.
#' 
#' @param combo_items the nodes in the graph.
#' @param bootnet_output bootnet::bootnet() boots object.
#' @param alpha alpha level for evaluation
#' 
#' @returns a dataframe containing difference test results
edgeweight_diff_test <- function(
    combo_items,
    bootnet_output,
    alpha = 0.05
    ) {
  # All combinations of nodes - creates a matrix 2xE of characters
  node_combos <- combn(combo_items, 2)
  E = ncol(node_combos)
  
  # All combinations of edges: 2 x choose(E,2)
  edgepair_idx <- combn(E,2)
  P <- ncol(edgepair_idx)
  
  # Alpha level
  alpha_level <- alpha / P
  
  # Edgeweight difference test results
  diff_tests_df <- data.frame(
    id1 = character(),
    id2 = character(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    significant = logical()
  )
  
  # Iterate node_combos into a list of (node_combo[1,x], node_combo[2,x])
  for (n in seq_len(P)) {
    # Edge pairs
    i <- edgepair_idx[1, n] # First edge idx
    j <- edgepair_idx[2, n] # Second edge idx
    
    # Set the nodes
    u1 = node_combos[1,i]    # Pair 1
    v1 = node_combos[2,i]    # Pair 1
    u2 = node_combos[1,j]  # Pair 2
    v2 = node_combos[2,j]  # Pair 2
    
    # Perform the edge difference test
    diff_test <- differenceTest(
      bootnet_output,
      x=u1,
      x2=v1,
      y=u2,
      y2=v2,
      measure="edge",
      alpha=alpha_level,
      verbose=FALSE
    )
    
    # Add new row from diff_test
    new_df <- data.frame(
      id1 = diff_test['id1'],
      id2 = diff_test['id2'],
      ci_lower = diff_test['lower'],
      ci_upper = diff_test['upper'],
      significant = diff_test['significant']
    )
    # Bind to diff_tests_df
    diff_tests_df <- rbind(diff_tests_df, new_df)
  }
  # Count how many statistically significant
  n_sig_comparisons = nrow(filter(diff_tests_df, significant == TRUE))
  
  # Output of results
  print(paste0("At significance level ", alpha_level, ", there are ", n_sig_comparisons," significant edge weight comparisons."))
  
  # Return diff_tests_df
  return(diff_tests_df)
}



#' Measure (non-edge weight) difference tests
#' 
#' @description
#' Measures differences for a specified metric within a network.
#' 
#' @param combo_items the nodes in the graph.
#' @param bootnet_output bootnet::bootnet() boots object.
#' @param measure description
#' @param alpha alpha level for evaluation
#' 
#' @returns a dataframe containing difference test results 
measures_diff_test <- function(
    combo_items,
    bootnet_output,
    measure,
    alpha = 0.05  
  ) {
  # All combinations of nodes - creates a matrix 2xE of characters
  node_combos <- combn(combo_items, 2)
  # Number of node combinations to test
  N = ncol(node_combos) 
  
  # Alpha level
  alpha_level <- alpha / N
  
  # Edgeweight difference test results
  diff_tests_df <- data.frame(
    id1 = character(),
    id2 = character(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    significant = logical()
  )
  
  # Iterate node_combos into a list of (node_combo[1,x], node_combo[2,x])
  for (n in seq_len(N)) {
    # Set the nodes
    u = node_combos[1,n]    # Pair 1
    v = node_combos[2,n]    # Pair 1
    
    # Perform the edge difference test
    diff_test <- differenceTest(
      bootnet_output,
      u,
      v,
      measure=measure,
      alpha=alpha_level,
      verbose=FALSE
    )
    
    # Add new row from diff_test
    new_df <- data.frame(
      id1 = diff_test['id1'],
      id2 = diff_test['id2'],
      ci_lower = diff_test['lower'],
      ci_upper = diff_test['upper'],
      significant = diff_test['significant']
    )
    # Bind to diff_tests_df
    diff_tests_df <- rbind(diff_tests_df, new_df)
  }
  # Count how many statistically significant
  n_sig_comparisons = nrow(filter(diff_tests_df, significant == TRUE))
  
  # Output of results
  print(paste0(measure, ": at significance level ", alpha_level, ", there are ", n_sig_comparisons," significant comparisons."))
  
  # Return diff_tests_df
  return(diff_tests_df)
}



#' Network Comparison Test
#' 
#' @description
#' A wrapper for NetworkComparisonTest::NCT with additional outputs providing 
#' insights into edge differences and overall network strength.
#' 
#' @note
#' Use of this functions introduces a multiplicity problem around using the appropriate alpha levels.
#' For the tests within this function, set an alpha level of 0.05 or 0.01 and allow p-method to control
#' that for each individual test. Judge M and S values at a controlled error rate based on the number 
#' of networks being compared.If you are comparison 3 networks, then the M and S values would be judged 
#' at 0.05/3. However, within each comparison, individual tests will be judged at the 0.05 level and 
#' adjusted according to the selected method.
#' 
#' 
#' @param df1,df2 dataframes for networks to compare.
#' @param nw1_label,nw2_label labels to identify each network.
#' @param p_method the method for family-wise error rate control. Recommended to use Benjamini-Hochberg.
#' @param alpha overall alpha level that need to control to.
#' 
compare_networks <- function(
    df1,
    df2,
    nw1_label,
    nw2_label,
    p_method,
    alpha = 0.05
    ) {
  ### CHECK VARIABLE NAMES
  vars <- colnames(df1)
  stopifnot(identical(colnames(df1), colnames(df2))) # Check if they're identical, fails if not
  
  # Set seed locally to stop RNG depletion
  set.seed(42)
  
  ### DROP ROWS MISSING 1 OR MORE RESPONSES - BRING THIS INSIDE NOW IN PIPELINE
  data_sanitizer <- function(
    df1,
    df2
  ) {
    # Align the columns
    cols <- intersect(names(df1), names(df2))
    df1 <- df1[, cols, drop=FALSE]
    df2 <- df2[, cols, drop=FALSE]
    
    # Retain only completed cases
    df1 <- df1[complete.cases(df1), , drop=FALSE]
    df2 <- df2[complete.cases(df2), , drop=FALSE]
    
    # Drop any inf
    df1 <- df1[apply(df1, 1, function(r) all(is.finite(r))), , drop=FALSE]
    df2 <- df2[apply(df2, 1, function(r) all(is.finite(r))), , drop=FALSE]
    
    # Remove 0 variance columns
    zero_var <- function(x) length(unique(x)) <= 1
    # Set as cols to drop those with 0 variance.
    drop_cols <- union(
      names(which(vapply(df1, zero_var, logical(1)))),
      names(which(vapply(df2, zero_var, logical(1))))
    )
    if (length(drop_cols)) {
      df1 <- df1[, setdiff(names(df1), drop_cols), drop=FALSE]
      df2 <- df2[, setdiff(names(df2), drop_cols), drop=FALSE]
    }
    
    # Cleaned df as lists
    list(
      df1 = df1,
      df2 = df2,
      dropped = drop_cols
    )
  }
  
  ### CLEANED DATA SETS
  cleaned <- data_sanitizer(df1, df2)
  df1 <- cleaned$df1
  df2 <- cleaned$df2
  
  ### CONDUCT THE NCT
  nct_output <- NetworkComparisonTest::NCT(
    data1 = df1,
    data2 = df2,
    it = 1000,
    paired = FALSE,
    binary.data = FALSE,
    test.edges = TRUE,
    edges = "all",
    progressbar = FALSE,
    test.centrality = TRUE,
    centrality="strength",
    p.adjust.methods = p_method,
    verbose=FALSE
  )
  
  # Sets the row and column names for more convenient indexing.
  dimnames(nct_output$nw1) <- list(vars, vars)
  dimnames(nct_output$nw2) <- list(vars, vars)
  
  # ----- Global Test -----
  # M, network invariance test statistic
  m_n1n2 <- nct_output$nwinv.real
  m_pval_n1n2 <- nct_output$nwinv.pval
  
  # S, global strength test statistic
  s_n1n2 <- nct_output$glstrinv.real
  s_pval_n1n2 <- nct_output$glstrinv.pval
  s_group_n1n2 <- nct_output$glstrinv.sep
  s_n1 <- nct_output$glstrinv.sep[1]
  s_n2 <- nct_output$glstrinv.sep[2]
  
  # Symmetric percent difference
  s_pct_diff <- round(100 * (s_n2 - s_n1)/((s_n1+s_n2)/2), 2)
  
  # ----- Edge Differences, E -----
  e_n1n2 <- nct_output$einv.pvals$`Test statistic E`
  e_pval_n1n2 <- nct_output$einv.pvals$`p-value`
  e_sig_diff_n1n2 <- subset(nct_output$einv.pvals,`p-value` < alpha) # Subset to find the edge weights that are different between the two graphs.
  edge_diffs <- list(
    e_n1n2,
    e_pval_n1n2,
    e_sig_diff_n1n2
  )
  
  # REWRITE FROM DEBUGGING
  if (nrow(e_sig_diff_n1n2) > 0) {
    
    e_sig_diff_n1n2$nw1_edge_weight <- NA_real_
    e_sig_diff_n1n2$nw2_edge_weight <- NA_real_
    
    for (r in seq_len(nrow(e_sig_diff_n1n2))) {
      # Nodes
      u <- as.character(e_sig_diff_n1n2[r, "Var1"])
      v <- as.character(e_sig_diff_n1n2[r, "Var2"])
      
      # Safety check in case a node name doesn't exist in the matrices
      if (!(u %in% colnames(nct_output$nw1)) || !(v %in% colnames(nct_output$nw1))) {
        warning(sprintf("Edge (%s, %s) not found in nw1 dimnames", u, v))
        next
      }
      # Edge weights between same 2 nodes in the compared networks.
      e_sig_diff_n1n2$nw1_edge_weight[r] <- nct_output$nw1[u, v]
      e_sig_diff_n1n2$nw2_edge_weight[r] <- nct_output$nw2[u, v]
    }
  }
  
  # Build a summary tibble
  summary <- tibble::tibble(
      nw1 = nw1_label,
      nw2 = nw2_label,
      alpha = alpha,
      p_method = p_method,
      M = m_n1n2,
      M_p = m_pval_n1n2,
      S = s_n1n2,
      S_p = s_pval_n1n2,
      strength_1 = s_n1,
      strength_2 = s_n2,
      strength_pct_diff = s_pct_diff
    )
  
  # Tibble to output with additional information
  output <- list(
    nct = nct_output,
    summary = summary,
    edge_differences = edge_diffs
  )
}
