# function for RADAR plot
RADAR_V2 <- function(ground_truth = NULL, predicted_list = NULL, 
                  matrix_labels = NULL,
                  zero_diag = TRUE, show_legend = TRUE, colors = NULL,
                  legend_pos = "topright", legend_cex = 0.9, legend_inset = 0.03,
                  stats_df = NULL,
                  metrics = c("TPR", "SPEC", "PREC", "F1", "B.A.")) {
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("fmsb", quietly = TRUE)  # fmsb for radar plot
  requireNamespace("viridis", quietly = TRUE) 
  # (you are using viridisLite::viridis below, so make sure viridisLite is installed)
  
  ## ------------------------------------------------------------------
  ## MODE 1: use precomputed stats (e.g. mean_mat or mean_mat_sub)
  ## ------------------------------------------------------------------
  if (!is.null(stats_df)) {
    df <- as.data.frame(stats_df)
    
    # Determine labels for each row
    if ("Predicted_Matrix" %in% names(df)) {
      labels <- as.character(df$Predicted_Matrix)
    } else if (!is.null(matrix_labels) && length(matrix_labels) == nrow(df)) {
      labels <- matrix_labels
    } else if (!is.null(rownames(df)) && all(nzchar(rownames(df)))) {
      labels <- rownames(df)
    } else {
      labels <- paste("Matrix", seq_len(nrow(df)))
    }
    
    # Ensure we have a Predicted_Matrix column for plotting
    df$Predicted_Matrix <- labels
    
    # Which metrics to actually use (intersection with columns of df)
    metrics_used <- metrics[metrics %in% colnames(df)]
    if (length(metrics_used) == 0) {
      stop("None of the requested metrics found in stats_df.")
    }
    
    # Data used for radar: one row per method, selected metrics
    radar_data <- df[, c("Predicted_Matrix", metrics_used), drop = FALSE]
    
    # Scale structure for fmsb: first row = max, second = min, then data
    max_vals <- rep(1, length(metrics_used))  # assuming metrics are in [0, 1]
    min_vals <- rep(0, length(metrics_used))
    radar_data_scaled <- rbind(max_vals, min_vals, radar_data[, -1, drop = FALSE])
    
    num_matrices_plot <- nrow(radar_data)
    if (is.null(colors)) colors <- viridisLite::viridis(num_matrices_plot)
    
    par(mar = c(2, 2, 2, 2))
    fmsb::radarchart(
      radar_data_scaled, 
      axistype = 2, 
      pcol = colors,
      pfcol = NA, 
      plwd = 2, 
      plty = 1,
      cglcol = "grey", cglty = 1, axislabcol = "black",
      caxislabels = seq(0, 1, 0.2),
      vlcex = 1.1 
    )
    
    if (show_legend) {
      legend(legend_pos,
             legend = radar_data$Predicted_Matrix,
             col = colors, lty = 1, lwd = 2,
             bty = "n", cex = legend_cex, inset = legend_inset)
    }
    
    # Return the stats (with Predicted_Matrix column attached)
    out_stats <- df[, c("Predicted_Matrix", setdiff(colnames(df), "Predicted_Matrix")), drop = FALSE]
    return(list(Statistics = out_stats))
  }
  
  ## ------------------------------------------------------------------
  ## MODE 2: ORIGINAL BEHAVIOUR (ground truth + predicted matrices)
  ## ------------------------------------------------------------------
  if (is.null(ground_truth) || is.null(predicted_list)) {
    stop("Either provide stats_df, or both ground_truth and predicted_list.")
  }
  
  ground_truth <- as.matrix(ground_truth)
  if (zero_diag) diag(ground_truth) <- 0  
  
  n_pred <- length(predicted_list)
  default_labels <- paste("Matrix", seq_len(n_pred))
  candidate_labels <- if (!is.null(matrix_labels)) {
    matrix_labels
  } else if (!is.null(names(predicted_list)) && any(nzchar(names(predicted_list)))) {
    names(predicted_list)
  } else {
    default_labels
  }
  
  if (length(candidate_labels) != n_pred) {
    warning("`matrix_labels` non ha la stessa lunghezza di `predicted_list`. Uso etichette di default.")
    candidate_labels <- default_labels
  }
  candidate_labels[!nzchar(candidate_labels)] <- default_labels[!nzchar(candidate_labels)]
  
  all_matrices <- c(list(ground_truth), predicted_list)
  num_matrices <- length(all_matrices)
  
  stats_df <- data.frame(
    Predicted_Matrix = c("Ground Truth", paste("Matrix", seq_along(predicted_list))),
    TP = integer(num_matrices),
    TN = integer(num_matrices),
    FP = integer(num_matrices),
    FN = integer(num_matrices),
    TPR = numeric(num_matrices),
    FPR = numeric(num_matrices),
    SPEC = numeric(num_matrices),
    PREC = numeric(num_matrices),
    F1 = numeric(num_matrices),
    B.A. = numeric(num_matrices),
    MCC = numeric(num_matrices),
    stringsAsFactors = FALSE
  )
  
  get_upper_tri <- function(mat) {
    mat[upper.tri(mat)]
  }
  
  ground_truth_upper <- as.numeric(get_upper_tri(ground_truth) > 0)
  
  stats_df[-1, ] <- do.call(rbind, lapply(seq_along(predicted_list), function(i) {
    matrix_i <- as.matrix(predicted_list[[i]])
    binary_i <- as.numeric(get_upper_tri(matrix_i) > 0)
    
    TP <- sum(binary_i == 1 & ground_truth_upper == 1)
    TN <- sum(binary_i == 0 & ground_truth_upper == 0)
    FP <- sum(binary_i == 1 & ground_truth_upper == 0)
    FN <- sum(binary_i == 0 & ground_truth_upper == 1)
    
    TPR <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    FPR <- ifelse((FP + TN) > 0, FP / (FP + TN), 0)
    SPEC <- 1 - FPR
    PREC <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
    F1 <- ifelse((PREC + TPR) > 0, 2 * (PREC * TPR) / (PREC + TPR), 0)
    B.A. <- (2 * TP * TN + TP * FP + TN * FN) / (2 * (TP + FN) * (TN + FP))
    denominator <- sqrt(as.numeric(TP + FP) * as.numeric(TP + FN) *
                          as.numeric(TN + FP) * as.numeric(TN + FN))
    MCC <- ifelse(denominator > 0, (TP * TN - FP * FN) / denominator, 0)
    
    c(Predicted_Matrix = paste("Matrix", i), TP, TN, FP, FN, TPR, FPR, SPEC, PREC, F1, B.A., MCC)
  }))
  
  stats_df$Predicted_Matrix[-1] <- candidate_labels
  
  stats_df[, 2:ncol(stats_df)] <- lapply(stats_df[, 2:ncol(stats_df)], as.numeric)
  stats_df_filtered <- stats_df[-1, ]
  
  # Use same metrics logic as in stats_df mode
  metrics_used <- metrics[metrics %in% colnames(stats_df_filtered)]
  if (length(metrics_used) == 0) {
    stop("None of the requested metrics found in computed statistics.")
  }
  
  radar_data <- stats_df_filtered %>%
    dplyr::select(Predicted_Matrix, dplyr::all_of(metrics_used))
  
  max_vals <- rep(1, length(metrics_used))  # Max values set to 1
  min_vals <- rep(0, length(metrics_used))  # Min values set to 0
  
  radar_data_scaled <- rbind(max_vals, min_vals, radar_data[, -1, drop = FALSE])
  
  num_matrices_plot <- nrow(stats_df_filtered)
  if (is.null(colors)) colors <- viridisLite::viridis(num_matrices_plot)
  
  par(mar = c(2, 2, 2, 2))
  fmsb::radarchart(
    radar_data_scaled, 
    axistype = 2, 
    pcol = colors,
    pfcol = NA, 
    plwd = 2, 
    plty = 1, 
    cglcol = "grey", cglty = 1, axislabcol = "black",
    caxislabels = seq(0, 1, 0.2),
    vlcex = 1.1 
  )
  if (show_legend) {
    legend(legend_pos,
           legend = radar_data$Predicted_Matrix,
           col = colors, lty = 1, lwd = 2,
           bty = "n", cex = legend_cex, inset = legend_inset)
  }
  
  return(list(Statistics = stats_df_filtered))
}
