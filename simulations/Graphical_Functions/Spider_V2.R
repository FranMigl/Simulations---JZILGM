spider_V2 <- function(ground_truth = NULL, predicted_list = NULL,
matrix_labels = NULL, zero_diag = TRUE,
show_legend = TRUE, colors = NULL,
legend_pos = "topright", legend_cex = 0.9, legend_inset = 0.03,
single_color_multi_lty = FALSE,
# NEW: allow precomputed performance table
stats_df = NULL,
metrics = c("TPR","SPEC","PREC","F1","B.A.")) {
  
  requireNamespace("fmsb", quietly = TRUE)
  requireNamespace("viridisLite", quietly = TRUE)
  
  # ----- MODE B: plot from precomputed stats_df -----
  if (!is.null(stats_df)) {
    df <- as.data.frame(stats_df)
    
    # Infer labels
    if ("Predicted_Matrix" %in% names(df)) {
      labels <- as.character(df$Predicted_Matrix)
    } else if (!is.null(rownames(df)) && all(nzchar(rownames(df)))) {
      labels <- rownames(df)
      df$Predicted_Matrix <- labels
    } else if (!is.null(matrix_labels) && length(matrix_labels) == nrow(df)) {
      labels <- matrix_labels
      df$Predicted_Matrix <- labels
    } else {
      labels <- paste("Matrix", seq_len(nrow(df)))
      df$Predicted_Matrix <- labels
    }
    
    keep_metrics <- metrics[metrics %in% colnames(df)]
    if (!length(keep_metrics)) stop("None of the requested metrics are present in stats_df.")
    
    radar_data <- df[, c("Predicted_Matrix", keep_metrics), drop = FALSE]
    max_vals <- rep(1, length(keep_metrics))
    min_vals <- rep(0, length(keep_metrics))
    radar_scaled <- rbind(max_vals, min_vals, radar_data[, -1, drop = FALSE])
    
    nm <- nrow(radar_data)
    if (is.null(colors)) colors <- viridisLite::viridis(nm)
    
    if (isTRUE(single_color_multi_lty)) {
      colors <- rep("black", nm)
      line_types <- rep(1:6, length.out = nm)
    } else {
      line_types <- rep(1, nm)
    }
    
    op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
    par(mar = c(2,2,2,2))
    fmsb::radarchart(
      radar_scaled, axistype = 2,
      pcol = colors, pfcol = NA, plwd = 2, plty = line_types,
      cglcol = "grey", cglty = 1, axislabcol = "black",
      caxislabels = seq(0, 1, 0.2), vlcex = 1.1
    )
    if (show_legend) {
      legend(legend_pos,
             legend = radar_data$Predicted_Matrix,
             col = colors, lty = line_types, lwd = 2,
             bty = "n", cex = legend_cex, inset = legend_inset)
    }
    return(list(Statistics = radar_data))
  }
  
  # ----- MODE A: compute stats from ground_truth + predicted_list -----
  if (is.null(ground_truth) || is.null(predicted_list))
    stop("Provide either `stats_df`, or both `ground_truth` and `predicted_list`.")
  
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
    warning("`matrix_labels` length differs from `predicted_list`. Using defaults where needed.")
    candidate_labels <- default_labels
  }
  candidate_labels[!nzchar(candidate_labels)] <- default_labels[!nzchar(candidate_labels)]
  
  get_upper_tri <- function(mat) mat[upper.tri(mat)]
  ground_truth_upper <- as.numeric(get_upper_tri(ground_truth) > 0)
  
  stats_df <- data.frame(
    Predicted_Matrix = candidate_labels,
    TP = integer(n_pred), TN = integer(n_pred), FP = integer(n_pred), FN = integer(n_pred),
    TPR = numeric(n_pred), FPR = numeric(n_pred), SPEC = numeric(n_pred),
    PREC = numeric(n_pred), F1 = numeric(n_pred), B.A. = numeric(n_pred),
    MCC = numeric(n_pred),
    stringsAsFactors = FALSE
  )
  
  for (i in seq_len(n_pred)) {
    matrix_i <- as.matrix(predicted_list[[i]])
    binary_i <- as.numeric(get_upper_tri(matrix_i) > 0)
    
    TP <- sum(binary_i == 1 & ground_truth_upper == 1)
    TN <- sum(binary_i == 0 & ground_truth_upper == 0)
    FP <- sum(binary_i == 1 & ground_truth_upper == 0)
    FN <- sum(binary_i == 0 & ground_truth_upper == 1)
    
    TPR  <- ifelse((TP + FN) > 0, TP / (TP + FN), 0)
    FPR  <- ifelse((FP + TN) > 0, FP / (FP + TN), 0)
    SPEC <- 1 - FPR
    PREC <- ifelse((TP + FP) > 0, TP / (TP + FP), 0)
    F1   <- ifelse((PREC + TPR) > 0, 2 * (PREC * TPR) / (PREC + TPR), 0)
    B.A. <- (2*TP*TN + TP*FP + TN*FN) / (2*(TP + FN)*(TN + FP))
    
    denom <- sqrt(as.numeric(TP + FP) * as.numeric(TP + FN) *
                    as.numeric(TN + FP) * as.numeric(TN + FN))
    MCC   <- if (denom > 0) (TP * TN - FP * FN) / denom else 0
    
    stats_df[i, c("TP","TN","FP","FN","TPR","FPR","SPEC","PREC","F1","B.A.","MCC")] <-
      c(TP, TN, FP, FN, TPR, FPR, SPEC, PREC, F1, B.A., MCC)
  }
  
  # Radar plot as above
  keep_metrics <- metrics[metrics %in% colnames(stats_df)]
  if (!length(keep_metrics)) stop("Requested metrics not available.")
  
  radar_data <- stats_df[, c("Predicted_Matrix", keep_metrics), drop = FALSE]
  max_vals <- rep(1, length(keep_metrics))
  min_vals <- rep(0, length(keep_metrics))
  radar_data_scaled <- rbind(max_vals, min_vals, radar_data[, -1, drop = FALSE])
  
  nm <- nrow(radar_data)
  if (is.null(colors)) colors <- viridisLite::viridis(nm)
  
  if (isTRUE(single_color_multi_lty)) {
    colors <- rep("black", nm)
    line_types <- rep(1:6, length.out = nm)
  } else {
    line_types <- rep(1, nm)
  }
  
  op <- par(no.readonly = TRUE); on.exit(par(op), add = TRUE)
  par(mar = c(2,2,2,2))
  fmsb::radarchart(
    radar_data_scaled, axistype = 2,
    pcol = colors, pfcol = NA, plwd = 2, plty = line_types,
    cglcol = "grey", cglty = 1, axislabcol = "black",
    caxislabels = seq(0, 1, 0.2), vlcex = 1.1
  )
  if (show_legend) {
    legend(legend_pos,
           legend = radar_data$Predicted_Matrix,
           col = colors, lty = line_types, lwd = 2,
           bty = "n", cex = legend_cex, inset = legend_inset)
  }
  
  return(list(Statistics = stats_df))
}
