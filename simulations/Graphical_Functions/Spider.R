# function for RADAR plot
spider <- function(ground_truth, predicted_list, matrix_labels = NULL,
                  zero_diag = TRUE, show_legend = TRUE, colors = NULL,
                  legend_pos = "topright", legend_cex = 0.9, legend_inset = 0.03,
                  single_color_multi_lty = FALSE) {
  
  requireNamespace("dplyr", quietly = TRUE)
  requireNamespace("tidyr", quietly = TRUE)
  requireNamespace("fmsb", quietly = TRUE)  # fmsb for radar plot
  requireNamespace("viridis", quietly = TRUE) 
  # (you already used viridisLite::viridis below)
  
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
    B.A. <- (2*TP*TN +TP*FP + TN*FN)/(2*(TP + FN)*(TN + FP))
    denominator <- sqrt(as.numeric(TP + FP) * as.numeric(TP + FN) *
                          as.numeric(TN + FP) * as.numeric(TN + FN))
    MCC <- ifelse(denominator > 0, (TP * TN - FP * FN) / denominator, 0)
    
    c(Predicted_Matrix = paste("Matrix", i), TP, TN, FP, FN, TPR, FPR, SPEC, PREC, F1, B.A., MCC)
  }))
  
  stats_df$Predicted_Matrix[-1] <- candidate_labels
  
  stats_df[, 2:ncol(stats_df)] <- lapply(stats_df[, 2:ncol(stats_df)], as.numeric)
  stats_df_filtered <- stats_df[-1, ]
  
  metrics <- c("TPR", "SPEC", "PREC", "F1", "B.A.")
  radar_data <- stats_df_filtered %>%
    dplyr::select(Predicted_Matrix, dplyr::all_of(metrics))
  
  max_vals <- rep(1, length(metrics))  # Max values set to 1
  min_vals <- rep(0, length(metrics))  # Min values set to 0
  
  radar_data_scaled <- rbind(max_vals, min_vals, radar_data[, -1])
  
  num_matrices_plot <- nrow(stats_df_filtered)
  
  if (is.null(colors)) {
    colors <- viridisLite::viridis(num_matrices_plot)
  }
  
  # NEW: define line types and possibly override colors when requested
  if (single_color_multi_lty) {
    # force a single color (black) and different lty per matrix
    colors <- rep("black", num_matrices_plot)
    line_types <- rep(1:6, length.out = num_matrices_plot)  # 1=solid, 2=dashed, 3=dotted, etc.
  } else {
    # original behavior: all solid lines
    line_types <- rep(1, num_matrices_plot)
  }
  
  par(mar = c(2,2,2,2))
  fmsb::radarchart(
    radar_data_scaled, 
    axistype = 2, 
    pcol = colors,
    pfcol = NA, 
    plwd = 2, 
    plty = line_types,
    #title = "Pentagon Spider Chart of Performance Metrics",
    cglcol = "grey", cglty = 1, axislabcol = "black",
    caxislabels = seq(0, 1, 0.2),
    vlcex = 1.1 
  )
  if (show_legend) {
    legend(legend_pos,
           legend = radar_data$Predicted_Matrix,
           col = colors,
           lty = line_types,
           lwd = 2,
           bty = "n", cex = legend_cex, inset = legend_inset)
  }
  
  return(list(Statistics = stats_df_filtered))
}
