# Here we aggregate the results of JZILGM and ZILGM, then we compare them.
                      
                                                  # JZILGM vs ZILGM


# Loading Networks:

net100 <- readRDS("net100.RDS")

net200 <- readRDS("net200.RDS")

net400 <- readRDS("net400.RDS")

# Loading JZILGM results

{
  ######### net100
  
  net100_250_results <- readRDS("res_100_250_0.3.RDS")
  time_net100_250 <- readRDS("time_100_250_0.3.RDS")
  
  net100_500_results <- readRDS("res_100_500_0.3.RDS")
  time_net100_500 <- readRDS("time_100_500_0.3.RDS")
  
  net100_1000_results <- readRDS("res_100_1000_0.3.RDS")
  time_net100_1000 <- readRDS("time_100_1000_0.3.RDS")
  
  ######### net200
  
  net200_250_results <- readRDS("res_200_250_0.3.RDS")
  time_net200_250 <- readRDS("time_200_250_0.3.RDS")
  
  net200_500_results <- readRDS("res_200_500_0.3.RDS")
  time_net200_500 <- readRDS("time_200_500_0.3.RDS")
  
  net200_1000_results <- readRDS("res_200_1000_0.3.RDS")
  time_net200_1000 <- readRDS("time_200_1000_0.3.RDS")
  
  ######### net400
  
  net400_250_results <- readRDS("res_400_250_0.3.RDS")
  time_net400_250 <- readRDS("time_400_250_0.3.RDS")
  
  net400_500_results <- readRDS("res_400_500_0.3.RDS")
  time_net400_500 <- readRDS("time_400_500_0.3.RDS")
  
  net400_1000_results <- readRDS("res_400_1000_0.3.RDS")
  time_net400_1000 <- readRDS("time_400_1000_0.3.RDS")
  
  
}

# Loading ZILGM results (we still have to "late integrate" them)

# p100
Park_p100_n250 <- readRDS("Park_p100_n250.RDS")
time_Park_p100_n250 <- readRDS("ParkTime_p100_n250.RDS")

Park_p100_n500 <- readRDS("Park_p100_n500.RDS")
time_Park_p100_n500 <- readRDS("ParkTime_p100_n500.RDS")

Park_p100_n1000 <- readRDS("Park_p100_n1000.RDS")
time_Park_p100_n1000 <- readRDS("ParkTime_p100_n1000.RDS")

# p200
Park_p200_n250 <- readRDS("Park_p200_n250.RDS")
time_Park_p200_n250 <- readRDS("Park_multi_sim/ParkTime_p200_n250.RDS")

Park_p200_n500 <- readRDS("Park_p200_n500.RDS")
time_Park_p200_n500 <- readRDS("ParkTime_p200_n500.RDS")

Park_p200_n1000 <- readRDS("Park_p200_n1000.RDS")
time_Park_p200_n1000 <- readRDS("ParkTime_p200_n1000.RDS")

# p400 (lacking n1000)
Park_p400_n250 <- readRDS("Park_p400_n250.RDS")
time_Park_p400_n250 <- readRDS("ParkTime_p400_n250.RDS")

Park_p400_n500 <- readRDS("Park_p400_n500.RDS")
time_Park_p400_n500 <- readRDS("ParkTime_p400_n500.RDS")

Park_p400_n1000 <- readRDS("Park_p400_n1000.RDS")
time_Park_p400_n1000 <- readRDS("ParkTime_p400_n1000.RDS")


# brief investigation of the results
sum(Park_p100_n500[[1]][[1]]$network[[1]]!=0)
sum(Park_p100_n500[[1]][[1]]$network[[50]]!=0)
which(Park_p100_n500[[1]][[1]]$lambda == Park_p100_n500[[1]][[1]]$opt_lambda)
# lambda are in decreasing order, the networks are coherently in increasing number of edges order


# Receving lists of best ZILGM networks
Opt_Park_p100_n250 <- list()
Opt_Park_p100_n500 <- list()
Opt_Park_p100_n1000 <- list()

Opt_Park_p200_n250 <- list()
Opt_Park_p200_n500 <- list()
Opt_Park_p200_n1000 <- list()

Opt_Park_p400_n250 <- list()
Opt_Park_p400_n500 <- list()
Opt_Park_p400_n1000 <- list()

for (j in 1:length(Park_p400_n1000)){
  
  Opt_Park_p100_n250[[j]] <- list()
  Opt_Park_p100_n500[[j]] <- list()
  Opt_Park_p100_n1000[[j]] <- list()
  #
  Opt_Park_p200_n250[[j]] <- list()
  Opt_Park_p200_n500[[j]] <- list()
  Opt_Park_p200_n1000[[j]] <- list()
  #
  Opt_Park_p400_n250[[j]] <- list()
  Opt_Park_p400_n500[[j]] <- list()
  Opt_Park_p400_n1000[[j]] <- list()
  
  
  for (i in 1:length(Park_p400_n1000[[j]])){
    Opt_Park_p100_n250[[j]][[i]] <- Park_p100_n250[[j]][[i]]$network[which(Park_p100_n250[[j]][[i]]$lambda == Park_p100_n250[[j]][[i]]$opt_lambda)]
    Opt_Park_p100_n500[[j]][[i]] <- Park_p100_n500[[j]][[i]]$network[which(Park_p100_n500[[j]][[i]]$lambda == Park_p100_n500[[j]][[i]]$opt_lambda)]
    Opt_Park_p100_n1000[[j]][[i]] <- Park_p100_n1000[[j]][[i]]$network[which(Park_p100_n1000[[j]][[i]]$lambda == Park_p100_n1000[[j]][[i]]$opt_lambda)]
    #
    Opt_Park_p200_n250[[j]][[i]] <- Park_p200_n250[[j]][[i]]$network[which(Park_p200_n250[[j]][[i]]$lambda == Park_p200_n250[[j]][[i]]$opt_lambda)]
    Opt_Park_p200_n500[[j]][[i]] <- Park_p200_n500[[j]][[i]]$network[which(Park_p200_n500[[j]][[i]]$lambda == Park_p200_n500[[j]][[i]]$opt_lambda)]
    Opt_Park_p200_n1000[[j]][[i]] <- Park_p200_n1000[[j]][[i]]$network[which(Park_p200_n1000[[j]][[i]]$lambda == Park_p200_n1000[[j]][[i]]$opt_lambda)]
    #
    Opt_Park_p400_n250[[j]][[i]] <- Park_p400_n250[[j]][[i]]$network[which(Park_p400_n250[[j]][[i]]$lambda == Park_p400_n250[[j]][[i]]$opt_lambda)]
    Opt_Park_p400_n500[[j]][[i]] <- Park_p400_n500[[j]][[i]]$network[which(Park_p400_n500[[j]][[i]]$lambda == Park_p400_n500[[j]][[i]]$opt_lambda)]
    Opt_Park_p400_n1000[[j]][[i]] <- Park_p400_n1000[[j]][[i]]$network[which(Park_p400_n1000[[j]][[i]]$lambda == Park_p400_n1000[[j]][[i]]$opt_lambda)]
    
  }
}

# Brief objects check

a <- Opt_Park_p400_n1000[[3]][[2]][[1]]
b <- Park_p400_n1000[[3]][[2]]$network[which(Park_p400_n1000[[3]][[2]]$lambda == Park_p400_n1000[[3]][[2]]$opt_lambda)]
b <- b[[1]]
sum(a==b)
rm(a,b)
#

# Checking if we have symmetrix matrices
is_symmetric_matrix <- function(mat, tol = .Machine$double.eps^0.5) {
  if (!inherits(mat, "Matrix")) {
    stop("Input must be from the 'Matrix' package (e.g., dsCMatrix)")
  }
  
  if (!is(mat, "symmetricMatrix")) {
    # Check manually if equal to its transpose within tolerance
    return(isTRUE(all.equal(mat, t(mat), tolerance = tol)))
  }
  
  return(TRUE)
}

is_symmetric_matrix(Opt_Park_p400_n1000[[3]][[2]][[1]])

# Building consensus matrices:
prova <- (Opt_Park_p400_n1000[[1]][[1]][[1]] + Opt_Park_p400_n1000[[1]][[2]][[1]] + Opt_Park_p400_n1000[[1]][[3]][[1]])
consensus <- as(prova >= 2, "dMatrix")
prova <- as((Opt_Park_p400_n1000[[1]][[1]][[1]] + Opt_Park_p400_n1000[[1]][[2]][[1]] + Opt_Park_p400_n1000[[1]][[3]][[1]]) >=2, "dMatrix")
sum(prova == consensus)
rm(prova,consensus)

# let's apply the process to all cases:
Cons_Opt_Park_p100_n250 <- list()
Cons_Opt_Park_p100_n500 <- list()
Cons_Opt_Park_p100_n1000 <- list()

Cons_Opt_Park_p200_n250 <- list()
Cons_Opt_Park_p200_n500 <- list()
Cons_Opt_Park_p200_n1000 <- list()

Cons_Opt_Park_p400_n250 <- list()
Cons_Opt_Park_p400_n500 <- list()
Cons_Opt_Park_p400_n1000 <- list()

#Cons_Opt_Park_p100_n500[[3]] <- as((Opt_Park_p100_n500[[3]][[1]][[1]] + Opt_Park_p100_n500[[3]][[2]][[1]] + Opt_Park_p100_n500[[3]][[3]][[1]]) >=2, "dMatrix")

for (j in 1:length(Park_p400_n1000)){
  Cons_Opt_Park_p100_n250[[j]] <- as((Opt_Park_p100_n250[[j]][[1]][[1]] + Opt_Park_p100_n250[[j]][[2]][[1]] + Opt_Park_p100_n250[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p100_n500[[j]] <- as((Opt_Park_p100_n500[[j]][[1]][[1]] + Opt_Park_p100_n500[[j]][[2]][[1]] + Opt_Park_p100_n500[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p100_n1000[[j]] <- as((Opt_Park_p100_n1000[[j]][[1]][[1]] + Opt_Park_p100_n1000[[j]][[2]][[1]] + Opt_Park_p100_n1000[[j]][[3]][[1]]) >=2, "dMatrix")
  #
  Cons_Opt_Park_p200_n250[[j]] <- as((Opt_Park_p200_n250[[j]][[1]][[1]] + Opt_Park_p200_n250[[j]][[2]][[1]] + Opt_Park_p200_n250[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p200_n500[[j]] <- as((Opt_Park_p200_n500[[j]][[1]][[1]] + Opt_Park_p200_n500[[j]][[2]][[1]] + Opt_Park_p200_n500[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p200_n1000[[j]] <- as((Opt_Park_p200_n1000[[j]][[1]][[1]] + Opt_Park_p200_n1000[[j]][[2]][[1]] + Opt_Park_p200_n1000[[j]][[3]][[1]]) >=2, "dMatrix")
  #
  Cons_Opt_Park_p400_n250[[j]] <- as((Opt_Park_p400_n250[[j]][[1]][[1]] + Opt_Park_p400_n250[[j]][[2]][[1]] + Opt_Park_p400_n250[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p400_n500[[j]] <- as((Opt_Park_p400_n500[[j]][[1]][[1]] + Opt_Park_p400_n500[[j]][[2]][[1]] + Opt_Park_p400_n500[[j]][[3]][[1]]) >=2, "dMatrix")
  Cons_Opt_Park_p400_n1000[[j]]<- as((Opt_Park_p400_n1000[[j]][[1]][[1]] + Opt_Park_p400_n1000[[j]][[2]][[1]] + Opt_Park_p400_n1000[[j]][[3]][[1]]) >=2, "dMatrix")
}

# Plotting functions libraries:
library(igraph)
library(fmsb)
library(ggplotify)
library(patchwork)
# to avoid a geom_point to be drawn over the legend (tl;dr a graphic error) we resort to cowplot
library(cowplot)
# Loading graphical functions
source("Graphical_Functions/RADAR_V2.R")
matrix_names <- list("Cons-Opt Park",
                     "Quantile",
                     "StARS (piMB)")
thresholds <- c("ZILGM - Voting", "JZILGM - Quantile", "JZILGM - piMB")
colore <- viridis::viridis(4)[c(1,2,4)]


#######################-----------------------------##################################
# Qui ci occupiamo dei confronti JZILGM-ZILGM late dove abbiamo entrambi
########################------------------------------################################


# dobbiamo mediare i risultati sulle varie generazioni dati


# p100 n250

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net100_250_results), function(j) {
  RADAR_V2(
    net100,
    list(
      Cons_Opt_Park_p100_n250[[j]],
      net100_250_results[[j]]$quantile_network,
      net100_250_results[[j]]$freq_matrix > net100_250_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_100_250 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_100_250 <- sqrt((m/(m-1)) * (m2 - mean_mat_100_250^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd

#mean_mat <- Reduce(`+`, stats_indexed) / length(stats_indexed)


RADAR_V2(
  stats_df    = mean_mat_100_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# p100 n500

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net100_500_results), function(j) {
  RADAR_V2(
    net100,
    list(
      Cons_Opt_Park_p100_n500[[j]],
      net100_500_results[[j]]$quantile_network,
      net100_500_results[[j]]$freq_matrix > net100_500_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_100_500 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_100_500 <- sqrt((m/(m-1)) * (m2 - mean_mat_100_500^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd


RADAR_V2(
  stats_df    = mean_mat_100_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# p100 n1000

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net100_1000_results), function(j) {
  RADAR_V2(
    net100,
    list(
      Cons_Opt_Park_p100_n1000[[j]],
      net100_1000_results[[j]]$quantile_network,
      net100_1000_results[[j]]$freq_matrix > net100_1000_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_100_1000 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_100_1000 <- sqrt((m/(m-1)) * (m2 - mean_mat_100_1000^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd


RADAR_V2(
  stats_df    = mean_mat_100_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# p200 n250

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net200_250_results), function(j) {
  RADAR_V2(
    net200,
    list(
      Cons_Opt_Park_p200_n250[[j]],
      net200_250_results[[j]]$quantile_network,
      net200_250_results[[j]]$freq_matrix > net200_250_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_200_250 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_200_250 <- sqrt((m/(m-1)) * (m2 - mean_mat_200_250^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd


RADAR_V2(
  stats_df    = mean_mat_200_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# p200 n500

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net200_500_results), function(j) {
  RADAR_V2(
    net200,
    list(
      Cons_Opt_Park_p200_n500[[j]],
      net200_500_results[[j]]$quantile_network,
      net200_500_results[[j]]$freq_matrix > net200_500_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_200_500 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_200_500 <- sqrt((m/(m-1)) * (m2 - mean_mat_200_500^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd

RADAR_V2(
  stats_df    = mean_mat_200_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# # p200 n1000
# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net200_1000_results), function(j) {
  RADAR_V2(
    net200,
    list(
      Cons_Opt_Park_p200_n1000[[j]],
      net200_1000_results[[j]]$quantile_network,
      net200_1000_results[[j]]$freq_matrix > net200_1000_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_200_1000 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_200_1000 <- sqrt((m/(m-1)) * (m2 - mean_mat_200_1000^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd

RADAR_V2(
  stats_df    = mean_mat_200_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)


# p400 n250

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net400_250_results), function(j) {
  RADAR_V2(
    net400,
    list(
      Cons_Opt_Park_p400_n250[[j]],
      net400_250_results[[j]]$quantile_network,
      net400_250_results[[j]]$freq_matrix > net400_250_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_400_250 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_400_250 <- sqrt((m/(m-1)) * (m2 - mean_mat_400_250^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd


RADAR_V2(
  stats_df    = mean_mat_400_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

# p400 n500

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net400_500_results), function(j) {
  RADAR_V2(
    net400,
    list(
      Cons_Opt_Park_p400_n500[[j]],
      net400_500_results[[j]]$quantile_network,
      net400_500_results[[j]]$freq_matrix > net400_500_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_400_500 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_400_500 <- sqrt((m/(m-1)) * (m2 - mean_mat_400_500^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd

RADAR_V2(
  stats_df    = mean_mat_400_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)

## p400 n1000
# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net400_1000_results), function(j) {
  RADAR_V2(
    net400,
    list(
      Cons_Opt_Park_p400_n1000[[j]],
      net400_1000_results[[j]]$quantile_network,
      net400_1000_results[[j]]$freq_matrix > net400_1000_results[[j]]$piMB
    ),
    matrix_labels = matrix_names,
    colors = colore,
    show_legend = FALSE   # evitiamo ripetizione del comando legenda
  )$Statistics
})

#
apply(stats_list[[1]], 2, class)
#
# # Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})
#
# # Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)
#
# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_400_1000 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_400_1000 <- sqrt((m/(m-1)) * (m2 - mean_mat_400_1000^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd
#
RADAR_V2(
  stats_df    = mean_mat_400_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = TRUE
)
#


# salviamo i dataframe delle medie e delle deviazioni standard

sd_list <- mget(ls(pattern = "^sd_mat_"), envir = .GlobalEnv)
mean_list <- mget(ls(pattern = "^mean_mat_"), envir = .GlobalEnv)


saveRDS(mean_list, file = "mean_df_list.RDS")
saveRDS(sd_list, file = "sd_df_list.RDS")

# Leggiamo le liste (se non vogliamo girare tutto)

mean_list <- readRDS("mean_df_list.RDS")
sd_list <- readRDS("sd_df_list.RDS")

# produzione grafici:

# packages
library(ggplot2)
library(ggplotify)   # you already used as.ggplot()
library(patchwork)
library(viridis)

p100_250 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_100_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p100_500 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_100_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p100_1000 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_100_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))

p200_250 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_200_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p200_500 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_200_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p200_1000 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_200_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))

p400_250 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_400_250,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p400_500 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_400_500,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))
p400_1000 <- as.ggplot(~RADAR_V2(
  stats_df    = mean_mat_400_1000,   # aggregated stats
  colors      = colore,         # same color palette as before
  show_legend = FALSE
))



add_tag <- function(p, pval, nval) {
  p + labs(title = paste0("p = ", pval, ", n = ", nval)) +
    theme(plot.title = element_text(hjust = 0.5, size = 11, margin = margin(b = 6)))
}

p100_250  <- add_tag(p100_250,  100,  250)
p100_500  <- add_tag(p100_500,  100,  500)
p100_1000 <- add_tag(p100_1000, 100, 1000)

p200_250  <- add_tag(p200_250,  200,  250)
p200_500  <- add_tag(p200_500,  200,  500)
p200_1000 <- add_tag(p200_1000, 200, 1000)

p400_250  <- add_tag(p400_250,  400,  250)
p400_500  <- add_tag(p400_500,  400,  500)
p400_1000 <- add_tag(p400_1000, 400, 1000)

grid_3x3 <-
  (
    (p100_250  + p200_250  + p400_250) /
      (p100_500  + p200_500  + p400_500) /
      (p100_1000 + p200_1000 + p400_1000)
  )

pdf("Voting_3x3patch_complete.pdf", height = 14, width = 18)
grid_3x3
dev.off()

# Now we add the legend below the plot

thresholds <- c("ZILGM - Voting", "JZILGM - Quantile", "JZILGM - piMB")
colore <- viridis(4)[c(1,2,4)]

library(ggplot2)
library(patchwork)
# to avoid a geom_point to be drawn over the legend (tl;dr a graphic error) we resort to cowplot
library(cowplot)



legend_df <- data.frame(
  method = factor(thresholds, levels = thresholds),
  x = 1,
  y = 1
)

legend_plot <- ggplot(legend_df, aes(x = x, y = y, color = method)) +
  geom_point(size = 3) +
  scale_color_manual(values = colore, name = NULL) +  # no legend title
  guides(color = guide_legend(direction = "horizontal")) +
  theme_void() +
  theme(
    legend.position = "bottom",   # legend at bottom of this small panel
    legend.box.margin = margin(t = 0, r = 0, b = 5, l = 0),
    legend.text = element_text(size = 15)
  )

# here we use cowplot to get the legend
shared_legend <- cowplot::get_legend(legend_plot)
shared_legend <- patchwork::wrap_elements(shared_legend)

final_plot <- grid_3x3 / shared_legend +
  plot_layout(heights =  c(15,15,15,1))

final_plot

final_plot <- final_plot +
  plot_annotation(
    title = "JZILGM vs ZILGM - Voting",
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        size  = 14,
        face  = "bold"
      )
    )
  )
final_plot




pdf("Voting_3x3_legend_complete.pdf", height = 14, width = 18)
final_plot
dev.off()



#######################################################################################################
############################################### TIME PLOTS ############################################
#######################################################################################################

# !!!!!!!!!!!!!!!!!! NOTICE !!!!!!!!!!!!!!!!!!!!!
# y_ref obtained by the last plot so these following plots must be exectued from the last plot to the first here below

# p100

# n250

# mean(sapply(1:length(time_net100_250), function(i){sum(unlist(time_Park_p100_n250[[i]]))/3600}))
# sd(sapply(1:length(time_net100_250), function(i){sum(unlist(time_Park_p100_n250[[i]]))/3600}))
# mean(as.numeric(sapply(time_net100_250, function(x) x/3600)))
# sd(as.numeric(sapply(time_net100_250, function(x) x/3600)))
y_ref=40

df_time_100_250 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net100_250), function(i){sum(unlist(time_Park_p100_n250[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net100_250, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net100_250), function(i){sum(unlist(time_Park_p100_n250[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net100_250, function(x) x/3600))))
)



t100_250 <- ggplot(df_time_100_250, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_100_250$gruppo))) +
  labs(x = NULL, y = "Hours", title = "p = 100, n = 250") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()
t100_250
# n500
df_time_100_500 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net100_500), function(i){sum(unlist(time_Park_p100_n500[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net100_500, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net100_500), function(i){sum(unlist(time_Park_p100_n500[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net100_500, function(x) x/3600))))
)


t100_500 <- ggplot(df_time_100_500, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_100_500$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 100, n = 500") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

#n1000
df_time_100_1000 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                               data = c(mean(sapply(1:length(time_net100_1000), function(i){sum(unlist(time_Park_p100_n1000[[i]]))/3600})),
                                        mean(as.numeric(sapply(time_net100_1000, function(x) x/3600)))),
                               sd = c(sd(sapply(1:length(time_net100_1000), function(i){sum(unlist(time_Park_p100_n1000[[i]]))/3600})),
                                      sd(as.numeric(sapply(time_net100_1000, function(x) x/3600))))
)



t100_1000 <- ggplot(df_time_100_1000, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_100_1000$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 100, n = 1000") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()



# p200

# n250
df_time_200_250 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net200_250), function(i){sum(unlist(time_Park_p200_n250[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net200_250, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net200_250), function(i){sum(unlist(time_Park_p200_n250[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net200_250, function(x) x/3600))))
)


t200_250 <- ggplot(df_time_200_250, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_200_250$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 200, n = 250") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

# n500
df_time_200_500 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net200_500), function(i){sum(unlist(time_Park_p200_n500[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net200_500, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net200_500), function(i){sum(unlist(time_Park_p200_n500[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net200_500, function(x) x/3600))))
)

t200_500 <- ggplot(df_time_200_500, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_200_500$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 200, n = 500") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

#n1000

df_time_200_1000 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                               data = c(mean(sapply(1:length(time_net200_1000), function(i){sum(unlist(time_Park_p200_n1000[[i]]))/3600})),
                                        mean(as.numeric(sapply(time_net200_1000, function(x) x/3600)))),
                               sd = c(sd(sapply(1:length(time_net200_1000), function(i){sum(unlist(time_Park_p200_n1000[[i]]))/3600})),
                                      sd(as.numeric(sapply(time_net200_1000, function(x) x/3600))))
)


t200_1000 <- ggplot(df_time_200_1000, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_200_1000$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 200, n = 1000") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

# p400

# n250
df_time_400_250 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net400_250), function(i){sum(unlist(time_Park_p400_n250[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net400_250, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net400_250), function(i){sum(unlist(time_Park_p400_n250[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net400_250, function(x) x/3600))))
)



t400_250 <- ggplot(df_time_400_250, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_400_250$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 400, n = 250") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

# n500
df_time_400_500 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                              data = c(mean(sapply(1:length(time_net400_500), function(i){sum(unlist(time_Park_p400_n500[[i]]))/3600})),
                                       mean(as.numeric(sapply(time_net400_500, function(x) x/3600)))),
                              sd = c(sd(sapply(1:length(time_net400_500), function(i){sum(unlist(time_Park_p400_n500[[i]]))/3600})),
                                     sd(as.numeric(sapply(time_net400_500, function(x) x/3600))))
)

#y_ref <- max(df_time_400_1000$data) * 1.08

t400_500 <- ggplot(df_time_400_500, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_400_500$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 400, n = 500") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()


#n1000

#t400_1000 <- ggplot() + theme_void()
df_time_400_1000 <- data.frame(gruppo = c("ZILGM - Voting", "JZILGM"),
                               data = c(mean(sapply(1:length(time_net400_1000), function(i){sum(unlist(time_Park_p400_n1000[[i]]))/3600})),
                                        mean(as.numeric(sapply(time_net400_1000, function(x) x/3600)))),
                               sd = c(sd(sapply(1:length(time_net400_1000), function(i){sum(unlist(time_Park_p400_n1000[[i]]))/3600})),
                                      sd(as.numeric(sapply(time_net400_1000, function(x) x/3600))))
)

#y_ref <- max(df_time_400_1000$data) * 1.08

t400_1000 <- ggplot(df_time_400_1000, aes(x = gruppo, y = data)) +
  geom_col(fill = viridis(length(df_time_400_500$gruppo))) + coord_flip() +
  labs(x = NULL, y = "Hours", title = "p = 400, n = 1000") +
  coord_cartesian(ylim = c(0, y_ref), clip = "off") +
  geom_text(aes(label = round(data, digits = 1)), vjust = -0.35, size = 3.8) +
  theme_minimal()

# Picking the reference value for the plot size

library(dplyr)


# patchworking
t_grid_3x3 <- (t100_250 + t200_250 + t400_250) /
  (t100_500 + t200_500 + t400_500) /
  (t100_1000 + t200_1000 + t400_1000) +
  # 1) collect a single, shared legend for the whole patchwork
  plot_layout(guides = "collect") +
  # 2) add a title (we'll center it via the theme below)
  plot_annotation(
    title = "Hours runtime of JZILGM vs ZILGM - Voting:",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14)),
  ) &
  # Apply to all subplots:
  theme(
    legend.position   = "top",        # place legend above
    legend.direction  = "horizontal", # horizontal items
    legend.box        = "horizontal"  # keep multiple guides on one row
  ) &
  # Make legend keys flow on one row even with many colors
  guides(
    fill   = guide_legend(nrow = 1, byrow = TRUE),
    colour = guide_legend(nrow = 1, byrow = TRUE)
  )

pdf(file = "time3x3Article.pdf")
t_grid_3x3
dev.off()


time_legend_df <- data.frame(
  methods = factor(df_time_100_1000$gruppo, levels = df_time_100_1000$gruppo)
)

time_legend_plot <- ggplot(time_legend_df, aes(x = 1, y = 1, color = methods)) +
  geom_point(size = 3) +  # just to create keys; points are fine
  scale_color_manual(values = viridis(2), name = NULL) +  # no legend title
  guides(color = guide_legend(direction = "horizontal")) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.margin = margin(t = 2, b = 2),
    legend.text = element_text(size = 11)
  )

# Extract the legend as a grob
time_shared_legend <- cowplot::get_legend(time_legend_plot)

time_legend_plot_only <- patchwork::wrap_elements(time_shared_legend)

time_final_plot <- t_grid_3x3 / time_legend_plot_only +
  plot_layout(heights = c(15, 15, 15, 1))  # adjust ratio legend : grid if needed

time_final_plot


pdf(file = "time3x3_legend_Complete.pdf")
time_final_plot
dev.off()

save.image("RadarPlots_Workspace.RData")

################################################### DIFFERENT THRESHOLDS - FIXED (N,P) ###########################################################
# here we need to plot a radarchart where different thresholds are shown on the same plot.
# Each of these thresholds must have a different line style

# We'll use a different function
source("Graphical_Functions/Spider_V2.R")
source("Graphical_Functions/Spider.R")

linestyle_names = list("Quantile", "30%", "50%", "70%", "85%", "piMB")


# We are only concerned about the case p 200, n 500

# 1) Calcoliamo le statistiche per ogni realizzazione e per ogni taglio
stats_list <- lapply(seq_along(net200_500_results), function(j) {
  spider(
    net200,
    list(net200_500_results[[j]]$quantile_network,
         net200_500_results[[j]]$freq_matrix > 0.3,
         net200_500_results[[j]]$freq_matrix > 0.5,
         net200_500_results[[j]]$freq_matrix > 0.7,
         net200_500_results[[j]]$freq_matrix > 0.85,
         net200_500_results[[j]]$freq_matrix > net200_500_results[[j]]$piMB),
    matrix_labels = linestyle_names,
    single_color_multi_lty = TRUE,
    show_legend = TRUE
  )$Statistics
})

apply(stats_list[[1]], 2, class)

# Trasformiamo la prima colonna in indice
stats_indexed <- lapply(stats_list, function(df) {
  df2 <- df
  # usiamo la prima colonna come indice
  rownames(df2) <- df2[[1]]
  # rimuoviamo la prima colonna
  df2[[1]] <- NULL
  df2
})

# Ora è correttamente tutto numerico
apply(stats_indexed[[1]], 2, class)

# matrice media e deviazione standard:
m <- length(stats_indexed)
mean_mat_200_500 <- Reduce(`+`, stats_indexed) / m # E(X)
m2 <- Reduce(`+`, lapply(stats_indexed, function(M) M^2)) / m # E(X^2)
sd_mat_200_500 <- sqrt((m/(m-1)) * (m2 - mean_mat_200_500^2)) #sqrt([m/(m-1)]*E(X^2)-E(X)^2) = sd

#mean_mat <- Reduce(`+`, stats_indexed) / length(stats_indexed)

pdf(file = "Spider_Thresh.pdf")

spider_V2(stats_df = mean_mat_200_500,
          matrix_labels = linestyle_names,
          single_color_multi_lty = TRUE,
          show_legend = TRUE,
)
dev.off()



# Now we need to do it for n = 500, p = 200
linestyle_names = list("Quantile", "30%", "50%", "70%", "85%", "piMB")


pdf(file = "Plots/Spider_Thresh.pdf")
spider(net200,
       list(net200_500_results[[1]]$quantile_network,
            net200_500_results[[1]]$freq_matrix > 0.3,
            net200_500_results[[1]]$freq_matrix > 0.5,
            net200_500_results[[1]]$freq_matrix > 0.7,
            net200_500_results[[1]]$freq_matrix > 0.85,
            net200_500_results[[1]]$freq_matrix > net200_500_results[[1]]$piMB),
       matrix_labels = linestyle_names,
       single_color_multi_lty = TRUE,
       show_legend = TRUE
)
dev.off()


pdf(file = "Plots/Spider_Thresh_p100.pdf")
spider(net100,
       list(net100_500_results[[1]]$quantile_network,
            net100_500_results[[1]]$freq_matrix > 0.3,
            net100_500_results[[1]]$freq_matrix > 0.5,
            net100_500_results[[1]]$freq_matrix > 0.7,
            net100_500_results[[1]]$freq_matrix > 0.85,
            net100_500_results[[1]]$freq_matrix > net100_500_results[[1]]$piMB),
       matrix_labels = linestyle_names,
       single_color_multi_lty = TRUE,
       show_legend = TRUE
)
dev.off()

