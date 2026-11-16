############################################################################################################################
############################################################################################################################

# We need to have multiple results for statistical power purposes; the data generation and method is run on multiple lists
# of matrices

############################################################################################################################
############################################################################################################################

library(igraph)
library(Matrix)
library(netUtils)
library(rafalib)
library(JZILGM)

############################################################################################################################
############################################################################################################################

# Network configuration
p <- 400

# Generate the ground-truth network
set.seed(123)

# Network creation:
net400 <- netUtils::sample_lfr(n = 400,
                               mu = 0.01,
                               average_degree = 20,
                               max_degree = 30,
                               min_community = 18,
                               max_community = 35)

# to visualize the network:

plot(net400,
     vertex.size = 5,
     vertex.label = NA) 

# We want a circa 95% sparse network, i.e. only 5% of the edges that a fully connected network would have:

# We check if our network verifies our needs by investigating its adjacency matrix A:

A = as.matrix(igraph::as_adjacency_matrix(net400))

(netspar <- 1-sum(A!=0)/length(A))

# to visualize the sparsity:
rafalib::imagemat(A!=0)


# A deeper look to the network, its nodes degree:
hist(igraph::degree(net400), col = "azure3", main = "Node Degrees Freqency")

paste("mean network degree:", mean(igraph::degree(net400)), sep = " ")

############################################################################################################################
############################################################################################################################

# Selecting five seeds: each one is the starting point to generate H matrices representing the ground truth
SEED <- c(1926, 7, 194, 30, 618)

# Number of matrices or experiments k (H in the article)
k = 3

# Now we generate different number of samples scenario: n = 250, n = 500 and n = 1000
Xs_250 <- list()
for (i in 1:length(SEED)){
  Xs_250[[i]] <- k_net_datagen(net = net400, k = k, seed = SEED[i], n = 250, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.84)
} # vedere più su per dettagli della funzione linea 97


Xs_500 <- list()
for (i in 1:length(SEED)){
  Xs_500[[i]] <- k_net_datagen(net = net400, k = k, seed = SEED[i], n = 500, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.84)
}


Xs_1000 <- list()
for (i in 1:length(SEED)){
  Xs_1000[[i]] <- k_net_datagen(net = net400, k = k, seed = SEED[i], n = 1000, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.84)
}

# Here we verify if the data generation meets the 0.3 zero percentage desired in the simulated matrices

zeros_n250 <- list()
zeros_n500 <- list()
zeros_n1000 <- list()

for(i in 1:length(SEED)){
  
  (zeros_n250[[i]] <- sapply(Xs_250[[i]], calculate_dropout))
  (zeros_n500[[i]] <- sapply(Xs_500[[i]], calculate_dropout))
  (zeros_n1000[[i]] <- sapply(Xs_1000[[i]], calculate_dropout))
}

############################################################################################################################
############################################################################################################################

# Parallelization settings
available = parallel::detectCores(logical = FALSE)
ncores <- available - 1 

# Here we have the object receiving the time output
running_time_250 <- list()
# Here the method output
net400_250_results <- list()

for (i in 1:length(Xs_250)){
  running_time_250[[i]] <- system.time({
    net400_250_results[[i]] <- zinb_LGM_grp(Xlist = Xs_250[[i]],
                                            sym = "AND",
                                            nCores = ncores,
                                            verbose = 1
                                            )
    })[3]
}

mean(sapply(running_time_250, function(x) x/60))

saveRDS(net400_250_results, file = "res_400_250_0.3.RDS")
saveRDS(running_time_250, file = "time_400_250_0.3.RDS")

# We repeat the process for n = 500

running_time_500 <- list()
net400_500_results <- list()

for (i in 1:length(Xs_500)){
  running_time_500[[i]] <- system.time({
    net400_500_results[[i]] <- zinb_LGM_grp(Xs_500[[i]], sym = "AND", nCores = ncores, verbose = 1)
  })[3]
}

mean(sapply(running_time_500, function(x) x/60))

saveRDS(net400_500_results, file = "res_400_500_0.3.RDS")
saveRDS(running_time_500, file = "time_400_500_0.3.RDS")

# We repeat the process for n = 1000

running_time_1000 <- list()
net400_1000_results <- list()

for (i in 1:length(Xs_1000)){
  running_time_1000[[i]] <- system.time({
    net400_1000_results[[i]] <- zinb_LGM_grp(Xs_1000[[i]], sym = "AND", nCores = ncores, verbose = 1)
  })[3]
}

mean(sapply(running_time_1000, function(x) x/60))

saveRDS(net400_1000_results, file = "res_400_1000_0.3.RDS")
saveRDS(running_time_1000, file = "time_400_1000_0.3.RDS")

############################################################################################################################
############################################################################################################################

mean(unlist(running_time_250))/60
mean(unlist(running_time_500))/60
mean(unlist(running_time_1000))/60

# We need to understand the average results for the quantile network, for the frequency network with 50, 60, 70 % of the frequency threshold, lasty we need to extract the PI frequency according o the theory of Meinshausen and Buhlmann and check the respective performance of the method.

# Quantile Net
{
  p400n250_quant_net <- lapply(net400_250_results, function(x){
    metrics(net400,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p400n250_quant_net)/length(p400n250_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p400n250_30freq_net <- lapply(net400_250_results, function(x){
    metrics(net400, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p400n250_30freq_net)/length(p400n250_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p400n250_50freq_net <- lapply(net400_250_results, function(x){
    metrics(net400, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p400n250_50freq_net)/length(p400n250_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p400n250_60freq_net <- lapply(net400_250_results, function(x){
    metrics(net400, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p400n250_60freq_net)/length(p400n250_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p400n250_70freq_net <- lapply(net400_250_results, function(x){
    metrics(net400, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p400n250_70freq_net)/length(p400n250_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net400_250_PIMB <- net400_250_results[[1]]$piMB
# PIMB
{
  p400n250_PIMB_freq_net <- lapply(net400_250_results, function(x){
    metrics(net400, x$freq_matrix > net400_250_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p400n250_PIMB_freq_net)/length(p400n250_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn250p400 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn250p400) <- c("95Quantile","30%", "50%","60%","70%",paste("PIMB",signif(net400_250_PIMB,2),sep = "_"))


saveRDS(resdfn250p400, file = "dataframe_n250p400_0.3.RDS")

########################### p400 n500 ###############################################

# Quantile Net
{
  p400n500_quant_net <- lapply(net400_500_results, function(x){
    metrics(net400,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p400n500_quant_net)/length(p400n500_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p400n500_30freq_net <- lapply(net400_500_results, function(x){
    metrics(net400, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p400n500_30freq_net)/length(p400n500_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p400n500_50freq_net <- lapply(net400_500_results, function(x){
    metrics(net400, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p400n500_50freq_net)/length(p400n500_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p400n500_60freq_net <- lapply(net400_500_results, function(x){
    metrics(net400, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p400n500_60freq_net)/length(p400n500_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p400n500_70freq_net <- lapply(net400_500_results, function(x){
    metrics(net400, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p400n500_70freq_net)/length(p400n500_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net400_500_PIMB <- net400_500_results[[1]]$piMB
# PIMB
{
  p400n500_PIMB_freq_net <- lapply(net400_500_results, function(x){
    metrics(net400, x$freq_matrix > net400_500_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p400n500_PIMB_freq_net)/length(p400n500_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn500p400 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn500p400) <- c("95Quantile","30%","50%","60%","70%",paste("PIMB",signif(net400_500_PIMB,2),sep = "_"))

saveRDS(resdfn500p400, file = "dataframe_n500p400_0.3.RDS")


################################### p400 n1000 ###################################


# Quantile Net
{
  p400n1000_quant_net <- lapply(net400_1000_results, function(x){
    metrics(net400,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p400n1000_quant_net)/length(p400n1000_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p400n1000_30freq_net <- lapply(net400_1000_results, function(x){
    metrics(net400, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p400n1000_30freq_net)/length(p400n1000_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p400n1000_50freq_net <- lapply(net400_1000_results, function(x){
    metrics(net400, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p400n1000_50freq_net)/length(p400n1000_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p400n1000_60freq_net <- lapply(net400_1000_results, function(x){
    metrics(net400, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p400n1000_60freq_net)/length(p400n1000_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p400n1000_70freq_net <- lapply(net400_1000_results, function(x){
    metrics(net400, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p400n1000_70freq_net)/length(p400n1000_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net400_1000_PIMB <- net400_1000_results[[1]]$piMB
# PIMB
{
  p400n1000_PIMB_freq_net <- lapply(net400_1000_results, function(x){
    metrics(net400, x$freq_matrix > net400_1000_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p400n1000_PIMB_freq_net)/length(p400n1000_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn1000p400 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn1000p400) <- c("95Quantile","30%","50%","60%","70%",paste("PIMB",signif(net400_1000_PIMB,2),sep = "_"))

saveRDS(resdfn1000p400, file = "dataframe_n1000p400_0.3.RDS")

