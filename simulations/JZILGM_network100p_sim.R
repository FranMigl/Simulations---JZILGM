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
p <- 100

# Generate the ground-truth network
set.seed(123)

# Network creation:
net100 <- netUtils::sample_lfr(n = p,
                               mu = 0.01,
                               average_degree = 5,
                               max_degree = 10,
                               min_community = 5,
                               max_community = 20)

# let's save the network to subsequently run the ZILGM method on it:
saveRDS(net100, "net100.RDS")

# to visualize the network:

plot(net100,
     vertex.size = 5,
     vertex.label = NA) 

# We want a circa 95% sparse network, i.e. only 5% of the edges that a fully connected network would have:

# We check if our network verifies our needs by investigating its adjacency matrix A:

A = as.matrix(igraph::as_adjacency_matrix(net100))

(netspar <- 1-sum(A!=0)/length(A))

# to visualize the sparsity:
rafalib::imagemat(A!=0)


# A deeper look to the network, its nodes degree:
hist(igraph::degree(net100), col = "azure3", main = "Node Degrees Freqency")

paste("mean network degree:", mean(igraph::degree(net100)), sep = " ")

############################################################################################################################
############################################################################################################################

# Selecting five seeds: each one is the starting point to generate H matrices representing the ground truth
SEED <- c(1926, 7, 194, 30, 618)

# Number of matrices or experiments k (H in the article)
k = 3

# Now we generate different number of samples scenario: n = 250, n = 500 and n = 1000
Xs_250 <- list()
for (i in 1:length(SEED)){
  Xs_250[[i]] <- k_net_datagen(net = net100, k = k, seed = SEED[i], n = 250, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.4)
} # vedere più su per dettagli della funzione linea 97


Xs_500 <- list()
for (i in 1:length(SEED)){
  Xs_500[[i]] <- k_net_datagen(net = net100, k = k, seed = SEED[i], n = 500, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.4)
}


Xs_1000 <- list()
for (i in 1:length(SEED)){
  Xs_1000[[i]] <- k_net_datagen(net = net100, k = k, seed = SEED[i], n = 1000, p = p, theta = 0.125, means = c(3,6,9), zerop = 0.4)
}

# Let's store the matrices in objects we can use when we need to run the comparison with ZILGM:

saveRDS(Xs_250, "net100_n250_Xs_03.RDS")
saveRDS(Xs_500, "net100_n500_Xs_03.RDS")
saveRDS(Xs_1000, "net100_n1000_Xs_03.RDS")

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
net100_250_results <- list()

for (i in 1:length(Xs_250)){
  running_time_250[[i]] <- system.time({
    net100_250_results[[i]] <- zinb_LGM_grp(Xlist = Xs_250[[i]],
                                            sym = "AND",
                                            nCores = ncores,
                                            verbose = 1
                                            )
    })[3]
}

mean(sapply(running_time_250, function(x) x/60))

saveRDS(net100_250_results, file = "res_100_250_0.3.RDS")
saveRDS(running_time_250, file = "time_100_250_0.3.RDS")

# We repeat the process for n = 500

running_time_500 <- list()
net100_500_results <- list()

for (i in 1:length(Xs_500)){
  running_time_500[[i]] <- system.time({
    net100_500_results[[i]] <- zinb_LGM_grp(Xs_500[[i]], sym = "AND", nCores = ncores, verbose = 1)
  })[3]
}

mean(sapply(running_time_500, function(x) x/60))

saveRDS(net100_500_results, file = "res_100_500_0.3.RDS")
saveRDS(running_time_500, file = "time_100_500_0.3.RDS")

# We repeat the process for n = 1000

running_time_1000 <- list()
net100_1000_results <- list()

for (i in 1:length(Xs_1000)){
  running_time_1000[[i]] <- system.time({
    net100_1000_results[[i]] <- zinb_LGM_grp(Xs_1000[[i]], sym = "AND", nCores = ncores, verbose = 1)
  })[3]
}

mean(sapply(running_time_1000, function(x) x/60))

saveRDS(net100_1000_results, file = "res_100_1000_0.3.RDS")
saveRDS(running_time_1000, file = "time_100_1000_0.3.RDS")

############################################################################################################################
############################################################################################################################

mean(unlist(running_time_250))/60
mean(unlist(running_time_500))/60
mean(unlist(running_time_1000))/60

# We need to understand the average results for the quantile network, for the frequency network with 50, 60, 70 % of the frequency threshold, lasty we need to extract the PI frequency according o the theory of Meinshausen and Buhlmann and check the respective performance of the method.

# Quantile Net
{
  p100n250_quant_net <- lapply(net100_250_results, function(x){
    metrics(net100,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p100n250_quant_net)/length(p100n250_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p100n250_30freq_net <- lapply(net100_250_results, function(x){
    metrics(net100, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p100n250_30freq_net)/length(p100n250_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p100n250_50freq_net <- lapply(net100_250_results, function(x){
    metrics(net100, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p100n250_50freq_net)/length(p100n250_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p100n250_60freq_net <- lapply(net100_250_results, function(x){
    metrics(net100, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p100n250_60freq_net)/length(p100n250_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p100n250_70freq_net <- lapply(net100_250_results, function(x){
    metrics(net100, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p100n250_70freq_net)/length(p100n250_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net100_250_PIMB <- net100_250_results[[1]]$piMB
# PIMB
{
  p100n250_PIMB_freq_net <- lapply(net100_250_results, function(x){
    metrics(net100, x$freq_matrix > net100_250_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p100n250_PIMB_freq_net)/length(p100n250_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn250p100 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn250p100) <- c("95Quantile","30%", "50%","60%","70%",paste("PIMB",signif(net100_250_PIMB,2),sep = "_"))


saveRDS(resdfn250p100, file = "dataframe_n250p100_0.3.RDS")

########################### p100 n500 ###############################################

# Quantile Net
{
  p100n500_quant_net <- lapply(net100_500_results, function(x){
    metrics(net100,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p100n500_quant_net)/length(p100n500_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p100n500_30freq_net <- lapply(net100_500_results, function(x){
    metrics(net100, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p100n500_30freq_net)/length(p100n500_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p100n500_50freq_net <- lapply(net100_500_results, function(x){
    metrics(net100, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p100n500_50freq_net)/length(p100n500_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p100n500_60freq_net <- lapply(net100_500_results, function(x){
    metrics(net100, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p100n500_60freq_net)/length(p100n500_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p100n500_70freq_net <- lapply(net100_500_results, function(x){
    metrics(net100, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p100n500_70freq_net)/length(p100n500_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net100_500_PIMB <- net100_500_results[[1]]$piMB
# PIMB
{
  p100n500_PIMB_freq_net <- lapply(net100_500_results, function(x){
    metrics(net100, x$freq_matrix > net100_500_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p100n500_PIMB_freq_net)/length(p100n500_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn500p100 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn500p100) <- c("95Quantile","30%","50%","60%","70%",paste("PIMB",signif(net100_500_PIMB,2),sep = "_"))

saveRDS(resdfn500p100, file = "dataframe_n500p100_0.3.RDS")


################################### p100 n1000 ###################################


# Quantile Net
{
  p100n1000_quant_net <- lapply(net100_1000_results, function(x){
    metrics(net100,x$quantile_network)
  })
  quant_mean <- data.frame(t(Reduce("+", p100n1000_quant_net)/length(p100n1000_quant_net)))
  quant_mean$J <- quant_mean$TPR - quant_mean$FPR 
}

# frequency nets:
# 30
{
  p100n1000_30freq_net <- lapply(net100_1000_results, function(x){
    metrics(net100, x$freq_matrix > 0.3)
  })
  mean30 <- data.frame(t(Reduce("+", p100n1000_30freq_net)/length(p100n1000_30freq_net)))
  mean30$J <- mean30$TPR - mean30$FPR 
}
# 50
{
  p100n1000_50freq_net <- lapply(net100_1000_results, function(x){
    metrics(net100, x$freq_matrix > 0.5)
  })
  mean50 <- data.frame(t(Reduce("+", p100n1000_50freq_net)/length(p100n1000_50freq_net)))
  mean50$J <- mean50$TPR - mean50$FPR 
}
# 60
{
  p100n1000_60freq_net <- lapply(net100_1000_results, function(x){
    metrics(net100, x$freq_matrix > 0.6)
  })
  mean60 <- data.frame(t(Reduce("+", p100n1000_60freq_net)/length(p100n1000_60freq_net)))
  mean60$J <- mean60$TPR - mean60$FPR 
}
#70
{
  p100n1000_70freq_net <- lapply(net100_1000_results, function(x){
    metrics(net100, x$freq_matrix > 0.7)
  })
  mean70 <- data.frame(t(Reduce("+", p100n1000_70freq_net)/length(p100n1000_70freq_net)))
  mean70$J <- mean70$TPR - mean70$FPR 
}

net100_1000_PIMB <- net100_1000_results[[1]]$piMB
# PIMB
{
  p100n1000_PIMB_freq_net <- lapply(net100_1000_results, function(x){
    metrics(net100, x$freq_matrix > net100_1000_PIMB)
  })
  meanPIMB <- data.frame(t(Reduce("+", p100n1000_PIMB_freq_net)/length(p100n1000_PIMB_freq_net)))
  meanPIMB$J <- meanPIMB$TPR - meanPIMB$FPR 
}

resdfn1000p100 <- as.data.frame(rbind(quant_mean, mean30, mean50, mean60, mean70, meanPIMB))
rownames(resdfn1000p100) <- c("95Quantile","30%","50%","60%","70%",paste("PIMB",signif(net100_1000_PIMB,2),sep = "_"))

saveRDS(resdfn1000p100, file = "dataframe_n1000p100_0.3.RDS")

