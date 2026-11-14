############################################################################################################################
############################################################################################################################
# In this script we 

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


