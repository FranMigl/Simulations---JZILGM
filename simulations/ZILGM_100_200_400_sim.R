# Loading Networks

net100 <- readRDS("net100.RDS")
net200 <- readRDS("net200.RDS")
net400 <- readRDS("net400.RDS")

# Loading Data

# p 100
Xs_100_250 <- readRDS("net100_n250_Xs_03.RDS")
Xs_100_500 <- readRDS("net100_n500_Xs_03.RDS")
Xs_100_1000 <- readRDS("net100_n1000_Xs_03.RDS")

# p 200
Xs_200_250 <- readRDS("net200_n250_Xs_03.RDS")
Xs_200_500 <- readRDS("net200_n500_Xs_03.RDS")
Xs_200_1000 <- readRDS("net200_n1000_Xs_03.RDS")

# p 400
Xs_400_250 <- readRDS("net400_n250_Xs_03.RDS")
Xs_400_500 <- readRDS("net400_n500_Xs_03.RDS")
Xs_400_1000 <- readRDS("net400_n1000_Xs_03.RDS")

library("ZILGM")
library("parallel")

# Setting up parallelization:
numWorkers <- parallel::detectCores(logical = TRUE)-1

# Creating a ZILGM method running function:
docall_NBII_AND <- function(x){
  ZILGM::zilgm(X = x, family = "NBII", update_type = "IRLS", do_boot = TRUE, boot_num = 10,
               beta = 0.05, sym = "AND", nCores = numWorkers)
}


Park_net100_250 <- list()
time_park_100_250 <- list()

for (j in 1:length(Xs_100_250)){
  time_park_100_250[[j]] <- list()
  Park_net100_250[[j]] <- list()
  
  for (i in 1:length(Xs_100_250[[j]])){
    time_park_100_250[[j]][[i]] <- system.time({
      Park_net100_250[[j]][[i]] <- docall_NBII_AND(Xs_100_250[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_100_250, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_100_250, file = "ParkTime_p100_n250.RDS")
saveRDS(Park_net100_250, file = "Park_p100_n250.RDS")


Park_net100_500 <- list()
time_park_100_500 <- list()

for (j in 1:length(Xs_100_500)){
  time_park_100_500[[j]] <- list()
  Park_net100_500[[j]] <- list()
  
  for (i in 1:length(Xs_100_500[[j]])){
    time_park_100_500[[j]][[i]] <- system.time({
      Park_net100_500[[j]][[i]] <- docall_NBII_AND(Xs_100_500[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_100_500, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_100_500, file = "ParkTime_p100_n500.RDS")
saveRDS(Park_net100_500, file = "Park_p100_n500.RDS")




Park_net100_1000 <- list()
time_park_100_1000 <- list()

for (j in 1:length(Xs_100_1000)){
  time_park_100_1000[[j]] <- list()
  Park_net100_1000[[j]] <- list()
  
  for (i in 1:length(Xs_100_1000[[j]])){
    time_park_100_1000[[j]][[i]] <- system.time({
      Park_net100_1000[[j]][[i]] <- docall_NBII_AND(Xs_100_1000[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_100_1000, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_100_1000, file = "ParkTime_p100_n1000.RDS")
saveRDS(Park_net100_1000, file = "Park_p100_n1000.RDS")



Park_net200_250 <- list()
time_park_200_250 <- list()

for (j in 1:length(Xs_200_250)){
  time_park_200_250[[j]] <- list()
  Park_net200_250[[j]] <- list()
  
  for (i in 1:length(Xs_200_250[[j]])){
    time_park_200_250[[j]][[i]] <- system.time({
      Park_net200_250[[j]][[i]] <- docall_NBII_AND(Xs_200_250[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_200_250, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_200_250, file = "ParkTime_p200_n250.RDS")
saveRDS(Park_net200_250, file = "Park_p200_n250.RDS")


Park_net200_500 <- list()
time_park_200_500 <- list()

for (j in 1:length(Xs_200_500)){
  time_park_200_500[[j]] <- list()
  Park_net200_500[[j]] <- list()
  
  for (i in 1:length(Xs_200_500[[j]])){
    time_park_200_500[[j]][[i]] <- system.time({
      Park_net200_500[[j]][[i]] <- docall_NBII_AND(Xs_200_500[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_200_500, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_200_500, file = "ParkTime_p200_n500.RDS")
saveRDS(Park_net200_500, file = "Park_p200_n500.RDS")




Park_net200_1000 <- list()
time_park_200_1000 <- list()

for (j in 1:length(Xs_200_1000)){
  time_park_200_1000[[j]] <- list()
  Park_net200_1000[[j]] <- list()
  
  for (i in 1:length(Xs_200_1000[[j]])){
    time_park_200_1000[[j]][[i]] <- system.time({
      Park_net200_1000[[j]][[i]] <- docall_NBII_AND(Xs_200_1000[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_200_1000, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_200_1000, file = "ParkTime_p200_n1000.RDS")
saveRDS(Park_net200_1000, file = "Park_p200_n1000.RDS")




Park_net400_250 <- list()
time_park_400_250 <- list()

for (j in 1:length(Xs_400_250)){
  time_park_400_250[[j]] <- list()
  Park_net400_250[[j]] <- list()
  
  for (i in 1:length(Xs_400_250[[j]])){
    time_park_400_250[[j]][[i]] <- system.time({
      Park_net400_250[[j]][[i]] <- docall_NBII_AND(Xs_400_250[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_400_250, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_400_250, file = "ParkTime_p400_n250.RDS")
saveRDS(Park_net400_250, file = "Park_p400_n250.RDS")


Park_net400_500 <- list()
time_park_400_500 <- list()

for (j in 1:length(Xs_400_500)){
  time_park_400_500[[j]] <- list()
  Park_net400_500[[j]] <- list()
  
  for (i in 1:length(Xs_400_500[[j]])){
    time_park_400_500[[j]][[i]] <- system.time({
      Park_net400_500[[j]][[i]] <- docall_NBII_AND(Xs_400_500[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_400_500, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_400_500, file = "ParkTime_p400_n500.RDS")
saveRDS(Park_net400_500, file = "Park_p400_n500.RDS")




Park_net400_1000 <- list()
time_park_400_1000 <- list()

for (j in 1:length(Xs_400_1000)){
  time_park_400_1000[[j]] <- list()
  Park_net400_1000[[j]] <- list()
  
  for (i in 1:length(Xs_400_1000[[j]])){
    time_park_400_1000[[j]][[i]] <- system.time({
      Park_net400_1000[[j]][[i]] <- docall_NBII_AND(Xs_400_1000[[j]][[i]])
    })[3]
  }
}


mean(sapply(time_park_400_1000, function(x){sum(sapply(x, function(y) y/3600))}))


saveRDS(time_park_400_1000, file = "ParkTime_p400_n1000.RDS")
saveRDS(Park_net400_1000, file = "Park_p400_n1000.RDS")

