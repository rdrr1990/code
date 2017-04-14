library(rbenchmark)
library(KRLS)
library(bigKRLS)
library(ggplot2)

# numeric convergence with bigKRLS and KRLS
set.seed(1776)
N <- 1000  
P <- 4
X <- matrix(rnorm(N*P), ncol=P)
X[,P] <- ifelse(X[,P] > 0.12345, 1, 0)
b <- runif(ncol(X))
y <- X %*% b + rnorm(nrow(X))
bigKRLS.out <- bigKRLS(X = X, y = y)
summary(bigKRLS.out, digits=5)

KRLS.out <- KRLS::krls(X = X, y = y, print.level = 0)
max(abs(bigKRLS.out$derivatives - KRLS.out$derivatives)) < 0.00000001
# should be true

tmp.exp <- ceiling(log(max(abs(bigKRLS.out$derivatives - KRLS.out$derivatives)), base=10))

# In this case, all *N* x *P* = 5,000 estimates of the marginal effect fall within 10^`r tmp.exp` of each other.
# usually on the order of 10^-13...

# speed testing bigKRLS vs. KRLS
set.seed(2017)
N <- 4000
Pcontinuous <- 10
Pbinary <- 10
X <- cbind(matrix(runif(N*Pcontinuous), ncol=Pcontinuous),
           matrix(sample(0:1, N*Pbinary, replace = T), ncol=Pbinary))
y <- X %*% runif(Pcontinuous + Pbinary) + rnorm(N)

# does 4 trials at N = 1000, 2000... 4000
# each round of results in two rows, one for bigKRLS::bigKRLS, one for KRLS::krls

results <- matrix(nrow=10, ncol=10)
Nsample <- seq(1000, N, 1000)
results <- cbind(results, unlist(lapply(Nsample, rep, 2)), Pbinary, Pcontinuous)
rownames(results) <- paste(rep(c("bigKRLS_N", "krls_N"), 5), Nsample, sep="")

dryrun <- benchmark(krls(X[1:100,], y[1:100]), 
                 bigKRLS(y[1:100], X[1:100,]), replications = 10)
colnames(results)[1:7] <- colnames(dryrun)[2:8]

for(i in 1:length(Nsample)){
  tmp <- benchmark(krls(X[1:Nsample[i], ], y[1:Nsample[i]], print.level=2), 
                   bigKRLS(y[1:Nsample[i]], X[1:Nsample[i], ]), 
                   replications = 1)
  results[((2*i - 1):(2*i)), 1:7] <- as.numeric(as.matrix(tmp[,2:8]))
  cat("\n\nfinished", i, "\n\n")
  print(results[1:(2*i), ])
  cat("\n\n")
  write.csv(results, file="krls_speedtests.csv")
}

N <- results$Nsample
graph <- data.frame(N)
graph$Runtime <- results$elapsed/60
graph$Algorithm <- rep(c("bigKRLS", "krls"), 5)

speedplot <- ggplot(data=graph, aes(x = N, y = Runtime, group = Algorithm, colour=Algorithm)) +
  geom_line() + geom_point(aes(shape=Algorithm)) + ylab("Runtime in Minutes")

ggsave("krls_speed10binary10continuous.png")
