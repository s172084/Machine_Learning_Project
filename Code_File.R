
# -- Libraries
library(MASS)
library(ggplot2)
library(data.table)
library(reshape)
library(reshape2)

# -- Diamonds
library(tidyverse)
data(diamonds)
?diamonds
str(diamonds)



# PCA ##########################################################################

D_pca <- diamonds[,-c(2:4,7)]
colMeans(D_pca)
D_pca <- t(apply(D_pca, 1,"-",colMeans(D_pca)))
S <- svd(D_pca)
diagS <- S$d
rho <- diagS^2 / sum(diagS^2)

threshold <- 0.95

xlimits <- c(1, M)
plot(rho,
     type = "o",
     main = "Variance explained by principal componenets",
     xlab = "Principal components",
     ylab = "Variance explained",
     xlim = xlimits,
     ylim = c(0, 1),
     col = "blue"
)

lines(cumsum(rho), type = "o", col = "orange")
lines(xlimits, c(threshold, threshold), lty = "dashed")

legend("right", # Define position
       legend = c("Individual", "Cumulative", "Threshold"), # Set strings for legend
       col = c("orange", "blue", "black"), lty = c(1, 1, 2), # Match appereance of lines
       cex = 1, bg = "lightblue"
) # Setup how the box looks (cex controls size)

# Manual projecting data onto principal component.
Z <- S$u %*% diag(S$d)

i <- 1
j <- 2

ggplot() +
  geom_point(aes(x = Z[, i], y = Z[, j]), size = 1, alpha = 0.5) +
  theme(legend.position = c(0.88, 0.22), legend.title = element_blank()) +
  labs(x = colnames(Z)[i], y = colnames(Z)[j])


V <- S$v

pcs <- 1:2
test <- as.data.frame(melt(data.table(V[, pcs])))
ggplot(test, aes(x = rep(1:6, length(pcs)), y = value, fill=variable)) +
  geom_bar(position="dodge", stat = "identity") +
  labs(fill="PC", x = "Attributes", y = "Component coefficients")
