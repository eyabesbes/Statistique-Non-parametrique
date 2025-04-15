---
title: "R Notebook"
output: html_notebook
---


# Code R et illustration graphique du choix du nombre de classes
```{r}
hist(data, breaks = 10, main = "Histogramme avec 10 intervalles", freq = FALSE)
curve(dnorm(x, mean(data), sd(data)), add = TRUE, col = "blue", lwd = 2, lty = 2)
```

# Définition de la fonction de calcul de noyau gaussien
```{r}
KG = function(u) {
  (1 / sqrt(2 * pi)) * exp(-(u^2) / 2)
}
```

#Génération des données et calcule de la densité
```{r}
n <- 500  # Number of random values
X <- rnorm(n) # Generate n random values from standard normal distribution

x <- seq(min(X), max(X), length = n)  # Sequence of x values
h <- n^(-1/5)                         # Bandwidth

f <- c()                             # Initialize empty vector for density estimates

for (i in 1:length(x)) {
  f[i] <- fcharG2(X, x[i], h)  # Kernel density estimation
}

# Further code to use or plot the density estimate 'f' would go here.
```
#Génération des données et calcule de la densité
```{r}
plot(x, dnorm(x), type = "l", lwd = 2)
lines(x, f, col = "red")

fcharE = function(X, xi, h) {
  n = length(X)
  s = (1 / (n * h)) * sum(KG((X - xi) / h))
  return(s)
}
```


