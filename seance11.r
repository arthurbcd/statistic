# Exercice 1

# Exercice 1. suites et séries

# poisson

# le nombre de pannes de tarmway obsrve en un jour est distribué selon P(2).

X <- rpois(1000, lambda = 2)
Y <- rpois(1000, lambda = 3) # same days

xy <- cbind(X, Y)

sum <- sum((X + Y) >= 4)
sum / 1000

Y <- rpois(1000, lambda = 3)
X <- rpois(1000, lambda = 2)

X[X >= 2]
sum(X[X >= 2] >= 3) / sum(X >= 2)

# as
