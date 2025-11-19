library(FactoMineR)

table <- read.csv("table.data", header = FALSE, dec = ".")
table <- table[, 2:8]

# QUESTÃO 1
# 1. Combien de CP retient-on pour relater au moins 95% de la variabilité des observations? A. 3 B. 4 C. 2 # nolint
pca <- PCA(table, graph = TRUE, scale.unit = TRUE)
pca$eig
# 'eig[,1]': eigenvalues (quanto de variância esse CP explica)
# 'eig[,2]': percentual de variância explicada por esse CP
# 'eig[,3]': cumulative % of variance "variabilité des observations"
q1 <- which(pca$eig[, 3] > 95)[1]
cat("QUESTÃO 1 - A =", q1, "\n") # A. 3


# QUESTÃO 2
# 2. Quelle est l'abscisse de l'ormeau 2345 sur la seconde CP ? A. 0,24 B. 0,21 C. 0,18 # nolint
q2 <- pca$ind$coord[2345, 2]
cat("QUESTÃO 2 - Coordonnée 2345 CP2 =", q2, "\n")
# C. 0,18

# QUESTÃO 3
# -> cos2	Qualidade de representação
# contrib	Contribuição para a variância
# dist	    Distância ao espaço p/ detectar outliers
q3 <- which.max(rowSums(pca$ind$cos2[, 1:2])) # aprender também
cat("QUESTÃO 3 - Abalone melhor representado =", q3, "\n")

# QUESTÃO 4

corr <- cor(pca$ind$coord[, 1], scale(table))
q4 <- which.max(abs(corr))
cat("QUESTÃO 4 - Variável melhor correlacionada (índice) =", q4, "\n")

# QUESTÃO 5
# Q5 - Escolher K onde inércia intraclasses < 200 e K é mínimo
for (k in 1:10) {
    km <- kmeans(table, centers = k)
    if (km$tot.withinss < 200) {
        k_final <- k
        break
    }
}
q5 <- k_final - 1 # "retient-on" elbow method
cat("QUESTÃO 5 - K =", q5, "\n")

# QUESTÃO 6
km <- kmeans(table, centers = k_final)
candidates <- c(4128, 2811, 3787)
q6 <- candidates[which(km$cluster[candidates] == km$cluster[1113])[1]]
cat("QUESTÃO 6 - Abalone no mesmo cluster que 1113 =", q6, "\n")

# QUESTÃO 7
q7 <- km$betweenss
cat("QUESTÃO 7 - Inércia interclasses =", q7, "\n")
# 1178.381 então resposta A
