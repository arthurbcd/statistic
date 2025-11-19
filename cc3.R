# cc3

df <- data.frame(
    Salaire = c(2.9, 3.0, 2.6, 2.8, 3.5, 3.6, 5.9, 4.4, 2.5, 2.9, 3.8, 3.3, 3.2), # nolint
    Genre = c("J", "J", "J", "J", "J", "J", "P", "P", "P", "P", "P", "P", "P")
)

juges <- df$Salaire[df$Genre == "J"]
procureurs <- df$Salaire[df$Genre == "P"]
mean_j <- mean(juges)
mean_p <- mean(procureurs)
var_j <- var(juges)
var_p <- var(procureurs)

cat("mean_H =", mean_j, "\n")
cat("mean_F =", mean_p, "\n")
cat("var_H  =", var_j, "\n")
cat("var_F  =", var_p, "\n")

cat("\n\n Question 1 ----------------------------------------\n")

nj <- sum(Genre == "J")
np <- sum(Genre == "P")

# desvio-padrão combinado (pooled standard deviation)
s_p <- sqrt(((nj - 1) * var_j + (np - 1) * var_p) / (nj + np - 2))

# method 1
t_stat <- (mean_p - mean_j) / (s_p * sqrt(1 / np + 1 / nj))
cat("t_stat =", t_stat, "\n")

# method 2
t_stat2 <- t.test(procureurs, juges, alternative = "less", var.equal = TRUE)$statistic # nolint
cat("t_stat2 =", t_stat2, "\n")


# Question 2
cat("\n\n Question 2 ----------------------------------------\n")

t_crit <- qt(0.10, 11)
cat("t_crit =", t_crit, "\n")

# Question 3
# Question 4
# Question 5
# Question 6

var_j <- sqrt(6)
var_p <- 2
s_p <- sqrt(((nj - 1) * var_j + (np - 1) * var_p) / (nj + np - 2))

test <- chisq.test(c(var_j, var_p), correct = FALSE)

# p.value
cat("p_value =", test$p.value, "\n")


client <- read.csv("client.csv", header = TRUE, sep = ";")

# risk = 0
# flux moyen avec risk == sans risk

sans_risk <- client$flux[client$risk == 0]
avec_risk <- client$flux[client$risk == 1]
