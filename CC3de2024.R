# cc3 2024

df <- data.frame(
    Salaire = c(2.2, 2.3, 2.8, 1.9, 1.5, 3.4, 4.9, 2.6, 2.5, 1.8, 1.6, 2, 1.9), # nolint
    Genre = c("H", "H", "H", "H", "H", "H", "H", "F", "F", "F", "F", "F", "F")
)
# avoid attach() to prevent masking; reference columns explicitly
hommes <- df$Salaire[df$Genre == "H"]
femmes <- df$Salaire[df$Genre == "F"]
mean_h <- mean(hommes)
mean_f <- mean(femmes)
var_h <- var(hommes)
var_f <- var(femmes)

cat("mean_H =", mean_h, "\n")
cat("mean_F =", mean_f, "\n")
cat("var_H  =", var_h, "\n")
cat("var_F  =", var_f, "\n")

# Question 1
cat("\n\n Question 1 ----------------------------------------\n")
# -> -1.3

nh <- sum(Genre == "H")
nf <- sum(Genre == "F")

# desvio-padrão combinado (pooled standard deviation)
s_p <- sqrt(((nh - 1) * var_h + (nf - 1) * var_f) / (nh + nf - 2))

# method 1
t_stat <- (mean_f - mean_h) / (s_p * sqrt(1 / nf + 1 / nh))
cat("t_stat =", t_stat, "\n")

# method 2
t_stat2 <- t.test(femmes, hommes, alternative = "less", var.equal = TRUE)$statistic # nolint
cat("t_stat2 =", t_stat2, "\n")


# Question 2
cat("\n\n Question 2 ----------------------------------------\n")
# -> -1.8

t_crit <- qt(0.05, 11)
cat("t_crit =", t_crit, "\n")


# Question 3
cat("\n\n Question 3 ----------------------------------------\n")
# -> 1.64

q95 <- qnorm(0.95)
cat("qnorm(0.95) =", q95, "\n")

# Question 4
cat("\n\n Question 4 ----------------------------------------\n")
# -> 61%

p_value <- 2 * (1 - pnorm(0.5185))
cat("p_value =", p_value, "\n")

# Question 5
cat("\n\n Question 5 ----------------------------------------\n")
# -> 8.20

e <- 1.304762 / 0.1586667
cat("e =", e, "\n")

# Question 6
cat("\n\n Question 6 ----------------------------------------\n")
# -> 0,17 (f_low)

f_low <- qf(0.025, 6, 5)
f_high <- qf(0.975, 6, 5)
cat("f_low  =", f_low, "\n")
cat("f_high =", f_high, "\n")

# Question 7
cat("\n\n Question 7 ----------------------------------------\n")
# -> 1.6% (p-value)

bank <- read.csv("bank.csv", header = TRUE, sep = ";")
test <- chisq.test(table(bank$housing, bank$marital), correct = FALSE)
cat("p-value =", test$p.value, "\n")

# Question 8
cat("\n\n Question 8 ----------------------------------------\n")
# -> 6 (5.99) valeur critique

chi_crit <- qchisq(0.95, df = (2 - 1) * (3 - 1))
cat("valeur critique =", chi_crit, "\n")

# Question 9
cat("\n\n Question 9 ----------------------------------------\n")

passage <- read.csv("passage.csv", header = TRUE, sep = ";")
sum(passage$REGION == "France") # 553 donc Nf = 553
passage_france <- passage[c(passage$REGION == "France" & passage$DEAL == 1), ]
nrow(passage_france) # 267 donc pF = 267/553 = 0.483

sum(passage$REGION == "Amérique") # 104 donc Nf = 104
passage_usa <- passage[c(passage$REGION == "Amérique" & passage$DEAL == 1), ]
nrow(passage_usa) # 55 donc pf = 55/104 = 0.529

pooled_prop <- (104 * 0.529 + 553 * 0.483) / (104 + 553)
cat("pooled_prop =", pooled_prop, "\n")

p_tail <- 1 - pnorm(1.02)
cat("p_tail =", p_tail, "\n")
