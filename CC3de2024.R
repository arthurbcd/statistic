# cc3 2024

df <- data.frame(
    Salaire = c(2.2, 2.3, 2.8, 1.9, 1.5, 3.4, 4.9, 2.6, 2.5, 1.8, 1.6, 2, 1.9),
    Genre = c("H", "H", "H", "H", "H", "H", "H", "F", "F", "F", "F", "F", "F")
)
attach(df)
mean(Salaire[Genre == "H"])
mean(Salaire[Genre == "F"])
var(Salaire[Genre == "H"])
var(Salaire[Genre == "F"])

# question 1

#-1.3

# question 2

qt(0.05, 11)

#-1.8

# question 3

qnorm(0.95)
# 1.64

# question 4

2 * (1 - pnorm(0.5185))
# 61%

# question 5

1.304762 / 0.1586667
# 8.20

# question 6

qf(0.025, 6, 5)
qf(0.975, 6, 5)
# 0,17

# question 7

bank <- read.csv("bank.csv", header = TRUE, sep = ";")
attach(bank)
chisq.test(table(bank$housing, bank$marital))

# question 8
qchisq(0.95, 2)

# question 10

passage <- read.csv("passage.csv", header = TRUE, sep = ";")
attach(passage)
sum(REGION == "France") # 553 donc Nf = 553
passage_france <- passage[c(passage$REGION == "France" & passage$DEAL == 1), ]
nrow(passage_france) # 267 donc pF = 267/553 = 0.483

sum(REGION == "Amérique") # 104 donc Nf = 104
passage_usa <- passage[c(passage$REGION == "Amérique" & passage$DEAL == 1), ]
nrow(passage_usa) # 55 donc pf = 55/104 = 0.529

(104 * 0.529 + 553 * 0.483) / (104 + 553)

# question 9

(1 - pnorm(1.02))
