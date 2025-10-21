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

# question 1 - oi amor

#-1.3
