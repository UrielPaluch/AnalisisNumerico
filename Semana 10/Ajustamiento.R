
# Inicializo el data frame ------------------------------------------------
x <- c(1, 1.3, 1.6, 1.9, 2.2)
fx <- c(0.7651977, 0.620086, 0.2554022, 0.2818186, 0.1103623)

df <- data.frame(x, fx)


# Polinomio de grado 4 ----------------------------------------------------
#I = as is.
#In function formula. There it is used to inhibit the interpretation of operators such as "+", "-", "*" and "^" as formula operators, so they are used as arithmetical operators. This is interpreted as a symbol by terms.formula.
MC_4 <- lm(fx ~ x + I(x^2) + I(x^3) + I(x^4), data = df)

#Genero el polinomio
polionomio_grado4 <- glue::glue("F(x) = ", round(MC_4$coefficients[[1]], 4))
for(i in 2:length(MC_4$coefficients)){
  polionomio_grado4 <- polionomio_grado4 + glue::glue(
    if(MC_4$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_4$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado4

summary(MC_4)

# Gáfico ------------------------------------------------------------------
df_polinomio4 <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio4$fit = predict(MC_4, newdata = df_polinomio4)

plot(df, type = "p", pch = 16, ylim = c(0, 0.8))
lines(df_polinomio4$x, df_polinomio4$fit, lty = 1, col = "red")
  
# Polinomio de grado 3 ----------------------------------------------------
MC_3 <- lm(fx ~ x + I(x^2) + I(x^3), data = df)

#Genero el polinomio
polionomio_grado3 <- glue::glue("F(x) = ", round(MC_3$coefficients[[1]], 4))
for(i in 2:length(MC_3$coefficients)){
  polionomio_grado3 <- polionomio_grado3 + glue::glue(
    if(MC_3$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_3$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado3

summary(MC_3)

# Gráfico -----------------------------------------------------------------
df_polinomio3 <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio3$fit <- predict(MC_3, newdata = df_polinomio3)

lines(df_polinomio3$x, df_polinomio3$fit, lty = 1, col = "green")



# Exponencial -------------------------------------------------------------
exponencial <- lm(fx ~ exp(x), data = df)

#Genero el polinomio
funcion_exponencial <- glue::glue("F(x) = ", round(exponencial$coefficients[[1]], 4))
for(i in 2:length(exponencial$coefficients)){
  funcion_exponencial <- funcion_exponencial + glue::glue(
    if(exponencial$coefficients[[i]] > 0){" + "}else{" "}, 
    round(exponencial$coefficients[[i]],4), " * e^(x)")
};rm(i)
funcion_exponencial

summary(exponencial)

# Gráfico -----------------------------------------------------------------
df_funcion_exponencial <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_funcion_exponencial$fit <- predict(exponencial, newdata = df_funcion_exponencial)

lines(df_funcion_exponencial$x, df_funcion_exponencial$fit, lty = 1, col = "blue")


# Ejercicio ---------------------------------------------------------------
# Inicializo el data frame ------------------------------------------------
x <- c(3.05, 3.86, 4.67, 5.48, 6.29, 7.1, 7.91)
fx <- c(0.3619, -3.048, -10.0123, -3.4302, 0.0272, 2.7344, 3.6877)

df <- data.frame(x, fx)

# Polinomio de grado 4 ----------------------------------------------------
#I = as is.
#In function formula. There it is used to inhibit the interpretation of operators such as "+", "-", "*" and "^" as formula operators, so they are used as arithmetical operators. This is interpreted as a symbol by terms.formula.
MC_4 <- lm(fx ~ x + I(x^2) + I(x^3) + I(x^4), data = df)

#Genero el polinomio
polionomio_grado4 <- glue::glue("F(x) = ", round(MC_4$coefficients[[1]], 4))
for(i in 2:length(MC_4$coefficients)){
  polionomio_grado4 <- polionomio_grado4 + glue::glue(
    if(MC_4$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_4$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado4

summary(MC_4)


# Gáfico ------------------------------------------------------------------
df_polinomio <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio$fit = predict(MC_4, newdata = df_polinomio)

plot(df, type = "p", pch = 16, ylim = c(-11, 4))
lines(df_polinomio$x, df_polinomio$fit, lty = 1, col = "red")

# Polinomio de grado 3 ----------------------------------------------------
MC_3 <- lm(fx ~ x + I(x^2) + I(x^3), data = df)

#Genero el polinomio
polionomio_grado3 <- glue::glue("F(x) = ", round(MC_3$coefficients[[1]], 4))
for(i in 2:length(MC_3$coefficients)){
  polionomio_grado3 <- polionomio_grado3 + glue::glue(
    if(MC_3$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_3$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado3

summary(MC_3)

# Gráfico -----------------------------------------------------------------
df_polinomio3 <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio3$fit <- predict(MC_3, newdata = df_polinomio3)

lines(df_polinomio3$x, df_polinomio3$fit, lty = 1, col = "green")

# Polinomio de grado 2 ----------------------------------------------------
MC_2 <- lm(fx ~ x + I(x^2), data = df)

#Genero el polinomio
polionomio_grado2 <- glue::glue("F(x) = ", round(MC_2$coefficients[[1]], 4))
for(i in 2:length(MC_2$coefficients)){
  polionomio_grado2 <- polionomio_grado2 + glue::glue(
    if(MC_2$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_2$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado2

summary(MC_2)

# Gráfico -----------------------------------------------------------------
df_polinomio2 <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio2$fit <- predict(MC_2, newdata = df_polinomio2)

lines(df_polinomio2$x, df_polinomio2$fit, lty = 3, col = "blue")

# Polinomio de grado 2 ----------------------------------------------------
MC_1 <- lm(fx ~ x + I(x^1), data = df)

#Genero el polinomio
polionomio_grado1 <- glue::glue("F(x) = ", round(MC_1$coefficients[[1]], 4))
for(i in 2:length(MC_1$coefficients)){
  polionomio_grado1 <- polionomio_grado1 + glue::glue(
    if(MC_1$coefficients[[i]] > 0){" + "}else{" "}, 
    round(MC_1$coefficients[[i]],4), " * x^(", (i-1), ")")
};rm(i)
polionomio_grado1

summary(MC_1)

# Gráfico -----------------------------------------------------------------
df_polinomio1 <- data.frame(x = seq(from = min(x), to = max(x), length.out = 1000))

df_polinomio1$fit <- predict(MC_1, newdata = df_polinomio1)

lines(df_polinomio1$x, df_polinomio1$fit, lty = 6, col = "orange")
