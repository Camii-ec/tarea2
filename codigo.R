# Tarea 02 -----
# 
# Tópicos Aplicados a Estadística - EYP3407
# 
# Francisca Vilca Sanchez
# ¿y yo toy de adorno? JDKSJDKSJ

# PAQUETES A USAR

library(ISLR)
library(dplyr)
library(rio)

# DATOS A UTILIZAR -----

datos <- Hitters

str(datos)
summary(datos)

datos <- na.omit(datos)
datos$Salary <- log(datos$Salary)

#openxlsx::write.xlsx(datos, "hitters.xlsx")

library(leaps)

mod_back <- regsubsets(Salary ~., datos, nvmax = 19,
                           method = "backward")

mod_forw <- regsubsets(Salary ~., datos, nvmax = 19,
                       method = "forward")

muerte <- summary(mod_back)
destruccion <- summary(mod_forw)
# Asterisco indica que la variable es incluida en el modelo correspondiente
# Modelo de 1 predictor: CRuns
# Modelo de 2 predictores: 

muerte$adjr2
muerte$cp
muerte$bic
muerte$rss

destruccion$adjr2
destruccion$cp
destruccion$bic
destruccion$rss

summary(datos$Salary)

coef(mod_forw, 19)
coef(mod_back, 19)


# Regresión Lasso ---------------------------------------------------------

library(glmnet)

x <- model.matrix(Salary ~., datos)[,-1]
y <- datos$Salary

grilla <- 10^seq(10, -2, length = 100)

set.seed(3312)

mod_lasso <- glmnet(x, y, alpha = 1, lambda = grilla)

cv_lasso <- cv.glmnet(x, y, alpha = 1)
lambda <- cv_lasso$lambda.min #0.003980233

lasso_2.0 <- predict(mod_lasso, type = "coefficients", s = lambda)

round(lasso_2.0, 4)

# Ridge -------------------------------------------------------------------

mod_ridge <- glmnet(x, y, alpha = 0, lambda = grilla)

cv_ridge <- cv.glmnet(x, y, alpha = 0)
lambda <- cv_ridge$lambda.min #0.0551217

ridge_2.0 <- predict(mod_ridge, type = "coefficients", s = lambda)

round(ridge_2.0, 4)

# Ridge le da importancia a todos los culiaos, mientras que Lasso manda a la mierda a casi todos los ctm


# Elasticnet (?) ----------------------------------------------------------

library(caret)

cv_5 <-  trainControl(method = "cv", number = 5)

hit_elnet <-  train(
  Salary ~ ., data = datos,
  method = "glmnet",
  trControl = cv_5
)

hit_elnet$bestTune

mod_elnet <- glmnet(x, y, 
                    alpha = hit_elnet$bestTune[1],
                    lambda = as.numeric(hit_elnet$bestTune[2]))

cv_elnet <- cv.glmnet(x, y, alpha = hit_elnet$bestTune[1])
lambda <- cv_elnet$lambda.min #0.1215506

elnet_2.0 <- predict(mod_elnet, type = "coefficients", s = lambda)

round(elnet_2.0, 4)


# Lasso adaptativo --------------------------------------------------------

peso <- 1/abs(matrix(coef(
  cv_ridge, s=cv_ridge$lambda.min)[, 1][2:(ncol(x)+1)] ))^1 #Gamma=1

mod_lassopro <- glmnet(x, y, alpha = 1, penalty.factor = peso)

cv_lassopro <- cv.glmnet(x, y, alpha = 1, penalty.factor = peso)

lambda <- cv_lassopro$lambda.min #0.1215506

lassopro_2.0 <- predict(mod_lassopro, type = "coefficients", s = lambda)

round(lassopro_2.0, 4)


# Ajuste CV Lasso ---------------------------------------------------------

set.seed(3312)

# Funciones de vital importancia
calcular_mseval <- function(prediccion){
  mse <- mean((prediccion - y[val[[i]]])^2)
  return(mse)
}

calcular_msetest <- function(prediccion){
  mse <- mean((prediccion - y[test[[i]]])^2)
  return(mse)
}

# Estimación de la volaíta
mse_lasso <- c()

# Separamos los datos
train <- list()
val <- list()
test <- list()

for(i in 1:10){
  train[[i]] <- sample(1:nrow(datos), 0.6*nrow(datos))
  val[[i]] <- sample(c(1:nrow(datos))[-train[[i]]], 0.2*nrow(datos))
  test[[i]] <- c(1:nrow(datos))[-c(train[[i]], val[[i]])]
}

for(i in 1:10){
  
  # Modelo con los datos de entrenamiento
  cv_lasso <- cv.glmnet(x[train[[i]],], y[train[[i]]], alpha = 1, lambda = grilla, 
                        nfolds = 10, keep = TRUE)
  lambda <- cv_lasso$lambda
  
  # Ajustamos el parámetro según el error en los datos de validación
  evaluacion <- predict(cv_lasso, newx = x[val[[i]],], s = lambda)
  mse <- apply(evaluacion, 2, calcular_mseval)
  
  # Evaluamos el modelo en los datos de testeo
  
  testeo <- predict(cv_lasso, newx = x[test[[i]],], s = lambda[which.min(mse)])
  mse_lasso[i] <- apply(testeo, 2, calcular_msetest)
}

# MSE promedio para el método Lasso:
mean(mse_lasso)


# Ajuste CV Ridge ---------------------------------------------------------

## Y LASSO ADAPTATIVO JIJII porque me daba paja tener que hacerlo a parte, sobre todo si usa las mismas weás del Ridge

mse_ridge <- c()
mse_lassoA <- c()

for(i in 1:10){
  
  ## Para regresión Ridge
  # Modelo con los datos de entrenamiento
  cv_ridge <- cv.glmnet(x[train[[i]],], y[train[[i]]], alpha = 0, lambda = grilla, 
                        nfolds = 10, keep = TRUE)
  lambda <- cv_ridge$lambda
  
  # Ajustamos el parámetro según el error en los datos de validación
  evaluacion <- predict(cv_ridge, newx = x[val[[i]],], s = lambda)
  mse <- apply(evaluacion, 2, calcular_mseval)
  
  # Evaluamos el modelo en los datos de testeo
  
  testeo <- predict(cv_ridge, newx = x[test[[i]],], s = lambda[which.min(mse)])
  mse_ridge[i] <- apply(testeo, 2, calcular_msetest)
  
  ## Para Lasso Adaptativo
  peso <- 1/abs(matrix(coef(
    cv_ridge, s=lambda[which.min(mse)])[,1][2:(ncol(x)+1)]))
  
  cv_lassoA <- cv.glmnet(x[train[[i]],], y[train[[i]]], alpha = 1, 
                         penalty.factor = peso, nfolds = 10, keep = TRUE)
  
  lambda <- cv_lassoA$lambda
  
  evaluacion <- predict(cv_lassoA, newx = x[val[[i]],], s = lambda)
  mse <- apply(evaluacion, 2, calcular_mseval)
  
  # Evaluamos el modelo en los datos de testeo
  
  testeo <- predict(cv_lassoA, newx = x[test[[i]],], s = lambda[which.min(mse)])
  mse_lassoA[i] <- apply(testeo, 2, calcular_msetest)
}

# MSE promedio para el método Ridge:
mean(mse_ridge)

# MSE promedio para el método Lasso Adaptativo:
mean(mse_lassoA)


# Ajuste CV Elastic-Net ---------------------------------------------------

library(caret)

mse_elastic <- c()
cv <-  trainControl(method = "cv", number = 10)

for(i in 1:10){
  
  alpha <-  train(Salary ~ ., data = datos[train[[i]],],
    method = "glmnet",
    trControl = cv,
    tuneLength = 10)$bestTune[1]
  
  # Modelo con los datos de entrenamiento
  cv_elastic <- cv.glmnet(x[train[[i]],], y[train[[i]]], alpha = alpha, 
                          lambda = grilla, nfolds = 10, keep = TRUE)
  lambda <- cv_elastic$lambda
  
  # Ajustamos el parámetro según el error en los datos de validación
  evaluacion <- predict(cv_elastic, newx = x[val[[i]],], s = lambda)
  mse <- apply(evaluacion, 2, calcular_mseval)
  
  # Evaluamos el modelo en los datos de testeo
  
  testeo <- predict(cv_elastic, newx = x[test[[i]],], s = lambda[which.min(mse)])
  mse_elastic[i] <- apply(testeo, 2, calcular_msetest)
}

beepr::beep(3)

# MSE promedio para el método Elastic-Net:
mean(mse_elastic)


# Ajuste CV MCO -----------------------------------------------------------

mse_mco <- c()

## ¿lm a lo bestia no más? porque caret ta como raro
for(i in 1:10){
  mod_lm <- lm(Salary ~., data = datos[train[[i]],])
  testeo <- predict(mod_lm, datos[test[[i]],])
  mse_mco[i] <- mean((testeo - y[test[[i]]])^2)
}

mean(mse_mco)
