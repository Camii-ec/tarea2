# Tarea 02 -----
# 
# Tópicos Aplicados a Estadística - EYP3407
# 
# Francisca Vilca Sanchez

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

openxlsx::write.xlsx(datos, "hitters.xlsx")

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


sample(rep(1:10, length = nrow(datos)))
train <- sample(1:nrow(datos), 0.6*nrow(datos))
val <- sample(c(1:nrow(datos))[-train], 0.2*nrow(datos))
test <- c(1:nrow(datos))[-c(train, val)]

grilla <- 10^seq(10, -2, length = 100)

# Orden de esta cosa: entrenar el modelo con el set de entrenamiento, calibrar la weá con el de validación (val) y probar qué chucha con el set de testeo (test)

# 10 veces la validación cruzada

cv_lasso <- cv.glmnet(x[train,], y[train], alpha = 1, lambda = grilla, 
                      nfolds = 10, keep = TRUE)

lambda <- cv_lasso$lambda

# Vemos los errores utilizando el conjunto de validación

evaluacion <- predict(cv_lasso, newx = x[val,], s = lambda)

calcular_mseval <- function(prediccion){
  mse <- mean((prediccion - y[val])^2)
  return(mse)
}

mse_lasso <- apply(evaluacion, 2, calcular_mseval)
lambda[which.min(mse_lasso)] #lambda que minimiza el mse en el conjunto de validación
## ¿En volá mostrar los coeficientes de este modelo? Problema pa la Camilita del futuro

# Evaluar la weá con el lambda nuevo en los weones de testeo

testeo <- predict(cv_lasso, newx = x[test,], s = lambda[which.min(mse_lasso)])
## Duda existencial, ¿tendría que hacer un for pa repetir esta weá 10 veces? porque lo único que realmente se hace 10 veces es la weá de generar el modelo con el set de entrenamiento

# Error final de la volaíta (MSE no más)

calcular_msetest <- function(prediccion){
  mse <- mean((prediccion - y[test])^2)
  return(mse)
}

apply(testeo, 2, calcular_msetest)

