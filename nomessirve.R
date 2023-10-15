# Esta weá ya no me sirve, pero sé que es mejor no matarlo por si acaso xd

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