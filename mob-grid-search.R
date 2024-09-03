library(party)
library(caret)


grid_search_mob <- function(train_data, test_data, response, model_formula, optimize_for = "precision"){
  
  # Definir a função de objetivo personalizada
  custom_objfun <- switch(optimize_for,
                          "precision" = function(model, data) {
                            predictions <- ifelse(predict(model, newdata = data) < .5, 0, 1)
                            actuals <- data[[response]]
                            confusion <- confusionMatrix(as.factor(predictions), as.factor(actuals))
                            precision <- confusion$byClass['Pos Pred Value']
                            return(1 - precision)  # Minimizar 1 - precisão
                          },
                          "f1" = function(model, data) {
                            predictions <- predict(model, newdata = data)
                            actuals <- data[[response]]
                            confusion <- confusionMatrix(as.factor(predictions), as.factor(actuals))
                            precision <- confusion$byClass['Pos Pred Value']
                            sensitivity <- confusion$byClass['Sensitivity']
                            f1 <- 2 * (precision * sensitivity) / (precision + sensitivity)
                            return(1 - f1)  # Minimizar 1 - F1-score
                          },
                          stop("Invalid option for optimize_for. Choose 'precision' or 'f1'.")
  )
  
  # Definir grid de hiper parâmetros
  grid <- expand.grid(
    minsplit = c(10, 20, 30),
    alpha = c(0.01, 0.05, 0.1),
    trim = c(0.05, 0.1, 0.2)
  )
  
  best_model = NULL
  best_params = NULL
  best_score = Inf # Inicializa score com o pior valor
  
  # Loop para o search
  for (i in 1:nrow(grid)){
    params <- grid[i,]
    
    # Parametros do grid
    control <- mob_control(
      minsplit = params$minsplit,
      alpha = params$alpha,
      trim = params$trim,
      objfun = custom_objfun
    )
    
    # Ajustar modelo na combinação atual de parâmetros
    mob_model <- mob(model_formula, data = train_data, control = control)
    
    # Avaliar o modelo nos dados de teste utilizando a função objetivo
    score <- custom_objfun(mob_model, test_data)
    
    # Atualizar o melhor modelo se o escore for o menor
    if (score < best_score){
      best_score <- score
      best_model <- mob_model
      best_params <- params
    }
  }
  
  return(list(best_model = best_model, best_score = best_score, best_params = best_params))

}

# dataset <- data.frame(y = sample(0:1, 100, replace = TRUE), x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100))
# Dividindo treino teste (70% e 30%)
# set.seed(1111)
# trainIndex <- createDataPartition(dataset[["y"]], p = .7, list = FALSE)
# train_data = dataset[trainIndex,]
# test_data = dataset[-trainIndex,]
# grid_search_mob(train_data = train_data, test_data = test_data, response = "y", model_formula = as.formula("y ~ x3 | x1 + x2 + x4"), optimize_for = "precision")

















