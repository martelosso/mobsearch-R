library(party)
library(caret)


generate_mob_models <- function(dataset, response, mob_control = mob_control()){
  # Identificar variáveis explicativas
  explanatory_vars <- colnames(dataset)[-which(colnames(dataset) == response)]
  print(explanatory_vars)
  
  # Lista para armazenar os modelos e métricas
  mob_models <- list()
  evaluation_metrics <- list()
  nodes_metrics <- list()
  
  index <- 1
  
  # Loop para gerar todas as combinações possíveis
  for (k in 1:(length(explanatory_vars) - 1)) {
    comb <- combn(explanatory_vars, k, simplify = FALSE)
    
    # Para cada combinação de X e Z de tamanho k
    for (X in comb) {
      # Seleciona Z como sendo tudo que não está em X
      Z <- setdiff(explanatory_vars, X)
      
      # Formula da regressão
      formula <- as.formula(paste(response, "~", paste(X, collapse = " + "), "|", paste(Z, collapse = " + ")))
      
      # Dividindo treino teste (70% e 30%)
      set.seed(1111)
      trainIndex <- createDataPartition(dataset[[response]], p = .7, list = FALSE)
      train_data = dataset[trainIndex,]
      test_data = dataset[-trainIndex,]
      
      # Ajusta o mob e guarda
      mob_model = mob(formula, data = train_data, control = mob_control)
      mob_models[[index]] <- mob_model
      
      # Resposta observada
      actuals <- test_data[[response]]
      
      # Realizando predições no teste
      probs <- predict(mob_model, newdata = test_data)
      predictions <- ifelse(probs < 0.5, 0, 1)
      
      # Matriz de confusão
      confusion <- confusionMatrix(as.factor(predictions), as.factor(actuals))
      
      # Guarda performance de cada um
      accuracy <- confusion$overall['Accuracy']
      precision <- confusion$byClass['Precision']
      recall <- confusion$byClass['Sensitivity']
      f1 <- 2 * (precision * recall) / (precision + recall)
      
      # Guarda performance de cada nó
      evaluation_metrics[[index]] <- list(
        accuracy = accuracy,
        precision = precision,
        recall = recall,
        f1 = f1,
        confusion = confusion$table
      )
      
      # Nós preditos
      pred_nodes <- predict(mob_model, newdata = test_data, type = "node")
      
      # Obtém nós únicos 
      nodes <- unique(pred_nodes)
      print(nodes)
      
      # Metricas por nó
      node_metrics <- list()
      for (j in nodes){
        # Resultados no j-ésimo nó
        node_index = pred_nodes == j
        confusion <- confusionMatrix(as.factor(predictions[node_index]), as.factor(actuals[node_index]))
        accuracy <- confusion$overall['Accuracy']
        precision <- confusion$byClass['Precision']
        recall <- confusion$byClass['Sensitivity']
        f1 <- 2 * (precision * recall) / (precision + recall)
        
        # Guarda na lista
        node_metrics[[j]] <- list(
          accuracy = accuracy,
          precision = precision,
          recall = recall,
          f1 = f1,
          confusion = confusion$table
        )
      }
      
      # Guarda lista de métricas por nó na lista geral
      nodes_metrics[[index]] <- node_metrics
      
      # Incrementa o index
      index <- index + 1
      
    }
    
  }
  
  # Retorna a lista de modelos e lista de resultados
  return(list(models = mob_models, metrics = evaluation_metrics, node_metrics = nodes_metrics))
  
}

# Exemplo de uso da função
my_mob_control <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 20, trim = 0.1)
dataset <- data.frame(y = sample(0:1, 100, replace = TRUE), x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100))
results <- generate_mob_models(data = dataset, response = "y", mob_control = my_mob_control)

# Acessar o primeiro modelo da lista
mob_model_1 <- results$model[[1]]

# Acessar resultados do primeiro modelo da lista
metrics_1 <- results$metrics[[1]]

# Acessar metrics por nó do primeiro modelo
node_metrics <- results$node_metrics[[1]]
