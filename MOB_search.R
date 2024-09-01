library(party)


generate_mob_models <- function(dataset, response, mob_control = mob_control()){
  # Identificar variáveis explicativas
  explanatory_vars <- colnames(dataset)[-which(colnames(dataset) == response)]
  print(explanatory_vars)
  
  # Lista para armazenar os modelos
  mob_models <- list()
  
  index <- 1
  
  # Loop para gerar todas as combinações possíveis
  for (k in 1:(length(explanatory_vars) - 1)) {
    comb <- combn(explanatory_vars, k, simplify = FALSE)
    print(comb)
    
    # Para cada combinação de X e Z de tamanho k
    for (X in comb) {
      # Seleciona Z como sendo tudo que não está em X
      Z <- setdiff(explanatory_vars, X)
      print(Z)
      
      # Formula da regressão
      formula <- as.formula(paste(response, "~", paste(X, collapse = " + "), "|", paste(Z, collapse = " + ")))
      print(formula)
      
      # Ajusta o mob
      mob_models[[index]] <- mob(formula, data = dataset, control = mob_control)
      
      # Incrementa o index
      index <- index + 1
      
    }
    
  }
  
  # Retorna a lista de modelos
  return(mob_models)
  
}

# Exemplo de uso da função
my_mob_control <- mob_control(alpha = 0.05, bonferroni = TRUE, minsplit = 20, trim = 0.1)
dataset <- data.frame(y = rnorm(100), x1 = rnorm(100), x2 = rnorm(100), x3 = rnorm(100), x4 = rnorm(100))
mob_models <- generate_mob_models(data = dataset, response = "y", mob_control = my_mob_control)

# Acessar o primeiro modelo da lista
mob_models[[1]]












