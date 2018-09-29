# Hill Climbing
# Método de busca local que utiliza o procedimento 'iterative improvement (ou melhora iterativa). A estratégia é aplicada em um ponto único, chamado de solução candidata, no espaço de busca.
# O método funciona como escalada a uma montanha. Sempre que um ponto melhor for alcançado (mais próximo ao esperado), esse valor é considerado como valor atual.
# Fonte: ftp://ftp.dca.fee.unicamp.br/pub/docs/vonzuben/ia707_02/topico1_02.pdf

# A implementação abaixo segue a documentação do pacote <https://www.rdocumentation.org/packages/FSelector/versions/0.31/topics/hill.climbing.search>.
# O dataset utilizado se denomina IRIS, está disponível em: <https://archive.ics.uci.edu/ml/datasets/iris>
# Contém as seguintes informações:
#1. sepal length in cm 
#2. sepal width in cm 
#3. petal length in cm 
#4. petal width in cm 
#5. class: 
#  -- Iris Setosa 
#  -- Iris Versicolour 
#  -- Iris Virginica



library(rpart)
data(iris)

evaluator <- function(subset) {
  #k-fold cross validation
  k <- 5
  splits <- runif(nrow(iris))
  results = sapply(1:k, function(i) {
    test.idx <- (splits >= (i - 1) / k) & (splits < i / k)
    train.idx <- !test.idx
    test <- iris[test.idx, , drop=FALSE]
    train <- iris[train.idx, , drop=FALSE]
    tree <- rpart(as.simple.formula(subset, "Species"), train)
    error.rate = sum(test$Species != predict(tree, test, type="c")) / nrow(test)
    return(1 - error.rate)
  })
  print(subset)
  print(mean(results))
  return(mean(results))
}

subset <- hill.climbing.search(names(iris)[-5], evaluator)
f <- as.simple.formula(subset, "Species")
print(f)
