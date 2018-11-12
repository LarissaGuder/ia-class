tamanhoTabuleiro <- 8

f <- function(x)
{
  diag_coli = 0
  for (i in 1:tamanhoTabuleiro) {
    for (j in 1:tamanhoTabuleiro) {
      dx <- abs(i-j)
      dy <- abs(x[i] - x[j])
      if(i == j && x[i] == x[j]){
        next
      }
      conflito <- dx == dy
      if (conflito) {
        diag_coli <- (diag_coli + 1)
        conflito <- FALSE
      }
    }
  }
  
  diag_coli <- (diag_coli / 2) #we were counting the diagonal collisions double
  fitness <- (28 - diag_coli)
  return(fitness)
}
resultado = ga(
  "permutation",
  fitness = f,
  lower = rep(1, tamanhoTabuleiro),
  upper = rep(8, tamanhoTabuleiro),
  maxiter = 10000,
  pmutation = 0.3
)

# resultados
summary(resultado)

# solução
summary(resultado)$solution

#grafico
plot(resultado)
