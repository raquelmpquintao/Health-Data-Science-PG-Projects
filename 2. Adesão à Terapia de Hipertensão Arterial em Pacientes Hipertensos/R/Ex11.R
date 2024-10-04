# 11. Verifique se existem diferenças significativas entre o valor médio do poder de compra (Poder_compra) 
# entre homens e mulheres.  

homens<-subset(amostra, sexo == 1)
mulheres<-subset(amostra, sexo == 2)

nrow(homens) # n= 40
nrow(mulheres) # n= 61

# n > 30
# σ desconhecido
# Nível de significância
nivel_significancia <- 0.05


resultado_teste_11 <- t.test(homens$poder_compra, mulheres$poder_compra)
print(resultado_teste_11)
valor_p_11 <- resultado_teste_11$p.value

cat("Valor p:", resultado_teste_11$p.value, "\n")

if (valor_p_11 < nivel_significancia) {
  print("Há diferença significativa no poder de compra médio entre homens e mulheres.")
} else {
  print("Não podemos afirmar que há diferença significativa no poder de compra médio entre homens e mulheres.")
}


