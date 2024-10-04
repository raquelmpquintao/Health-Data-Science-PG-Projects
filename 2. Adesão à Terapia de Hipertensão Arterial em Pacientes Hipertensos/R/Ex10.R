# 10. Considerando apenas os indivíduos com idade inferior ou igual a 45 anos, 
# pode afirmar-se que o valor médio dos valores obtidos para o custo inicial (Custo_inicial) 
# é significativamente superior a 3?  

individuos_inf_45<-subset(amostra, idade < 46)

nrow(individuos_inf_45)  # n = 6
mean(individuos_inf_45$custo_inicial) 

skewness(individuos_inf_45$custo_inicial)

#Quantile-Quantile Plot
individuos_inf_45_quantile <- qqnorm(individuos_inf_45$custo_inicial, 
                              col="forestgreen",
                              main = "Q-Q Plot - Custo inicial da receita  com idade inferior ou igual a 45 anos",
                              ylab = "Quantis da Amostra", 
                              xlab = "Quantis teóricos",
                              cex.main=0.9)

individuos_inf_45_quantile <- qqline(individuos_inf_45$custo_inicial, col ="red")

shapiro.test(individuos_inf_45$custo_inicial)

# distribuição normal
# n < 30
# σ desconhecido
# Nível de significância
nivel_significancia <- 0.05


resultado_teste_10 <- t.test(individuos_inf_45$custo_inicial, mu = 3, alternative = "greater")
print(resultado_teste_10)
valor_p_10 <- resultado_teste_10$p.value

cat("Valor p:", resultado_teste_10$p.value, "\n")

if (valor_p_10 < nivel_significancia) {
  print("O valor médio do custo inicial para os indivíduos com idade inferior ou igual a 45 anos é significativamente superior a 3.")
} else {
  print("Não podemos afirmar que o valor médio do custo inicial para os indivíduos com idade inferior ou igual a 45 anos é significativamente superior a 3.")
}
