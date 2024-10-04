# 5. Construa um diagrama de dispersão entre o poder de compra (Poder-compra) e o custo inicial 
# (custo_inicial)  e conclua. Complemente com uma estatística que considere adequada.  

library(e1071)

#Poder_compra: poder de compra (dados segundo a PORDATA) 
#Custo_inicial: custo da 1ª receita

# Diagrama de dispersão
d_disp_poder_custo<-plot(amostra$custo_inicial, amostra$poder_compra,
     main = "Diagrama de Dispersão: Poder de Compra vs. Custo Inicial",
     xlab = "Custo Inicial (€)",
     ylab = "Poder de Compra (€)",
     col="forestgreen")

# Avaliar se as variáveis têm distribuição normal

poder_compra_boxplot<-boxplot(amostra$poder_compra,
                              col="forestgreen",
                              horizontal = TRUE,
                              main = "BoxPlot - Poder de Compra", 
                              xlab="Poder de Compra (€)",
                              cex.main=0.9)

poder_compra_skew<-skewness(amostra$poder_compra,type = 1)
poder_compra_skew

poder_compra_curt<-kurtosis(amostra$poder_compra,type = 1)
poder_compra_curt

poder_compra_qq<-qqnorm(amostra$poder_compra,
                        col="forestgreen",
                        main = "Q-Q Plot - Poder de Compra",
                        ylab = "Quantis da Amostra", 
                        xlab = "Quantis teóricos",
                        cex.main=0.9)
poder_compra_qq<-qqline(amostra$poder_compra, col = "red")

poder_compra_summary<-summary(amostra$poder_compra)

shapiro.test(amostra$poder_compra)


custo_inicial_boxplot<-boxplot(amostra$custo_inicial,
                               col="forestgreen",
                               horizontal = TRUE,
                               main = "BoxPlot - Custo Inicial", 
                               xlab="Custo Inicial (€)",
                               cex.main=0.9)

custo_inicial_skew<-skewness(amostra$custo_inicial,type = 1)
custo_inicial_skew

custo_inicial_curt<-kurtosis(amostra$custo_inicial,type = 1)
custo_inicial_curt

custo_inicial_qq<-qqnorm(amostra$custo_inicial,
                         col="forestgreen",
                         main = "Q-Q Plot - Custo Inicial",
                         ylab = "Quantis da Amostra", 
                         xlab = "Quantis teóricos",
                         cex.main=0.9)
                         
custo_inicial_qq<-qqline(amostra$custo_inicial, col = "red")

custo_inicial_summary<-summary(amostra$custo_inicial)

shapiro.test(amostra$custo_inicial)


# Calcular a correlação entre as duas variáveis (Spearman - distribuição não normal)
correlacao_poder_custo <- cor(amostra$custo_inicial, amostra$poder_compra, method="spearman")
cat("Correlação entre Custo Inicial e Poder de Compra:", correlacao_poder_custo, "\n")
