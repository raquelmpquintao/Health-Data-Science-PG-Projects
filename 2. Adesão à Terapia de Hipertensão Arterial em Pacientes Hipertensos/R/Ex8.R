# 8. Obtenha uma estimativa pontual para o valor médio do poder de compra (Poder_compra) e uma 
# estimativa intervalar com uma confiança de 99% apenas para os indivíduos que aviam dois ou mais 
# medicamentos.  

# variáveis: poder_compra e grupo_terap

# Indivíduos que aviam 2 ou mais medicamentos

med_2_mais <- subset(amostra, grupo_terap == "6")

# Valor médio poder de compra

library(e1071)

poder_compra_media <-mean(amostra$poder_compra)
poder_compra_media_2 <-mean(med_2_mais$poder_compra)
print(poder_compra_media)
print(poder_compra_media_2)

summary(med_2_mais$poder_compra)

n_8<-length(med_2_mais$poder_compra) # n da amostra = 18

med_2_mais_hist<-hist(med_2_mais$poder_compra,
                      col = "forestgreen", 
                      main = "Distribuição - Poder de compra de doentes que aviam 2 ou mais medicamentos",
                      xlab = "Poder de Compra (€)", 
                      ylab = "Nº de doentes",
                      labels = TRUE,
                      breaks = 5, 
                      ylim= c(0,10), #limites eixo y, nº doentes
                      border= "black",
                      cex.main=0.9)



#Quantile-Quantile Plot
med_2_mais_quantile <- qqnorm(med_2_mais$poder_compra, 
                              col="forestgreen",
                              main = "Q-Q Plot - Poder de compra de doentes que aviam 2 ou mais medicamentos",
                              ylab = "Quantis da Amostra", 
                              xlab = "Quantis teóricos",
                              cex.main=0.9)

med_2_mais_quantile <- qqline(med_2_mais$poder_compra, col ="red")

#Boxplot

med_2_mais_boxplot<-boxplot(med_2_mais$poder_compra,
                            col="forestgreen",
                            horizontal = TRUE,
                            main = "BoxPlot - Poder de compra de doentes que aviam 2 ou mais medicamentos", 
                            xlab = "Poder de compra (€)",
                            cex.main=0.9)

skewness(med_2_mais$poder_compra)
kurtosis(med_2_mais$poder_compra)
shapiro.test(med_2_mais$poder_compra)

# Distribuição não normal, n < 30 -> abordagem não paramétrica