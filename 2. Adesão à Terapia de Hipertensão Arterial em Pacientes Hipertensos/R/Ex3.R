# 3. Recorrendo a gráficos, compare o tempo até à primeira aquisição (tempo_inicio) por grupo 
# terapêutico (grupo_terap).  
library(e1071)
grupos_terap <- unique(amostra$grupo_terap)

for (grupo in grupos_terap) {
  # Filtra os dados para o tipo de condiçao
  grupos_terap_all<-subset(amostra, grupo_terap == grupo)
  grupos_terap_tempo<-grupos_terap_all$time_inicio
  
  # Imprime o resultado
  cat("Grupo terapêutico", grupo, "\t Tamanho amostra:", nrow(grupos_terap_all),"\n")
  
  print(summary(grupos_terap_tempo, na.rm=TRUE))
}

# Boxplot

grupo_terap_boxplot<-boxplot(amostra$time_inicio ~ amostra$grupo_terap, na.rm=TRUE,
        main = "Boxplot - Tempo até à primeira aquisição por Grupo Terapêutico",
        col = grupos_cores,
        xlab = "Grupo Terapêutico", 
        ylab = "Tempo até à primeira aquisição",
        xaxt = "n") # remove os rótulos originais
axis(1, at = 1:length(grupos_nomes), labels = grupos_nomes, cex.axis=0.5) # substitui os rótulos em x pelos nomes dos grupos

# Gráfico de barras - média de tempo até à primeira aquisição por grupo terapêutico

# Vetor para armazenar as médias de tempo por grupo
medias_tempo <- numeric(length(grupos_terap))

# Calcula as médias de tempo para cada grupo
for (grupo in grupos_terap) {
  grupos_terap_all <- subset(amostra, grupo_terap == grupo)
  medias_tempo[grupo] <- mean(grupos_terap_all$time_inicio, na.rm = TRUE)
}

grupos_nomes_3<-c("Anti-hipertensivos", "Diuréticos","Agentes \n Bloqueadores beta","Bloqueadores do \n canal de cálcio ","Agentes que \n atuam no sistema \n renina-angiotensina","2 ou mais")
grupos_cores_3 <- c("white","lightpink", "lightgoldenrodyellow", "lightcyan", "lightcoral", "lemonchiffon3", "salmon3")

# Gráfico de barras com as médias de tempo por grupo
medias_tempo_grupo_barplot<- barplot(medias_tempo, 
                                     xlab = "Grupo Terapêutico", 
                                     ylab = "Tempo Médio", 
                                     main = "Tempo médio até à primeira aquisição por Grupo Terapêutico",
                                     ylim=c(0,50),
                                     names.arg = grupos_nomes_3,
                                     cex.names=0.5,
                                     col=grupos_cores_3)
