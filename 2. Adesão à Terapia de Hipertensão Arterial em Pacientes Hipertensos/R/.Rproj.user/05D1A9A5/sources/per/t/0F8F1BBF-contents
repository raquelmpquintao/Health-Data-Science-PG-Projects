# 2 - Caracterize as variáveis Idade, Sexo, grupo_terap, nr_receita 
# recorrendo a gráficos e/ou estatísticas de acordo com as características das variáveis e analise os resultados.  

# Identificar variáveis com valores omissos 

colSums(is.na(amostra))

library(e1071)

# Idade
# Sem valores omissos (NA)


# Estatísticas Descritivas
idade_summary <- summary(amostra$idade)
idade_aiq <- quantile(amostra$idade,.75,type=2)-quantile(amostra$idade,.25,type=2) #amplitude interquartil
idade_moda <- as.numeric(names(table(amostra$idade))[which.max(table(amostra$idade))]) #moda
idade_skew <- skewness(amostra$idade, type=1) #skewness
idade_curt <- kurtosis(amostra$idade, type=1) #curtose
idade_sd <- sd(amostra$idade) #desvio padrão

# Gráficos

#Histograma
idade_histogram <- hist(amostra$idade, 
                      col = "royalblue", 
                      main = "Distribuição Etária",
                      xlab = "Idade (em anos)", 
                      ylab = "Nº de doentes",
                      labels = TRUE,
                      breaks = 5,       # 10 em 10 anos, adaptado à amostra e seu min/max
                      ylim= c(0,40),    #limites eixo y, nº doentes
                      border= "darkblue")

#Quantile-Quantile Plot
idade_quantile <- qqnorm(amostra$idade, col="navy",
                       main = "Q-Q Plot Idade",
                       ylab = "Quantis da Amostra", 
                       xlab = "Quantis teóricos")

idade_quantile <- qqline(amostra$idade, col ="red")

#Boxplot
idade_boxplot <- boxplot(amostra$idade, 
                      col="royalblue",
                       horizontal = TRUE,
                       main = "BoxPlot - Idade", 
                       xlab = "Idade (em anos)")


# Exibir Gráfico e Estatísticas

print(idade_summary)
print(paste("Amplitude Interquartil:", idade_aiq))
print(paste("Skew:",idade_skew))
print(paste("Curt:",idade_curt))
print(paste("Moda:",idade_moda))
print(paste("Desvio padrão:",idade_sd))

print("Idade (Idade)")
print(idade_histogram)
print(idade_quantile)
print(idade_boxplot)


# Sexo
# Sem valores omissos (NA)
# Masculino 1, Feminino 2

# Estatísticas Descritivas
sexo_summary <- table(amostra$sexo) # frequência absoluta
sexo_freqrel <- prop.table(sexo_summary)*100 # frequência relativa
sexo_moda <- as.numeric(names(table(amostra$sexo))[which.max(table(amostra$sexo))]) # moda

# Gráfico de barras

sexos<-c("Masculino", "Feminino")
sexo_barplot <- barplot(sexo_summary, 
                          col = c("lightblue","pink"),    
                          border = c("steelblue","lightcoral"),
                          names.arg = sexos,
                          main = "Distribuição - Sexo",
                          xlab = "Sexo", 
                          ylab = "Nº de doentes",
                          ylim= c(0,80))

# Gráfico circular

sexo_piechart <- pie(sexo_freqrel, col = c("lightblue","pink"),
                          main = "Distribuição - Sexo",
                          labels = paste(sexos, "\n", sprintf("%.2f%%", sexo_freqrel)))


# Exibir Gráficos e Estatísticas
print("Sexo")
print(sexo_summary)
print(sexo_freqrel)
print(paste("Moda:",sexo_moda))
print(sexo_barplot)
print(sexo_piechart)


# grupo_terap
# Sem valores omissos (NA)

# Tabela de Frequências

grupo_terap_summary <- table(amostra$grupo_terap) #frequência absoluta
grupo_terap_freqrel <- prop.table(grupo_terap_summary)*100 #frequência relativa

grupos_nomes<-c("Diuréticos","Agentes Bloqueadores beta","Bloqueadores do \n canal de cálcio ","Agentes que atuam \n no sistema \n renina-angiotensina","2 ou mais")
grupos_cores <- c("lightpink", "lightgoldenrodyellow", "lightcyan", "lightcoral", "lemonchiffon3", "salmon3")

# Gráfico barras

grupo_terap_barplot <- barplot(grupo_terap_summary, 
                           col = grupos_cores,
                           main = "Distribuição - Grupo Terapêutico",
                           xlab = "Grupos terapêuticos",
                           ylab= "Frequência absoluta",
                           ylim= c(0,80),
                           names.arg=grupos_nomes,
                           cex.names=0.5)
                                                     
#Gráfico circular

grupo_terap_piechart <- pie(grupo_terap_freqrel, col = grupos_cores,
                     main = "Distribuição - Grupo Terapêutico",
                     labels = paste(grupos_nomes, "- \t", sprintf("%.2f%%", grupo_terap_freqrel)),
                     cex=0.7)


# Exibir Gráfico e Estatísticas

print("Grupo Terapêutico")
print(grupo_terap_summary)
print(grupo_terap_freqrel)
print(grupo_terap_barplot)
print(grupo_terap_piechart)


# nr_receita (coluna nr_medic)
# número de medicamentos na 1ª receita (1-um; 2-dois ou mais) 
# Sem valores omissos (NA)

nr_receita_summary <- table(amostra$nr_medic) # frequência absoluta
nr_receita_freqrel <- prop.table(nr_receita_summary)*100 # frequência relativa
nr_receita_moda <- as.numeric(names(table(amostra$nr_medic))[which.max(table(amostra$nr_medic))]) # moda

# Gráfico de barras

nr_receita_code<-c("Um", "Dois ou mais")
nr_receita_barplot <- barplot(nr_receita_summary, 
                        col = c("seagreen","palegreen"),    
                        border = c("black","black"),
                        names.arg = nr_receita_code,
                        main = "Número de medicamentos na 1ª receita",
                        xlab = "Nº medicamentos", 
                        ylab = "Nº de doentes",
                        ylim= c(0,100))

# Gráfico circular

nr_receita_piechart <- pie(nr_receita_freqrel, col = c("seagreen","palegreen"),
                     main = "Distribuição - Número de medicamentos na 1ª receita",
                     labels = paste(nr_receita_code, "\n", sprintf("%.2f%%", nr_receita_freqrel)))

# Exibir Gráficos e Estatísticas
print("Número de Receitas:")
print(nr_receita_summary)
print(nr_receita_freqrel)
print(paste("Moda:",nr_receita_moda))
print(nr_receita_barplot)
print(nr_receita_piechart)
