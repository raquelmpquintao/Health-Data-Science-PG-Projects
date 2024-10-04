############Instalação de bibliotecas###########

#install.packages("psych")
#install.packages("fBasics")
#install.packages("corrgram")
#install.packages("car")
#install.packages(c("FactoMineR", "factoextra"))    
#install.packages("factoextra")
#install.packages("FactorAssumptions")
#install.packages("devtools")
#install.packages("tidyverse")
#install.packages("Hmisc")

library("FactoMineR")
library("factoextra")
library ("devtools")
library ("dplyr")
library ("ggplot2")
library ("shiny")
library("tidyverse")
library("Hmisc")

################################################################################################################
############Importação dataset original######################

library(readxl)
BD_ORIGINAL <- read_excel("C:/Users/raque/OneDrive/Desktop/PG HDS/Aulas - 3º Trimestre/Estatistica Multivariada/Projeto/Base de dados_26.04.xlsx")

################################################################################################################
############TRATAR VALORES MISSING#####################

## Identificar variáveis com valores omissos##

colSums(is.na(BD_ORIGINAL))

## Obter valor médias de cada variavel ##
medias <- colMeans(BD_ORIGINAL, na.rm = TRUE)


## Substituir valores ausentes pela média de cada coluna ##
Base_Dados<- BD_ORIGINAL
for (i in 1:ncol(Base_Dados)) {
  Base_Dados[is.na(Base_Dados[, i]), i] <- medias[i]
}

## Verificar se já não temos missings ##
colSums(is.na(Base_Dados)) 

View(Base_Dados)

############TRATAR DADOS (casas decimais)###################

# Definir as variáveis que não devem apresentar casas decimais 

variaveis_sem_decimais <- c("Idade", "EVA_0") 

# Arredondar as variáveis selecionadas para zero casas decimais 

Base_Dados[, variaveis_sem_decimais] <- round(Base_Dados[, variaveis_sem_decimais]) 


# Arredondando os valores da coluna "F_0" para duas casas decimais
Base_Dados$F_0 <- round(Base_Dados$F_0, 2)

View(Base_Dados)
################################################################################################################
############ ALINEA B) - CLASSIFICAÇÃO DAS VARIAVEIS ################


## Definir as variáveis quantitativas, continuas e discretas para criar os histogramas
variaveis_quantitativas <- c("Idade", "Peso", "Altura", "IMC", "F_0", "PM6_0","WD_0", "WR_0", "WA_0", "WT_0","F_90", "PM6_90", "WD_90", "WR_90", "WA_90", "WT_90")
variaveis_continuas <- c("Peso", "Altura", "IMC", "F_0", "PM6_0", "F_90", "PM6_90")
variaveis_discretas <- c("Idade", "WD_0", "WR_0", "WA_0", "WT_0", "WD_90", "WR_90", "WA_90", "WT_90")

## Definir as variáveis qualitativas
variaveis_qualitativas <- c("sexo","Grupo_pre", "Grupo_pos", "EVA_0", "EVA_90", "Satisfação")


## Base de dados somente com dados quantitativos
Base_Dados_QT <- subset(Base_Dados, select = -c(sexo, Grupo_pre, Grupo_pos, EVA_0, EVA_90, Satisfação))
View(Base_Dados_QT)

## Base de dados somente com dados qualitativos
Base_Dados_QL <- subset(Base_Dados, select = c(sexo, Grupo_pre, Grupo_pos, EVA_0, EVA_90, Satisfação))
View(Base_Dados_QL)

################################################################################################################

############ ALINEA C) - ANALISE DESCRITIVA ################


#### Moda ####
# Calcular e exibir a moda para cada variável do data frame
for (nome_variavel in names(Base_Dados)) {
  moda <- as.numeric(names(table(Base_Dados[[nome_variavel]]))[which.max(table(Base_Dados[[nome_variavel]]))])
  cat("Moda para", nome_variavel, ":", moda, "\n")
}

#### Summary_QT ####
variable.names(Base_Dados_QT)
summary(Base_Dados_QT)

#### Desvio Padrão_QT####

# Calcular e exibir o desvio padrão para cada variável do data frame
for (nome_variavel in names(Base_Dados_QT)) {
  cat("Desvio Padrão para", nome_variavel, ":", sd(Base_Dados_QT[[nome_variavel]]), "\n")
}

#### Coeficiente Variação_QT ####

# Calcular o coeficiente de variação para cada variável
cv_resultados <- sapply(Base_Dados_QT, function(x) sd(x) / mean(x) * 100)

# Exibir os coeficientes de variação
print(cv_resultados)

#### Frequencias relativas e absolutas_QL####

##VARIAVEL: "sexo"
sexo_freqabs<- table(Base_Dados$sexo) # frequência absoluta
sexo_freqrel<- prop.table(sexo_freqabs)*100 # frequência relativa

print(sexo_freqabs)
print(sexo_freqrel)

##VARIAVEL: "Grupo_pre"
Grupo_pre_freqabs<- table(Base_Dados$Grupo_pre) # frequência absoluta
Grupo_pre_freqrel<- prop.table(Grupo_pre_freqabs)*100 # frequência relativa

print(Grupo_pre_freqabs)
print(Grupo_pre_freqrel)

##VARIAVEL: "Grupo_pos"
Grupo_pos_freqabs<- table(Base_Dados$Grupo_pos) # frequência absoluta
Grupo_pos_freqrel<- prop.table(Grupo_pos_freqabs)*100 # frequência relativa

print(Grupo_pos_freqabs)
print(Grupo_pos_freqrel)

##VARIAVEL: "EVA_0"
EVA_0_freqabs<- table(Base_Dados$EVA_0) # frequência absoluta
EVA_0_freqrel<- prop.table(EVA_0_freqabs)*100 # frequência relativa

print(EVA_0_freqabs)
print(EVA_0_freqrel)

##VARIAVEL: "EVA_90"
EVA_90_freqabs<- table(Base_Dados$EVA_90) # frequência absoluta
EVA_90_freqrel<- prop.table(EVA_90_freqabs)*100 # frequência relativa

print(EVA_90_freqabs)
print(EVA_90_freqrel)

##VARIAVEL: "Satisfação"
Satisfação_freqabs<- table(Base_Dados$Satisfação) # frequência absoluta
Satisfação_freqrel<- prop.table(Satisfação_freqabs)*100 # frequência relativa

print(Satisfação_freqabs)
print(Satisfação_freqrel)


#### Gráficos ####

## Gráficos de barras para variáveis qualitativas
par(mfrow = c(3, 4)) 
for (variavel in variaveis_qualitativas) {
  print(Base_Dados[[variavel]])
  barplot(table(Base_Dados[[variavel]]), main = variavel, xlab = "Categoria", ylab = "Frequência")
}
par(mfrow = c(1, 1)) 


## Histogramas_QT 

par(mfrow = c(3, 3)) # Dividir a área do gráfico em uma matriz de 3x3
for (variavel in variaveis_quantitativas) {
  hist(Base_Dados[[variavel]], main = variavel, xlab = variavel)
}
par(mfrow = c(1, 1)) # Restaurar o layout padrão do gráfico


## BOXPLOT_QT


windows(width = 10, height = 10)  # Ajuste o tamanho conforme necessário

par(mfrow = c(3, 3)) # Dividir a área do gráfico em uma matriz de 3x3

for (variavel in variaveis_quantitativas) {
  boxplot(Base_Dados[[variavel]], main = variavel, ylab = "Valores")
}

par(mfrow = c(1, 1))

############ ALINEA D) - ANALISE DESCRITIVA
#### 1º passo - Selecionar variaveis ####

#### Caracteristicas individuos pre-cirurgia
Grupo_pre<- subset(Base_Dados, select = c(Idade, Peso, Altura, IMC, F_0, PM6_0, WD_0, WR_0, WA_0, WT_0))
View(Grupo_pre)

#### Caracteristicas individuos pre-cirurgia
Grupo_pos<- subset(Base_Dados, select = c(Idade, Peso,Altura, IMC, F_90, PM6_90, WD_90, WR_90,WA_90, WT_90))
View(Grupo_pos)

#### 2º passo - Normalizar os dados dos grupos de variaveis ####


## Base de dados original + variaveis normalizadas

Base_Dados_NORMALIZED<-as.data.frame(cbind(Base_Dados,scale(Base_Dados_QT)))

Base_Dados_QT_NORMALIZED<-as.data.frame(scale(Base_Dados_QT))

## Base dados pre-cirurgia e pos-cirurgia normalizadas
Grupo_pre_NORMALIZED<- subset(Base_Dados_QT_NORMALIZED, select = c(Idade, Peso, Altura, IMC, F_0, PM6_0, WD_0, WR_0, WA_0, WT_0))
View(Grupo_pre_NORMALIZED)


Grupo_pos_NORMALIZED<- subset(Base_Dados_QT_NORMALIZED, select = c(Idade, Peso,Altura, IMC, F_90, PM6_90, WD_90, WR_90,WA_90, WT_90))
View(Grupo_pos_NORMALIZED)

#### 3º Passo - AFCP - Variaveis Pre-cirugia ####

# Realizar a análise fatorial de componentes principais (AFCP)

res.pca_pre<- PCA(Grupo_pre_NORMALIZED, graph = FALSE) #Run da analise fatorial

res.pca_pre[["var"]]
res.pca_pre[["var"]][["coord"]]

### Analisar eigen value - verificar quantas dimensões retemos no espaço de dim reduzida:
eigenvalue_pre <- get_eigenvalue(res.pca_pre)

res.pca_pre[["eig"]]


#RESULTADO:

#RETÊM-SE 3 DIMENSÕES 

#comp 1  3.389721e+00           
#comp 2  1.916347e+00        
#comp 3  1.504498e+00

#Aporte de informação retida no espaço reduzido, discriminado por dimensão (Percetagem de variância):

#comp 1  3.389721e+00          33.89721%
#comp 2  1.916347e+00          53.06068%
#comp 3  1.504498e+01          68.10566%

#Grafico de barras das dimensões/%de variancia 

fviz_eig(res.pca_pre, addlabels = TRUE, ylim = c(0, 50))


#### 4º Passo - Validar a qualidade da AFCP - Variaveis pre-cirurgia ####


#### 1.Avaliar o determinante da matriz de correlações das variaveis originais: 

# Calcular a matriz de correlação
correlacoes_pre<- cor(Grupo_pre_NORMALIZED)

print(correlacoes_pre)

det(cor(Grupo_pre_NORMALIZED))

#det matriz de correlções= -5.207493e-20

#RESPOSTA: Sendo que determinante da matriz de correlações deve apresentar o valor mais proximo possível de 0, podemos verificar uma garantia de que as var's estão correlacionadas

#### 2.Avaliar o valor de KMO (Kaiser Meier Olkin) 

psych::KMO(Grupo_pre_NORMALIZED)  

##DEVE SER OBRIGATORIAMENTE SER SUP 0.5! E VARIA ENTRE 0 E 1.

##RESULTADO: Overall MSA =  0.5

##COMO ESTÁ AINDA SOBRE O LIMITE DE INTERVALO [0.5,1]! POSSO APLICAR UMA AFCP
##Quando as correlações estao equitativamente identicas (o montante de correlações das variaveis e dimensoes sao iguais) -> correlacionam-se igualmente como as variaveis

#### 3. Verificar os resultados do teste de bartlett

bartlett.test(Grupo_pre)

#testa se o determinante = 1 (H0)    se p.value<0.05 rejeitamos H0, logo det !=1 
#realizado teste utilizando as variáveis originais 

#RESULTADO: p-value < 2.2e-16 . 


##### GARANTIDADA A VALIDADE DA APLICABILIDADE DA AFCP 

#CONCLUSOES AFCP pré-cirurgia : teve qualidade - Reduzimos o espaço original, retemos 3 dimensões que representam 68.1% de informaçao retida no espaço acumulado.  


#### 5º Passo - Gráfico AFCP - Variaveis pre-cirurgia ####

## Analise dimensão 1 e 2

fviz_pca_biplot(res.pca_pre)                    #### Make a biplot of individuals and variables
fviz_pca_var(res.pca_pre, col.var = "blue")    #### Plotting just variables.

## Analise dimensão 1 e 3
fviz_pca_biplot(res.pca_pre, axes = c(1, 3))
fviz_pca_var(res.pca_pre, axes = c(1, 3), col.var = "darkgreen")

## Analise dimensão 2 e 3

fviz_pca_biplot(res.pca_pre, axes = c(2, 3))
fviz_pca_var(res.pca_pre, axes = c(2, 3), col.var = "purple")

#### 6º Passo - ANALISE HIERQUICA DE CLUESTERS PARA O ESPAÇO DE DIMENSÃO REDUZIDA - Variaveis pré-curgia ####

res.hcpc_pre <- HCPC(res.pca_pre, graph = FALSE)

# Dendrograma - Variaveis pré-cirurgia 
fviz_dend(res.hcpc_pre, 
          cex = 0.7,                     
          palette = "jco",              
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",           
          labels_track_height = 0.8      
)

#fviz_dend(res.hcpc_pre) #outra forma de grafico 


#Para a analise do projeto e considerando a classificação dendogramica, opta-se por selecionar 3 grupos de clusters diferentes

#### 7º Passo - Caracterização dos grupos de clusters da Base_dados - Variaveis pré-curgia ####

### Atribuir cada cluster a cada individuo na tabela de dados original 

clusters_pre<- res.hcpc_pre$data.clust$clust #defenir os clusters

Base_Dados_Clusters <- Base_Dados #replicar Base dados originais

Base_Dados_Clusters$cluster_pre <- clusters_pre

View (Base_Dados_Clusters)

### Saber quantos individuos tem cada cluster 

summary(Base_Dados_Clusters$cluster_pre)   


# Calcular as médias das variáveis por cluster 

medias_por_cluster_pre<- aggregate(. ~ clusters_pre, data = Base_Dados_Clusters, FUN = mean) 

View (medias_por_cluster_pre)


summary_por_cluster_pre <- aggregate(. ~ cluster_pre, data = Base_Dados_Clusters, FUN = summary)


# Visualizar o resumo
View(summary_por_cluster_pre)


par(mfrow=c(2,2))

#Idade por cluster_pre - boxplot
boxplot(Idade  ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")

# Sexo por cluster_pre - Grafico de barras de frequencias absolutas 
freq_table_sexo <- table(Base_Dados_Clusters$sexo, Base_Dados_Clusters$cluster_pre)

legend_labels <- c("Feminino", "Masculino")

barplot(freq_table_sexo, beside = TRUE, legend.text = legend_labels, main = "Frequência Absoluta por Cluster_pre/Sexo")

#Grupo_pre por cluster_pre - Gráfico de barras de frequências absolutas
freq_table_grupo_pre <- table(Base_Dados_Clusters$Grupo_pre, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_grupo_pre, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Grupo_pre/Cluster_pre")



#Grupo_pos por cluster_pre - Gráfico de barras de frequências absolutas
freq_table_grupo_pos <- table(Base_Dados_Clusters$Grupo_pos, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_grupo_pos, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Cluster_pre/Grupo_pos")

#Peso por cluster_pre - boxplot
boxplot(Peso  ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#IMC por cluster_pre - boxplot
boxplot(IMC  ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")

# EVA_0  por cluster pre- Gráfico de barras de frequências absolutas
freq_table_eva <- table(Base_Dados_Clusters$EVA_0, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_eva, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Cluster_pre/EVA_0")

# EVA_0 por cluster_pre - Gráfico de barras de frequências absolutas

#F_0 por cluster_pre - boxplot
boxplot(F_0 ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#PM6_0 por cluster_pre - boxplot
boxplot(PM6_0 ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#Boxplot WOMAC_pre por cluster_pre
par(mfrow=c(2,2))

selected_vars <- c("WR_0", "WD_0", "WA_0", "WT_0")

for (var in selected_vars) {
  boxplot(formula(paste(var, "~ cluster_pre")), data = Base_Dados_Clusters, main = var)
}

# EVA_90  por cluster pre- Gráfico de barras de frequências absolutas
freq_table_eva90 <- table(Base_Dados_Clusters$EVA_90, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_eva90, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por EVA_0/Cluster_pre")


#F_90 por cluster_pre - boxplot
boxplot(F_90 ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#PM6_90 por cluster_pre - boxplot
boxplot(PM6_90 ~ clusters_pre, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#Boxplot WOMAC_pos por cluster_pre
par(mfrow=c(2,2))

selected_vars <- c("WR_90", "WD_90", "WA_90", "WT_90")

for (var in selected_vars) {
  boxplot(formula(paste(var, "~ cluster_pre")), data = Base_Dados_Clusters, main = var)
}


# Satisfaçao por cluster_pre - Grafico de barras de frequencias absolutas 
freq_table_satisfacao <- table(Base_Dados_Clusters$Satisfação, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_satisfacao, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Satisfaçao/Cluster_pre")


#####################REPLICAR PROCESSO PARA VARIAVEIS POS CIRURGIA ############################################
#### 8º Passo - AFCP - Variaveis Pos-cirugia ####


# Realizar a análise fatorial de componentes principais (AFCP)

res.pca_pos<- PCA(Grupo_pos_NORMALIZED, graph = FALSE) #Run da analise fatorial
res.pca_pos[["var"]]

### Analisar eigen value - verificar quantas dimensões retemos no espaço de dim reduzida:
eigenvalue_pos<- get_eigenvalue(res.pca_pos)

res.pca_pos[["eig"]]

#RESULTADO:

#RETÊM-SE 4 DIMENSÕES 

#comp 1  3.106206e+00       
#comp 2  2.038154e+00           
#comp 3  1.605471e+00           
#comp 4  1.016358e+00  


#Aporte de informação retida no espaço reduzido, discriminado por dimensão (Percetagem de variância):

#comp 1  3.106206e+01                          31.06206
#comp 2  2.038154e+01                          51.44360
#comp 3  1.605471e+01                          67.49831
#comp 4  1.016358e+01                          77.66189

#Grafico de barras das dimensões/%de variancia 

fviz_eig(res.pca_pos, addlabels = TRUE, ylim = c(0, 50))



#### 9º Passo - Validar a qualidade da AFCP - Variaveis pós-cirurgia ####


#### 1.Avaliar o determinante da matriz de correlações das variaveis originais: 

# Calcular a matriz de correlação
correlacoes_pos<- cor(Grupo_pos_NORMALIZED)

print(correlacoes_pos)

det(cor(Grupo_pos_NORMALIZED))

#det matriz de correlções= -2.533659e-19

#RESPOSTA: Sendo que determinante da matriz de correlações deve apresentar o valor mais proximo possível de 0, podemos verificar uma garantia de que as var's estão correlacionadas

#### 2.Avaliar o valor de KMO (Kaiser Meier Olkin) 

psych::KMO(Grupo_pos_NORMALIZED)  

##DEVE SER OBRIGATORIAMENTE SER SUP 0.5! E VARIA ENTRE 0 E 1.

##RESULTADO: Overall MSA =  0.5

##COMO ESTÁ AINDA SOBRE O LIMITE DE INTERVALO [0.5,1]! POSSO APLICAR UMA AFCP
##Quando as correlações estao equitativamente identicas (o montante de correlações das variaveis e dimensoes sao iguais) -> correlacionam-se igualmente como as variaveis

#### 3. Verificar os resultados do teste de bartlett

bartlett.test(Grupo_pos)

#testa se o determinante = 1 (H0)    se p.value<0.05 rejeitamos H0, logo det !=1 
#realizado teste utilizando as variáveis originais 

#RESULTADO: p-value < 2.2e-16 . 


##### GARANTIDADA A VALIDADE DA APLICABILIDADE DA AFCP 

#CONCLUSOES AFCP pré-cirurgia : teve qualidade - Reduzimos o espaço original, retemos 3 dimensões que representam 68.1% de informaçao retida no espaço acumulado.  

#### 10º Passo - Gráfico AFCP - Variaveis pos-cirurgia ####

## Analise dimensão 1 e 2

fviz_pca_biplot(res.pca_pos)                    #### Make a biplot of individuals and variables
fviz_pca_var(res.pca_pos, col.var = "blue")    #### Plotting just variables.

## Analise dimensão 1 e 3
fviz_pca_biplot(res.pca_pos, axes = c(1, 3))
fviz_pca_var(res.pca_pos, axes = c(1, 3), col.var = "darkgreen")

## Analise dimensão 2 e 3

fviz_pca_biplot(res.pca_pos, axes = c(2, 3))
fviz_pca_var(res.pca_pos, axes = c(2, 3), col.var = "purple")

## Analise dimensão 1 e 4
fviz_pca_biplot(res.pca_pos, axes = c(1, 4))
fviz_pca_var(res.pca_pos, axes = c(1, 4), col.var = "orange")

## Analise dimensão 2 e 4

fviz_pca_biplot(res.pca_pos, axes = c(2, 4))
fviz_pca_var(res.pca_pos, axes = c(2, 4), col.var = "#E75480")

## Analise dimensão 3 e 4

fviz_pca_biplot(res.pca_pos, axes = c(3, 4))
fviz_pca_var(res.pca_pos, axes = c(3, 4), col.var = "darkblue")


#Quanto maior os vetores, maior a variabilidade 

#### 11º Passo - ANALISE HIERQUICA DE CLUESTERS PARA O ESPAÇO DE DIMENSÃO REDUZIDA - Variaveis pós-curgia ####

res.hcpc_pos <- HCPC(res.pca_pos, graph = FALSE)

# Dendrograma - Variaveis pós-cirurgia 
fviz_dend(res.hcpc_pos, 
          cex = 0.7,                     
          palette = "jco",              
          rect = TRUE, rect_fill = TRUE, 
          rect_border = "jco",           
          labels_track_height = 0.8      
)

#fviz_dend(res.hcpc_pre) #outra forma de grafico 

#### 12 º Passo - Caracterização dos grupos de clusters da Base_dados - Variaveis pós-curgia ####

### Atribuir cada cluster a cada individuo na tabela de dados original 

clusters_pos<- res.hcpc_pos$data.clust$clust #defenir os clusters

Base_Dados_Clusters$cluster_pos<- clusters_pos

View (Base_Dados_Clusters)

### Saber quantos individuos tem cada cluster 

summary(Base_Dados_Clusters$cluster_pos)   


# Calcular as médias das variáveis por cluster 

medias_por_cluster_pos<- aggregate(. ~ clusters_pos, data = Base_Dados_Clusters, FUN = mean) 

View (medias_por_cluster_pos)

Base_Dados_clusters<-as.data.frame(cbind(Base_Dados,clusters_pre,clusters_pos))



summary_por_cluster_pos <- aggregate(. ~ clusters_pos, data = Base_Dados_Clusters, FUN = summary)

# Visualizar o resumo
View(summary_por_cluster_pos)


par(mfrow=c(2,2))

#Idade por cluster_pos- boxplot
boxplot(Idade  ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster")

# Sexo por cluster_pre - Grafico de barras de frequencias absolutas 
freq_table_sexo_pos <- table(Base_Dados_Clusters$sexo, Base_Dados_Clusters$cluster_pos)

legend_labels <- c("Feminino", "Masculino")

barplot(freq_table_sexo_pos, beside = TRUE, legend.text = legend_labels, main = "Frequência Absoluta por Cluster_pos/Sexo")

#Grupo_pre por cluster_pos - Gráfico de barras de frequências absolutas
freq_table_grupo_pre2 <- table(Base_Dados_Clusters$Grupo_pre, Base_Dados_Clusters$cluster_pos)

barplot(freq_table_grupo_pre2, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Grupo_pre/Cluster_pos")



#Grupo_pos por cluster_pos - Gráfico de barras de frequências absolutas
freq_table_grupo_pos2 <- table(Base_Dados_Clusters$Grupo_pos, Base_Dados_Clusters$cluster_pos)

barplot(freq_table_grupo_pos2, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Grupo_por/Cluster_pos")

#Peso por cluster_pos - boxplot
boxplot(Peso  ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#IMC por cluster_pos - boxplot
boxplot(IMC  ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster")

# EVA_0  por cluster pos- Gráfico de barras de frequências absolutas
freq_table_eva0 <- table(Base_Dados_Clusters$EVA_0, Base_Dados_Clusters$cluster_pos)

barplot(freq_table_eva0, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por EVA_0/Cluster_pos")


#F_0 por cluster_pos - boxplot
boxplot(F_0 ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#PM6_0 por cluster_pre - boxplot
boxplot(PM6_0 ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster")


#Boxplot WOMAC_pre por cluster_pos
par(mfrow=c(2,2))

selected_vars <- c("WR_0", "WD_0", "WA_0", "WT_0")

for (var in selected_vars) {
  boxplot(formula(paste(var, "~ cluster_pos")), data = Base_Dados_Clusters, main = var)
}

# EVA_90  por cluster pos- Gráfico de barras de frequências absolutas
freq_table_eva90_pos <- table(Base_Dados_Clusters$EVA_90, Base_Dados_Clusters$cluster_pos)

  barplot(freq_table_eva90, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por EVA90/Cluster_Pos")


#Boxplot WOMAC_pos por cluster_pos
par(mfrow=c(2,2))

selected_vars <- c("WR_90", "WD_90", "WA_90", "WT_90")

for (var in selected_vars) {
  boxplot(formula(paste(var, "~ cluster_pos")), data = Base_Dados_Clusters, main = var)
}

#F_90 por cluster_pos - boxplot
boxplot(F_90 ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster_pos")


#PM6_90 por cluster_pos - boxplot
boxplot(PM6_90 ~ clusters_pos, data = Base_Dados_Clusters, main="Boxplots por Cluster_pos")



# Satisfaçao por cluster_pos - Grafico de barras de frequencias absolutas 
freq_table_satisfacao <- table(Base_Dados_Clusters$Satisfação, Base_Dados_Clusters$cluster_pos)

barplot(freq_table_satisfacao, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por Satisfaçao/Cluster_pos")

View(Base_Dados_Clusters)



#Clusters pre/cluster pos -> grafico de frequencias relativas
freq_table_cluster_pre_pos <- table(Base_Dados_Clusters$cluster_pre, Base_Dados_Clusters$cluster_pos)
cores <- c("darkred", "orange", "yellow")
barplot(freq_table_cluster_pre_pos, beside = TRUE, legend.text = TRUE, col = cores, main = "Frequência Relativa por Cluster_pre/cluster_pos",
        names.arg = paste("Cluster_pre", sort(unique(Base_Dados_Clusters$cluster_pre))))
legend("topright", legend = c("Cluster_pos_1", "Cluster_pos_2", "Cluster_pos_3"), fill = cores)


#Clusters pre/cluster pos -> grafico de frequencias relativas
freq_table_cluster_pre_pos_rel <- prop.table(table(Base_Dados_Clusters$cluster_pre, Base_Dados_Clusters$cluster_pos), margin = 1)

cores <- c("darkred", "orange", "yellow")

barplot(freq_table_cluster_pre_pos_rel, beside = TRUE, legend.text = TRUE, col = cores, main = "Frequência Relativa por Cluster_pre/cluster_pos",
        names.arg = paste("Cluster_pre", sort(unique(Base_Dados_Clusters$cluster_pre))))

legend("topright", legend = c("Cluster_pos_1", "Cluster_pos_2", "Cluster_pos_3"), fill = cores)

#Clustes pos por cluster_pre - Grafico de barras de frequencias absolutas 
freq_table_cluster_pos_pre <- table(Base_Dados_Clusters$cluster_pos, Base_Dados_Clusters$cluster_pre)

barplot(freq_table_cluster_pos_pre, beside = TRUE, legend.text = TRUE, main = "Frequência Absoluta por CLuster_pos/Cluster_pre")



######## alinea H - Teste de Hipoteses #######
#Para utentes com uma dor pós-cirurgia superior ou igual a 3 segundo a escala EVA_90, pode-se afirmar que o valor médio de satisfação é inferior a 7?

# Selecionando os indivíduos com EVA_90 maior ou igual a 5
Amostra_Dor3 <- Base_Dados[Base_Dados$EVA_90 >= 3, ]

#Avaliar a dimensão da amostra 
nrow(Amostra_Dor3)

# = 35 indivíduos 


#Formulação das hipóteses: 
#HO: valor médio da satistação = 7
#H1: valor médio da satisfação < 7

#Condições de aplicabilidade 
# X v.a.: nível de satisfação 
# N = 35 
# Distribição ~ Qualquer 
# Desvio padrão da população desconhecido 
# Nível de Significância – 0,05 


resultado_teste <- t.test(Amostra_Dor3$Satisfação, mu = 7, alternative = "less")

# Exibir o resultado do teste
print(resultado_teste)

