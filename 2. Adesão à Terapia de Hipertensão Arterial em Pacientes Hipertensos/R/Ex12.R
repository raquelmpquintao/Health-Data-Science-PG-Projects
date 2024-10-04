# 12. Utilizando a base de dados de que dispõe proceda a uma análise estatística suplementar elaborando 
# 3 questões e tire conclusões. 

# 12.1) Compare o custo médio da primeira receita entre tipos de hipertensão e represente-o graficamente.

# ICPC: código da hipertensão: K86 – hipertensão sem complicações (1) / 
# K87 – hipertensão com complicações (2) #Tem valores omissos

HTA_K86<-subset(amostra, ICPC == 1)
HTA_K87<-subset(amostra, ICPC == 2) 

HTA_K86_media<- mean(HTA_K86$custo_inicial, na.rm=TRUE)
HTA_K87_media<- mean(HTA_K87$custo_inicial, na.rm=TRUE)

print(HTA_K86_media)
print(HTA_K87_media)

medias <- c(HTA_K86_media, HTA_K87_media)
nomes_barras <- c("Sem complicações", "Com complicações")

medias_HTA_barplot<-barplot(medias, 
                            names.arg = nomes_barras,
                            main = "Custo inicial médio por tipos de hipertensão",
                            xlab = "Tipo de Hipertensão",
                            ylab = "Custo Inicial Médio (€)",
                            col = c("forestgreen","lightgreen"),
                            ylim=c(0,6))



# 12.2) Verifique se existem diferenças significativas entre o valor médio do custo da 1ª receita 
#e o custo da receita 24 meses depois.

length(amostra$custo_inicial) # n = 101
length(amostra$custo_24) # n = 101

# n > 30
# σ desconhecido
# Nível de significância
nivel_significancia <- 0.05

resultado_teste_12 <- t.test(amostra$custo_inicial, amostra$custo_24, paired=TRUE)
print(resultado_teste_12)
valor_p_12 <- resultado_teste_12$p.value

cat("Valor p:", resultado_teste_12$p.value, "\n")

if (valor_p_12 < nivel_significancia) {
  print("Há diferença significativa entre o custo médio da 1ª receita e o custo médio da receita 24 meses depois.")
} else {
  print("Não podemos afirmar que há diferença significativa entre o custo médio da 1ª receita e o custo médio da receita 24 meses depois.")
}

# 12.3) Verifique se existe associação entre o sexo e o início do tratamento e tire conclusões.

#  2 variáveis qualitativas nominais dicotómicas - Phi

library(vcd)   #biblioteca necessária para o phi
# sexo - Masculino 1, Feminino 2
# inicio: Doente inicia tratamento (sim (1) ou não (0))

sexo_inicio_table <- table(amostra$sexo, amostra$inicio) # tabela freq. absoluta
sexo_inicio_freqrel <- prop.table(sexo_inicio_table) # tabela freq. relativa

print(sexo_inicio_table)
print(sexo_inicio_freqrel)

assocstats(sexo_inicio_table) # pouca ou nenhuma associação
