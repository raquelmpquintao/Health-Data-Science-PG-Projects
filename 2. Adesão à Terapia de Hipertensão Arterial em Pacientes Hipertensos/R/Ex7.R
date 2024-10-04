# 7. Analise as variáveis descontinua, persist_6m, persist_24m, descontinua2, 
# recorrendo a estatísticas e/ou gráficos e conclua sobre a adesão à terapêutica dos pacientes.

# Descontinua: O doente descontinua prematuramente: 0=não, 1=sim 

# Estatísticas Descritivas
descontinua_summary <- table(amostra$descontinua) # frequência absoluta
descontinua_freqrel<-prop.table(descontinua_summary)*100 # frequência relativa
descontinua_moda <- as.numeric(names(table(amostra$descontinua))[which.max(table(amostra$descontinua))]) # moda

# Gráfico de barras

legenda_7<-c("Não", "Sim")
descontinua_barplot <- barplot(descontinua_freqrel, 
                        col = c("navyblue","lightskyblue"),
                        names.arg = legenda_7,
                        main = "O doente descontinua prematuramente o tratamento?",
                        ylab = "% de doentes",
                        ylim=c(0,100))

# Exibir Gráficos e Estatísticas
print("Descontinua")
print(descontinua_summary)
print(descontinua_freqrel)
print(paste("Moda:",descontinua_moda)) #Moda é zero, logo "Não"
print(descontinua_barplot)


# Persist_6m: persistência ao fim de 6 meses: 0=não, 1=sim 

# Estatísticas Descritivas
persist_6m_summary <- table(amostra$persist_6m) # frequência absoluta
persist_6m_freqrel<-prop.table(persist_6m_summary)*100 # frequência relativa
persist_6m_moda <- as.numeric(names(table(amostra$persist_6m))[which.max(table(amostra$persist_6m))]) # moda

# Gráfico de barras

legenda_7<-c("Não", "Sim")
persist_6m_barplot <- barplot(persist_6m_freqrel, 
                              col = c("navyblue","lightskyblue"),
                               names.arg = legenda_7,
                               main = "O doente mantém o tratamento ao fim de 6 meses?",
                               ylab = "% de doentes",
                               ylim=c(0,100))

# Exibir Gráficos e Estatísticas

print(persist_6m_summary)
print(persist_6m_freqrel)
print(paste("Moda:",persist_6m_moda)) # Moda é 1, logo "Sim"
print(persist_6m_barplot)

# Persist_24m: persistência ao fim de 24 meses: 0=não, 1=sim 

# Estatísticas Descritivas
persist_24m_summary <- table(amostra$persist_24m) # frequência absoluta
persist_24m_freqrel<-prop.table(persist_24m_summary)*100 # frequência relativa
persist_24m_moda <- as.numeric(names(table(amostra$persist_24m))[which.max(table(amostra$persist_24m))]) # moda

# Gráfico de barras

legenda_7<-c("Não", "Sim")
persist_24m_barplot <- barplot(persist_24m_freqrel, 
                              col = c("navyblue","lightskyblue"),
                              names.arg = legenda_7,
                              main = "O doente mantém o tratamento ao fim de 24 meses?",
                              ylab = "% de doentes",
                              ylim=c(0,100))

# Exibir Gráficos e Estatísticas

print(persist_24m_summary)
print(persist_24m_freqrel)
print(paste("Moda:",persist_24m_moda)) # Moda é 1, logo "Sim"
print(persist_24m_barplot)


# Descontinua2: doente descontinua ao fim de 2 anos: 0=não, 1=sim 


# Estatísticas Descritivas
descontinua2_summary <- table(amostra$descontinua2) # frequência absoluta
descontinua2_freqrel<-prop.table(descontinua2_summary)*100 # frequência relativa
descontinua2_moda <- as.numeric(names(table(amostra$descontinua2))[which.max(table(amostra$descontinua2))]) # moda

# Gráfico de barras

legenda_7<-c("Não", "Sim")
descontinua2_barplot <- barplot(descontinua2_freqrel, 
                               col = c("navyblue","lightskyblue"),
                               names.arg = legenda_7,
                               main = "O doente descontinua o tratamento ao fim de dois anos?",
                               ylab = "% de doentes",
                               ylim=c(0,100))

# Exibir Gráficos e Estatísticas
print("Descontinua2")
print(descontinua2_summary)
print(descontinua2_freqrel)
print(paste("Moda:",descontinua2_moda)) #Moda é zero, logo "Não"
print(descontinua2_barplot)


