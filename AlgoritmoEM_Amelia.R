require(tidyverse)
require(readxl)
require(visdat)

Portos <- read_excel("Tabela_para_R2.xlsx", 
                     sheet = "dados sem NA")
View(Portos)


Portos = mutate(Portos, 
                `Ano(anuário)` = as.numeric(`Ano(anuário)`))

vis_dat(Portos %>% 
          mutate(`Tempo médio de escala (h/vio)` = as.numeric(`Tempo médio de escala (h/vio)`)),
        sort_type = FALSE, palette = "default")
vis_miss(Portos)

table(is.na(Portos$`vios de cruzeiro (por ano)`))

# Contar o numero de missings e criar uma nova coluna
Portos$na_count <- apply(Portos, 1, function(x) sum(is.na(x)))
Portos = mutate(Portos, missing_pct = (na_count/(dim(Portos)[2]-2))*100)

exportar = select(Portos, Unidade,na_count,missing_pct)
write_csv2(exportar,"Contagem_missing.csv")

# Perfil dessa nova coluna
summary(Portos$na_count)
hist(Portos$na_count, col="red")


nomes<-c("Complexo",
         "Unidade" ,
         "Volume_transportado"      ,
         "navios_de_cruzeiro",
         "navios " ,
         "Numero_de_Passageiros",
         "Tamanho_do_porto" ,
         "Tempo_medio_de_escala"          ,
         "Funcionarios"   ,
         "Gastos_Totais"        ,
         "Arrecadacao"          ,
         "Receita_total"        ,
         "Receita_Bruta_tonelada"            ,
         "Gastos_tonelada"      ,
         "Anos"    ,
         "Demanda_de_acesso"  ,
         "Capacidade_de_acesso"          ,
         "Ano"   ,
         "Complexo Portuário"        ,
         "UF_Instalacao"  ,
         "Total_de Atracacoes."      ,
         "Tempo_Medio_para_Atracacao"    ,
         "Tempo_Medio_para_Inicio"        ,
         "Tempo_Medio_de_Operacao"       ,
         "Tempo_Medio_para_Desatracacao" ,
         "Tempo_Medio_Atracado"          ,
         "Tempo_Medio_de_Estadia"        ,
         "Receita_Tarifaria_Media",
         "Autoridade_Portuaria"   ,
         "tipo"    ,
         "classe"  ,
         "na_count",
         "missing_pct")


Portos2 <- Portos[Portos$na_count<=15, ]
colnames(Portos2)<-nomes
Portos2 = select(Portos2,-na_count,-missing_pct)

row.names(Portos2)<-Portos2$Unidade
missmap(Portos2, x.cex = 0.6,rank.order = FALSE)

vis_miss(Portos2)

# Remove duplicate rows of the dataframe
Portos3 <- distinct(Portos2,`Volume transportado(em toneladas)`, .keep_all = TRUE)

grafico_missing<-vis_miss(Portos3)

#---------------------------------------------------------
names(Portos3)

variaveis_deletadas<-c("Complexo", "Número de Passageiros (por ano)", "tipo",
                       "Demanda_de_acesso", "Complexo Portuário", "Ano",
                       "UF_Instalacao", "Autoridade_Portuaria" ,"classe")

Portos4<- Portos3 %>%
  select(-c("Complexo", "Número de Passageiros (por ano)", "tipo",
            "Demanda de acesso (vios)", "Complexo Portuário", "Ano(anuário)",
            "UF Instalação", "Autoridade Portuariária" ,"classe"))

row.names(Portos4)<-Portos4$Unidade
Portos4<- Portos4 %>%
  select(-"Unidade")
vis_miss(Portos4)

Portos4$Tempo_medio_de_escala = as.numeric(Portos4$`Tempo médio de escala (h/vio)`)

#### Aplicando Amelia

library(Amelia)
library(missForest)

#Removendo as variaveis com mais de 60 porcento de NA
Portos4 = select(Portos4, -c(`vios de cruzeiro (por ano)`,`Arrecadação (R$)`))
missmap(Portos4, rank.order = FALSE, x.cex = 0.6)
vis_miss(Portos4)

Portos4 = mutate_if(Portos4,is.character,as.numeric)

dados_faltantes = amelia(Portos4[,2:10])

aux = as.matrix(Portos4[,-1])

faltantes1 = amelia(aux)


########

Portos4<- Portos3 %>%
  select(-c("Complexo", 
            "Complexo Portuário",
            "Ano(anuário)",
            "UF Instalação",
            "Autoridade Portuariária", 
            "classe"))
missmap(Portos4, rank.order = FALSE, x.cex = 0.6)

#Removendo as variaveis com mais de 60 porcento de NA
Portos4 = select(Portos4, -c( "Número de Passageiros (por ano)",
                              "tipo",
                              "Demanda de acesso (vios)",
                              `vios de cruzeiro (por ano)`,
                              `Arrecadação (R$)`))
missmap(Portos4, rank.order = FALSE, x.cex = 0.6)



#Aplicando Amelia

require(Amelia)
dados = amelia(Portos1)
imputacao = (dados_faltantes[["imputations"]][["imp1"]]+dados_faltantes[["imputations"]][["imp2"]]+
               dados_faltantes[["imputations"]][["imp3"]] + dados_faltantes[["imputations"]][["imp4"]]+
               dados_faltantes[["imputations"]][["imp5"]])/5

