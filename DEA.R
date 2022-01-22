load("C:/Users/Steven/Documents/GitHub/Portos/dados trabalhados/dados_portos.RData")

names(dados_portos)
summary(dados_portos)

dados_portos$Tempo_Medio=
  dados_portos$Tempo_Medio_para_Atracacao +  
  dados_portos$Tempo_Medio_para_Inicio      +
  dados_portos$Tempo_Medio_de_Operacao      +
  dados_portos$Tempo_Medio_para_Desatracacao+
  dados_portos$Tempo_Medio_Atracado         +
  dados_portos$Tempo_Medio_de_Estadia       

#----------------------------------------------------------------------------------------
#variaveis que devem sair da análise:
#  nomes,
#  Anos,
# "Receita_Bruta_tonelada"       
# "Gastos_tonelada"              
#"Capacidade_de_acesso"         
#"Receita_Tarifaria_Media"  
#"Tempo_Medio_para_Atracacao"   
#"Tempo_Medio_para_Inicio"      
#"Tempo_Medio_de_Operacao"      
#"Tempo_Medio_para_Desatracacao"
#"Tempo_Medio_Atracado"         
#"Tempo_Medio_de_Estadia"       

#----------------------------------------------------------------------------------------
#variaveis de output
"Volume_transportado"          
"Total_de_Atracacoes"          
"Receita_total"

#----------------------------------------------------------------------------------------
#variaveis de input
#"navios"                       
#"Tamanho_do_porto"             
#"Tempo_medio_de_escala"        
#"Funcionarios"                 
#"Gastos_Totais"                
# Tempo_Medio                


input<-dados_portos[,c("navios","Tamanho_do_porto","Tempo_medio_de_escala",        
                       "Funcionarios", "Gastos_Totais")]                


output<-dados_portos[,c("Volume_transportado",          
                        "Total_de_Atracacoes",          
                        "Receita_total")]



library(corrplot)
corrplot(cor(input), order = "hclust")
corrplot(cor(output), order = "hclust")


require(Benchmarking)
require(boot)

e1 <- dea(input,output, RTS="vrs", ORIENTATION="out")
eff(e1)
eficiencia_vrs_out<-(1/eff(e1))
eficiencia_vrs_out


e2 <- dea(input,output, RTS="vrs", ORIENTATION="in")
eff(e2)
#eficiencia2<-(1/eff(e2))
eficiencia_vrs_in<-eff(e2)
eficiencia_vrs_in

f1 <- dea(input,output, RTS="crs", ORIENTATION="out")
eficiencia_crs_out<-(1/eff(f1))
eficiencia_crs_out

f2 <- dea(input,output, RTS="crs", ORIENTATION="in")
eficiencia_crs_in<-eff(f2)
eficiencia_crs_in

input<-as.matrix(input)
output<-as.matrix(output)
z<-dea.boot(input,output, NREP = 2000, EFF = NULL, RTS = "vrs", ORIENTATION="out",
            alpha = 0.05, XREF = NULL, YREF = NULL, EREF = NULL, DIRECT = NULL, TRANSPOSE = FALSE, LP = FALSE)
eff(z)
eficiencia_boot<-(1/eff(z))
eficiencia_boot
plot(eficiencia_boot)
eff.dens.plot(eficiencia_boot)

#------------------------------------------------------------------------------
BOOT <- unlist(z)
boxplot(BOOT,col="tomato4")
hist(BOOT,xlim=c(-1,4), scale="frequency",   breaks="Sturges", col="royalblue1")
library(RcmdrMisc)
numSummary(BOOT, statistics=c("mean", "se(mean)", "cv"))

#------------------------------------------------------------------------------------------------------------------------------------
# RESULTADO 1
#------------------------------------------------------------------------------------------------------------------------------------

eficiencias<-data.frame(eficiencia_vrs_out,eficiencia_vrs_in,eficiencia_crs_out,eficiencia_crs_in,eficiencia_boot)

#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------------------------------

intervalo<-z[["conf.int"]]
colnames(intervalo)<-c("inferior","superior")
intervalo<-data.frame(intervalo)
bias<-z[["bias"]]
bias<-data.frame(bias)
eff.bc<-z[["eff.bc"]]
eff.bc<-data.frame(eff.bc)
LS<-1/(intervalo$superior+bias)  
LI<-1/(intervalo$inferior+bias)
eff2<-1/(eff.bc+bias)

IC<-data.frame(eff2,LS,LI)
colnames(IC)<-c("eff","LI","LS")
rownames(IC)<-dados_portos$nomes
ICnomes<-dados_portos$nomes

eficiencias<-data.frame(eficiencias,IC)
eficiencias<-eficiencias[,-6]
save(eficiencias,file = "C:/Users/Steven/Documents/GitHub/Portos/dados trabalhados/eficiencias.RData")


#------------------------------------------------------------------------------------------------------------------------------------
# RESULTADO 2
#------------------------------------------------------------------------------------------------------------------------------------


# plota IC
par(mar=c(15,4,1,1))
plot(1:31,col=0,ylim=c(0,1.3),axes=F,xlab=" ",ylab="Eficiência",main=" ")
axis(2)
axis(1,1:31,ICnomes,las=2)
abline(v=seq(1,31,1),lty=2,col="gray")  

for(i in 1:31){
  segments(i,IC$LS[i],i,IC$LI[i])
  points(i,IC$eff[i],pch=19,cex=1.5)
  if (IC$eff[i] < 1){
    points(i,IC$eff[i],pch=19,cex=1.5,col=2)
  }
}


