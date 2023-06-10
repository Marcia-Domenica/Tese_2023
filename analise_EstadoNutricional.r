library(readxl)
library(dplyr)
#------------------------------------------------------------------
#            Análise do estado nutricional
#----------------------------------------------------------------
#Alterar base de dados -FEMININO ou MASCULINO, PR,SC ouRS

dados<-read_excel("EN_idososSC.xlsx",
                        sheet='ambos', col_names = T)  

estadosNutricionais<-names(dados)[5:7]; estadosNutricionais


sexo<-c("ambos") #alterar sexo
sexo<-c("masculino")
estado<-c("SC")  #alterar estado

#----------------
ind<-1  #baixo peso

EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 


estadosNutricionais[ind]   
#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(`Baixo peso`))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}


TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                    variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB1<-TabelaFinal

#----------------
ind<-2  #eutrofia
EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 

estadosNutricionais[ind]   
#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(Eutrofia))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}


TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                   variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB2<-TabelaFinal
#----------------
ind<-3 # excesso de peso

EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 

estadosNutricionais[ind]   

#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(`Excesso de peso`))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}



TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                   variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB3<-TabelaFinal

#Tabela final
getwd()
TB<-rbind(TB1,TB2,TB3)

sexo;estado
write.table(TB,file="TabelaFinalASC.csv",sep=";",quote=F,dec=",",row.names=F)
#-----------------------------------------------------------

#----------------------------------------------------------------
#                análise estado nutricional
#-------------------------------------------------------------
library(readxl)
library(dplyr)
#Alterar base de dados -FEMININO ou MASCULINO, PR,SC ouRS

dados<-read_excel("Crianca0a6meses_RS.xlsx",
                  sheet='masculino', col_names = T)  
names(dados)
estadosNutricionais<-names(dados)[6]; estadosNutricionais


sexo<-c("feminino") #alterar sexo
sexo<-c("masculino")
estado<-c("RS")  #alterar estado

#----------------
ind<-1  #baixo peso

EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 


estadosNutricionais[ind]   
#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(`Aleitamento materno exclusivo em menores de 6 meses`))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}


TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                   variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB1<-TabelaFinal

#----------------
ind<-2  #eutrofia
EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 

estadosNutricionais[ind]   
#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(Eutrofia))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}


TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                   variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB2<-TabelaFinal
#----------------
ind<-3 # excesso de peso

EN1<-dados[,c(estadosNutricionais[ind], "Total", "regional","ano")] 

estadosNutricionais[ind]   

#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(`Excesso de peso`))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(Total)) 
resumo3 <-cbind(resumo1,resumo2$Total)
names(resumo3)[4]<-c("Total")


prop<-c()
for(i in 1:dim(resumo3)[1])
  ifelse(resumo3$Total[i]==0, prop[i]<-0 , prop[i]<-resumo3$prevalencia[i]/resumo3$Total[i])

resumo3$prop<-prop


regionais<-unique(resumo3$regional)

regional_i<-matrix(nrow=7, ncol=length(regionais))

for(i in 1:length(regionais))
  regional_i[,i]<-subset(resumo3, regional==regionais[i])$prop

variacao<-c()
pvalor<-c()
for(i in 1:dim(regional_i)[2])
{
  Temporal<-ts((regional_i[,i]), start = 2015,
               end = 2021,frequency = 1)
  tempo <- time(Temporal)
  modelo <- lm(Temporal ~ tempo) #modelo de tendência linear
  variacao[i]<-summary(modelo)$coefficients[2,1] #variação média anual
  pvalor[i]<-summary(modelo)$coefficients[2,4] #p-valor
}



TabelaFinal<-cbind(regionais,rep(estadosNutricionais[ind],length(regionais)),as.data.frame(t(regional_i*100)),
                   variacao*100,pvalor, rep(sexo, length(regionais)),
                   rep(estado, length(regionais))) 

TB3<-TabelaFinal

#Tabela final
getwd()
TB<-rbind(TB1,TB2,TB3)

sexo;estado
write.table(TB1,file="TabelaFinalMRS.csv",sep=";",quote=F,dec=",",row.names=F)
