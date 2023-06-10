library(readxl)
library(dplyr)
#Alterar base de dados -FEMININO ou MASCULINO, PR,SC ouRS

dadosAMC<-read_excel("Consumo_Crianca0a6meses_SC.xlsx",
                     sheet='ambos', col_names = T)  
names(dadosAMC)
#----------------
#Aleitamento Materno Continuado

EN1<-dadosAMC[,c("sucesso", "total", "regional","ano")] 

#mudar estado nutricional
resumo1<-group_by(EN1,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN1,regional, ano)%>%summarise(Total=sum(total)) 
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

TB1<-cbind(regionais,rep(dadosAMC$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosAMC$sexo[1], length(regionais)),
           rep(dadosAMC$UF[1], length(regionais))) 
names(TB1)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosAMC$consumo[1:dim(resumo3)[1]]
r1<-resumo3

#Tabela final
getwd()
TB<-TB1
names(TB1)
TB$sexo[1];TB$uF[1]
write.table(TB,file="TabelaFinalASC.csv",sep=";",quote=F,dec=",",row.names=F)
TR<-r1
write.table(TR,file="TabelaAuxASC.csv",sep=";",quote=F,dec=",",row.names=F)
