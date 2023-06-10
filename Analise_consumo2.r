#----------------------------------------------------------------
#                análise do CONSUMO 2
#-------------------------------------------------------------
library(readxl)
library(dplyr)
#Alterar base de dados -FEMININO ou MASCULINO

dadosH3R<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='H3R', col_names = T)  

names(dadosH3R)

#----------------
#3 refeições

EN1<-dadosH3R[,c("sucesso", "total", "regional","ano")] 

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


TB1<-cbind(regionais,rep(dadosH3R$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosH3R$sexo[1], length(regionais)),
           rep(dadosH3R$UF[1], length(regionais))) 
names(TB1)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosH3R$consumo[1:dim(resumo3)[1]]
r1<-resumo3
#----------------
dadosATV<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='ATV', col_names = T)  
names(dadosATV)

#----------------
#assiste TV


EN2<-dadosATV[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN2,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN2,regional, ano)%>%summarise(Total=sum(total)) 
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


TB2<-cbind(regionais,rep(dadosATV$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosATV$sexo[1], length(regionais)),
           rep(dadosATV$UF[1], length(regionais))) 
names(TB2)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosATV$consumo[1:dim(resumo3)[1]]
r2<-resumo3
#----------------
dadosCFE<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CFE', col_names = T)  
names(dadosCFE)

#----------------
#Feijão


EN3<-dadosCFE[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN3,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN3,regional, ano)%>%summarise(Total=sum(total)) 
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


TB3<-cbind(regionais,rep(dadosCFE$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCFE$sexo[1], length(regionais)),
           rep(dadosCFE$UF[1], length(regionais))) 
names(TB3)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCFE$consumo[1:dim(resumo3)[1]]
r3<-resumo3
#----------------
dadosCFR<-read_excel("Consumo_idoso_SC_A.xlsx",
                      sheet='CFR', col_names = T)  
names(dadosCFR)

#----------------
#Consumo de Frutas


EN4<-dadosCFR[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN4,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN4,regional, ano)%>%summarise(Total=sum(total)) 
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


TB4<-cbind(regionais,rep(dadosCFR$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCFR$sexo[1], length(regionais)),
           rep(dadosCFR$UF[1], length(regionais))) 
names(TB4)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCFR$consumo[1:dim(resumo3)[1]]
r4<-resumo3
#----------------
dadosCVL<-read_excel("Consumo_idoso_SC_A.xlsx",
                      sheet='CVL', col_names = T)  
names(dadosCVL)

#----------------
#Consumo de legumes

EN5<-dadosCVL[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN5,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN5,regional, ano)%>%summarise(Total=sum(total)) 
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


TB5<-cbind(regionais,rep(dadosCVL$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCVL$sexo[1], length(regionais)),
           rep(dadosCVL$UF[1], length(regionais))) 
names(TB5)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCVL$consumo[1:dim(resumo3)[1]]
r5<-resumo3
#----------------
dadosCAU<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CAU', col_names = T)  
names(dadosCAU)

#----------------
#Consumo de Alimentos Ultraprocessados

EN6<-dadosCAU[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN6,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN6,regional, ano)%>%summarise(Total=sum(total)) 
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


TB6<-cbind(regionais,rep(dadosCAU$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCAU$sexo[1], length(regionais)),
           rep(dadosCAU$UF[1], length(regionais))) 
names(TB6)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCAU$consumo[1:dim(resumo3)[1]]
r6<-resumo3
#----------------
dadosCHE<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CHE', col_names = T)  
names(dadosCHE)

#----------------
#Consumo de Hambúrguer e/ou Embutidos

EN7<-dadosCHE[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN7,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN7,regional, ano)%>%summarise(Total=sum(total)) 
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


TB7<-cbind(regionais,rep(dadosCHE$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCHE$sexo[1], length(regionais)),
           rep(dadosCHE$UF[1], length(regionais))) 
names(TB7)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCHE$consumo[1:dim(resumo3)[1]]
r7<-resumo3

#----------------
dadosCBA<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CBA', col_names = T)  
names(dadosCBA)

#----------------
#Consumo de Bebidas Adoçadas


EN8<-dadosCBA[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN8,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN8,regional, ano)%>%summarise(Total=sum(total)) 
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


TB8<-cbind(regionais,rep(dadosCBA$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCBA$sexo[1], length(regionais)),
           rep(dadosCBA$UF[1], length(regionais)))
names(TB8)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCBA$consumo[1:dim(resumo3)[1]]
r8<-resumo3

#----------------
dadosCMI<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CMI', col_names = T)  
names(dadosCMI)

#----------------
#Consumo de Macarrão Instantâneo, Salgadinhos de Pacote ou Biscoitos Salgados

EN9<-dadosCMI[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN9,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN9,regional, ano)%>%summarise(Total=sum(total)) 
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


TB9<-cbind(regionais,rep(dadosCMI$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCMI$sexo[1], length(regionais)),
           rep(dadosCMI$UF[1], length(regionais)))
names(TB9)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCMI$consumo[1:dim(resumo3)[1]]
r9<-resumo3

#----------------
dadosCBR<-read_excel("Consumo_idoso_SC_A.xlsx",
                     sheet='CBD', col_names = T)  
names(dadosCBR)

#----------------
#Consumo de Biscoito Recheado, Doces ou Guloseimas

EN10<-dadosCBR[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN10,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN10,regional, ano)%>%summarise(Total=sum(total)) 
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


TB10<-cbind(regionais,rep(dadosCBR$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
            variacao*100,pvalor, rep(dadosCBR$sexo[1], length(regionais)),
            rep(dadosCBR$UF[1], length(regionais)))
names(TB10)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCBR$consumo[1:dim(resumo3)[1]]
r10<-resumo3
#-------------------------------------
dadosCANM<-rbind(dadosCFE, dadosCFR, dadosCVL)

#----------------
#Consumo de alimentos in natura e minimamente processados


EN11<-dadosCANM[,c("sucesso", "total", "regional","ano")] 

#resumo consumo
resumo1<-group_by(EN11,regional, ano)%>%summarise(prevalencia=sum(sucesso))
resumo2<-group_by(EN11,regional, ano)%>%summarise(Total=sum(total)) 
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


TB11<-cbind(regionais,rep(c('Consumo de alimentos in natura e minimamente processados'),length(regionais)),as.data.frame(t(regional_i*100)),
            variacao*100,pvalor, rep(dadosCANM$sexo[1], length(regionais)),
            rep(dadosCANM$UF[1], length(regionais)))
names(TB11)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-rep('Consumo de alimentos in natura e minimamente processados',dim(resumo3)[1])
r11<-resumo3

#---------------------------------------------------------
#Tabela final
getwd()
TB<-rbind(TB1,TB2,TB3,TB4,TB5,TB6,TB7,TB8,TB9,TB10,TB11)
names(TB1)
dadosCBR$sexo[1];dadosCBR$UF[1]
write.table(TB,file="TabelaFinalASC.csv",sep=";",quote=F,dec=",",row.names=F)
TR<-rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11)
write.table(TR,file="TabelaAuxASC.csv",sep=";",quote=F,dec=",",row.names=F)

