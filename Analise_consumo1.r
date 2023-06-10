#----------------------------------------------------------------
#                análise do CONSUMO
#-------------------------------------------------------------
library(readxl)
library(dplyr)
#Alterar base de dados -FEMININO ou MASCULINO, PR,SC ouRS

dadosAMC<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
                  sheet='AMC', col_names = T)  
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
#----------------
dadosDAM<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
sheet='DAM', col_names = T)  
names(dadosDAM)

#----------------
#Diversidade Alimentar Mínima


EN2<-dadosDAM[,c("sucesso", "total", "regional","ano")] 

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


TB2<-cbind(regionais,rep(dadosDAM$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosDAM$sexo[1], length(regionais)),
           rep(dadosDAM$UF[1], length(regionais))) 
names(TB2)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosDAM$consumo[1:dim(resumo3)[1]]
r2<-resumo3
#----------------
dadosFMC<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
                     sheet='FMC', col_names = T)  
names(dadosFMC)

#----------------
#Frequência Mínima e Consistência Adequada

EN3<-dadosFMC[,c("sucesso", "total", "regional","ano")] 

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

TB3<-cbind(regionais,rep(dadosFMC$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosFMC$sexo[1], length(regionais)),
           rep(dadosFMC$UF[1], length(regionais))) 
names(TB3)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosFMC$consumo[1:dim(resumo3)[1]]
r3<-resumo3
#----------------
dadosCARF<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
                     sheet='CARF', col_names = T)  
names(dadosCARF)

#----------------
#Consumo de Alimentos Ricos em Ferro

EN4<-dadosCARF[,c("sucesso", "total", "regional","ano")] 

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


TB4<-cbind(regionais,rep(dadosCARF$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCARF$sexo[1], length(regionais)),
           rep(dadosCARF$UF[1], length(regionais))) 
names(TB4)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCARF$consumo[1:dim(resumo3)[1]]
r4<-resumo3
#----------------
dadosCARV<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
                      sheet='CARV', col_names = T)  
names(dadosCARV)

#----------------
#Consumo de Alimentos Ricos em Vitamina A

EN5<-dadosCARV[,c("sucesso", "total", "regional","ano")] 

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


TB5<-cbind(regionais,rep(dadosCARV$consumo[1],length(regionais)),as.data.frame(t(regional_i*100)),
           variacao*100,pvalor, rep(dadosCARV$sexo[1], length(regionais)),
           rep(dadosCARV$UF[1], length(regionais))) 
names(TB5)<-c('regional','consumo','p2015','p2016','p2017','p2018','p2019','p2020','p2021','var','pvalor','sexo','uF')

flag<-ifelse(resumo3$prevalencia==0 & resumo3$Total==0, 'excluir','')
resumo3$flag<-flag
resumo3$consumo<-dadosCARV$consumo[1:dim(resumo3)[1]]
r5<-resumo3
#----------------
dadosCAU<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
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
dadosCHE<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
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
dadosCBA<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
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
dadosCMI<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
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
dadosCBR<-read_excel("Consumo_Crianca6a23meses_RS_A.xlsx",
                     sheet='CBR', col_names = T)  
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
dadosCANM<-rbind(dadosAMC, dadosCARF, dadosCARV, dadosDAM,dadosFMC)

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
write.table(TB,file="TabelaFinalARS.csv",sep=";",quote=F,dec=",",row.names=F)
TR<-rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11)
write.table(TR,file="TabelaAuxARS.csv",sep=";",quote=F,dec=",",row.names=F)
