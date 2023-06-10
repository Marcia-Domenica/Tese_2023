library(readxl)
library(ggplot2)

dados<-read_excel("graficos_PorEstadoNutricional.xlsx",
                     sheet=1, col_names = T) 
#-----------------------------------------------------
#Graficos de linha por faixa de idade e estado
#-----------------------------------------------------
table(dados$`Estado nutricional`)

#mudar estado nutricional
en<-subset(dados, dados$`Estado nutricional`=='Excesso de peso')

#----PR
pr<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO PARANÁ'))
pr1<-reshape(pr, direction = 'long', varying = 3:9,idvar='Fase da vida',
             v.names='Porcentagem',times=2015:2021)

pr1$nivel<-factor(pr1$`Fase da vida`, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=pr1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Excesso de peso no estado do Paraná")+
  theme(plot.title = element_text(size=10, hjust= 1.1))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("cyan","violetred2","grey30","orange1","blue","red","green"))
#colors() 
#scale_colour_brewer(palette = "Set1")

#----RS
rs<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO RIO GRANDE DO SUL'))
rs1<-reshape(rs, direction = 'long', varying = 3:9,idvar='Fase da vida',
             v.names='Porcentagem',times=2015:2021)

rs1$nivel<-factor(rs1$`Fase da vida`, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=rs1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Excesso de peso no estado do Rio Grande do Sul")+
  theme(plot.title = element_text(size=10, hjust= 2.3))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("cyan","violetred2","grey30","orange1","blue","red","green"))

#----SC
sc<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO SANTA CATARINA'))
sc1<-reshape(sc, direction = 'long', varying = 3:9,idvar='Fase da vida',
             v.names='Porcentagem',times=2015:2021)

sc1$nivel<-factor(sc1$`Fase da vida`, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=sc1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Excesso de peso no estado de Santa Catarina")+
  theme(plot.title = element_text(size=10, hjust=2.0))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("cyan","violetred2","grey30","orange1","blue","red","green"))


#----Região Sul
rg<-as.data.frame(subset(en, en$Região=='TOTAL REGIÃO SUL'))
rg1<-reshape(rg, direction = 'long', varying = 3:9,idvar='Fase da vida',
             v.names='Porcentagem',times=2015:2021)

rg1$nivel<-factor(rg1$`Fase da vida`, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=rg1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Excesso de peso na região Sul")+
  theme(plot.title = element_text(size=10, hjust=1.1))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("cyan","violetred2","grey30","orange1","blue","red","green"))

#----Brasil
br<-as.data.frame(subset(en, en$Região=='TOTAL BRASIL'))
br1<-reshape(br, direction = 'long', varying = 3:9,idvar='Fase da vida',
             v.names='Porcentagem',times=2015:2021)

br1$nivel<-factor(br1$`Fase da vida`, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=br1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Excesso de peso no Brasil")+
  theme(plot.title = element_text(size=10,hjust=1.0))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("cyan","violetred2","grey30","orange1","blue","red","green"))
#-------------------------------------------------
#   Graficos 2
#-------------------------------------------------
table(dados$`Estado nutricional`)
en<-subset(dados, dados$`Estado nutricional`=='Eutrofia')

table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Crianças de 0 a 6 meses'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
             v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                  levels = c("TOTAL ESTADO PARANÁ",
                             "TOTAL ESTADO SANTA CATARINA", 
                             "TOTAL ESTADO RIO GRANDE DO SUL", 
                             "TOTAL REGIÃO SUL", 
                             "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Crianças de 0 a 6 meses")+
  theme(plot.title = element_text(size=10, hjust=3.0))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))
#------------
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Crianças de 6 a 23 meses'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Crianças de 6 a 23 meses")+
  theme(plot.title = element_text(size=10, hjust=3.3))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))
#-----------------
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Crianças de 2 a 4 anos'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Crianças de 2 a 4 anos")+
  theme(plot.title = element_text(size=10, hjust= 2.5))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))

#-----
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Crianças de 5 a 9 anos'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Crianças de 5 a 9 anos")+
  theme(plot.title = element_text(size=10, hjust=2.5))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))

#------
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Adolescentes'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Adolescentes")+
  theme(plot.title = element_text(size=10, hjust=1.5))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))
#------
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Adultos'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Adultos")+
  theme(plot.title = element_text(size=10, hjust=1.3))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))

#------
table(dados$`Fase da vida`)
base<-as.data.frame(subset(en, en$`Fase da vida`=='Idosos'))


base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)

base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Eutrofia em Idosos")+
  theme(plot.title = element_text(size=10, hjust=1.3))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","orange1","red"))

#--------------------------------------------------------
#    Gráficos de consumo
#-------------------------------------------------------
dados<-read_excel("tabelaParaGraficos.xlsx",
                  sheet=1, col_names = T) 

table(dados$Consumo)[3]

#mudar estado nutricional
en<-subset(dados, dados$Consumo=='Consumo de alimentos in natura e minimamente processados')

#----PR
pr<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO PARANÁ'))
pr1<-reshape(pr, direction = 'long', varying = 3:9,idvar="Idade",
             v.names='Porcentagem',times=2015:2021)

pr1$nivel<-factor(pr1$Idade, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=pr1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo de alimentos in natura e minimamente processados  no estado do Paraná")+
  theme(plot.title = element_text(size=10, hjust=-0.3))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("violetred2","grey30","orange1","blue","red","green"))
#colors() 
#scale_colour_brewer(palette = "Set1")

#----RS
rs<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO RIO GRANDE DO SUL'))
rs1<-reshape(rs, direction = 'long', varying = 3:9,idvar='Idade',
             v.names='Porcentagem',times=2015:2021)

rs1$nivel<-factor(rs1$Idade, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=rs1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo de alimentos in natura e minimamente processados no estado do Rio Grande do Sul")+
  theme(plot.title = element_text(size=10, hjust=-0.0))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("violetred2","grey30","orange1","blue","red","green"))

#----SC
sc<-as.data.frame(subset(en, en$Região=='TOTAL ESTADO SANTA CATARINA'))
sc1<-reshape(sc, direction = 'long', varying = 3:9,idvar='Idade',
             v.names='Porcentagem',times=2015:2021)

sc1$nivel<-factor(sc1$Idade, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=sc1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo de alimentos in natura e minimamente processados no estado de Santa Catarina")+
  theme(plot.title = element_text(size=10, hjust=-0.0))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("violetred2","grey30","orange1","blue","red","green"))


#----Região Sul
rg<-as.data.frame(subset(en, en$Região=='TOTAL REGIÃO SUL'))
rg1<-reshape(rg, direction = 'long', varying = 3:9,idvar='Idade',
             v.names='Porcentagem',times=2015:2021)

rg1$nivel<-factor(rg1$Idade, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=rg1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo de alimentos in natura e minimamente processados na região Sul")+
  theme(plot.title = element_text(size=10,hjust= -0.5))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("violetred2","grey30","orange1","blue","red","green"))

#----Brasil
br<-as.data.frame(subset(en, en$Região=='TOTAL BRASIL'))
br1<-reshape(br, direction = 'long', varying = 3:9,idvar='Idade',
             v.names='Porcentagem',times=2015:2021)

br1$nivel<-factor(br1$Idade, 
                  levels = c("Crianças de 0 a 6 meses",
                             "Crianças de 6 a 23 meses", 
                             "Crianças de 2 a 4 anos", 
                             "Crianças de 5 a 9 anos", 
                             "Adolescentes", 
                             "Adultos", 
                             "Idosos"))

ggplot(data=br1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo de alimentos in natura e minimamente processados no Brasil")+
  theme(plot.title = element_text(size=10, hjust= -0.9))+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("violetred2","grey30","orange1","blue","red","green"))
#-------------------------------------------------
# Graficos consumo 2
#--------------------------------------------------
#criança de 6 a 23 meses
table(dados$Consumo)[1]
en<-subset(dados, dados$Consumo=='Consumo de alimentos in natura e minimamente processados')
enAux<-subset(dados,dados$Consumo=='Consumo de Alimentos Ultraprocessados')
enAux1<-subset(dados,dados$Consumo=='Aleitamento Materno Continuado')

table(en$Idade)

base<-as.data.frame(subset(en, en$Idade=='Crianças de 6 a 23 meses'))
baseAux<-as.data.frame(subset(enAux, enAux$Idade=='Crianças de 6 a 23 meses'))
baseAux1<-as.data.frame(subset(enAux1, enAux1$Idade=='Crianças de 6 a 23 meses'))

base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)
base1Aux<-reshape(baseAux, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)
base2Aux<-reshape(baseAux1, direction = 'long', varying = 3:9,idvar= 'Região',
                  v.names='Porcentagem',times=2015:2021)

base2<-rbind(base1,base1Aux, base2Aux)

base2$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))

base3<-subset(base2, base2$Região=="TOTAL REGIÃO SUL")

ggplot(data=base3, aes(x=time, y=Porcentagem, group=Consumo))+
  geom_line(aes(color=Consumo),size=1)+
  geom_point(aes(color=Consumo),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Consumo alimentar em crianças de 6 a 23 meses na região Sul")+
  theme(plot.title = element_text(size=10, hjust=0.5), legend.position = "bottom",
        legend.direction ="vertical" )+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("green","blue","red","violetred2","grey30","orange1","red"))

#criança de 0 a 6 meses
table(dados$Consumo)[2]
en<-subset(dados, dados$Idade=='Crianças de 0 a 6 meses')
base<-as.data.frame(en)

base1<-reshape(base, direction = 'long', varying = 3:9,idvar= "Região",
               v.names='Porcentagem',times=2015:2021)


base1$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))


ggplot(data=base1, aes(x=time, y=Porcentagem, group=nivel))+
  geom_line(aes(color=nivel),size=1)+
  geom_point(aes(color=nivel),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Aleitamento materno exclusivo em crianças de 0 a 6 meses")+
  theme(plot.title = element_text(size=10, hjust=0.5), legend.position = "bottom",
        legend.direction ="vertical" )+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("blue","violetred2","grey30","green","red"))
#----------------------------------
#criança de 2 a 4 anos para cima
table(dados$Idade)[7]
table(dados$Consumo)[20]
en<-subset(dados, dados$Consumo=='Hábito de realizar as refeições assistindo à televisão')
enAux<-subset(dados,dados$Consumo=='Hábito de realizar no mínimo as três refeições principais do dia')

base<-as.data.frame(subset(en, en$Idade=='Idosos'))
baseAux<-as.data.frame(subset(enAux, enAux$Idade=='Idosos'))

base1<-reshape(base, direction = 'long', varying = 3:9,idvar= 'Região',
               v.names='Porcentagem',times=2015:2021)
base1Aux<-reshape(baseAux, direction = 'long', varying = 3:9,idvar= 'Região',
                  v.names='Porcentagem',times=2015:2021)


base2<-rbind(base1,base1Aux)

base2$nivel<-factor(base1$Região, 
                    levels = c("TOTAL ESTADO PARANÁ",
                               "TOTAL ESTADO SANTA CATARINA", 
                               "TOTAL ESTADO RIO GRANDE DO SUL", 
                               "TOTAL REGIÃO SUL", 
                               "TOTAL BRASIL"))

base3<-subset(base2, base2$Região=="TOTAL REGIÃO SUL")

ggplot(data=base3, aes(x=time, y=Porcentagem, group=Consumo))+
  geom_line(aes(color=Consumo),size=1)+
  geom_point(aes(color=Consumo),size=1.2)+
  ylab("%")+ xlab("Ano")+
  ggtitle("Hábitos alimentares em idosos na região Sul")+
  theme(plot.title = element_text(size=10, hjust=0.5), legend.position = "bottom",
        legend.direction ="vertical" )+
  scale_x_continuous(breaks=seq(2015, 2021, 1))+
  theme(legend.title=element_blank())+
  scale_colour_manual(values = c("red","blue","violetred2","grey30","orange1","red"))


  


