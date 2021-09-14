#SCRITP R 
#Dissertação: Perfil, Trajetórias e Saúde de Solicitantes de Refúgio Atendidos pela Cáritas Arquidiocesana do Rio de Janeiro
#IMS/UERJ
#Mestrando: João Roberto Cavalcante
#Contato: joao.rcs@hotmail.com

#Abrindo o banco
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados")
dissertacao=read.csv2("banco.csv", header=TRUE, sep=";")
library(foreign)
attach(dissertacao)
#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#Análises Exploratórias

#anoid
table(anoid)
prop.table(table(anoid))

#status
table(status)
prop.table(table(status))

#idiomaform
table(idiomaform)
prop.table(table(idiomaform))

#sexo
table(sexo)
prop.table(table(sexo))

#paisnasc
table(paisnasc)
prop.table(table(paisnasc))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#linguamaterna
table(linguamaterna1)
prop.table(table(linguamaterna1))

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Religiao
table(religiao)
prop.table(table(religiao))

#Nacional
table(nacional)
prop.table(table(nacional))

#Statusmigrafinal
table(statusmigrafinal)
prop.table(table(statusmigrafinal))

#Estado
table(estadobr)
prop.table(table(estadobr))

#Servico Militar
table(militar)
prop.table(table(militar))

table(militar2)
prop.table(table(militar2))

table(tempomilitar)
prop.table(table(tempomilitar))
tempmili=cut(tempomilitar, c(0,1,5,25), include.lowest = T)
summary(tempmili)
prop.table(table(tempmili))

table(formamilitar)
prop.table(table(formamilitar))

#Documentos
table(documento2)
prop.table(table(documento2))

#Passaporte
table(documento1)
prop.table(table(documento1))

#passaportenacional
table(passaporte)
prop.table(table(passaporte))

#autorizacao
table(autorizacao)
prop.table(table(autorizacao))

#identidade
table(identidade)
prop.table(table(identidade))

#motorista
table(motorista)
prop.table(table(motorista))

#certidaonasc
table(certidaonasc)
prop.table(table(certidaonasc))

#outrodoc
table(outrodoc)
prop.table(table(outrodoc))

#Passaporte
table(documento1)
prop.table(table(documento1))

#Familiares
table(gravida)
prop.table(table(gravida))

table(menores)
prop.table(table(menores))

table(familiar1)
prop.table(table(familiar1))

table(familiar2)
prop.table(table(familiar2))

table(familiar3)
prop.table(table(familiar3))

table(familiar4)
prop.table(table(familiar4))

#Protecao nacional e internacional
table(refugio)
prop.table(table(refugio))

table(refugio2)
prop.table(table(refugio2))

table(refugio3)
prop.table(table(refugio3))

table(docrefugio1)
prop.table(table(docrefugio1))

table(protecao1)
prop.table(table(protecao1))

table(protecao2)
prop.table(table(protecao2))

table(retorno)
prop.table(table(retorno))

#Motivos

table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

#doenca

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(dissertacao)

#idade

#data preenchimento
setwd("C:\\Users\\joaor\\Dropbox\\DISSERTAÇÃO\\Resultados\\Dissertação")
data2=read.csv2("datadepreenchimentodoentes.csv", header=TRUE, sep=";", dec=",")
library(foreign)
attach(data2)
table(datapreenchimento)
prop.table(table(datapreenchimento))
detach(data2)

install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade=floor(age_calc(datanasci,datapreen,units="years"))
idade=age_calc(datanasci,datapreen, units ="years")
summary(idade)
agecat=cut(idade, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade)
median(idade)
sd(idade)
length(idade)
hist(idade, prob=T,main="Histograma da Idade dos Solicitante de RefÃºgio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade,na.rm=T),sd=sd(idade, na.rm=T)),add=T,lwd=2,col="red") 

#Analise Somente para Doentes

#doentes
doentes=read.csv2("Doentes.csv", header=TRUE, sep=";")
library(foreign)
attach(doentes)

#sexo
table(sexo)
prop.table(table(sexo))

#linguamaterna
table(linguamaterna1)
prop.table(table(linguamaterna1))

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Religiao
table(religiao)
prop.table(table(religiao))

#Nacional
table(nacional)
prop.table(table(nacional))

#nao_doentes
naodoentes=read.csv2("naodoentes.csv", header=TRUE, sep=";")
library(foreign)
attach(naodoentes)

#sexo
table(sexo)
prop.table(table(sexo))

#linguamaterna
table(linguamaterna1)
prop.table(table(linguamaterna1))

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Religiao
table(religiao)
prop.table(table(religiao))

#Nacional
table(nacional)
prop.table(table(nacional))

#idade
setwd("C:\\Users\\joaor\\Dropbox\\DISSERTAÇÃO\\Resultados\\SPPS")
idade=read.csv2("naodoentes.csv", header=TRUE, sep=";")
library(foreign)
attach(idade)
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitante de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#Analise de Associação 

#associacao
associacao=read.csv2("medidasdeassociacao.csv", header=TRUE, sep=";")
library(foreign)
attach(associacao)

m1=glm(doenca~sexo,family=binomial(link=logit))
summary(m1)
exp(coef(m1))
exp(confint(m1))

m2=glm(doenca~estadocivil,family=binomial(link=logit))
summary(m2)
exp(coef(m2))
exp(confint(m2))

m3=glm(doenca~nacional,family=binomial(link=logit))
summary(m3)
exp(coef(m3))
exp(confint(m3))

m4=glm(doenca~escolaridade,family=binomial(link=logit))
summary(m4)
exp(coef(m4))
exp(confint(m4))

m5=glm(doenca~tortura,family=binomial(link=logit))
summary(m5)
exp(coef(m5))
exp(confint(m5))

#media_solteiros_casados_doentes
estadoc=read.csv2("doentes_solteiros.csv", header=TRUE, sep=";", dec=",")
library(foreign)
attach(estadoc)
mean(idade)
sd(idade)
detach(estadoc)

estadoca=read.csv2("doentes_casados.csv", header=TRUE, sep=";", dec=",")
library(foreign)
attach(estadoca)
mean(idade)
sd(idade)
detach(estadoca)

#Analise de Associação por País de Nascimento

#ANGOLA
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados\\Bancos paises")
angola=read.csv2("angola.csv", header=TRUE, sep=";")
attach(angola)
#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#paisnasc
table(paisnasc)

#anoid
table(anoid)
prop.table(table(anoid))

#sexo
table(sexo)
prop.table(table(sexo))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#linguamaterna
table(linguamaterna1)
prop.table(table(linguamaterna1))

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Religiao
table(religiao)
prop.table(table(religiao))

#Nacional
table(nacional)
prop.table(table(nacional))

#Statusmigrafinal
table(statusmigrafinal)
prop.table(table(statusmigrafinal))

#Tipos de Perseguição
table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(angola)

#VENEZUELA
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados\\Bancos paises")
venezuela=read.csv2("venezuela.csv", header=TRUE, sep=";")
attach(venezuela)

#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#paisnasc
table(paisnasc)

#anoid
table(anoid)
prop.table(table(anoid))

#sexo
table(sexo)
prop.table(table(sexo))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#linguamaterna
table(linguamaterna1)
prop.table(table(linguamaterna1))

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Nacional
table(nacional)
prop.table(table(nacional))

#Tipos de Perseguição
table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(venezuela)

#RDC
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados\\Bancos paises")
rdc=read.csv2("rdc.csv", header=TRUE, sep=";")
attach(rdc)

#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#paisnasc
table(paisnasc)

#anoid
table(anoid)
prop.table(table(anoid))

#sexo
table(sexo)
prop.table(table(sexo))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Nacional
table(nacional)
prop.table(table(nacional))

#Tipos de Perseguição
table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(rdc)

#CUBA
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados\\Bancos paises")
cuba=read.csv2("cuba.csv", header=TRUE, sep=";")
attach(cuba)

#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#paisnasc
table(paisnasc)

#anoid
table(anoid)
prop.table(table(anoid))

#sexo
table(sexo)
prop.table(table(sexo))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Nacional
table(nacional)
prop.table(table(nacional))

#Tipos de Perseguição
table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(cuba)

#SIRIA
setwd("C:\\Users\\joaor\\Dropbox\\_DISSERTAÇÃO\\Resultados\\Bancos paises")
siria=read.csv2("siria.csv", header=TRUE, sep=";")
attach(siria)

#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)

#paisnasc
table(paisnasc)

#anoid
table(anoid)
prop.table(table(anoid))

#sexo
table(sexo)
prop.table(table(sexo))

#idade
install.packages("eeptools")
library(eeptools)
datanasci=as.Date(datanasc, "%d/%m/%Y")
datapreen=as.Date(datapreenchimento, "%d/%m/%Y")
idade2=floor(age_calc(datanasci,datapreen,units="years"))
idade2=age_calc(datanasci,datapreen, units ="years")
summary(idade2)
agecat=cut(idade2, c(0,18,30,45,60,70), include.lowest = T)
summary(agecat)
prop.table(table(agecat))
mean(idade2)
median(idade)
sd(idade2)
length(idade)
hist(idade2, prob=T,main="Histograma da Idade dos Solicitantes de Refugio", xlab = "Idade", ylab = "Densidade")
curve(dnorm(x,mean(idade2,na.rm=T),sd=sd(idade2, na.rm=T)),add=T,lwd=2,col="red") 

#Estado Civil
table(estadocivil)
prop.table(table(estadocivil))

#Escolaridade
table(escolaridade)
prop.table(table(escolaridade))

#Trabalho
table(setoratividade)
prop.table(table(setoratividade))

#Nacional
table(nacional)
prop.table(table(nacional))

#Tipos de Perseguição
table(raca)
prop.table(table(raca))

table(religiao2)
prop.table(table(religiao2))

table(nacionalidade)
prop.table(table(nacionalidade))

table(gruposocial)
prop.table(table(gruposocial))

table(politica)
prop.table(table(politica))

table(direitos)
prop.table(table(direitos))

table(tortura)
prop.table(table(tortura))

table(doenca)
prop.table(table(doenca))

table(tratamento1)
prop.table(table(tratamento1))

table(deficiencia1)
prop.table(table(deficiencia1))

table(deficiencia3)
prop.table(table(deficiencia3))

table(deficiencia5)
prop.table(table(deficiencia5))

detach(siria)

#Fim das Análises e do Script