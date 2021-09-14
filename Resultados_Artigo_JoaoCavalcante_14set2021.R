#Script R
#Artigo: Perfil sociodemográfico e de saúde de solicitantes de refúgio do Rio de Janeiro
#Autor das analises: João Roberto Cavalcante
#Contato: joao.rcs@hotmail.com

#Abindo o banco
setwd("C:\\Users\\joaor\\Dropbox\\_Produção\\Artigos\\1. EM ANDAMENTO\\1. Perfil Sociodemográfico e de Saúde dos Solicitantes")
dissertacao=read.csv2("banco_artigo_1.csv", header=TRUE, sep=";")
library(foreign)
attach(dissertacao)
#length(var)
#table(var)
#prop.table(table(var))
#mean(var)
#sd(var)
#median(var)
detach(dissertacao)

#Análises Exploratórias

#Tabela 1

#anoid
length(anoid)
table(anoid)
prop.test(x=529,n=818, conf.level = 0.95)
prop.test(x=289,n=818, conf.level = 0.95)

#status
length(status)
table(status)
prop.test(x=11,n=818, conf.level = 0.95)
prop.test(x=758,n=818, conf.level = 0.95)
prop.test(x=1,n=818, conf.level = 0.95)
prop.test(x=48,n=818, conf.level = 0.95)

#sexo
length(sexo)
table(sexo)
prop.test(x=308,n=818, conf.level = 0.95)
prop.test(x=510,n=818, conf.level = 0.95)

#idiomaform
length(idiomaform)
table(idiomaform)
prop.test(x=292,n=818, conf.level = 0.95)
prop.test(x=233,n=818, conf.level = 0.95)
prop.test(x=87,n=818, conf.level = 0.95)
prop.test(x=206,n=818, conf.level = 0.95)

#Estado Civil
length(estadocivil)
table(estadocivil)
prop.test(x=551,n=775, conf.level = 0.95)
prop.test(x=199,n=775, conf.level = 0.95)
prop.test(x=11,n=775, conf.level = 0.95)
prop.test(x=4,n=775, conf.level = 0.95)
prop.test(x=5,n=775, conf.level = 0.95)
prop.test(x=3,n=775, conf.level = 0.95)
prop.test(x=2,n=775, conf.level = 0.95)

#Escolaridade
length(escolaridade)
table(escolaridade)
prop.test(x=119,n=771, conf.level = 0.95)
prop.test(x=312,n=771, conf.level = 0.95)
prop.test(x=340,n=771, conf.level = 0.95)

#Trabalho
length(setoratividade)
table(setoratividade)
prop.test(x=348,n=675, conf.level = 0.95)
prop.test(x=99,n=675, conf.level = 0.95)
prop.test(x=65,n=675, conf.level = 0.95)
prop.test(x=4,n=675, conf.level = 0.95)
prop.test(x=27,n=675, conf.level = 0.95)
prop.test(x=132,n=675, conf.level = 0.95)

#Nacional
length(nacional)
table(nacional)
prop.test(x=502,n=628, conf.level = 0.95)
prop.test(x=126,n=628, conf.level = 0.95)

#Militar
length(militar2)
table(militar2)
prop.test(x=42,n=706, conf.level = 0.95)
prop.test(x=664,n=706, conf.level = 0.95)

#Maneira Militar
length(formamilitar)
table(formamilitar)
prop.test(x=31,n=40, conf.level = 0.95)
prop.test(x=9,n=40, conf.level = 0.95)

#Documentos
length(documento2)
table(documento2)
prop.test(x=632,n=716, conf.level = 0.95)
prop.test(x=84,n=716, conf.level = 0.95)

#Tabela 2
#Familiares

length(menores)
table(menores)
prop.test(x=101,n=648, conf.level = 0.95)
prop.test(x=547,n=648, conf.level = 0.95)

length(familiar1)
table(familiar1)
prop.test(x=96,n=640, conf.level = 0.95)
prop.test(x=544,n=640, conf.level = 0.95)

length(familiar2)
table(familiar2)
prop.test(x=28,n=638, conf.level = 0.95)
prop.test(x=610,n=638, conf.level = 0.95)

length(familiar3)
table(familiar3)
prop.test(x=17,n=642, conf.level = 0.95)
prop.test(x=625,n=642, conf.level = 0.95)

length(familiar4)
table(familiar4)
prop.test(x=64,n=556, conf.level = 0.95)
prop.test(x=492,n=556, conf.level = 0.95)

#Protecao nacional e internacional
length(refugio)
table(refugio)
prop.test(x=8,n=767, conf.level = 0.95)
prop.test(x=759,n=767, conf.level = 0.95)

length(refugio2)
table(refugio2)
prop.test(x=11,n=696, conf.level = 0.95)
prop.test(x=685,n=696, conf.level = 0.95)

length(refugio3)
table(refugio3)
prop.test(x=4,n=792, conf.level = 0.95)
prop.test(x=788,n=792, conf.level = 0.95)

length(protecao1)
table(protecao1)
prop.test(x=68,n=727, conf.level = 0.95)
prop.test(x=659,n=727, conf.level = 0.95)

length(protecao2)
table(protecao2)
prop.test(x=89,n=699, conf.level = 0.95)
prop.test(x=610,n=699, conf.level = 0.95)

length(retorno)
table(retorno)
prop.test(x=65,n=731, conf.level = 0.95)
prop.test(x=666,n=731, conf.level = 0.95)

#Motivos da Fuga

length(raca)
table(raca)
prop.test(x=55,n=700, conf.level = 0.95)
prop.test(x=645,n=700, conf.level = 0.95)

length(religiao2)
table(religiao2)
prop.test(x=68,n=700, conf.level = 0.95)
prop.test(x=632,n=700, conf.level = 0.95)

length(nacionalidade)
table(nacionalidade)
prop.test(x=17,n=700, conf.level = 0.95)
prop.test(x=683,n=700, conf.level = 0.95)

length(gruposocial)
table(gruposocial)
prop.test(x=93,n=700, conf.level = 0.95)
prop.test(x=607,n=700, conf.level = 0.95)

length(politica)
table(politica)
prop.test(x=332,n=700, conf.level = 0.95)
prop.test(x=368,n=700, conf.level = 0.95)

length(direitos)
table(direitos)
prop.test(x=345,n=700, conf.level = 0.95)
prop.test(x=355,n=700, conf.level = 0.95)

length(tortura)
table(tortura)
prop.test(x=582,n=687, conf.level = 0.95)
prop.test(x=105,n=687, conf.level = 0.95)

#saude

length(doenca_final)
table(doenca_final)
prop.test(x=216,n=746, conf.level = 0.95)
prop.test(x=530,n=746, conf.level = 0.95)

length(gravida)
table(gravida)
prop.test(x=20,n=244, conf.level = 0.95)
prop.test(x=224,n=244, conf.level = 0.95)

length(tratamento1)
table(tratamento1)
prop.test(x=15,n=694, conf.level = 0.95)
prop.test(x=679,n=694, conf.level = 0.95)

length(deficiencia1)
table(deficiencia1)
prop.test(x=14,n=700, conf.level = 0.95)
prop.test(x=686,n=700, conf.level = 0.95)

length(deficiencia3)
table(deficiencia3)
prop.test(x=4,n=700, conf.level = 0.95)
prop.test(x=696,n=700, conf.level = 0.95)

length(deficiencia5)
table(deficiencia5)
prop.test(x=42,n=700, conf.level = 0.95)
prop.test(x=658,n=700, conf.level = 0.95)

#IC das doenças e agravos
prop.test(x=49,n=216, conf.level = 0.95)
prop.test(x=28,n=216, conf.level = 0.95)
prop.test(x=14,n=216, conf.level = 0.95)
prop.test(x=14,n=216, conf.level = 0.95)
prop.test(x=11,n=216, conf.level = 0.95)
prop.test(x=11,n=216, conf.level = 0.95)
prop.test(x=11,n=216, conf.level = 0.95)
prop.test(x=6,n=216, conf.level = 0.95)
prop.test(x=5,n=216, conf.level = 0.95)
prop.test(x=4,n=216, conf.level = 0.95)
prop.test(x=4,n=216, conf.level = 0.95)
prop.test(x=3,n=216, conf.level = 0.95)
prop.test(x=3,n=216, conf.level = 0.95)
prop.test(x=53,n=216, conf.level = 0.95)

#IC faixa etária
prop.test(x=21,n=818, conf.level = 0.95)
prop.test(x=427,n=818, conf.level = 0.95)
prop.test(x=315,n=818, conf.level = 0.95)
prop.test(x=52,n=818, conf.level = 0.95)
prop.test(x=3,n=818, conf.level = 0.95)


#IC lingua materna
prop.test(x=216,n=737, conf.level = 0.95)
prop.test(x=185,n=737, conf.level = 0.95)
prop.test(x=83,n=737, conf.level = 0.95)
prop.test(x=41,n=737, conf.level = 0.95)
prop.test(x=37,n=737, conf.level = 0.95)
prop.test(x=175,n=737, conf.level = 0.95)

#IC segundo idioma
prop.test(x=105,n=471, conf.level = 0.95)
prop.test(x=99,n=471, conf.level = 0.95)
prop.test(x=91,n=471, conf.level = 0.95)
prop.test(x=46,n=471, conf.level = 0.95)
prop.test(x=37,n=471, conf.level = 0.95)
prop.test(x=93,n=471, conf.level = 0.95)

#IC religiao
prop.test(x=515,n=641, conf.level = 0.95)
prop.test(x=71,n=641, conf.level = 0.95)
prop.test(x=25,n=641, conf.level = 0.95)
prop.test(x=3,n=641, conf.level = 0.95)
prop.test(x=27,n=641, conf.level = 0.95)

#IC denominacoes cristas
prop.test(x=223,n=515, conf.level = 0.95)
prop.test(x=76,n=515, conf.level = 0.95)
prop.test(x=21,n=515, conf.level = 0.95)
prop.test(x=8,n=515, conf.level = 0.95)
prop.test(x=6,n=515, conf.level = 0.95)
prop.test(x=181,n=515, conf.level = 0.95)

#IC etnia
prop.test(x=102,n=396, conf.level = 0.95)
prop.test(x=28,n=396, conf.level = 0.95)
prop.test(x=21,n=396, conf.level = 0.95)
prop.test(x=16,n=396, conf.level = 0.95)
prop.test(x=15,n=396, conf.level = 0.95)
prop.test(x=214,n=396, conf.level = 0.95)

#IC ultimo status
prop.test(x=208,n=381, conf.level = 0.95)
prop.test(x=87,n=381, conf.level = 0.95)
prop.test(x=38,n=381, conf.level = 0.95)
prop.test(x=14,n=381, conf.level = 0.95)
prop.test(x=34,n=381, conf.level = 0.95)

#IC meio de transporte
prop.test(x=629,n=781, conf.level = 0.95)
prop.test(x=12,n=781, conf.level = 0.95)
prop.test(x=78,n=781, conf.level = 0.95)
prop.test(x=5,n=781, conf.level = 0.95)
prop.test(x=50,n=781, conf.level = 0.95)
prop.test(x=2,n=781, conf.level = 0.95)
prop.test(x=5,n=781, conf.level = 0.95)

#IC documento entrar brasil
prop.test(x=578,n=701, conf.level = 0.95)
prop.test(x=84,n=701, conf.level = 0.95)
prop.test(x=19,n=701, conf.level = 0.95)
prop.test(x=12,n=701, conf.level = 0.95)
prop.test(x=4,n=701, conf.level = 0.95)
prop.test(x=4,n=701, conf.level = 0.95)

idade=read.csv2("idade.csv", header=TRUE, sep=";")
attach(idade)
mean(idade)
sd(idade)

#Fim do Script
