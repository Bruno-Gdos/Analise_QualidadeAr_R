
library("Ecdat") #Carrega Pacote
library("ggplot2")
require(moonBook)
library("ggiraphExtra")

data(Airq) #Carrega banco de dados

names(Airq) #Nome das variaveis

#airq: Indice de qualidade de ar(menor melhor)
#vala: valores das empresas nas cidades(em milhares de $)
#rain: Qntd de chuva (em polegadas)
#coas: posição costeira da cidade( sim ou n)
#medi: renda media per capita(dolares)
#dens: densidade

#Analise descritiva ou exploratória

summary(Airq) #Sumario

#As variaveis podem ser continuar ou categóricas (dividas em categorias)
#e a variavel resposta é a qualidade de ar(airq)

plot(airq~vala, data=Airq)

#Criando modelo estatististico
#y(resposta) ~ x (explicativa)

#--------
#--------

#Montando modelo

m1 <- lm(airq~vala, data=Airq) #modelo linear

summary(m1)# Significancia modelo

#p-valor indica a significância do modelo ou variavel
#se o p-valor  < 0.05 a variavel é significativa
# se o p valor > 0.05 não existe o efeito esperado

#A variavel vala, não influenciou a qualidade do ar nas cidades(airq)

#A variavel "coas" afeta a variavel "airq" ?
m2 <- lm(airq~coas, data=Airq)
summary(m2)
plot(airq~coas, data=Airq)
#Sim! a posição costeria influencia a qualidade de ar nas cidades


#A variavel "medi" afeta a variavel "airq" ?
m3 <- lm(airq~medi, data=Airq)
summary(m3)
plot(airq~medi, data=Airq)
#A variavel medi, não influenciou a qualidade do ar nas cidades(airq)


#A variavel "rain" afeta a variavel "airq"?
m4 <- lm(airq~rain, data=Airq)
summary(m4)
plot(airq~rain, data=Airq)
#A variavel rain, não influenciou a qualidade do ar nas cidades(airq)


#A variavel "densi" afeta a variavel "airq"?
m5 <- lm(airq~dens, data=Airq)
summary(m5)
plot(airq~dens, data=Airq)
#A variavel dens, não influenciou a qualidade do ar nas cidades(airq)



#melhorando o grafico

plot(airq~medi, data=Airq, xlab="Renda media per capita", ylab="Qualidade do ar", pch=1, col="blue", cex.lab=1.3
     ,main="Renda media - 2010")
abline(m3, col="darkblue", lwd=2,lty=2)

#Regressão multipla

mRM1 <- lm(airq~vala+coas, data=Airq)
summary(mRM1)

#Então existe um efeito da posição costeira e do valor das empresas
#na qualidade do ar

#Grafico regressão multipla

equation1=function(x){coef(mRM1)[2]*x+coef(mRM1)[1]}
equation2=function(x){coef(mRM1)[2]*x+coef(mRM1)[1]+coef(mRM1)[3]}


ggplot(Airq,aes(y=airq,x=vala,color=coas))+geom_point()+  xlab("Valor das empresas") +
  ylab("Qualidade do ar") + ggtitle("Efeito da qualidade do ar para o valor das empresas e cidades costeiras") +
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

ggPredict(mRM1,se=TRUE,interactive=TRUE)




