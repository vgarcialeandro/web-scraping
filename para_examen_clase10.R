muni<-read.delim("clipboard")

########################
# PRUEBA BINOMIAL (Los resultados de n experimentos se expresan en forma dicot�mica)
##########################
#------------------------------
# 1. La proporci�n de entrevistados del g�nero femenino en el distrito de Comas es
#    mayor al 30%.
#------------------------------
table(muni$Distrito,muni$G�nero)
  #                           Femenino Masculino
  # Comas                      26        24
  # Los Olivos                  8        22
  # San Mart�n de Porras       17        23

# Parametro de Entrada
### Cantidad de positivos
### Cantidad Total
### Probabilidad
### La alternativa de hipostesis (Como es mayor es "g")  "two.sided", "greater" or "less".

binom.test(26,50,p=0.3,alternative="g", conf.level = 0.95)

  #        Exact binomial test
  # 
  # data:  26 and 50
  # number of successes = 26, number of trials = 50, p-value = 0.0009332
  # alternative hypothesis: true probability of success is greater than 0.3
  # 95 percent confidence interval:
  #   0.395412 1.000000
  # sample estimates:
  #   probability of success 
  # 0.52 

#---------------------------------------------------

  # H0: Muestra <= 1/3
  # H1: Muestra >  1/3

binom.test(18,26,1/3,alternative="g")
  # p-value = 0.0001972

  # CONCLUSI�N: Existe suficiente evidencia estad�stica a un nivel de significaci�n de 0.05, 
  # para rechazar H0. Por lo tanto, se puede afirmar que la probabilidad de que los degustadores 
  # realicen la prueba correcta es mayor a 1/3.

#######################################
# PRUEBA DE FRECUENCIAS O PROPORCIONES
#######################################
#------------------------------
# 2. La opini�n de atenci�n de los que acuden a la municipalidad de Comas no est�
#    en la relaci�n 1:2:3:1:2
#------------------------------
tabla<-table(muni$Distrito,muni$Opini�n)[1,]
#Ordernar la viable 
  # Buena Excelente Muy Buena    P�sima   Regular 
  # 8         9        13        10        10 

obs<-c(10,10,8,13,9)
prob1<-c(1,2,3,1,2)/9
resul<-chisq.test(obs,p=prob1)
resul
resul$expected

prob2<-c(1,2,3,1,2)
chisq.test(obs,p=prob2,rescale.p = T)

# Si hubiera esperados menores a 5
chi<-sum((abs(resul$observed-resul$expected)-0.5)^2/resul$expected)
chi
1-pchisq(chi,length(obs)-1)

#----------------------------------------
  # M�quina A M�quina B M�quina C
  # 43        53        39

  # H0: Muestra1 = 1/3,  Muestra2 = 1/3,  Muestra3 = 1/3
  # H1: Al menos un muestra es diferente para i=1,2,3

x=c(43,53,39)
prob = c(1/3,1/3,1/3)
chisq.test(x,p=prob)

  #        Chi-squared test for given probabilities
  # 
  # data:  x
  # X-squared = 2.3111, df = 2, p-value = 0.3149

# CONCLUSION: A un nivel de significaci�n del 5% no se puede afirmar que
#             las 3 m�quinas no producen en igual proporci�n.


###################################
# PRUEBA DE RACHAS (probar la aleatoriedad de una serie cuando es asignada a una de dos categor�as.)
###################################

#------------------------------
# 3. Los datos con respecto al tiempo que se demora en ser atendido en la municipalidad 
#    de Los Olivos no se distribuye aleatoriamente con respecto a su mediana.
#------------------------------

lo<-subset(muni,muni$Distrito=="Los Olivos")
library(tseries)
me<-median(lo$Atenci�n)
#### De tiene que transformar los valores a Factor
runs.test(as.factor(lo$Atenci�n>me),alternative="t")

  #        Runs Test
  # 
  # data:  as.factor(lo$Atenci�n > me)
  # Standard Normal = 0.74322, p-value = 0.4573
  # alternative hypothesis: two.sided



  # H0: La secuencia de observaciones es aleatoria
  # H1: La secuencia de observaciones no es aleatoria

runs.test(as.factor(datos>90))
  # Pvalor=0.210
  # Conclusi�n: A un nivel de significaci�n del 5% no se puede afirmar que las observaciones no siguen una secuencia aleatoria. 




###################################
# Prueba de Kolmogorov-Smirnov (Que tan bien se ajusta la distribuci�n de los datos de la muestra a alguna distribuci�n te�rica F(X))
###################################

#------------------------------
#Pregunta Numero 4
# El tiempo que se demora en ser atendido en la municipalidad de San Mart�n de
# Porras para pagar sus impuestos se distribuye de manera exponencial.
#------------------------------

sm<-subset(muni,muni$Distrito=="San Mart�n de Porras")

hist(sm$Atenci�n)
# Para una distribucion exponencial
# Depende de la estimacion agregar los parametros
ks.test(sm$Atenci�n,"pexp",rate=1/mean(sm$Atenci�n))

  #        One-sample Kolmogorov-Smirnov test
  # 
  # data:  sm$Atenci�n
  # D = 0.076342, p-value = 0.9739
  # alternative hypothesis: two-sided

# Para una distribucion normal
#ks.test(sm$Atenci�n,"pnorm",mean(sm$Atenci�n),sd(sm$Atenci�n))

# Para una distribucion uniforme
#ks.test(sm$Atenci�n,"punif",min(sm$Atenci�n),max(sm$Atenci�n))



  # H0: F(x) = FT(x), (Las observaciones siguen una distribuci�n te�rica espec�fica)
  # H1: F(x) <>??? FT(x), (Las observaciones no siguen un adistribuci�n te�rica espec�fica)




###################################
# Prueba de Signos
###################################
### NOTA: esto es solo paa validar la prueba.
#---------------------------------------------------
# 5. El n�mero mediano de delitos sufridos en los entrevistados en el distrito de Los
#    Olivos es mayor a 3.
#---------------------------------------------------
library(moments)
skewness(lo$Robos)
library(lawstat)
symmetry.test(lo$Robos,option="MGG",boot=F)

library(BSDA)
SIGN.test(lo$Robos,md=3,alternative="g")

###################################
# Prueba de Wilcoxon
###################################

#---------------------------------------------------
# 5. El n�mero mediano de delitos sufridos en los entrevistados en el distrito de Los
#    Olivos es mayor a 3.
#---------------------------------------------------
#### Tiene problemas con los empates (NO USAR ESTAS)
wilcox.test(lo$Robos,alternative="g",mu=3)

library(exactRankTests)
wilcox.exact(lo$Robos,alternative="g",mu=3)

























