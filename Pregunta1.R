#pregunta 1

x <- c(0,1)
f <- c(0.68,0.32)

plot(x, f, type="h", ylim=c(0,1),col="red")

# en una enquesta es fixa el numero de persones 43 persones, 
#quina es la prob de que 13 persones tinguin com a minim 13 televisors si la P = 0,32

# x = el num de 1 en els n enquestats 43
# vull la P(X=13) les perones que han dit que si que tenen +2

#T = (k1, k2,...k43) ensayo de bernoilli
# t = (1,0,1,1,0,0,1,0,1,0...) suma de t(i) = X els que m'han dit que si i que no

n <- 43
Y <-function(i){sum(sample(x, n, f, replace =TRUE))} #aixo agafa mo agafa un valor, es funcio
Y(i)# lhe de cridar ara si que agafa valor

encuestas <- sapply(1:4000, Y)
#grafic taula
fr <- table(encuestas)/4000
barplot(fr)
fr["13"]

# utilitzar formula on 13 es els que diuen que si, 43 son tots, o.32 es la prob de dir si
dbinom(13, 43, 0.32)
y <- 0:43
y
plot(y, dbinom(y,43, 0.32), type="h", col="red")
#ara em graficat els posibles resultats



# exemple si ara ensquestes a 17 persones i la enquesta es de 44
g <- 0:44
dbinom(17,44, 0.32)
plot(g, dbinom(g,44, 0.32), type="h", col="red")

#ara la probabili de que menys de 17 diguin que si

#funcio per calcular la prob de menys un num (sumar totes les binomials)
# pbinom es la F majuscula+ funcio de distibucio de probabilitat
# dbinom es la f(x) # funcio massa prob esta definida a punts

pbinom(16,44 ,0.32)

# la mitjana es F^-1(0.5)= xmed
#mediana
qbinom(0.5, 44, 0.32)

# per fer el primer quaril
qbinom(0.25, 44, 0.32)

# ara conto els que com a max tenen una tele P= 0.68 X= 1
# X=0 els que tenen coma  min 2 P = 0.32

####
24*0.68#mitja
24*0.68*0.32#variança
qbinom(0.25, 24, 0.68)#primer quartil

h <- 0:43
plot(y, pbinom(h, 24, 0.68), type="s") #aixo es F majuscula

#ultima pregunta
46*0.32 #pq hem tronat al primer exemple 
