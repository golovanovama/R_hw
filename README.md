# R_hw
HW #1
```
getwd()
expr=read.table('hw1.txt')
expr$
  
cor.test(expr[,1],expr[,2])
cor.test(expr[,1],expr[,3])
cor.test(expr[,2],expr[,3])

#Как видно, только в первой паре значение положительное, что говорит о том, что связь между переменными положительная.Но связь между переменными сильная, т.к. модуль коэффициента близок к 1.
#Наглядно можно увидеть на графиках

pairs(expr)

# Т.е.чем ближе модуль корреляции к 1, тем боллее компактно расположены точки друг к другу, чем ближе к 0, тем более рассеяны значения.
#посчитаем средние значения

mean(expr$RMD4)
mean(expr$CPY14)
mean(expr$htVWQ)
mean(expr$htVWQ)==mean(expr$CPY14)
mean(expr$RMD4)==mean(expr$CPY14)
mean(expr$RMD4)==mean(expr$htVWQ)

#средние значения экспрессий между парами генов не равны.

wilcox.test(expr[,1],expr[,2])
wilcox.test(expr[,1],expr[,3])
wilcox.test(expr[,2],expr[,3])

#так как значения p-value маленькие, можно говорить о том, что отличия в средней экспрессии статистически значимы
par(mfrow=c(1,3))
plot(expr[,1],expr[,2],xlab='CRY14',col='green',ylab='RMD4',asp=1)
m = lm(expr[,2] ~ expr[,1])
abline(m,col='red',lwd=3)
plot(expr[,1],expr[,3],xlab='CRY14',ylab='htVWQ',col='yellow',asp=1)
```
HW #2
```
#случайно делаем броски на обозначенном квадрате
x=runif(10000,-1,1)
y=runif(10000,-1,1)

#оцениваем вероятность
c=sqrt(x^2+y^2)
a=c<=1
v=sum(a)
n=v/10000

#оценка числа pi
pi_valued=4*n

#зависимость оценки числа pi от кол-ва бросков
b=1:100*100
plot(b,4*cumsum(a)[b]/b)
abline(h=pi, col='blue')
m = lm(expr[,2] ~ expr[,3])
abline(m,col='red',lwd=3)
```
HW #3
```
a=mtcars
typeof(a)
typeof(a[,2])
a[18,2]
rownames(a[a[,2]==4,])
min(a$cyl)
which(a$cyl==min(a$cyl))
cor(a)
typeof(cor(a))
class(cor(a))
cor(a) < -0.7
h=which(cor(a) < -0.7, arr.ind = T)
rownames(h)

#####
x=rnorm(1:100,mean=40,sd=10)
y=x[seq(3,100,3)]
c=x[-seq(5,100,5)]
v=as.integer(x)
b=which(v%%2 ==0, arr.ind=T)
x[b]

####
tree=list(left=list(left='a',right=list(left='b',right='c')),
          right=list(left='d',right='e'))
unlist(tree)
tree$left
tree$left$right
```
HW #4
```
install.packages('ape')
library(ape)

getwd()

fa = ape::read.dna('e.coli.fasta',as.character = T,
                   format='fasta',as.matrix = FALSE)
fa=fa[[1]]
fa = paste(fa,collapse = '')
nchar(fa)

kmers=substring(fa,1:(nchar(fa)-1),6:nchar(fa))
sort(table(kmers),decreasing = TRUE)[1:2]
sort(table(kmers), decreasing = FALSE)[2:3]
```
HW #5
```
#1 - Допустим есть вектор и нужно достать по убыванию второе по значение
a<- c(1,5,6,4)
x = readline();
2
x = as.integer(x);
print(sort(a,TRUE)[x])

#2
mandelbrot <- function(a,b,c,d,N){
  x0 = matrix(rep(seq(a,b,length.out=N), each=N), ncol=N)
  x0[1:4, 1:4]
  y0 = matrix(rep(seq(c,d,length.out=N), times=N), ncol=N)
  y0[1:4, 1:4]
  
  x = x0
  y = y0
  
  for(i in 1:30){
    x_ = x
    x = x^2 - y^2 + x0
    y = 2*x_ * y + y0
  }
  
  z = t(abs(x^2 + y^2))
  z[!is.na(z)] = rank(z[!is.na(z)])
 return(image(z^3,col=rev(terrain.colors(1000))))
}
mandelbrot(-1,1,-1,0,3000)# меняя эти параметры,можно рассмотреть интересуюущий участок
```
HW #6
```
library(plyr)
x=as.matrix(table(baseball$team))
y=x[x[,1]>199,]

df=baseball[baseball$team %in% names(y),]
df=unique(df[c('id','team')])
df1=merge(df,df,by='id')
df2=df1[c('team.x','team.y')]
g=aggregate(df1$id,by=df2,length)

library(igraph)
a=get.adjacency(graph.edgelist(as.matrix(df2)),sparse = F)

point=diag(a)/200

for(i in 1:(ncol(a)-1)) {
  for (j in (i+1):ncol(a)) {
    min_players_count = min(a[i,i], a[j,j])
    a[i,j] = a[i,j] / min_players_count
    a[j,i] = a[j,i] / min_players_count
  }
  a[i,i] = 1
}
a[ncol(a), ncol(a)] = 1
m=1-a

mds = cmdscale(m,k=2)
plot(mds,pch=19, cex=point)
text(mds,rownames(mds),adj=c(1.2,1.2),col='red')
```
HW #7
```
###1
my.barplot= function(d,x,y,e,t,n,col,border){
  d[]
  e=runif(n)
  t=LETTERS[]
  col='any'
  border()
  
}

my.barplot(d=1:5,t=LETTERS[1:5],e=runif(5,max = 3),col='red',border=NA)

####2
d=
imageWithText = function (d){
     
     d = matrix(1:8,ncol=2)
     colnames(d)=c('col1','col2')
     rownames(d) = paste0('row',1:4)
     par(mfrow=c(1,2))
     image(d,col=terrain.colors(100))
     t = paste0('x=',d)
     xlab='Cols'
     ylab='rows'
     main='table'
     col=terrain.colors(100)
}

d = matrix(1:8,ncol=2)
colnames(d)=c('col1','col2')
rownames(d) = paste0('row',1:4)
par(mfrow=c(1,2))
image(d,col=terrain.colors(100))
text(0,0,'x=1')
text(0.3,0,'x=2')
text(0.6,0,'x=3')
text(1,0,'x=4')
text(0,1,'x=5')
text(0.3,1,'x=6')
text(0.7,1,'x=7')
text(1,1,'x=8')
xlab='cols'

imageWithText(d,t = paste0('x=',d),
              xlab='Cols',ylab='rows',
              main='table',col=terrain.colors(100))
```
HW #8
n= 10000 #количество генов 
k=5 #кол-во образцов
a = matrix(rnorm(n*k,mean=0, sd=1),nrow=n, ncol=k) #здоровые
c= rnorm(500,mean=3, sd=1)
l=rnorm(9500,mean=0,sd=1)
b = matrix(rnorm(c(c,l),mean=0, sd=1),nrow=n, ncol=k)  #больные

m = cbind(a,b)
pca = prcomp(t(m))
barplot(pca$sdev)
plot(pca$center)
col = rep(c('green','orange'),times=k)

cr = cor(m,use='pair',method = 'sp')
heatmap(1-cr,symm=T)

mds = cmdscale(1-cr,k=2)
plot(mds,pch=19,col=col)

t = t.test(a,b)
g=t$p.value #разница статистически не значима

pv = apply(m, 1, function(i)t.test(i[1:5], i[6:10])$p.value)
table(pv < 0.05) #true=3241

sum(pv[501:n] < 0.05) / sum(pv < 0.05)  # FDR = 0.945

d = data.frame(pv=pv, fdr=p.adjust(pv, m='BH'))
pv.BH = d[which.min(abs(d$fdr - 0.05)), 'pv']
print(pv.BH)  # 0.002884695

sum(pv < pv.BH)  # кол-во значимых генов при p-value, соответствующем FDR = 0.05 - 466 
sum(pv[501:n] < pv.BH) / sum(pv < pv.BH)  # доля ложно-положительных при новом p-value - 0.944206
sum(pv[501:n] < pv.BH) #440

d = data.frame(pv=pv, fdr=p.adjust(pv, m='bonf'))
pv.bonf = d[which.min(abs(d$fdr - 0.05)), 'pv']
print(pv.bonf)  # 3.186347e-06

sum(pv < pv.bonf)  # кол-во значимых генов при p-value, соответствующем FDR = 0.05
sum(pv[501:n] < pv.bonf) / sum(pv < pv.bonf)  # доля ложно-положительных при новом p-value

stat.healthy.mean = apply(m[pv < pv.BH,1:5], 1, mean)
stat.cancer.mean = apply(m[pv < pv.BH,6:10], 1, mean)

plot(stat.cancer.mean - stat.healthy.mean, type='l')

#при повторном прохождении кода результаты не сильно отличались от первоначальных
#в моем эксперименте на 466 значимых гена пришлось 440 ложно-положительных результатов (94.4%)
```
