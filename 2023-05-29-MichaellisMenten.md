---
title : Michaellis-Menten Equation


다음은 사용한 R 코드이다.

out=read.csv("mglimOC.csv",header=F)
out

v3=out$V4/18.38
v2=out$V3/18.38
v1=out$V2/18.38
s=out$V1

val=c(v1,v2,v3)
sal=c(s,s,s)

plot(sal,val,xlab='[S]',ylab='V')
lines(sal[1:7],val[1:7],col='blue')
lines(sal[8:14],val[8:14],col='blue')
lines(sal[15:21],val[15:21],col='blue')
mv=(v1+v2+v3)/3
points(s,mv,pch=19)
lines(s,mv,col='blue',lwd=3)

#w=1/abs(v2-v1)

obj=function(xval,yval,alpha){
    
    beta=mv[3]+(mv[3]/6.7)*alpha
    
    f=beta*xval/(xval+alpha)
    err1=abs(yval-f)
    err=sum(err1[1:7])
}

alpha=seq(3.01,6,0.01)
n=length(alpha)

objout=alpha

for(i in (1:n)){
    objout[i]=obj(s,mv,alpha[i])
}

ind=which.min(objout)
kmest=alpha[ind]
vmaxest=mv[3]*(6.7+kmest)/6.7

kmest
vmaxest 

# kmest==4.76
# vmaxest== 0.01200477

# 오른쪽 부터 2:6 사용 -noweight 
#kmest
#[1] 7.25
#> vmaxest 
#[1] 1.342948

plot(alpha,objout,cex=0.05,xlab='km',ylab='error')
abline(v=4.76,col='blue')
text(5,0.00801,'km=4.76',col='blue')

plot(sal,val,xlab='[S]',ylab='V',ylim=c(0,0.01))
lines(sal[1:7],val[1:7],col='blue',cex=0.3)
lines(sal[8:14],val[8:14],col='blue',cex=0.3)
lines(sal[15:21],val[15:21],col='blue',cex=0.3)
mv=(v1+v2+v3)/3
points(s,mv,pch=5)
lines(s,mv,col='red',cex=0.5)

valfit=vmaxest*s/(s+kmest)
points(s,valfit,pch=19,col='purple')
lines(s,valfit,col='purple',lwd=3)

text(5,0.0075,'km=4.76',col='purple')
text(5,0.007,'vmax=0.0120',col='purple')

#text(12,0.90,'km=7.25',col='purple')
#text(12,0.86,'vmax=1.343',col='purple')

#text(12,0.90,'km=7.25',col='purple')
#text(12,0.86,'vmax=1.343',col='purple')

#2부터 6까지 사용

obj=function(xval,yval,alpha){
  
  beta=mv[3]+(mv[3]/6.7)*alpha
  
  f=beta*xval/(xval+alpha)
  err1=abs(yval-f)
  err=sum(err1[2:6])
}

alpha=seq(5.01,10,0.01)
n=length(alpha)

objout=alpha

for(i in (1:n)){
  objout[i]=obj(s,mv,alpha[i])
}

ind=which.min(objout)
kmest=alpha[ind]
vmaxest=mv[3]*(6.7+kmest)/6.7

kmest
vmaxest 

#kmest == 7.25
#vmaxest == 0.01461314

plot(alpha,objout,cex=0.05,xlab='km',ylab='error')
abline(v=7.25,col='blue')
text(7.25,0.00315,'km=7.25',col='blue')

plot(sal,val,xlab='[S]',ylab='V',ylim=c(0,0.012))
lines(sal[1:7],val[1:7],col='blue',cex=0.3)
lines(sal[8:14],val[8:14],col='blue',cex=0.3)
lines(sal[15:21],val[15:21],col='blue',cex=0.3)
mv=(v1+v2+v3)/3
points(s,mv,pch=5)
lines(s,mv,col='red',cex=0.5)

valfit=vmaxest*s/(s+kmest)
points(s,valfit,pch=19,col='purple')
lines(s,valfit,col='purple',lwd=3)

text(5,0.008,'km=7.25',col='purple')
text(5,0.0075,'vmax=0.0146',col='purple')

#==============================================
#===================== Linear Fit =================
#================================================

out=read.csv("mglimOC.csv",header=F)
out

v3=out$V4/18.38
v2=out$V3/18.38
v1=out$V2/18.38
s=out$V1

val=c(1/v1,1/v2,1/v3)
sal=c(1/s,1/s,1/s)

plot(sal,val,xlab='1/[S]',ylab='1/V',xlim=c(0.03,0.35))
lines(sal[1:7],val[1:7],col='blue')
lines(sal[8:14],val[8:14],col='blue')
lines(sal[15:21],val[15:21],col='blue')
mv=(val[1:7]+val[8:14]+val[15:21])/3
points(1/s,mv,pch=19)
lines(1/s,mv,col='blue',lwd=3)

lm.naive=lm(val~sal)
summary(lm.naive)
abline(lm.naive,col="purple",lwd=3)

=
text(0.17,280,'1/v=13.00+1206.8*1/[S]',col='purple')

vmaxest = 1/lm.naive$coefficients[1]
kmest = lm.naive$coefficients[2]*vmaxest

text(0.15,800,'Vmax=0.0769',col="purple",cex=2)
text(0.15,900,'Km=92.8280',col='purple',cex=2)

# sal=0.15 
#(val[3]+val[10]+val[17])/3
# val= 1.55045
'
vmaxest
(Intercept) 
0.07691986 

 kmest
sal 
92.82796 '

# Right 2nd point 이상한 점 빼기
#
#
#

valorg = val
val[20] = (val[6]+ val[13])/2



plot(sal,val,xlab='1/[S]',ylab='1/V',xlim=c(0.03,0.35))
lines(sal[1:7],val[1:7],col='blue')
lines(sal[8:14],val[8:14],col='blue')
lines(sal[15:21],val[15:21],col='blue')
mv=(val[1:7]+val[8:14]+val[15:21])/3
points(1/s,mv,pch=19)
lines(1/s,mv,col='blue',lwd=3)


lm.adj=lm(val~sal)
summary(lm.adj)
abline(lm.adj,col="purple",lwd=3)

text(0.17,250,'1/v=55.49+807.84*1/[S]',col='purple')

vmaxest = 1/lm.adj$coefficients[1]
kmest = lm.adj$coefficients[2]*vmaxest
vmaxest
kmest

text(0.15,400,'Vmax=0.0180',col="purple",cex=2)
text(0.15,450,'Km=14.5582',col='purple',cex=2)

#===================================

csal=sal-sal[3]
cval=val-mv[3]
lm.fit=lm(cval~csal-1)
summary(lm.fit)

hatbeta=lm.fit$coefficient[1]
hatalpha=mv[3]-(hatbeta*sal[3])

hatbeta 
hatalpha 

#> hatbeta 
#946.8126 
#> hatalpha 
# 1.171013 

plot(sal,val,xlab='1/[S]',ylab='1/V',xlim=c(0.03,0.35))
lines(sal[1:7],val[1:7],col='blue')
lines(sal[8:14],val[8:14],col='blue')
lines(sal[15:21],val[15:21],col='blue')
mv=(val[1:7]+val[8:14]+val[15:21])/3
points(1/s,mv,pch=19)
lines(1/s,mv,col='blue',lwd=3)

abline(hatalpha,hatbeta,col="purple",lwd=3)

text(0.17,250,'1/v=1.171+946.81*1/[S]',col='purple')

vmaxest = 1/hatalpha
kmest = vmaxest*hatbeta
vmaxest
kmest

text(0.15,450,'Vmax=0.854',col="purple",cex=2)
text(0.15,400,'Km=808.5414 ',col='purple',cex=2)


[mglimOC.csv](https://github.com/freshhighhope/freshhighhope.github.io/files/11585289/mglimOC.csv)

위는 본 실험 데이터 파일이다.
