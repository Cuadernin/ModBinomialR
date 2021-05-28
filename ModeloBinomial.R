library(glue)
library(fOptions)
bino=function(S,K,T,r,u,d,n,o,put=FALSE,am=FALSE){
  # """
  #   Argumentos:
  #       S (número): So ---> Valor de la acción
  #       K (número): Precio de ejercicio
  #       T (número): Total de periodos
  #       r (número): TLR ---> Tasa libre de riesgo
  #       u (número): Probabilidad de que suba
  #       d (número): Probabilidad de que baje
  #       n (número): Total de periodos
  #       o (número): volatilidad
  #   Returns:
  #       [Call,put]: [valor del call europeo, valor del put europeo]
  #   """
t=T/n
u=exp(o*sqrt(t))
d=1/u
p=(exp(r*t)-d)/(u-d)
val=matrix(0,n+1,n+1);C=matrix(0,n+1,n+1)
lista=c()

#if(put){pos=-1}else{pos=1} #ifelse(put,pos=1,pos=-1)
pos=ifelse(put,1,1)
  for(i in 1:n+1){
    for(j in 1:i){
      st=S*(u^(j-1)*d^(i-j))
      val[i,j]=st
      lista=c(lista,st)
    }
  }
lista=append(lista,S,0)
C[nrow(C),]=pmax(pos*(val[nrow(val),]-K),0)
for(i in (nrow(C)-1):1){
  for(j in 1:i){
    if(am){
      v1=exp(-r*t)*(p*C[i+1,j+1]+(1-p)*C[i+1,j])
      st=pos*(S*u^(j-1)*d^(i-j)-K)
      v2=max(st,0)
      C[i,j]=max(v1,v2)
    }
    else{
    C[i,j]=exp(-r*t)*(p*C[i+1,j+1]+(1-p)*C[i+1,j])
    }
  }
}
if(put){
  put=C[1,1]
  call=put-K*exp(-r*T)+S
  if(am){
    pritn(glue('Paridad call-put ------------------------> {put+S} = {call+K*exp(-r*T)}'))
    print(glue("Valor del call ==== {call}"))
    print(glue("Valor del put ==== {put}"))
  }else{
    print(glue('Paridad call-put ------------------------> {put+S} = {call+K*exp(-r*T)}'))
    print(glue("Valor del call ==== {call}"))
    print(glue("Valor del put ==== {put}"))
  }
}else{ 
  call=C[1,1]
  put=call+K*exp(-r*T)-S
  print(glue('Paridad call-put ------------------------> {put+S} = {call+K*exp(-r*T)}'))
  print(glue("Valor del call ==== {call}"))
  print(glue("Valor del put ==== {put}"))
}
return(val) #lista
}
N=2;S=50;K=51;T=6/12;r=0.05;u=1.06;d=0.95;o=0.20
b=bino(S,K,T,r,u,d,N,o,FALSE,FALSE)

