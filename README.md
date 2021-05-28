# ModBinomialR
Calculate the value of an American or European put/call with Volatility at N periods in R.  \
See the [Python code](https://github.com/Cuadernin/ModeloBinomial)

**You can graph the binomial tree with the library(fOptions)**
```
valor=BinomialTreeOption(TypeFlag="ce",S=S,X=K,Time=time,r=0.05,b=0,sigma=o,n=N,title="binomial model") 
BinomialTreePlot(valor)   
```
