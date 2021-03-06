#dados

# CAN
Dados<-read.csv("TFP.csv")
CAN<-Dados[Dados$isocode == "CAN", ]
CAN
rtfpna_CAN<-CAN$rtfpna
rtfpna_CAN

# USA
Dados<-read.csv("TFP.csv")
USA<-Dados[Dados$isocode == "USA", ]
USA
rtfpna_USA<-USA$rtfpna
rtfpna_USA

# M�xico
Dados<-read.csv("TFP.csv")
MEX<-Dados[Dados$isocode == "MEX", ]
MEX
rtfpna_MEX<-MEX$rtfpna
rtfpna_MEX

# An�lise descritiva dos dados

par(mfrow=c(3,1))

# Analisando a s�rie de dados temporal dos tr�s pa�ses
plot(x = CAN$year, y = rtfpna_CAN, type = "l",main='TFP CAN, constant prices, 2005 = 1',ylab = 'TFP', xlab='Year')
# A s�rie n�o apresenta uma tend�ncia determin�stica e clara ao longo do tempo, tamb�m n�o apresenta algum 
# fenomeno que demonstre repeti��o ou ciclo ao longo do tempo. Contudo � preciso verificar sua estacionariedade.

plot(x = USA$year, y = rtfpna_USA, type = "l",main='TFP USA, constant prices, 2005 = 1',ylab = 'TFP', xlab='Year')

plot(x = MEX$year, y = rtfpna_MEX, type = "l",main='TFP MEX, constant prices, 2005 = 1',ylab = 'TFP', xlab='Year')

# Estacionariedade

tseries::adf.test(rtfpna_CAN) # p-valor acima de 0,05 (=0.709), s�rie n�o estacion�ria.
tseries::adf.test(rtfpna_USA) # p-valor acima de 0,05 (=0.8475), s�rie n�o estacion�ria.
tseries::adf.test(rtfpna_MEX) # p-valor acima de 0,05 (=0.06289), s�rie n�o estacion�ria.

# ACF e PACF, s�rie original (ver se a s�rie tem estacionariedade)

par(mfrow=c(3,2))

acf((rtfpna_CAN),10) # sem sinal de sazonalidade
pacf((rtfpna_CAN),10)

acf((rtfpna_USA),10) # sem sinal de sazonalidade
pacf((rtfpna_USA),10)

acf((rtfpna_MEX),10) # sem sinal de sazonalidade
pacf((rtfpna_MEX),10)

# Transformando os dados para alcan�ar estacionariedade (diferenciar a s�rie)

par(mfrow=c(3,1))

diff(rtfpna_CAN)
# Refazendo teste adf para verificar se a s�rie estacionarizou
tseries::adf.test(diff(diff(rtfpna_CAN))) # p-vlor = 0.01125, menor que 5%
plot.ts(diff(diff(rtfpna_CAN)))

diff(rtfpna_USA)
# Refazendo teste adf para verificar se a s�rie estacionarizou
tseries::adf.test(diff(diff(rtfpna_USA))) # p-vlor = 0.03649, menor que 5%
plot.ts(diff(diff(rtfpna_USA)))

diff(rtfpna_MEX)
# Refazendo teste adf para verificar se a s�rie estacionarizou
tseries::adf.test(diff(rtfpna_MEX)) # p-vlor = 0.08547, maior que 5%

diff(diff(rtfpna_MEX))
tseries::adf.test(diff(diff(rtfpna_MEX))) # p-vlor = 0.01, maior que 5%
plot.ts(diff(diff(rtfpna_MEX)))

# Analisando as duas s�ries de dados:
par(mfrow=c(2,1))
plot(x = CAN$year, y = rtfpna, type = "l",main='TFP, constant prices, 2005 = 1',ylab = 'TFP', xlab='Year')
plot(diff(rtfpna),type="l")

# Teste de an�lise dos dados da s�rie serem ou n�o correlacionados, � preciso ser para que a s�rie n�o seja 
# totalmente aleat�ria, p-valor <0,05 para rejeitar hip�tese nula de n�o h� autocorrela��o entre as lags anteriores
# da s�rie diferenciada 2 vezes.

par(mfrow=c(3,1))
Box.test(diff(diff(rtfpna_CAN)), lag = log(length(diff(diff(rtfpna_CAN)))))
Box.test(diff(diff(rtfpna_USA)), lag = log(length(diff(diff(rtfpna_USA)))))
Box.test(diff(diff(rtfpna_MEX)), lag = log(length(diff(diff(rtfpna_MEX)))))

# Analisando as fun��es de autocorrela��o
par(mfrow=c(2,1))
acf(diff(diff(rtfpna_CAN))) # MA (q) = 0, 1, 2, 3
pacf(diff(diff(rtfpna_CAN))) # AR (p) = 0, 1, 2 

## Forecast
#CAN
d=2
for(p in 1:3){
  for(q in 1:4){
    if(p+d+q<=8){
      model<-arima(x=rtfpna_CAN, order = c((p-1),d,(q-1)))
      pval<-Box.test(model$residuals, lag=log(length(model$residuals)))
      sse<-sum(model$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

modelo<-arima(rtfpna_CAN,order=c(0,2,3)) # menor AIC
modelo

#An�lise dos res�duos
res<-resid(modelo)
res
forecast::checkresiduals(modelo)

require(forecast)
forecast(modelo,10)
plot(forecast(modelo),10)

par(mfrow=c(2,2))

plot(modelo$residuals)
acf(modelo$residuals)
pacf(modelo$residuals)
qqnorm(modelo$residuals)

#Autom�tico
require(forecast)
modelo1<-auto.arima(rtfpna_CAN)
plot(forecast(rtfpna_CAN),10)
forecast(modelo1)
modelo1
plot(modelo1$residuals)
acf(modelo1$residuals)
pacf(modelo1$residuals)
qqnorm(modelo1$residuals)

#USA
par(mfrow=c(2,1))
acf(diff(diff(rtfpna_USA))) # 0, 1
pacf(diff(diff(rtfpna_USA))) # 0, 1, 2

d=2
for(p in 1:3){
  for(q in 1:2){
    if(p+d+q<=8){
      model_USA<-arima(x=rtfpna_USA, order = c((p-1),d,(q-1)))
      pval<-Box.test(model_USA$residuals, lag=log(length(model_USA$residuals)))
      sse<-sum(model_USA$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model_USA$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

modelo_USA<-arima(rtfpna_USA,order=c(1,2,1)) # menor AIC
modelo_USA

#An�lise dos res�duos
res<-resid(modelo_USA)
forecast::checkresiduals(modelo_USA)

require(forecast)
forecast(modelo_USA,10)
plot(forecast(modelo_USA),10)

par(mfrow=c(2,2))

plot(modelo_USA$residuals)
acf(modelo_USA$residuals)
pacf(modelo_USA$residuals)
qqnorm(modelo_USA$residuals)

#Autom�tico
require(forecast)
modelo2<-auto.arima(rtfpna_USA)
plot(forecast(rtfpna_USA),10)
forecast(modelo2)
modelo2
plot(modelo2$residuals)
acf(modelo2$residuals)
pacf(modelo2$residuals)
qqnorm(modelo2$residuals)

#MEX
par(mfrow=c(2,1))
acf(diff(diff(rtfpna_MEX))) # 0, 1 
pacf(diff(diff(rtfpna_MEX))) # 0, 1, 2, 3

d=2
for(p in 1:4){
  for(q in 1:2){
    if(p+d+q<=8){
      model_MEX<-arima(x=rtfpna_MEX, order = c((p-1),d,(q-1)))
      pval<-Box.test(model_MEX$residuals, lag=log(length(model_MEX$residuals)))
      sse<-sum(model_MEX$residuals^2)
      cat(p-1,d,q-1, 'AIC=', model_MEX$aic, ' SSE=',sse,' p-VALUE=', pval$p.value,'\n')
    }
  }
}

modelo_MEX<-arima(rtfpna_MEX,order=c(0,2,1)) # menor AIC
modelo_MEX

#An�lise dos res�duos
res<-resid(modelo_MEX)
res
Box.test(model_MEX$residuals, lag=log(length(model_MEX$residuals)))
forecast::checkresiduals(modelo_MEX)

require(forecast)
forecast(modelo_MEX,10)
plot(forecast(modelo_MEX),10)

par(mfrow=c(2,2))

plot(modelo_USA$residuals)
acf(modelo_USA$residuals)
pacf(modelo_USA$residuals)
qqnorm(modelo_USA$residuals)

#Autom�tico
require(forecast)
modelo3<-auto.arima(rtfpna_MEX)
plot(forecast(rtfpna_MEX),10)
forecast(modelo3)
modelo3
plot(modelo3$residuals)
acf(modelo3$residuals)
pacf(modelo3$residuals)
qqnorm(modelo3$residuals)

# Arrumar acentos e Se der tempo inserir aqui um gr�fico com essas vari�veis.
