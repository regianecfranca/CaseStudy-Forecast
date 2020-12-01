# The dataset contains all trackings of monthly imports and exports of a range of products (soybeans, 
# soybean meal, soybean oil, corn, wheat and sugar), by brazilian states, by routes (air, sea, ground, etc) e 
# from/to which country;

# 1) Show the evolution of total monthly and total annual exports from Brazil (all states and to everywhere) of 
#'soybeans', 'soybean oil' and 'soybean meal';

# Extraindo os dados da base

library(plyr)
dados<-read.csv("data_comexstat.csv")
soybeans_export_tons<-subset(dados, product == "soybeans" & type == "Export") [,c("date","tons")]
soybeans_total_tons<-ddply(soybeans_export_tons, c("date"),summarise,soybeans_tons=(sum(tons))/1000000)
soybeans_total_tons$date<-as.Date(soybeans_total_tons$date)
soybeans_total_tons["Ano"]<-lubridate::year(soybeans_total_tons$date)
soybeans_total_tons<-ddply(soybeans_total_tons, c("Ano"),summarise,soybeans_tons=sum(soybeans_tons))
soybeans_total_tons

soybeansOil_export_tons<- subset(dados, product == "soybean_oil" & type == "Export") [,c("date","tons")]
soybeansOil_export_tons
soybeansOil_total_tons<-ddply(soybeansOil_export_tons, c("date"),summarise,soybeansOil_tons=(sum(tons))/1000000)
soybeansOil_total_tons$date<-as.Date(soybeansOil_total_tons$date)
soybeansOil_total_tons["Ano"]<-lubridate::year(soybeansOil_total_tons$date)
soybeansOil_total_tons<-ddply(soybeansOil_total_tons, c("Ano"),summarise,soybeansOil_tons=sum(soybeansOil_tons))
soybeansOil_total_tons

soybeansMeal_export_tons<- subset(dados, product == "soybean_meal" & type == "Export") [,c("date","tons")]
soybeansMeal_export_tons
soybeansMeal_total_tons<-ddply(soybeansMeal_export_tons, c("date"),summarise,soybeansMeal_tons=(sum(tons))/1000000)
soybeansMeal_total_tons$date<-as.Date(soybeansMeal_total_tons$date)
soybeansMeal_total_tons["Ano"]<-lubridate::year(soybeansMeal_total_tons$date)
soybeansMeal_total_tons<-ddply(soybeansMeal_total_tons, c("Ano"),summarise,soybeansMeal_tons=sum(soybeansMeal_tons))
soybeansMeal_total_tons

dados1<-soybeans_total_tons
dados1["SoybeansOil_Mtons"]<-soybeansOil_total_tons$soybeansOil_tons
dados1
dados1["SoybeansMeal_Mtons"]<-soybeansMeal_total_tons$soybeansMeal_tons
dados1

names(dados1)[2]<-"Soybeans_Mtons"
dados1

library(ggplot2)
g<-ggplot(dados1) + aes(x = Ano) + geom_line(aes(y = Soybeans_Mtons,col="Soybean"))
g

g + geom_line(aes(y = SoybeansOil_Mtons, col="Soybean Oil")) + geom_line(aes(y = SoybeansMeal_Mtons, col="Soybean Meal")) +
  labs(title = "Evolução da exportação dos produtos de soja no Brasil", x = "Ano", y="Em milhões de toneladas", color = "Produto")

# Análise mensal

soybeans_total_tonsM<-ddply(soybeans_export_tons, c("date"),summarise,soybeans_tons=(sum(tons))/1000000)
soybeansOil_total_tonsM<-ddply(soybeansOil_export_tons, c("date"),summarise,soybeansOil_tons=(sum(tons))/1000000)
soybeansOil_total_tonsM
soybeansMeal_total_tonsM<-ddply(soybeansMeal_export_tons, c("date"),summarise,soybeansMeal_tons=(sum(tons))/1000000)
soybeansMeal_total_tonsM

dadosM<-data.frame(soybeans_total_tonsM,soybeansOil_total_tonsM$soybeansOil_tons,soybeansMeal_total_tonsM$soybeansMeal_tons)
dadosM
names(dadosM)[2:4]<-c("Soybeans_Mtons","SoybeansOil_Mtons","SoybeansMeal_Mtons")
dadosM

dadosMe<-dadosM[grep("2019-",dadosM$date), ]
dadosMe

library(ggplot2)
g<-ggplot(dadosMe) + aes(x = as.Date(dadosMe$date)) + geom_line(aes(y = Soybeans_Mtons,col="Soybean"))
g

g + geom_line(aes(y = SoybeansOil_Mtons, col="Soybean Oil")) + geom_line(aes(y = SoybeansMeal_Mtons, col="Soybean Meal")) +
  labs(title = "Evolução da exportação dos produtos de soja no Brasil (2019)", x = "Mês", y="Em milhões de toneladas", color = "Produto")

# 2) What are the 3 most important products exported by Brazil in the last 5 years?

dados
dados$date<-as.Date(dados$date)
dados["Ano"]<-lubridate::year(dados$date)
dados

dadosExp<-subset(dados, type == "Export") [,c("Ano","product","state","country","route","tons","usd")]
dadosExp

dadosExp_Pr<-ddply(dadosExp, c("Ano","product"),summarise,usd=(sum(tons/1000000)))
dadosExp_Pr

dadosExp_Pr<-reshape(dadosExp_Pr, v.names = "usd", idvar = "Ano", timevar="product",direction="wide")
dadosExp_Pr

dadosExp_Pr<-dadosExp_Pr[19:23, ]
dadosExp_Pr

names(dadosExp_Pr)[2:7]<-c("Corn","Soybeans_Meal","Soybeans_Oil","Soybeans", "Sugar","Wheat")
dadosExp_Pr

dadosExp_Pr %>% mutate(Total = Corn+Soybeans_Meal+Soybeans_Oil+Soybeans+Sugar+Wheat)  %>%
  mutate(Maior_Share = pmax(Corn,Soybeans_Meal,Soybeans_Oil,Soybeans,Sugar,Wheat, na.rm = TRUE) / Total)

library(ggplot2)
gr<-ggplot(dadosExp_Pr) + aes(x = Ano) + geom_line(aes(y = Corn,col="Corn"))
gr

gr + geom_line(aes(y = Soybeans_Meal, col="Soybean Meal")) + geom_line(aes(y = SoybeansOil, col="Soybean Oil")) +
  geom_line(aes(y = Soybeans, col="Soybean")) + geom_line(aes(y = Sugar, col="Sugar")) + 
  geom_line(aes(y = Wheat, col="Wheat")) + 
  labs(title = "Evolução da exportação dos principais produtos no Brasil", x = "Ano", y="Em milhões de toneladas", color = "Produto")


# 3) What are the main routes through which Brazil have been exporting 'corn' in the last few years? Are there 
# differences in the relative importancem of routes depending on the product?
dadosExp
dadosR<-subset(dadosExp, product == "corn") [ ,c("Ano","state","country","route","tons","usd")]
dadosR
dadosR<-ddply(dadosR, c("Ano","route"),summarise,tons=(sum(tons/1000000)))
dadosR
dadosRo<-reshape(dadosR, v.names = "tons", idvar = "Ano", timevar="route",direction="wide")
dadosRo

dadosRou<-dadosRo[19:23, ]
dadosRou

names(dadosRou)[2:6]<-c("Air","Ground","Other","Sea","River")
dadosRou

dadosRou %>% mutate(Total = Air+Ground+Other+River+Sea)  %>%
  mutate(Maior_Participação = pmax(Air,Ground,Other,River,Sea, na.rm = TRUE) / Total)
# As rotas mais utilizadas são Sea e river para milho, nos últimos 5 anos.

library(ggplot2)
gr<-ggplot(dadosRou) + aes(x = Ano) + geom_line(aes(y = Air,col="Air"))
gr

gr + geom_line(aes(y = Ground, col="Ground")) +
  geom_line(aes(y = Other, col="Other")) + geom_line(aes(y = River, col="River")) + 
  geom_line(aes(y = Sea, col="Sea")) + 
  labs(title = "Exportações brasileiras por rota (milho)", x = "Ano", y="Em milhões de toneladas", color = "Rota")

# Todos os produtos

dadosA<-ddply(dadosExp, c("Ano","route","product"),summarise,tons=(sum(tons/1000000)))
dadosA
dadosAl<-dadosA%>%filter(Ano >"2014")
dadosAl

library(tidyr)
dadosAl<-spread(dadosAl,Ano,tons)

dadosAl<-replace(x = dadosAl, list = is.na(dadosAl), values = 0)
dadosAl

# Pela tabela é possível visualizar que não somente o milho é mais exportado pelo mar, mas os outros produtos
# também, a soja principalmente, seguido do açúcar e do farelo de soja.

# 4) Which countries have been the most important trade partners for Brazil in terms of 'corn' and 'sugar' in the 
# last 3 years?

# Milho
dadosC<-subset(dados, product == "corn") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosC
dadosCo<-ddply(dadosC, c("product","Ano","country","type"),summarise,tons=(sum(tons/1000000)))
dadosCor<-dadosCo%>%filter(Ano >"2016")
dadosCor

dadosCorn<-spread(dadosCor,Ano,tons)
dadosCorn

dadosCorn<-replace(x = dadosCorn, list = is.na(dadosCorn), values = 0)

dadosCorn<-arrange(dadosCorn,desc(dadosCorn$`2019`))
dadosCorn[1:20, ]

# Para o produto milho, a maioria do comércio internacional refere-se à exportações, ordenando pelo ano mais 
# recente, 6,5 milhôes de toneladas de milho foram exportadas para o Japão, seguido o Iran, Vietnam e 
# Coréia so Sul. Em relação as compras nacionais do produto, o Paraguai aparece em destaque no ano de 2019 com
# 1,4 milhôes de toneladas em importação.

# Açúcar

dadosAc<-subset(dados, product == "sugar") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosAc
dadosAc<-ddply(dadosAc, c("product","Ano","country","type"),summarise,tons=(sum(tons/1000000)))
dadosAc<-dadosAc%>%filter(Ano >"2016")
dadosAc

dadosAc<-spread(dadosAc,Ano,tons)
dadosAc

dadosAc<-replace(x = dadosAc, list = is.na(dadosAc), values = 0)

dadosAc<-arrange(dadosAc,desc(dadosAc$`2019`))
dadosAc[1:20, ]

# Entre os 20 países que mais compraram açúcar brasileiro em milhões de toneladas no 2019, destacam-se os países:
# Argélia, Bangladesh, Nigéria e Arábia Saudita.

# 5) For each of the products in the dataset, show the 5 most important states in terms of exports?

# Corn
dadosExp_Corn<-subset(dados, product == "corn" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Corn<-ddply(dadosExp_Corn, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_Corn<-dadosExp_Corn%>%filter(Ano >"2014")
dadosExp_Corn<-spread(dadosExp_Corn,Ano,tons)
dadosExp_Corn<-replace(x = dadosExp_Corn, list = is.na(dadosExp_Corn), values = 0)
dadosExp_Corn

dadosExp_Corn<-dadosExp_Corn %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
    mutate(Média = Soma/ 5)

dadosExp_Corn<-arrange(dadosExp_Corn,desc(dadosExp_Corn$Média))
dadosExp_Corn[1:5, ]

# soybean_meal
dadosExp_SoyM<-subset(dados, product == "soybean_meal" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_SoyM<-ddply(dadosExp_SoyM, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_SoyM<-dadosExp_SoyM%>%filter(Ano >"2014")
dadosExp_SoyM<-spread(dadosExp_SoyM,Ano,tons)
dadosExp_SoyM<-replace(x = dadosExp_SoyM, list = is.na(dadosExp_SoyM), values = 0)
dadosExp_SoyM

dadosExp_SoyM<-dadosExp_SoyM %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_SoyM<-arrange(dadosExp_SoyM,desc(dadosExp_SoyM$Média))
dadosExp_SoyM[1:5, ]

# soybean_oil
dadosExp_SoyO<-subset(dados, product == "soybean_oil" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_SoyO<-ddply(dadosExp_SoyO, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_SoyO<-dadosExp_SoyO%>%filter(Ano >"2014")
dadosExp_SoyO<-spread(dadosExp_SoyO,Ano,tons)
dadosExp_SoyO<-replace(x = dadosExp_SoyO, list = is.na(dadosExp_SoyO), values = 0)
dadosExp_SoyO

dadosExp_SoyO<-dadosExp_SoyO %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_SoyO<-arrange(dadosExp_SoyO,desc(dadosExp_SoyO$Média))
dadosExp_SoyO[1:5, ]

# soybeans
dadosExp_Soy<-subset(dados, product == "soybeans" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Soy<-ddply(dadosExp_Soy, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_Soy<-dadosExp_Soy%>%filter(Ano >"2014")
dadosExp_Soy<-spread(dadosExp_Soy,Ano,tons)
dadosExp_Soy<-replace(x = dadosExp_Soy, list = is.na(dadosExp_Soy), values = 0)
dadosExp_Soy

dadosExp_Soy<-dadosExp_Soy %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Soy<-arrange(dadosExp_Soy,desc(dadosExp_Soy$Média))
dadosExp_Soy[1:5, ]

# Wheat
dadosExp_Wh<-subset(dados, product == "wheat" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Wh<-ddply(dadosExp_Wh, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_Wh<-dadosExp_Wh%>%filter(Ano >"2014")
dadosExp_Wh<-spread(dadosExp_Wh,Ano,tons)
dadosExp_Wh<-replace(x = dadosExp_Wh, list = is.na(dadosExp_Wh), values = 0)
dadosExp_Wh

dadosExp_Wh<-dadosExp_Wh %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Wh<-arrange(dadosExp_Wh,desc(dadosExp_Wh$Média))
dadosExp_Wh[1:5, ]

# sugar
dadosExp_Su<-subset(dados, product == "sugar" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Su<-ddply(dadosExp_Su, c("Ano","product","state"),summarise,tons=(sum(tons/1000000)))
dadosExp_Su<-dadosExp_Su%>%filter(Ano >"2014")
dadosExp_Su<-spread(dadosExp_Su,Ano,tons)
dadosExp_Su<-replace(x = dadosExp_Su, list = is.na(dadosExp_Su), values = 0)
dadosExp_Su

dadosExp_Su<-dadosExp_Su %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Su<-arrange(dadosExp_Su,desc(dadosExp_Su$Média))
dadosExp_Su[1:5, ]

# 6) Now, we ask you to show your modelling skills. Feel free to use any type of modelling approach, but bear 
# in mind that the modelling approach depends on the nature of your data, and so different models yield different 
# estimates and forecasts. To help you out in this task we also provide you with a dataset of possible covariates 
# (.xlsx). They all come from public sources (IMF, World Bank) and are presented in index number format. 
# Question: What should be the total brazilian soybeans, soybean_meal, and corn export forecasts, in tons, for the 
# next 11 years (2020-2030)? We're mostly interested in the annual forecast.

## Exportações em tons:
# soybeans
dadosExp_Soyf<-subset(dados, product == "soybeans" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Soyf<-ddply(dadosExp_Soyf, c("Ano"),summarise,tons=(sum(tons/1000000)))
dadosExp_Soyf
Exp_Soy<-ts(dadosExp_Soyf$tons, start = 1997,frequency = 1)
Exp_Soy

# soybean_meal
dadosExp_SoyMf<-subset(dados, product == "soybean_meal" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_SoyMf<-ddply(dadosExp_SoyMf, c("Ano"),summarise,tons=(sum(tons/1000000)))
dadosExp_SoyMf
Exp_SoyMf<-ts(dadosExp_SoyMf$tons, start = 1997, frequency = 1)
Exp_SoyMf

#Corn
dadosExp_Cornf<-subset(dados, product == "corn" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Cornf<-ddply(dadosExp_Cornf, c("Ano"),summarise,tons=(sum(tons/1000000)))
dadosExp_Cornf
Exp_Corn<-ts(dadosExp_Cornf$tons, start = 1997, frequency = 1)
Exp_Corn

## Covriáveis:
base1<-readxl::read_xlsx("covariates.xlsx")
base1
write.csv2(base1,"C:/Users/regia/OneDrive/Documents/Regiane/CV/4Intelligence/Exercicio/base.csv")

base<-read.csv2("base.csv")
base

GDP_China<-base[19:41,c("gdp_china")]
GDP_China<-ts(GDP_China, start = 1997, frequency = 1)
GDP_China

GDP_Spain<-base[19:41,c("gdp_spain")]
GDP_Spain<-ts(GDP_Spain, start = 1997, frequency = 1)
GDP_Spain

GDP_Thailand<-base[19:41,c("gdp_thailand")]
GDP_Thailand<-ts(GDP_Thailand, start = 1997)
GDP_Thailand

GDP_Netherlands<-base[19:41,c("gpd_netherlands")]
GDP_Netherlands<-ts(GDP_Netherlands,start = 1997, frequency = 1)
GDP_Netherlands

GDP_Iran<-base[19:41,c("gdp_iran")]
GDP_Iran<-ts(GDP_Iran, start = 1997, frequency = 1)
GDP_Iran

GDP_World<-base[19:41,c("gdp_world")]
GDP_World<-ts(GDP_World, start = 1997, frequency = 1)
GDP_World

GDP_Vietnam<-base[19:41,c("gdp_vietnam")]
GDP_Vietnam<-ts(GDP_Vietnam, start = 1997, frequency = 1)
GDP_Vietnam

GDP_Japan<-base[19:41,c("gdp_japan")]
GDP_Japan<-ts(GDP_Japan, start = 1997, frequency = 1)
GDP_Japan

GDP_Egypt<-base[19:41,c("gdp_egypt")]
GDP_Egypt<-ts(GDP_Egypt, start = 1997, frequency = 1)
GDP_Egypt

# soybeans
# Preço
PSoy<-base[19:41,c("price_soybeans")]
PSoy<-ts(PSoy, start = 1997, frequency = 1)

# Principais países compradores
dadosExp_Soyf<-subset(dados, product == "soybeans" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Soyf<-ddply(dadosExp_Soyf, c("Ano","product","country"),summarise,tons=(sum(tons/1000000)))
dadosExp_Soyf<-dadosExp_Soyf%>%filter(Ano >"2014")
dadosExp_Soyf<-spread(dadosExp_Soyf,Ano,tons)
dadosExp_Soyf<-replace(x = dadosExp_Soyf, list = is.na(dadosExp_Soyf), values = 0)
dadosExp_Soyf

dadosExp_Soyf<-dadosExp_Soyf %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Soyf<-arrange(dadosExp_Soyf,desc(dadosExp_Soyf$Média))
dadosExp_Soyf[1:5, ]

# soybean_meal

# Preço
PSoyM<-base[19:41,c("price_soybean_meal")]
PSoyM<-ts(PSoyM, start = 1997, frequency = 1)
PSoyM

# Principais países compradores
dadosExp_SoyMf<-subset(dados, product == "soybean_meal" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_SoyMf<-ddply(dadosExp_SoyMf, c("Ano","product","country"),summarise,tons=(sum(tons/1000000)))
dadosExp_SoyMf<-dadosExp_SoyMf%>%filter(Ano >"2014")
dadosExp_SoyMf<-spread(dadosExp_SoyMf,Ano,tons)
dadosExp_SoyMf<-replace(x = dadosExp_SoyMf, list = is.na(dadosExp_SoyMf), values = 0)
dadosExp_SoyMf

dadosExp_SoyMf<-dadosExp_SoyMf %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_SoyMf<-arrange(dadosExp_SoyMf,desc(dadosExp_SoyMf$Média))
dadosExp_SoyMf[1:5, ]

GDP_Netherlands

GDP_Thailand

GDP_World

# corn

# Preço
PCor<-base[19:41,c("price_corn")]
PCor<-ts(PCor, start = 1997,frequency = 1)
PCor

# Principais países compradores
dadosExp_Cornf<-subset(dados, product == "corn" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Cornf<-ddply(dadosExp_Cornf, c("Ano","product","country"),summarise,tons=(sum(tons/1000000)))
dadosExp_Cornf<-dadosExp_Cornf%>%filter(Ano >"2014")
dadosExp_Cornf<-spread(dadosExp_Cornf,Ano,tons)
dadosExp_Cornf<-replace(x = dadosExp_Cornf, list = is.na(dadosExp_Cornf), values = 0)
dadosExp_Cornf

dadosExp_Cornf<-dadosExp_Cornf %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Cornf<-arrange(dadosExp_Cornf,desc(dadosExp_Cornf$Média))
dadosExp_Cornf[1:5, ]

GDP_Iran

GDP_Vietnam

GDP_Japan

GDP_Egypt

GDP_World<-base[19:52,c("year","gdp_world")]
GDP_World

# Visualização dos dados

# Soja 

library(urca)
library(vars)
library(mfilter)
library(tseries)
library(tidyverse)
library(forecast)
library(ggplot2)

graf<-ggplot(dadosExp_Soyf) + aes(x = Ano) + geom_line(aes(y = tons,col="tons"))
graf

gra<-ggplot(base) + aes(x = year) + geom_line(aes(y = price_soybeans,col="Preços Soja"))
gra

gra +  geom_line(aes(y = gdp_china, col="PIB China")) + geom_line(aes(y = gdp_spain, col="PIB Espanha")) + 
  geom_line(aes(y = gdp_thailand, col="PIB Thailândia")) + geom_line(aes(y = gpd_netherlands, col="PIB Netherlands"))+
  geom_line(aes(y = gdp_iran, col="PIB Iran")) + geom_line(aes(y = gdp_world, col="PIB Mundo")) +
  labs(title = "Covariáveis", x = "Ano", y="Em milhões de toneladas", color = "Legenda")

# Estacionariedade

# Soja
adfSoy<-adf.test(Exp_Soy)
adfSoy

adf.test(diff(diff(Exp_Soy)))

Exp_SoyEst<-diff(diff(Exp_Soy))
Exp_SoyEst

plot(Exp_SoyEst,type = "l")

# Preço
adfPr<-adf.test(PSoy)
adfPr

adf.test(diff(diff(PSoy)))

PSoyEst<-diff(diff(PSoy))
plot(PSoyEst, type = "l")

# PIB China

adfGDPCh<-adf.test(GDP_China)
adfGDPCh

adf.test(diff(diff(diff(GDP_China))))

GDP_ChinaEst<-diff(diff(diff(GDP_China)))
plot(GDP_ChinaEst, type="l")

# GDP_Spain

adfGDPSpa<-adf.test(GDP_Spain)
adfGDPSpa

adf.test(diff(diff(GDP_Spain)))

adf.test(diff(diff(diff(GDP_Spain))))
GDP_SpainEst<-diff(diff(diff(GDP_Spain)))
plot(GDP_SpainEst,type="l")

# GDP_Thailand

adfGDPThai<-adf.test(GDP_Thailand)
adfGDPThai

adf.test(diff(diff(diff(GDP_Thailand))))

GDP_ThailandEst<-diff(diff(diff(GDP_Thailand)))
plot(GDP_ThailandEst,type="l")

# GDP_Netherlands

adfGDPNe<-adf.test(GDP_Netherlands)
adfGDPNe

adf.test(diff(diff(diff(GDP_Netherlands))))

GDP_NetherlandsEst<-diff(diff(diff(GDP_Netherlands)))
plot(GDP_NetherlandsEst,type="l")

# GDP_Iran
adfGDPIr<-adf.test(GDP_Iran)
adfGDPIr

adf.test(diff(diff(diff(GDP_Iran))))

GDP_IranEst<-diff((diff(diff(GDP_Iran))))
plot(GDP_IranEst,type="l")

# GDP_World
adfGDPWo<-adf.test(GDP_World)
adfGDPWo

adf.test(diff(diff(GDP_World)))

GDP_WorldEst<-diff(diff(GDP_World))

plot(GDP_WorldEst)

# Juntando todas as variáveis

Soybase<-cbind(Exp_SoyEst,PSoyEst,GDP_ChinaEst,GDP_SpainEst,GDP_WorldEst)
Soybase<-replace(x = Soybase, list = is.na(Soybase), values = 0)
Soybase

# Lags ideal
lagselect<-vars::VARselect(Soybase,lag.max=12,type="both")
lagselect$selection # 2

# Teste de cointegração
cointest<-urca::ca.jo(Soybase, type="trace",K=2)
summary(cointest)

# Modelo
ModeloSoy<-vars::VAR(Soybase,p=2,type="const")
ModeloSoy
summary(ModeloSoy)
summary(ModeloSoy,equation="Exp_SoyEst")

# VECM
library(tsDyn)
ModeloVCSoy<-VECM(Soybase,2,r=4, estim = "ML")
ModeloVCSoy
summary(ModeloVCSoy)

# Diagnóstico do modelo

# VECM to VAR

ModelVAR<-vec2var(cointest, r=4)

# Autocorrelação serial
autocoSoy<-serial.test(ModeloSoy, lags.pt=12)
autocoSoy # > que 0,05 não tem autocorrelação serial
serialtest<-serial.test(ModelVAR, lags.pt = 5)
serialtest #>0,05 não tem autocorrelação serial

# Heterocedasticidade
ArchSoy<-arch.test(ModeloSoy, lags.multi = 7)
ArchSoy # > que 0,05 não tem hetecedasticidade

archtest<-arch.test(ModelVAR, lags.multi = 3)
archtest # > que 0,05 não tem hetecedasticidade

# distribuição normal dos resíduos

ResSoy<-normality.test(ModeloSoy, multivariate.only = TRUE)
ResSoy # não é normalmente distribuido

restest<-normality.test(ModelVAR, multivariate.only = TRUE)
restest # > que 0,05 os residuos são normalmente distribuidos

# Estabilidade dos resíduos

StabSoy<-stability(ModeloSoy)
plot(StabSoy) # não tem quebra estrutural nos residuos, são estáveis

# Forecast
forecastSoy<-predict(ModelVAR, n.ahead = 11)
forecastSoy
fanchart(forecastSoy, names = "Exp_SoyEst")

## soybean_meal

# Visualização dos dados

graf_ExpSoyM<-ggplot(dadosExp_SoyMf) + aes(x = Ano) + geom_line(aes(y = tons)) + labs(title = "Exportações brasileiras (Farelo de Soja)", x = "Ano", y="Em milhões de toneladas")
graf_ExpSoyM 

graf_PSoyM<-ggplot(base) + aes(x = year) + geom_line(aes(y = price_soybean_meal,col="Preços Farelo de Soja"))
graf_PSoyM

graf_PSoyM +  geom_line(aes(y = gdp_china, col="PIB China")) + geom_line(aes(y = gdp_spain, col="PIB Espanha")) + 
  geom_line(aes(y = gdp_thailand, col="PIB Thailândia")) + geom_line(aes(y = gpd_netherlands, col="PIB Netherlands"))+
  geom_line(aes(y = gdp_iran, col="PIB Iran")) + geom_line(aes(y = gdp_world, col="PIB Mundo")) +
  labs(title = "Covariáveis", x = "Ano", y="Em índice", color = "Legenda")

# Estacionariedade

# Exportações
adfExpSoyM<-adf.test(Exp_SoyMf)
adfExpSoyM

adf.test(diff(Exp_SoyMf))
adf.test(diff(diff(Exp_SoyMf)))
adf.test(diff(diff(diff(Exp_SoyMf))))

Exp_SoyMfEst<-diff(diff(diff(Exp_SoyMf)))
plot(Exp_SoyMfEst)

# Preço
adfPSoyM<-adf.test(PSoyM)
adfPSoyM

adf.test(diff(PSoyM))
adf.test(diff(diff(PSoyM)))

PSoyMEst<-diff(diff(PSoyM))
plot(PSoyMEst)

# PIBs
GDP_NetherlandsEst

GDP_ThailandEst

GDP_WorldEst

# Juntando todas as variáveis

SoyMbase<-cbind(Exp_SoyMfEst,PSoyMEst,GDP_NetherlandsEst,GDP_WorldEst)
SoyMbase<-replace(x = SoyMbase, list = is.na(SoyMbase), values = 0)
SoyMbase

# Lags ideal
lagselect<-vars::VARselect(SoyMbase,lag.max=10,type="both")
lagselect$selection # 3

# Teste de cointegração
cointestSoyM<-urca::ca.jo(SoyMbase, type="trace",K=3)
summary(cointestSoyM) #r = 2

# Modelo
# VECM
library(tsDyn)
ModelVECSoyM<-VECM(SoyMbase,3,r=2, estim = "ML")
ModelVECSoyM
summary(ModelVECSoyM)

# Diagnóstico do modelo

# Transformando VECM em VAR

ModelVARSoyM<-vec2var(cointestSoyM, r=2)

# Autocorrelação serial
serialtestSoyM<-serial.test(ModelVARSoyM, lags.pt = 10)
serialtestSoyM # > 0,05 não tem autocorrelação serial

# Heterocedasticidade
archtest<-arch.test(ModelVARSoyM, lags.multi = 7)
archtest # > que 0,05 não tem hetecedasticidade

# distribuição normal dos resíduos
ResSoyM<-normality.test(ModelVARSoyM, multivariate.only = TRUE)
ResSoyM # > 0,05 é normalmente distribuido

# Forecast
forecastSoyM<-predict(ModelVARSoyM, n.ahead = 11)
forecastSoyM
fanchart(forecastSoyM, names = "Exp_SoyMfEst")

## Corn

# Principais países compradores
dadosExp_Cornf<-subset(dados, product == "corn" & type == "Export") [,c("Ano","product","state","country","type","route","tons","usd")]
dadosExp_Cornf<-ddply(dadosExp_Cornf, c("Ano","product","country"),summarise,tons=(sum(tons/1000000)))
dadosExp_Cornf<-dadosExp_Cornf%>%filter(Ano >"2014")
dadosExp_Cornf<-spread(dadosExp_Cornf,Ano,tons)
dadosExp_Cornf<-replace(x = dadosExp_Cornf, list = is.na(dadosExp_Cornf), values = 0)
dadosExp_Cornf

dadosExp_Cornf<-dadosExp_Cornf %>% mutate(Soma = (`2015`+`2016`+`2017`+`2018`+`2019`))  %>%
  mutate(Média = Soma/ 5)

dadosExp_Cornf<-arrange(dadosExp_Cornf,desc(dadosExp_Cornf$Média))
dadosExp_Cornf[1:5, ]


GDP_Iran

GDP_Vietnam

GDP_Japan

GDP_Egypt

GDP_World

# Visualização dos dados

graf_ExpCorn<-ggplot(dadosExp_Cornf) + aes(x = Ano) + geom_line(aes(y = tons)) + labs(title = "Exportações brasileiras (Milho)", x = "Ano", y="Em milhões de toneladas")
graf_ExpCorn 

graf_PrCorn<-ggplot(base) + aes(x = year) + geom_line(aes(y = price_corn,col="Preços Milho"))
graf_PrCorn

graf_PrCorn + geom_line(aes(y = gdp_iran, col="PIB Iran")) + geom_line(aes(y = gdp_vietnam, col="PIB Vietnã")) + 
  geom_line(aes(y = gdp_world, col="PIB Mundo")) + geom_line(aes(y = gdp_japan, col="PIB Japão")) +
  geom_line(aes(y = gdp_egypt, col="PIB Egito")) +
  labs(title = "Covariáveis", x = "Ano", y="Em índice", color = "Legenda")

# Estacionariedade

# Exportações
adfExpCorn<-adf.test(Exp_Corn)
adfExpCorn

adf.test(diff(Exp_Corn))
adf.test(diff(diff(Exp_Corn)))

Exp_CornEst<-diff(diff(Exp_Corn))
plot(Exp_CornEst)

# Preço
adfPrCorn<-adf.test(PCorn)
adfPrCorn

adf.test(diff(PCorn))
adf.test(diff(diff(PCorn)))

PCornMEst<-diff(diff(PCorn))
plot(PCornMEst)

# PIBs
#GDP_Vietnam
adfGDPVi<-adf.test(GDP_Vietnam)
adfGDPVi

adf.test(diff(GDP_Vietnam))
adf.test(diff(diff(GDP_Vietnam)))
adf.test(diff(diff(diff(GDP_Vietnam))))

GDP_VietnamEst<-(diff(diff(diff(GDP_Vietnam))))
plot(GDP_VietnamEst)

# GDP_Japan
adfGDPJp<-adf.test(GDP_Japan)
adfGDPJp

adf.test(diff(GDP_Japan))
adf.test(diff(diff(GDP_Japan)))
adf.test(diff(diff(diff(GDP_Japan))))

GDP_JapanEst<-diff(diff(diff(GDP_Japan)))
plot(GDP_JapanEst)

# GDP_Egypt
adfGDPEg<-adf.test(GDP_Egypt)
adfGDPEg

adf.test(diff(GDP_Egypt))
adf.test(diff(diff(GDP_Egypt)))
adf.test(diff(diff(diff(GDP_Egypt))))
adf.test(diff(diff(diff(diff(GDP_Egypt)))))
adf.test(diff(diff(diff(diff(diff(GDP_Egypt))))))

GDP_EgyptEst<-diff(diff(diff(diff(diff(GDP_Egypt)))))
plot(GDP_EgyptEst)

GDP_WorldEst
GDP_IranEst

# Juntando todas as variáveis

Cornbase<-cbind(Exp_CornEst,PCornMEst,GDP_IranEst,GDP_VietnamEst,GDP_JapanEst)
Cornbase<-replace(x = Cornbase, list = is.na(Cornbase), values = 0)
Cornbase

# Lags ideal
lagselect<-vars::VARselect(Cornbase,lag.max = 10,type="both")
lagselect$selection # 2

# Teste de cointegração
cointestCorn<-urca::ca.jo(Cornbase, type="trace",K=2)
summary(cointestCorn) #r = 4

# Modelo
# VECM
library(tsDyn)
ModelVECCorn<-VECM(Cornbase,2,r=4, estim = "ML")
ModelVECCorn
summary(ModelVECCorn)

# Diagnóstico do modelo

# Transformando VECM em VAR

ModelVARCorn<-vec2var(cointestCorn, r=4)

# Autocorrelação serial
serialtestCorn<-serial.test(ModelVARCorn, lags.pt = 10)
serialtestCorn # > 0,05 não tem autocorrelação serial

# Heterocedasticidade
archtestCorn<-arch.test(ModelVARCorn, lags.multi = 3)
archtestCorn # > que 0,05 não tem hetecedasticidade

# distribuição normal dos resíduos
ResCorn<-normality.test(ModelVARCorn, multivariate.only = TRUE)
ResCorn # > 0,05 é normalmente distribuido

# Forecast
forecastCorn<-predict(ModelVARCorn, n.ahead = 11, lambda = 0)
forecastCorn
fanchart(forecastCorn, names = "Exp_CornEst")

