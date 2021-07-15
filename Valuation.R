#Carregando Pacotes
library(BatchGetSymbols)
library(GetDFPData)
library(rbcb)
library(ggplot2)
library(dplyr)
library(plyr)
library(reshape2)
library(psych)
library(forecast)
library(zoo)
library(xts)

#Organizando parâmetros
di=as.Date("01/01/2006","%d/%m/%Y")
din=as.Date("01/01/2021","%d/%m/%Y")
df=Sys.Date()
n_acao=15739243302

#Importando dados
data=read.csv2("C:\\Users\\evani\\Desktop\\Valuation.csv")

data$Data=as.Date(ISOdate(data$Data,1,1))
data$Data=as.Date(paste(data$Data, 1, 1, sep = "-"))
data$Data=as.Date(as.yearmon(data$Data))

ggplot(data) +
  aes(x = Data, y = FC) +
  geom_line(size = 1.8, colour = "#01021A") +
  labs(x = "Data", y = "Fluxo de Caixa", title = "Fluxo de Caixa Estimado", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x = Data, y = LL) +
  geom_line(size = 1.8, colour = "#01021A") +
  labs(x = "Data", y = "Fluxo de Caixa", title = "Lucro Líquido", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x = Data, y = CGLE) +
  geom_line(size = 1.8, colour = "#01021A") +
  labs(x = "Data", y = "Fluxo de Caixa", title = "Fluxo de Caixa Estimado", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

#RISCO

#Taxa livre de risco
TBF = get_series(c(TBF=253), start_date = di, as="data.frame")
colnames(TBF)=c("Data","Data Final","TBF")
TBF$TBFad=((1+(TBF$TBF/100))^(12/252))-1
TRF=geometric.mean(1+TBF$TBFad)-1

#Beta
ibov<-c("ABEV3.SA")
ibov<-unique(ibov)
ibov_i= c('^BVSP')
colect=c(ibov_i,ibov)
benchmark='^BVSP'

dados_ibov = BatchGetSymbols(
  tickers = colect,
  first.date = di,
  last.date = df,
  thresh.bad.data = 0.05,
  bench.ticker = benchmark,)
dados_ibov = dados_ibov$df.tickers
dados_ibov2=dlply(dados_ibov,.(ticker), function(x){rownames(x)=x$row;x$row = NULL;x})

dados_acao = dados_ibov2[[1]][,c(7,6)]
colnames(dados_acao) = c("Data", dados_ibov2[[1]][1,8])

for(i in 2:length(dados_ibov2)){
  acao = dados_ibov2[[i]][,c(7,6,5)]
  colnames(acao) = c("Data", dados_ibov2[[i]][1,8],"volume")
  dados_acao=merge(dados_acao, acao[,1:2], by="Data")
}
colnames(dados_acao)<-gsub(x=colnames(dados_acao), pattern = '\\^', replacement = '')

dados_acao$rBVSP=log(dados_acao$BVSP)-log(lag(dados_acao$BVSP,1))
dados_acao$rABEV3=log(dados_acao$ABEV3.SA)-log(lag(dados_acao$ABEV3.SA,1))

control=merge(dados_acao,TBF[,-2], by="Data")
control=control[rowSums(is.na(control))==0,]
control=xts(control[,-1],order.by = control$Data)

modelo.beta=lm((rABEV3-TBFad)~(rBVSP-TBFad),control)
summary(modelo.beta)
beta=modelo.beta$coefficients[2]

#Prêmio de Risco
PRA=geometric.mean(1+(control$rABEV3-control$TBFad))-1

#Custo do Capital Próprio
CCP=((1+TRF)^252-1)+beta*((1+PRA)^252-1)

#TAXA DE CRESCIMENTO

#Correlação entre variação do PIB e a Companhia
PIB = get_series(c(PIB=1208), start_date = as.Date("01/01/2005","%d/%m/%Y"), as="data.frame")

PI=NA
PF=NA
for (a in 2006:2020) {
  a=as.character(a)
  DAT=dados_acao$Data[format(dados_acao$Data,format="%Y")==a]
  PI=c(PI,dados_acao$ABEV3.SA[dados_acao$Data==min(DAT)])
  PF=c(PF,dados_acao$ABEV3.SA[dados_acao$Data==max(DAT)])
}

PIB$VAR=(PIB$PIB-lag(PIB$PIB,1))/lag(PIB$PIB,1)
PIB$PI=PI
PIB$PF=PF
PIB$VARA=(PF-PI)/PI
data=cbind(data,PIB[-1,2:6])
data$VFC=(data$FC-lag(data$FC,1))/lag(data$FC,1)
data$PIB=data$PIB/1000000000000

ggplot(data) +
  aes(x = VAR, y = VFC) +
  geom_point(size = 2.5L, colour = "#040EC9") +
  geom_smooth(method='lm')+
  labs(x = "Variação do PIB", y = "Variação do Fluxo de Caixa", title = "Correlação", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x = VAR, y = VARA) +
  geom_point(size = 2.5L, colour = "#040EC9") +
  geom_smooth(method='lm')+
  labs(x = "Variação do PIB", y = "Variação da Ação", title = "Correlação", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x = VARA, y = VFC) +
  geom_point(size = 2.5L, colour = "#040EC9") +
  geom_smooth(method='lm')+
  labs(x = "Variação da Ação", y = "Variação do Fluxo de Caixa", title = "Correlação", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x =PIB , y =FC) +
  geom_point(size = 2.5L, colour = "#040EC9") +
  geom_smooth(method='lm')+
  labs(x = "PIB", y = "Fluxo de Caixa", title = "Correlação", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

ggplot(data) +
  aes(x =PF , y =FC) +
  geom_point(size = 2.5L, colour = "#040EC9") +
  geom_smooth(method='lm')+
  labs(x = "PIB", y = "Fluxo de Caixa", title = "Correlação", subtitle = "Empresa: AmBev", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

#Estimação do Crescimento

#Análise do Cenário Econômico
#Coletando dados de Expectaticas
PIB=get_annual_market_expectations("PIB Total", start_date = di, as="data.frame")
PIB$date<-as.Date(PIB$date)
ANO=paste0("20",format(PIB$date[1], format="%y"))
DATE<-PIB$date[PIB$reference_year==ANO & PIB$date>=din]
PIB<-PIB$median[PIB$reference_year==ANO & PIB$date>=din]
PIB<-data.frame(date=DATE,PIB=PIB)

ggplot(PIB) +
  aes(x = date, y = PIB) +
  geom_line(size = 1.6, colour = "#638F8C") +
  labs(x = "Data", y = "Taxa de crescimento", title = "Expectativa do Crescimento do PIB", subtitle = "Dados do Relatório Focus - BACEN", caption = "Elaboração: Evânio Marques") +
  theme_minimal()

#Crescimento Composto
g=lm(log(FC)~Tempo,data)
summary(g)
ge=exp(g$coefficients[2])-1


#VALOR ESPERADO DO FC
st<-ts(data$FC,start = 1, frequency = 1)
modelo=Arima(st,order = c(2,1,1))
summary(modelo)
plot(forecast(object = modelo, h=2, level = 0.4))
ff=forecast(object = modelo, h=2, level = 0.4)
ff=data.frame(ff$mean,ff$low,ff$upper)

#VALUATION
Valuation=(as.vector(ff$ff.mean[1]))/(CCP-ge)
Valuation_inf=(as.vector(ff[1,2]))/(CCP-ge)
Valuation_sup=(as.vector(ff[1,3]))/(CCP-ge)


preço=(Valuation*1000000)/n_acao
preço_inf=(Valuation_inf*1000000)/n_acao
preço_sup=(Valuation_sup*1000000)/n_acao

dados_acao$pinf=preço_inf
dados_acao$pmed=preço
dados_acao$psup=preço_sup

graf=dados_acao[,-4:-5]
graf=melt(graf,id=c("Data"))

graf %>%
  filter(Data >= "2021-01-01" & Data <= "2021-05-10") %>%
  filter(!(variable %in% 
             "BVSP")) %>%
  ggplot() +
  aes(x = Data, y = value, colour = variable, group = variable) +
  geom_line(size = 1.3) +
  scale_color_manual(labels = c("AmBev","Inferior","Médio","Superior"), values = c("#324472", "#A41410", "#E86D1F", "#2693A1")) +
  labs(x = "Data", y = "Índice", title = "Comportamento do Portfólio de Ativos", caption = "Elaboração: Evânio Marques", color = "Legenda") +
  theme_minimal()

