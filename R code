#Reading the file
fig_count=0
adani_ports = read.csv("ADANIPORTS.csv")
adani_ports = adani_ports %>% select(1,10)
adani_ports = tail(adani_ports,n=1500)
#Data Preprocessing
adani_ports<-adani_ports %>% mutate(MonthName = as.character(month(Date, label = TRUE, 
abbr = TRUE))) ##convert numerical month into alphabets
adani_ports$Date <- as.Date(adani_ports$Date, format = "%Y-%m-%d")
adani_ports$year <- format(adani_ports$Date, format="%Y")
adani_ports$mon <- format(adani_ports$Date, format="%m")
adani_ports$year <- paste(adani_ports$year,adani_ports$mon,sep = )
adani_ports$date <- paste(adani_ports$year,adani_ports$MonthName,sep = )
adani_ports<-aggregate( VWAP ~ year, adani_ports, mean )
adani_ports = adani_ports[-c(1:2),]
adani_ts = ts(as.vector(adani_ports$VWAP),start=c(2015,4), end=c(2021,4), frequency=12)
#Custom layout – User defined function for further analysis
custom_layout <- function(p,c,title,xtitle,ytitle){
 plotly::layout(p,
 paper_bgcolor = "black",
 plot_bgcolor = "black",
 font = list(color = "white"),
 title= paste(c("Fig ", c,title), collapse = " "),
 yaxis = list(linecolor = "#6b6b6b",
 zerolinecolor = "#6b6b6b",
 title=ytitle,
 gridcolor= "#444444"),
 xaxis = list(linecolor = "#6b6b6b",
 zerolinecolor = "#6b6b6b",
 title=xtitle,
 gridcolor= "#444444"))
 
}
#Time_series_plot – User defined function for further analysis
time_series_plot <- function(){ts_plot(adani_ts,
 Ytitle ="VWAP of ADANI PORTS over years",
 color="#FFFF00",
 Xtitle = "Time(Years)", 
 Xgrid = TRUE,
 Ygrid = TRUE)}
fig<- time_series_plot() %>% custom_layout(fig_count<<-fig_count+1,": Time Series plot Adani
Port stocks","Time","Standardized residuals")
fig
#Correlation graph
fig <- plotly::plot_ly(data=data.frame(adani_ts),x =~zlag(adani_ts), y = ~adani_ts,text = ~paste("
VWAP: ", adani_ts, '$',size = ~adani_ts),marker = list(sizemode = 'diameter') ,
 type='scatter',mode='markers')%>% custom_layout(fig_count<<-fig_count+1,": Scatter plot of T
ime series data","Previous year VWAP Price","VWAP Price")
fig
#User-defined function for ACF (called whenever necessary)
ggacf <- function(series) {
 significance_level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(series))) 
 a<-acf(series, plot=F)
 a.2<-with(a, data.frame(lag, acf))
 g<- ggplot(a.2[-1,], aes(x=lag,y=acf)) +
 geom_bar(stat = "identity", position = position_dodge(width=0.2),fill="#1f77b4") + xlab('La
g') + ylab('ACF') +
 geom_hline(yintercept=c(significance_level,-significance_level), lty=3,color="white")+
 theme(axis.text.x = element_text(colour = "white"),
 axis.text.y = element_text(colour = "white"));
 # fix scale for integer lags
 if (all(a.2$lag%%1 == 0)) {
 g<- g + scale_x_discrete(limits = as.factor(seq(1, max(a.2$lag))));
 }
 return(g);
}
plot<-ggacf(adani_ts)
fig <- ggplotly(plot)
fig<- fig %>%
 
 
 custom_layout(fig_count<<-fig_count+1,": Autocorrelation plot","Lag","ACF") %>% layout(sho
wlegend = FALSE)
suppressWarnings(fig)
#User-defined function for PACF plot (used whenever necessary)
ggpacf <- function(series) {
 significance_level <- qnorm((1 + 0.95)/2)/sqrt(sum(!is.na(series))) 
 a<-pacf(series, plot=F)
 a.2<-with(a, data.frame(lag, acf))
 g<- ggplot(a.2[-1,], aes(x=lag,y=acf)) +
 geom_bar(stat = "identity", position = "identity",fill="#1f77b4") + xlab('Lag') + ylab('ACF') +
 geom_hline(yintercept=c(significance_level,-significance_level), lty=3,color="white")+
 theme(axis.text.x = element_text(colour = "white"),
 axis.text.y = element_text(colour = "white"));
 # fix scale for integer lags
 if (all(a.2$lag%%1 == 0)) {
 g<- g + scale_x_discrete(limits = as.factor(seq(1, max(a.2$lag))));
 }
 return(g);
}
plot<-ggpacf(adani_ts)
fig <- ggplotly(plot)
fig<- fig %>%
 
 
 custom_layout(fig_count<<-fig_count+1,": Partial Autocorrelation plot","Lag","PACF") %>% la
yout(showlegend = FALSE)
suppressWarnings(fig)
#First order seasonal differencing
m1 = arima(adani_ts,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=12))
res_m1 = residuals(m1)
#Seasonal differencing with higher orders of P and Q
m2 = arima(adani_ts,order=c(0,0,0),seasonal=list(order=c(1,1,1),period=12))
res_m2 = residuals(m2)
#Seasonal differencing with higher orders of Q
m3 = arima(adani_ts,order=c(0,0,0),seasonal=list(order=c(1,1,2),period=12))
res_m3 = residuals(m3)
#Transformation using Box-Cox Transformation method
adani_ts_transform = BoxCox.ar(adani_ts, method = "yule-walker")
text =paste("Fig",fig_count<<-fig_count+1,": BoxCox Transformation")
mtext(text, outer=FALSE,line=1,cex=1)
#Logarithmic Transformation
log.adani_ts = log(adani_ts)
p<-ts_plot(log.adani_ts,
 Ytitle ="VWAP of ADANI PORTS over years",
 Xtitle = "Time(Years)", 
 color = "#FFFF00",
 Xgrid = TRUE,
 Ygrid = TRUE)%>%
 
 custom_layout(fig_count<<-fig_count+1,": Time series plot with transformed data","Year","Log 
of VWAP prices") %>% layout(showlegend = FALSE)
p
#Residuals of transformed series
m4 = arima(log.adani_ts,order=c(0,0,0),seasonal=list(order=c(1,1,2), period=12))
res_m4 = residuals(m4)
#First order non-seasonal differencing
m5=arima(log.adani_ts,order=c(0,1,0),seasonal=list(order=c(1,1,2),period=12))
res_m5 = residuals(m5)
#Generate BIC using Yule-Walker method
res = armasubsets(y=res_m5,nar=12,nma=12,y.name='test',ar.method='yule-walker') 
#Paramter estimation with selected models
model1 = arima(log.adani_ts,order=c(0,1,1),seasonal=list(order=c(1,1,2), period=12))

coeftest(model1)
model2 = arima(log.adani_ts,order=c(1,1,1),seasonal=list(order=c(1,1,2), period=12))
coeftest(model2)
model3 = arima(log.adani_ts,order=c(2,1,1),seasonal=list(order=c(1,1,2), period=12))
coeftest(model3)
model4 = arima(log.adani_ts,order=c(3,1,1),seasonal=list(order=c(1,1,2), period=12))
coeftest(model4)
#AIC scoring
sort.score <- function(x, score = c("bic", "aic")){
if (score == "aic"){
x[with(x, order(AIC)),]
} else if (score == "bic") {
x[with(x, order(BIC)),]
} else {
warning('score = "x" only accepts valid arguments ("aic","bic")')
}
}
sort.score(AIC(model3, model4),score="aic")
#RESIDUAL ANALYSIS – Time series plot of selected model
fig<- ts_plot(resid(model3),
 Ytitle ="VWAP of ADANI PORTS over years",
 Xtitle = "Time(Years)", 
 Xgrid = TRUE,
 Ygrid = TRUE) %>% custom_layout(fig_count<<-fig_count+1,": Time Series plot ADANIP
ORT stocks","Time","Standardized residuals")
fig
#Histogram of residuals of selected model
plot<-plot_ly(x=resid(model3), 
 type = "histogram", name = "Data"
 , showlegend = F) %>%
 
 
 
 custom_layout(fig_count<<-fig_count+1,": Histogram for residuals of Linear Trend Model","Th
eoratical quantiles","Sample residuals") %>% layout(showlegend = FALSE)
plot
#QQ Plot of residuals
text=paste("Fig ",fig_count<<-fig_count+1,":QQ plot of standardised residuals")
qqnorm(resid(model3),main=text)
qqline(resid(model3), col = 2)
text=paste("Fig ",fig_count<<-fig_count+1,":QQ plot of standardised residuals")
qqnorm(resid(model3),main=text)
qqline(resid(model3), col = 2)
#Ljung-Box plot 
k=0
LBQPlot(residuals(model3), lag.max = length(model3$residuals)-1 , StartLag = k + 1, k = 0, Squa
redQ =FALSE)
#Forecasting for next two years
fore= Arima(adani_ts,order=c(2,1,1),seasonal=list(order=c(1,1,2), period=12))
future=forecast(fore, h = 24)
main_text=paste("Fig ",fig_count<<-fig_count+1,":Forecasting for the next 2 years")
plot(future,xlab=("Year(Time)"),ylab="Freq",main=main_text)
