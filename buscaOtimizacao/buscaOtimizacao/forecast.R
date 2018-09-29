#Importa gráfico forçando tipo numérico na coluna tarifa
jun17 <- read.csv('201706.csv', colClasses=c("TARIFA"="numeric"), sep = ";", header = TRUE)
jun18 <- read.csv('201806.csv', colClasses=c("TARIFA"="numeric"), sep = ";", header = TRUE)

#Média
mean17 <- mean(jun17$TARIFA)
mean18 <- mean(jun18$TARIFA)

sprintf("Media Junho17: %f",mean17)
sprintf("Media Junho18: %f",mean18)

#Desvio Padrão eliminando Outliers             
std17 <- mad(jun17$TARIFA)
std18 <- mad(jun18$TARIFA)

#Estrutura dados para o gráfico
result <- data.frame(mes=c("2017","2018"),
                  tarifa=c(mean17,mean18),
                  sd=c(std17,std18))

#Monta Gráfico
ggplot(result, aes(x=mes, y=tarifa)) + 
  geom_errorbar(aes(ymin=tarifa-sd, ymax=tarifa+sd), width=.2) +
  geom_line() +
  geom_point()

#Predict Jun19
#Monta uma timeseries tarifa
ts_tarifa = ts(result$tarifa,
                   start=2017,frequency=1)

#Monta uma timeseries std
ts_std = ts(result$sd,
               start=2017,frequency=1)

#predição através do metodo arima de regressao
m_aa = auto.arima(ts_tarifa)
f_aa = forecast(m_aa, h=1)

m_ab = auto.arima(ts_std)
f_ab = forecast(m_ab, h=1)

print(f_aa$fitted[1])
print(f_ab$fitted[1])

#Estrutura dados para o gráfico
result <- data.frame(ano=c("2017","2018","2019"),
                     tarifa=c(mean17,mean18,f_aa$fitted[1]),
                     sd=c(std17,std18,f_ab$fitted[1]))

#Monta Gráfico
ggplot(result, aes(x=ano, y=tarifa)) + 
  geom_errorbar(aes(ymin=tarifa-sd, ymax=tarifa+sd), width=.2) +
  geom_line() +
  geom_point()



