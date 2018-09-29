dat2017 <- read.csv("~/R/vra_062017.csv", header = TRUE)
dat2018 <- read.csv("~/R/vra_062018.csv", header = TRUE)

# Ocorrências por companhia área. Quem voa mais?
summary(dat2017$ICAO.Empresa.Aerea)
summary(dat2018)

# Qual ano se voou mais entre 2017 e 2018?
length(dat2017$ICAO.Empresa.Aerea)
length(dat2018$ICAO.Empresa.Aerea)

bindat = rbind(dat2017, dat2018)
summary(bindat)


# voos realizados e cancelados por companhia
