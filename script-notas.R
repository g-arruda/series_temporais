library(tidyverse)

path <- "/home/gabriel/pCloudDrive/UERJ/econometria 1/resumo/img"


#####################################
##### Deterministico

ggplot() +
  xlim(c(-10,10)) +
  geom_function(fun = function(x){1+0.1*x}) +
  labs(y="")+
  theme_bw()

#ggsave("deterministico.png", path = path, width = 13, height = 5, units = "cm")


#####################################
##### Ruido.branco

ruido.branco <- function(n){
  set.seed(123)
  df <- tibble(x = 1:n, y=rnorm(n, mean = 0, sd = 1))
  
  plot <- ggplot(df, aes(x,y)) +
    geom_line()+
    geom_hline(yintercept = 0)+
    labs(y="", x="")+
    theme_bw()
  
  return(plot)
}

ruido.branco(n = 100)

#ggsave("ruido_branco.png", path = path, width = 13, height = 5, units = "cm")




#####################################
##### Passeio.aleatório

passeio.aleatorio <- function(n){
  #set.seed(123)
  dados <- tibble(x = 1:n,
                  y1=rnorm(n, mean = 0, sd = 1),
                  y2=rnorm(n, mean = 0, sd = 1),
                  y3=rnorm(n, mean = 0, sd = 1))
  
  grafico <- ggplot(dados) +
    geom_line(aes(x, cumsum(y1), colour="red"))+
    geom_line(aes(x, cumsum(y2)))+
    geom_line(aes(x, cumsum(y3)))+
    labs(y="", x="")+
    theme_bw()
  
  return(grafico)
}

passeio.aleatorio(200)


passeio.aleatorio.drift <- function(n){
  #set.seed(123)
  dados <- tibble(x = 1:n, y=rnorm(n, mean = 0, sd = 1))
  
  grafico <- ggplot(dados) +
    geom_line(aes(x, cumsum(y+0.2), color="Drift = 0,2"))+
    geom_line(aes(x, cumsum(y),     color="Sem drift"))+
    geom_line(aes(x, cumsum(y-0.2), color="Drift = -0,2"))+
    labs(y="", x="", color="")+
    theme_bw()
  
  return(grafico)
  
}

passeio.aleatorio.drift(100)

#ggsave("passeio_aleatorio.png", path = path, width = 13, height = 5, units = "cm")


rnorm(100, mean = 0, sd = 1) %>% cumsum() %>%  ts.plot()



#### Processo AR(1)

set.seed(123)

ar_simulado <- arima.sim(model = list(order = c(2, 0, 0),
                                      ar = c(0.3, -0.6)), n = 200)

acf(ar_simulado)

ar_sumilado_tibble <- tibble(x = 1:200, y = ar_simulado)

ggplot(ar_sumilado_tibble) +
  geom_line(aes(x,y)) +
  labs(y="", x="")+
  theme_bw()

#ggsave("ar-2.png", path = path, width = 13, height = 5, units = "cm")


##### trabalhando com séries financeiras ----

raw_data <- quantmod::getSymbols("ITUB4.SA",
                                 src = "yahoo",
                                 auto.assign = FALSE)



####### PLOTANDO O RETORNO DO PREÇO DE FEHCAMENTO
plot(raw_data$ITUB4.SA.Close)


####### Retorno das ações

return <- quantmod::periodReturn(raw_data$ITUB4.SA.Close, 
                                 period = "daily",
                                 type = "arithmetic")

return_log <- quantmod::periodReturn(raw_data$ITUB4.SA.Close, 
                                     period = "daily",
                                     type = "log")



plot(return)
plot(return_log)


#######################################
### outra forma de calcular, na mão


alt_return <- diff(raw_data$ITUB4.SA.Close) / xts::lag.xts(raw_data$ITUB4.SA.Close)
alt_return_log <- diff(log(raw_data$ITUB4.SA.Close))

plot(alt_return, type = "l")
plot(alt_return_log, type = "l")



###############################################
###############################################
comparativo <- function(x,y){
  pacote <- cbind(mean(x), sd(y, na.rm = T))
  meu <- cbind(mean(x), sd(y, na.rm = T))
  k  <- c("quantmod", "na mao") 
  
  z <-  as.data.frame(rbind(pacote, meu))
  z <- cbind(k,z)
  names(z) <- c("" ,"return", "std")
  
  return(z)
}

comparativo(return, alt_return)

###############################################
###############################################



##### Teste de dick fuller

summary(urca::ur.df(raw_data$ITUB4.SA.Close))
summary(urca::ur.df(return_log))



##### autocorrelação

Box.test(return_log, lag = 1)
Box.test(return_log, lag = 5)
Box.test(return_log, lag = 10)


#### Teste normalidade
tseries::jarque.bera.test(return_log)


#### Teste heterocedasticidade
FinTS::ArchTest(return_log)



#### Exercicio 4
#### Calcular o desvio padrao

sd(return_log)
sd(raw_data$ITUB4.SA.Close)




#### exercicio 5
library(fGarch)
library(rugarch)

set.seed(12356)


ar1.arch1 <- function(ar=0.1, omega=1e-6, alpha=0.3, n=1000){
  
  spec.sim <- fGarch::garchSpec(model = list(ar=ar, omega=omega, alpha=alpha, beta=0),
                                cond.dist="std")
  
  sim <- fGarch::garchSim(spec.sim, n=n)
  
  spec.model <- rugarch::ugarchspec(mean.model = list(armaOrder = c(1,0)),
                                    variance.model = list(model = "fGARCH",
                                                          submodel = "GARCH",
                                                          garchOrder = c(1,0)),
                                    distribution.model = "std")
  
  model.fit <- rugarch::ugarchfit(spec = spec.model, data = sim, solver ='hybrid')
  
  return(model.fit)
  
}

ar.arch <- ar1.arch1()


ar1.arch1.2 <- function(ar=0.1, omega=1e-6, alpha=0.3, n=1000){
  
  spec.sim <- fGarch::garchSpec(model = list(ar=ar, omega=omega, alpha=alpha, beta=0),
                                cond.dist="std")
  
  sim <- fGarch::garchSim(spec.sim, n=n)
  
  model.fit <- fGarch::garchFit(
    ~arma(1,0) + garch(1,0), data = sim,
    trace = FALSE, include.mean = FALSE, cond.dist = "std"
  )
  
  return(model.fit)
  
}

ar.arch <- ar1.arch1.2()

summary(ar.arch)

x <- fGarch::predict(ar.arch, n.ahead=200)


plot(x$meanForecast, type="l")

#### Exercicio 7
#### Estimando um AR(1)-ARCH(1)


#dist Normal
spec_norm <- rugarch::ugarchspec(
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "fGARCH",
                        submodel = "GARCH",
                        garchOrder = c(1,1)),
  distribution.model = "norm"
)

garchfit_norm <- rugarch::ugarchfit(spec = spec_norm, data = return_log)



## dist t-student
spec_std <- rugarch::ugarchspec(
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "fGARCH",
                        submodel = "GARCH",
                        garchOrder = c(1,1)),
  distribution.model = "std"
)

garchfit_std <- rugarch::ugarchfit(spec = spec_std, data = return_log)



## dist GED
spec_ged <- rugarch::ugarchspec(
  mean.model = list(armaOrder = c(0,0)),
  variance.model = list(model = "fGARCH",
                        submodel = "GARCH",
                        garchOrder = c(1,1)),
  distribution.model = "ged"
)

garchfit_std <- rugarch::ugarchfit(spec = spec_ged, data = return_log)



##dist laplace

