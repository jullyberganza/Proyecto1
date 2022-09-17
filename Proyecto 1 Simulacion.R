library(dplyr)

vprob <- list(
  c(0.1, 0.15, 0.1, 0.35, 0.25, 0.05, 0  ),
  c(0.1, 0.1, 0.15, 0.2, 0.35, 0.1, 0    ),
  c(0    , 0.1, 0.1, 0.2, 0.1, 0.25, 0.25),
  c(0  , 0.15, 0.2, 0.2, 0.15, 0.15, 0.15),
  c(0.15, 0.15, 0.2, 0.2, 0.1, 0.1, 0.1  ),
  c(0.2, 0.15, 0.1, 0.5, 0.05, 0  , 0    ),
  c(0.35, 0.25, 0.2, 0.1, 0.1, 0  , 0    )
)

crearServ <- function(n){
  out <- data.frame(n=0,tInicio=0,tFin=0)
  for (i in 1:n) {
    out <- rbind(out,c(i,0,0))
  }
  return(out[-1, ])
}

intDia <- 480
dias <- 7
finAt <- intDia * dias

simstage <-function(nServers){
  tacum <<- 0
  serverF <<- 1
  out <- data.frame(dia=0,customer=0,last=0,service=0,arrival=0,begin=0,end=0,espera=0,server=0)
  i <<- 2
  for (d in 1:dias) {
    IDia <<- TRUE
    tacum <<- 0
    servers <<- crearServ(nServers)
    cliente <<- 1
    while(tacum <= intDia){
      llegada <- sample(c(0:6),1,prob = unlist(vprob[d]))
      if(tacum + llegada > intDia){
        break;
      }
      rServ <- rnorm(1,mean = 8,sd = 5)
      serv <- ceiling(ifelse(rServ>=1, yes = rServ, no = 0))
      last <- ifelse(!IDia,yes = llegada,no=0)
      service <- serv
      arrival <- ifelse(!IDia,yes = out[i-1,]$arrival+llegada,no=0)
      begin <- 0
      if(nrow(servers %>% filter(tFin <= arrival))>0){
        serverF <<- (servers %>% filter(tFin <= arrival))$n[1]
        begin <- ifelse(!IDia,yes = arrival,no=0)
      }else{
        serverF <<- (servers %>% filter(tFin == min(tFin)))$n[1]
        begin <- servers[serverF,]$tFin
      }
      
      end <- begin + service
      servers[serverF,]$tInicio <<- begin
      servers[serverF,]$tFin <<- end
      
      espera<- begin - arrival
      out <- rbind(out,c(d,cliente,last,service,arrival,begin,end,espera,serverF))
      
      tacum <<- arrival
      i <<- i+1
      cliente <<- cliente+1
      IDia <<- FALSE
    }
  }
  return (out[-1, ])
}

simul<- function(semanas, servidores){
  out <- data.frame(simul=0, tMedCola=0, tEsperaCola=0, tMaxEspera=0, persNoAt=0)
  
  for (i in 1:semanas) {
    escenario <- simstage(servidores)
    
    arrEspera <<- 0 
    for (j in 1:7) {
      n <- max((escenario %>% filter(dia == j))$arrival)
      arrEsperaAux <<- 0
      for (k in 1:n) {
        arrEsperaAux <<- append(arrEsperaAux,nrow(escenario %>% filter(dia == j) %>% filter(arrival <= k) %>% filter(begin>k)))
      }
      arrEspera <<- append(arrEspera, arrEsperaAux[-1])
    }
    tMedCola <- mean(arrEspera[-1])
    persNoAt <- ifelse(tMedCola >= 10, yes = tMedCola - 10, no = 0)
    tEsperaCola <- mean(escenario$espera)
    tMaxEspera <- max(escenario$espera)
    
    out <- rbind(out,c(i, tMedCola, tEsperaCola, tMaxEspera, persNoAt))
  }
  return(out[-1,])
}


#1. Haga una simulación para determinar el tamaño promedio de la cola cuando hay 1,2,3,4,5,6,7 servidores. (Cola Infinita)
#Para un servidor

sim1 <- simul(10,1)

head(sim1)

mean(sim1$tMedCola)

#Para 2 servidores

sim2 <- simul(10,2)
head(sim2)

mean(sim2$tMedCola)

#Para 3 servidores

sim3 <- simul(10,3)
head(sim3)

mean(sim3$tMedCola)

#Para 4 servidores

sim4 <- simul(10,4)
head(sim4)

mean(sim4$tMedCola)

#Para 5 servidores

sim5 <- simul(10,5)
head(sim5)

mean(sim5$tMedCola)

#Para 6 servidores
sim6 <- simul(10,6)
head(sim6)

mean(sim6$tMedCola)

#Para 7 servidores

sim7 <- simul(10,7)
head(sim7)

mean(sim7$tMedCola)

#2. Si no se quiere que un cliente esté más de 15 minutos en cola, cuántos agentes tiene que tener el banco? (Cola Infinita)
#Para 1 servidor
#Tiempo espera medio

mean(sim1$tEsperaCola)

#Para 2 servidores 
#Tiempo Espera Medio

mean(sim2$tEsperaCola)

#Para 3 servidores
#Tiempo Espera Medio

mean(sim3$tEsperaCola)

#Para 4 servidores
#Tiempo Espera Medio

mean(sim4$tEsperaCola)

#Para 5 servidores
#Tiempo Espera Medio

mean(sim5$tEsperaCola)

#Para 6 servidores 
#Tiempo Espera Medio

mean(sim6$tEsperaCola)

#Para 7 servidores
#Tiempo Espera Medio

mean(sim7$tEsperaCola)

#Para que un cliente no espere más de 15 minutos en una cola, debe tener 6 agentes atendiendo. 


#3. Com una restricción extra el banco no puede tener en cola más de k personas. Cuántas personas no son aceptadas en promedio por día al banco.
#Personas no atendias promedio

#Para 1 servidor

mean(sim1$persNoAt)

#Para 2 servidores

mean(sim2$persNoAt)

#Para 3 servidore

mean(sim3$persNoAt)

#Para 4 servidores 

mean(sim4$persNoAt)

#Para 5 servidores

mean(sim5$persNoAt)

#Para 6 servidores

mean(sim6$persNoAt)

#Para 7 servidores

mean(sim7$persNoAt)

#En promedio un cliente solo está dispuesto a esperar t minutos en la cola y si la cola es mayor de n personas los clientes deciden no hacer cola e irse del banco. Simule con estas restricciones el banco y determine cuántos clientes abandonan la cola y cuantos deciden no entrar al banco. Suponiendo cola infita.

simEsc2 <- function(nServers){
  tacum <<- 0
  serverF <<- 1
  out <- data.frame(dia=0, customer=0, last=0, service=0, arrival=0, begin=0, end=0, espera=0, server=0, abandona=0)
  out2 <- data.frame(dia=0, abandona=0, noEntra=0)
  i<<- 2
  for (d in 1:dias) {
    IDia <<- TRUE
    tacum <<- 0
    servers <<- crearServ(nServers)
    cliente <<- 1
    while (tacum <= intDia) {
      llegada <- sample(c(0:6),1,prob = unlist(vprob[d]))
      if(tacum + llegada > intDia){
        break;
      }
      rServ <- rnorm(1, mean = 8, sd = 5)
      serv <- ceiling(ifelse(rServ >= 1, yes = rServ, no=1))
      last <- ifelse(!IDia, yes = llegada, no=0)
      service <- serv
      arrival <- ifelse(!IDia, yes = tacum + llegada, no=0)
      begin <- 0 
      if(nrow(servers %>% filter(tFin <= arrival))>0){
        serverF <<- (servers %>% filter(tFin <= arrival))$n[1]
        begin <- ifelse(!IDia, yes = arrival, no=0)
      }else{
        serverF <<- (servers %>% filter(tFin == min(tFin)))$n[1]
        begin <- servers[serverF,]$tFin
      }

      end <- begin + service
      
      espera <- begin - arrival
      tiempoCola <- nrow(out %>% filter(dia == d) %>% filter(arrival <= arrival) %>% filter(begin > arrival))
      if(tiempoCola <= 10){
        if(espera <= 10){
          servers[serverF,]$tInicio <<- begin
          servers[serverF,]$tFin <<- end
          out <- rbind(out,c(d,cliente,last,service,arrival,begin,end,espera,serverF,0))
          i <- i + 1
        } else{
          out2 <- rbind(out2,c(d,0,1))
        }
        
        cliente <<- cliente + 1
      }else{
        out2 <- rbind(out2,c(d,0,1))
      }
      
      tacum <<- arrival
      
      IDia <<- FALSE
    }
    
  }
  
  return(out2[-1,])
}

simul2 <- function(semanas,servidores){
  out <- data.frame(mediaAbandona = 0, mediaNoEntra =0)
  
  for (i in 1:semanas) {
    escenario <- simEsc2(servidores)
    
    mediaAbandona <- mean((escenario %>% group_by(dia) %>% summarise(total = sum(abandona)))$total)
    mediaNoEntra <- mean((escenario%>%group_by(dia) %>% summarise(total = sum(noEntra)))$total)
    out <- rbind(out, c(mediaAbandona, mediaNoEntra))
    
  }
  return(out[-1,])
}

#Para 1 servidor 

simu1 <- simul2(10,1)
head(simu1)

mean(simu1$mediaAbandona)

mean(simu1$mediaNoEntra)

