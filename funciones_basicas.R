# funciones basicas para el ejemplo de Genetic Algorithm de Robby the Robot
library(crayon)
library(dplyr)
library(purrr)
library(furrr)

plan(multisession,workers=8)

acciones <- 0:6
situaciones <- -1:1
situaciones_posibles <- expand.grid(rep(list(situaciones), 5))

crear_codigo <- function(length_codigo=length(situaciones)^5){
  codigo <- sample(x = acciones,size = length_codigo,replace = TRUE)  
  return(codigo)
}

crear_mundo <- function(lado=10,latas=50){
  pop <- c(rep(1,latas),rep(0,lado*lado-latas))
  mundo <- matrix(data = sample(x = pop,size = lado*lado,replace = FALSE),nrow = lado,ncol = lado)
  return(mundo)
}

get_situacion_posicion <- function(mundo,x,y){
  current <- mundo[x,y]
  if(y!=1){
  norte <- mundo[x,y-1]
  } else {
    norte <- -1
  }
  if(x!=nrow(mundo)){
    este <- mundo[x+1,y]
  } else {
    este <- -1
  }
  if(x!=1){
    oeste <- mundo[x-1,y]
  } else {
    oeste <- -1
  }
  if(y!=nrow(mundo)){
    sur <- mundo[x,y+1]
  } else {
    sur <- -1
  }
  situacion <- c(current,norte,este,oeste,sur)
  return(situacion)
}

random_posicion <- function(lado){
  x <- sample(x = 1:lado,1)
  y <- sample(x = 1:lado,1)
  return(c(x,y))
}

get_accion_situacion_posicion <- function(situacion_posicion,codigo){
  nfila <- which(apply(situaciones_posibles, 1, function(x) identical(as.numeric(x), situacion_posicion)) == "TRUE")
  accion <- codigo[nfila]
  if(length(accion)==0 | is.na(accion)){
    print(situacion_posicion)
    print(codigo)
    stop("error")
  }
  return(accion)
}

hacer_accion_mundo <- function(mundo,accion,marcador,posicion){
  if(accion==5){
    #move random
    accion <- sample(x = 1:4,size = 1)
    }
  if(accion==1){
    #ir al norte
    if(posicion[2]==1){ #no se puede ir al norte, hay un muro
      marcador <- marcador - 5
    } else {
      posicion[2] <- posicion[2] - 1
    }
  }
  if(accion==2){
    #ir al este
    if(posicion[1]==ncol(mundo)){ #no se puede ir al este, hay un muro
      marcador <- marcador - 5
    } else {
      posicion[1] <- posicion[1] + 1
    }
  }
  if(accion==3){
    #ir al oeste
    if(posicion[1]==1){ #no se puede ir al oeste, hay un muro
      marcador <- marcador - 5
    } else {
      posicion[1] <- posicion[1] - 1
    }
  }
  if(accion==4){
    #ir al sur
    if(posicion[2]==nrow(mundo)){ #no se puede ir al sur, hay un muro
      marcador <- marcador - 5
    } else {
      posicion[2] <- posicion[2] + 1
    }
  }
  if(accion==6){
    if(mundo[posicion[1],posicion[2]]==1){
      marcador <- marcador + 10
      mundo[posicion[1],posicion[2]] <- 0
    } else {
      marcado <- marcador - 1
    }
  }
  resultado <- list(mundo=mundo,marcador=marcador,posicion=posicion)
  return(resultado)
}

operar_en_mundo <- function(codigo,posicion,marcador,mundo){
  situacion_posicion <- get_situacion_posicion(mundo = mundo,x=posicion[1],y = posicion[2])
  accion <- get_accion_situacion_posicion(situacion_posicion = situacion_posicion,codigo = codigo)
  resultado <- hacer_accion_mundo(mundo = mundo,accion = accion,marcador = marcador,posicion = posicion)
  return(resultado)
}

operar_en_mundo_bucle <- function(mundo,codigo,pasos=50){
  posicion <- random_posicion(10)
  marcador <- 0
  for (paso in 1:pasos){
    resultado <- operar_en_mundo(codigo = codigo,posicion = posicion,marcador = marcador,mundo = mundo)
    posicion <- resultado$posicion
    mundo <- resultado$mundo
    posicion <- resultado$posicion
    marcador <- resultado$marcador
  }
  return(marcador)
}

crear_primera_generacion <- function(N){
  generacion <- N %>% rerun(crear_codigo())
  return(generacion)
}

crear_mundos <- function(m){
  mundos <- m %>% rerun(crear_mundo())
  return(mundos)
}

operar_en_mundos_bucle <- function(codigo,pasos=50,mundos,paralelo=FALSE){
  if(paralelo){
    resultados <- mundos %>% furrr::future_map_dbl(operar_en_mundo_bucle,pasos=pasos,codigo=codigo)
  } else {
    resultados <- vector("numeric",length(mundos))
      for (n in 1:length(mundos)){
        resultados[n] <- operar_en_mundo_bucle(mundo = mundos[[n]],codigo = codigo,pasos = pasos)
      }
  }
  resultados <- mean(resultados)
  return(resultados)
}

probar_generacion <- function(generacion,nmundos,pasos,paralelo=FALSE){
  mundos <- crear_mundos(m = nmundos)
  resultados_gen <- vector("numeric",length(generacion))
  for (n in 1:length(generacion)){
    cat(sprintf("Probando replicante %03d\r",n))
    resultados_gen[n] <- operar_en_mundos_bucle(codigo = generacion[[n]],pasos = pasos,mundos = mundos,paralelo=paralelo)
  }
  return(resultados_gen)
}

reproducir_generacion <- function(fraccion=0.5,tasa_mutacion=0.005,generacion,resultados){
  nex <- round(length(generacion)*fraccion)
  if(nex %% 2 != 0){
    nex <- nex + 1
  }
  exitosos <- generacion[order(resultados)[1:nex]]
  #cada exitoso se empareja 2 veces
  parejas <- c(combinaciones_sin_repeticion(candidatos = 1:nex),combinaciones_sin_repeticion(candidatos = 1:nex))
  generacion <- vector("list",length(parejas)*2)
  n <- 1
  for (i in 1:length(parejas)){
    hijos <- reproducir_pareja(parejas[[i]],exitosos,tasa_mutacion=tasa_mutacion)
    generacion[[n]] <- hijos[[1]]
    n <- n+1
    generacion[[n]] <- hijos[[2]]
    n <- n+1
  }
  return(generacion)
}

combinaciones_sin_repeticion <- function(candidatos){
  parejas <- vector("list",length(candidatos)/2)
  for (n in 1:length(parejas)){
    parejas[[n]] <- sample(candidatos,2,replace = FALSE)
    candidatos <- candidatos[!(candidatos %in% parejas[[n]])]
  }
  return(parejas)
}

reproducir_pareja <- function(pareja,exitosos,tasa_mutacion=0.005){
  max <- length(situaciones)^5
  mitad <- round(max/2)
  hijo1 <- c(exitosos[[pareja[1]]][1:mitad],exitosos[[pareja[2]]][(mitad+1):max])
  hijo1 <- mutar(hijo1,tasa_mutacion = tasa_mutacion)
  hijo2 <- c(exitosos[[pareja[2]]][1:mitad],exitosos[[pareja[1]]][(mitad+1):max])
  hijo2 <- mutar(hijo2,tasa_mutacion = tasa_mutacion)
  return(list(hijo1,hijo2))
}

mutar <- function(codigo,tasa_mutacion=0.005){
  gen_muta <- rbinom(n = length(situaciones)^5,size = 1,prob = 0.005)
  if(sum(gen_muta)!=0){
    codigo[gen_muta] <- sample(x = acciones,size = sum(gen_muta),replace = TRUE)
  }
  return(codigo)
}

#juego a ser dios
evolucion <- function(poblacion_inicial=50,nmundos=50,generaciones=10,tasa_mutacion=0.005,pasos=50,paralelo=FALSE){
  resultados_evolucion <- vector("numeric",generaciones)
  generacion <- crear_primera_generacion(N = poblacion_inicial)
  for (i in 1:generaciones){
    cat(blue(sprintf("Generacion %03d\n",i)))
    print(system.time(resultados_todos <- probar_generacion(generacion = generacion,nmundos = nmundos,pasos = pasos,paralelo=paralelo)))
    resultados_evolucion[i] <- mean(resultados_todos)
    cat(red(sprintf("\nResultado medio de la generacion %03d %s\n",i,round(resultados_evolucion[i],2))))
    generacion <- reproducir_generacion(fraccion = 0.5,tasa_mutacion = tasa_mutacion,generacion = generacion,resultados = resultados_todos)
  }
  return(resultados_evolucion)
}