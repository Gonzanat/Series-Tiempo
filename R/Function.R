##Variables globales
assign("datos", 0, .GlobalEnv)
assign("v_Error2_lin", 0, .GlobalEnv)
assign("v_Error2_cua", 0, .GlobalEnv)
assign("v_Error2_cub", 0, .GlobalEnv)
assign("v_Error2_exp", 0, .GlobalEnv)
assign("v_Error2_log", 0, .GlobalEnv)



Playing_with_your_data <- function()
{

  library(shiny)
  library(e1071)
  library(boot)
  library(corrplot)
  library(forecast)



  runApp(shinyApp(
    ui = myFluidPage,
    server = myShinyServer
  ))

}


##2017-09-12: Funciones para aplicar bootstrapping sobre las variables que no tienen comportamiento de distribuciones
##teóricas:


##Media: Estimación de la media a través de muestreos.
fun_mean<-function(data, indices){

  muestra=d<-data[indices]

  valor =mean(muestra)
  return(valor)
}


##Mediana: Estimación de la mediana a través de muestreos.
fun_median<-function(data, indices){

  muestra=d<-data[indices]

  valor =median(muestra)
  return(valor)
}


##Desviación: Estimación de la desviación a través de muestreos.
fun_sd<-function(data, indices){

  muestra=d<-data[indices]

  valor =sd(muestra)
  return(valor)
}

##Varianza: Estimación de la Varianza a través de muestreos.
fun_var<-function(data, indices){

  muestra=d<-data[indices]

  valor =var(muestra)
  return(valor)
}


##Función para formar la serie de tiempo con base a los datos.

fun_TS <-function (data, start_year, period, freq){

  TS = ts(data = data, start=c(start_year,period), frequency = freq)

  print(TS)

  return (TS)

}
