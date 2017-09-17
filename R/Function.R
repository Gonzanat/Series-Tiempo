library(e1071)

#' Playing with your data
#'
#' Instruction about how to play with the tool:
#'
#' 1.  Choose CSV file: Load the CSV file with the variables that you want to analyse.
#'
#' 2.  Variables: Select the variable you want to analyse.
#'
#' 3.  Variable for Correlation: Select a second variable to calculate correlation .
#'
#' 4.  Number of bins: Define how many bins you want to add to the graph.
#'
#' 5.  Quantil Value: Define the quantil you want to get from the Selected Variable.
#'
#' 6.  Observations: Indicate how many records you expect to see from the loaded file.
#'
#' 7.  Density: Active this option to graph Density Function.
#'
#' 8.  Statistic: Active this option if you want to get basic statistics of the selected variable.
#'
#' 9.  Distribution Type: Select a Distribution function to comparte histogram calculated based on Selected variable.
#' @param
#' @return
#' @export
Playing_with_your_data <- function()
{
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
