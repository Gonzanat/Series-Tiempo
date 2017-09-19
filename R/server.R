#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(e1071)
library(corrplot)
library(boot)


##3. Definición de la parte lógica:

myShinyServer <- function(input, output, session) {



  ##Función que valida si ya se seleccionó un archivo. Y si se seleccionó, entonce se lee.
  ##Esta función devuelve los datos "read_data()" si hay archivo seleccionado, sino return
  read_data <- reactive({


  if(is.null(input$file1)){

      return()

  }else{

      file1<- input$file1
      read.table(file=file1$datapath, header=TRUE, sep=",", dec="." )

  }

  })


  ##Funcion principal que se llama en el Main del UI:
  ##FUNCIONALIDAD: Se pinta el histograma.
  output$distPlot <- renderPlot({


    ##Se valida si hay datos leidos.

    if(is.null(read_data())){

    }else{


     ##---De aquí hacia abajo no se ha cambiado nada

      myData<- read_data()

      #Selección de la columna que contiene la variable de análisis seleccionada por el usuario.

      if(is.numeric(myData[,input$listVar])){

        ##Septiembre 17 de 2017: Se valida si boostrapping está seleccionado
        ##Si no está seleccinado, se sigue con el análisis normal


        datos <<- myData[,input$listVar]

        ##x<- myData[,input$listVar]

        ##2017-09-12: aplicación bootstrapping
        if (is.null(input$Boots)){


          ##Si no está activada la opción de boots no se hace nada.

        }else{


          ##De lo contrario, es porque el usuario quiere aplicar bootstrapping.
          ##Se valida sobre qué estadístico quiere aplicar muestreo el usuario
          ##para inferir el estadístico que describe la muestra.

          if (input$Boots == "mean"){

            call_boots <- boot(data=datos,statistic=fun_mean, R=1000)
            datos <<- call_boots$t

          }else if (input$Boots == "median"){

              call_boots <- boot(data=datos,statistic=fun_median, R=1000)
              datos <<- call_boots$t

          }else if (input$Boots == "desviation") {

            call_boots <- boot(data=datos,statistic=fun_sd, R=1000)
            datos <<-call_boots$t

          }else if (input$Boots == "variance"){

            call_boots <- boot(data=datos,statistic=fun_var, R=1000)
            datos <<-call_boots$t

          }

        }




        #Generar los bins:
        bins <- seq(min(datos), max(datos), length.out = input$bins + 1)


        #Pintar histograma:
        hist(datos, main=input$listVar, breaks = bins, col = 'antiquewhite1', border = 'black', freq=FALSE)


        ##Se valida el Checkbock de Density, para pontar o no pintar la función de densidad.
        if(input$Density==TRUE){

          density<-density(datos)
          lines(density, col = "red", lty=2, lwd = 3)

          }


        ##Se valida si el Checkbox de Distribuciones está activo.
        ##Si no está activo, no se hace nada, de lo contrario, se valida cuál esta acitvo.
        if (is.null(input$Distribution)){

        ##NORMAL:
        }else if (input$Distribution == "norm"){


          ##Ajustar la variable a la distribución normal:
          x=datos
          curve( dnorm(x, mean=mean(x), sd=sd(x)) , col = "blue", lty=2, lwd = 2, add = TRUE)

          ##curve( dnorm(x, mean=10,sd=2), 5, 15, add=T, col="blue")

        ##UNIFORME:
        }else if (input$Distribution == "unif"){

          ##Ajustar la variable a la distribución uniforme:
          x=datos
          curve(dunif(x,min=min(x), max=max(x),  log=FALSE), col="green",lty=2, lwd = 2,  add = TRUE)

        ##L-NORMAL:
        }else if (input$Distribution =="lnorm"){

          ##Ajustar la variable a la distribución LogNormal
          x=datos
          curve(dlnorm(x, mean(x), sd(x)),col=25, lty=2, lwd = 2, add = TRUE )

        ##EXPONENCIAL:
        }else if (input$Distribution =="exp"){

          ##Ajustar la variable a la distribución Exponencia
          x=datos
          curve(dexp(x), col=30, add= TRUE, lty=2, lwd = 2)

        }


    }else{


      h4("Select variable is not Numeric.")

    }

  }

  })


  ##FUNCIONALIDAD: MODA.
  observeEvent(input$Boots,{
  output$median <-renderText({

    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        ##x<-myData[,input$listVar]
        median(datos)



      }else{
        return()
      }
    }
  }
  )

  })

  observeEvent(input$listVar,{
    output$median <-renderText({

      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          ##x<-myData[,input$listVar]
          median(datos)



        }else{
          return()
        }
      }
    }
    )

  })



  ##FUNCIONALIDAD: Desviación Standar
  observeEvent(input$Boots,{
  output$desv <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        ##x<-myData[,input$listVar]

        sd(datos)

      }else{
        return()
      }
    }
  }
  )
  })


  observeEvent(input$listVar,{
    output$desv <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          ##x<-myData[,input$listVar]

          sd(datos)

        }else{
          return()
        }
      }
    }
    )
  })



  ##FUNCIONALIDAD: Varianza.
  observeEvent(input$Boots,{
  output$var <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        var(datos)
      }else{
        return()
      }

    }
  }
  )
  })


  observeEvent(input$listVar,{
    output$var <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          x<-myData[,input$listVar]
          var(datos)
        }else{
          return()
        }

      }
    }
    )
  })


  #FUNCIONALIDAD: Kurtosis
  observeEvent(input$Boots,{
  output$kurtosis <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        kurtosis(datos, na.rm = TRUE)
      }else{
        return()

      }
    }
  }
  )
  })

  observeEvent(input$listVar,{
    output$kurtosis <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          x<-myData[,input$listVar]
          kurtosis(datos, na.rm = TRUE)
        }else{
          return()

        }
      }
    }
    )
  })



  ##FUNCIONALIDAD: Sesgo
  observeEvent(input$Boots,{
  output$sesgo <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        skewness(datos, na.rm = TRUE)
      }else{
        return()
      }

    }
  }
  )
  })

  observeEvent(input$listVar,{
    output$sesgo <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          x<-myData[,input$listVar]
          skewness(datos, na.rm = TRUE)
        }else{
          return()
        }

      }
    }
    )
  })



  ##FUNCIONALIDAD: QUANTILES

  observeEvent(input$Boots,{
  output$quantil <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        quantile(datos, input$quantile)
      }else{
        return()
      }

    }
  })

  })

  observeEvent(input$listVar,{
    output$quantil <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          x<-myData[,input$listVar]
          quantile(datos, input$quantile)
        }else{
          return()
        }

      }
    })

  })



  ##FUNCIONALIDAD: Número de Observaciones
  observeEvent(input$Boots,{
  output$records <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]

        length(datos)
      }else{
        return()
      }
    }
  }
  )

  })

  observeEvent(input$listVar,{
    output$records <-renderText({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){
          x<-myData[,input$listVar]

          length(datos)
        }else{
          return()
        }
      }
    }
    )

  })



  ##FUNCIONALIDAD: Generate a summary of the dataset ----

  observeEvent(input$Boots,{
  output$summary <- renderPrint({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){

        x<-myData[,input$listVar]

        summary(datos)
      }else{
        return()
      }
    }
  })

  })

  observeEvent(input$listVar,{
    output$summary <- renderPrint({
      if(is.null(read_data())){
        return()
      }else{
        myData<-read_data()

        if(is.numeric(myData[,input$listVar])){

          x<-myData[,input$listVar]

          summary(datos)
        }else{
          return()
        }
      }
    })

  })



  ##FUNCIONALIDAD: Para Ver una muestra de los datos entrados por parámetro
  output$Data <- renderTable({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        head(myData, n = input$obs)
      }else{
        return()

      }
    }
  })



  ##función para refrescar las variables disponibles del dataset de datos para que el usuario pueda seleccionar una de ellas.
  observe({

    if(is.null(read_data())){
      return()
    }else{

      myData<-read_data()

        updateSelectInput(session, "listVar",
                      label = "Variables",
                      choices = colnames(myData)
        )



        updateSelectInput(session, "listvarCo", "Variable for Correlation:",
                          choices = colnames(myData)
        )



   }
  })




  output$Correlation  <- renderPlot({

    if(is.null(read_data())){
      return()

    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar]) & is.numeric(myData[,input$listvarCo]) ){


        matrix1 = cbind(myData[,input$listVar], myData[,input$listvarCo])

        colnames(matrix1) <- c(input$listVar, input$listvarCo)

        M<-cor(matrix1)

        corrplot(M, method="circle")

      }else{



        return()
      }
   }

  })








}


