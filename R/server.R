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


##Funciones de las SERIES DE TIEMPO:

##Funcion para pintar la serie en su estado natural.

  output$NaturalSerie <- renderPlot({


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


        #Pintar datos
        plot(datos, main=input$listVar,  col = 'blue', border = 'black')


      }else{


        h4("Select variable is not Numeric.")

      }

    }

  })

  ##Fin


  ##----------------------------SERIES DE TIEMPO---------------------------------

##1.  función para obtener los datos de la serie de tiempo

  observeEvent(input$Start_Year, {
    output$TS_data <- renderPrint({


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


           ##gas = scan('http://www.uam.es/joser.berrendero/datos/gas6677.dat')

            print(fun_TS(datos,input$Start_Year,input$Periods,input$Frequency))

        }
      }


  })
  }
  )


##2.  Pintar Serie de tiempos:

  output$Graph_TS <- renderPlot({


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

        ##obtener la serie de tiempo
        TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

        #Pintar datos
        plot(TS, main=input$listVar,  col = 'green', border = 'black')


      }else{


        h4("Select variable is not Numeric.")

      }

    }

  })

  ##Fin



##3.  BoxPlot para comparar periodos

observeEvent(input$Frequency, {
 output$BoxPlot <- renderPlot({


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

        ##obtener la serie de tiempo
        TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

        print(input$Start_Year)
        print(input$Periods)
        print(input$Frequency)

        ##Pinte gráfico boxplot
        boxplot(TS~ cycle(TS),
                medcol=c("#FFDB00FF", "#B6FF00FF"),
                whiskcol=c("#49FF00FF", "#00FF24FF"),
                staplecol=c("#00FF92FF", "#00FFFFFF"),
                boxcol=c("#0092FFFF", "#0024FFFF"),
                outcol=c("#4900FFFF", "#B600FFFF"),
                outbg=c("#FF00DB66", "#FF006D66")
        )


      }else{


        h4("Select variable is not Numeric.")

      }

    }

  })

})

  ##Fin


 ##4.  Gráfica de componentes de la Serie de tiempo:

 observeEvent(input$Frequency, {
   output$ComponentPlot <- renderPlot({


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

         ##obtener la serie de tiempo
         TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

         #Optener componentes:
         TS_COMP=decompose(TS )


         ##Pintar gráfico:
         plot(TS_COMP,  col = 'green', border = 'black')


       }else{


         h4("Select variable is not Numeric.")

       }

     }

   })

 }
 )
   ##Fin



 ##5.  Gráfica de Tendencia (TREND)

 observeEvent(input$Frequency, {
   output$Plot_Trend <- renderPlot({


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

         ##obtener la serie de tiempo
         TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

         #Optener componentes:
         TS_COMP=decompose(TS)

         ##Pintar gráfico de tendencia:
         plot(TS_COMP$trend,  col = 'blue', border = 'black')


       }else{


         h4("Select variable is not Numeric.")

       }

     }

   })

 }
 )
 ##Fin



 ##6.  Gráfica de ESTACIONALIDAD (SEASON)

 observeEvent(input$Frequency, {
   output$Plot_seasonal <- renderPlot({


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

         ##obtener la serie de tiempo
         TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

         #Optener componentes:
         TS_COMP=decompose(TS )

         ##Pintar gráfico de tendencia:
         plot(TS_COMP$seasonal,  col = 'blue', border = 'black')


       }else{


         h4("Select variable is not Numeric.")

       }

     }

   })

 }
 )
 ##Fin



 ##7.  Gráfica de RESIDUOS (CAMBIOS DE LA SERIE IMPREDECIBLES)

 observeEvent(input$Frequency, {
   output$Plot_Residual <- renderPlot({


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

         ##obtener la serie de tiempo
         TS= fun_TS(datos,input$Start_Year,input$Periods,input$Frequency)

         #Optener componentes:
         TS_COMP=decompose(TS )

         ##Pintar gráfico de tendencia:
         plot(TS_COMP$random,  col = 'blue', border = 'black')


       }else{


         h4("Select variable is not Numeric.")

       }

     }

   })

 }
 )
 ##Fin



 ##2017-09-24: Funciones para ajustar la Serie de tiempo a un modelo: Reg Lineal, Cuadrático, Cubico, Expo


 ##AJUSTE: Regresión Lineal
 observeEvent(input$Lineal, {
   output$Plot_Lineal <- renderPlot({


     if(input$Lineal ==TRUE){

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

          n=length(datos)
          n<-seq(1:n)

          serie<-cbind(n,datos)

          colnames(serie)<- c("t", "y")
          serie<-data.frame(serie) ##convirtiendo el vector en df para que pueda ser leido por la función de regresión


          ##Aplicando regresión lineal para identificar si la Tendencia de la serie
          ##se ajusta a una regresión Lineal
          regresion<-lm(data = serie, y~t)

          summary(regresion)

          ##Cálculo de la y ajustad o teórica. Especie de predicción con el modelo ajustado.
          Y_teor<-regresion$fitted.values

          v_Error2_lin<<-sum( (serie$y-Y_teor)^2 )


          ##Gráfica de serie vs. Gráfica de ajuste de regresión"
          plot(serie$t, serie$y,  t='l')
          abline(regresion, col="orange")


        }else{


          h4("Select variable is not Numeric.")

       }

      }
     }

   })

 }
 )
 ##Fin



 ##AJUSTE: Regresión Cuadrática

 observeEvent(input$Quadratic, {
   output$Plot_Cuadratica <- renderPlot({


     if(input$Quadratic ==TRUE){

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

           n=length(datos)
           n<-seq(1:n)

           serie<-cbind(n,datos)

           colnames(serie)<- c("t", "y")
           serie<-data.frame(serie) ##convirtiendo el vector en df para que pueda ser leido por la función de regresión


           regresion_cuadra<-lm(data = serie, y~t + I(t^2))
           summary(regresion_cuadra)


           Y_teor<-regresion_cuadra$fitted.values

           v_Error2_cua<<-sum( (serie$y-Y_teor)^2 )

           plot(serie$t, serie$y,  t='l')
           curve(regresion_cuadra$coefficient[1]+regresion_cuadra$coefficient[2]*x+regresion_cuadra$coefficient[3]*x^2,add=T,col="orange")


         }else{


           h4("Select variable is not Numeric.")

         }

       }
     }

   })

 }
 )
 ##Fin



 ##AJUSTE: Regresión Cubica

 observeEvent(input$Cubic, {
   output$Plot_Cubica <- renderPlot({


     if(input$Cubic ==TRUE){

       ##Se valida si hay datos leidos.

       if(is.null(read_data())){

       }else{


         ##---De aquí hacia abajo no se ha cambiado nada

         myData<- read_data()

         #Selección de la columna que contiene la variable de análisis seleccionada por el usuario.

         if(is.numeric(myData[,input$listVar])){


           datos <<- myData[,input$listVar]

           n=length(datos)
           n<-seq(1:n)

           serie<-cbind(n,datos)

           colnames(serie)<- c("t", "y")
           serie<-data.frame(serie) ##convirtiendo el vector en df para que pueda ser leido por la función de regresión


           reg_cubica<-lm(data = serie, y~t+I(t^2)+I(t^3))
           summary(reg_cubica)

           Y_teor<-reg_cubica$fitted.values


           v_Error2_cub<<-sum( (serie$y-Y_teor)^2 )


           plot(serie$t, serie$y,  t='l')
           curve(reg_cubica$coefficient[1]+reg_cubica$coefficient[2]*x+reg_cubica$coefficient[3]*x^2,add=T,col="orange")


         }else{


           h4("Select variable is not Numeric.")

         }

       }
     }

   })

 }
 )
 ##Fin




 ##AJUSTE: Regresión Exponencial

 observeEvent(input$Exponential, {
   output$Plot_Exponential<- renderPlot({


     if(input$Exponential ==TRUE){

       ##Se valida si hay datos leidos.

       if(is.null(read_data())){

       }else{


         ##---De aquí hacia abajo no se ha cambiado nada

         myData<- read_data()

         #Selección de la columna que contiene la variable de análisis seleccionada por el usuario.

         if(is.numeric(myData[,input$listVar])){


           datos <<- myData[,input$listVar]

           n=length(datos)
           n<-seq(1:n)

           serie<-cbind(n,datos)

           colnames(serie)<- c("t", "y")
           serie<-data.frame(serie) ##convirtiendo el vector en df para que pueda ser leido por la función de regresión

           y_tras<-log(serie$y) ##Transponer y con la función log.

           reg_exp<-lm(y_tras~t,data=serie)
           summary(reg_exp)

           Y_teor<-reg_exp$fitted.values

           v_Error2_exp<<-sum( (serie$y-Y_teor)^2 )

           #plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste exponencial')
           plot(serie$t, serie$y,  t='l')
           curve(exp(reg_exp$coefficient[1])*exp(reg_exp$coefficient[2]*x),add=T,col="orange")


         }else{


           h4("Select variable is not Numeric.")

         }

       }
     }

   })

 }
 )
 ##Fin



 ##AJUSTE: Regresión Exponencial

 observeEvent(input$logarithmic, {
   output$Plot_Log<- renderPlot({


     if(input$logarithmic ==TRUE){

       ##Se valida si hay datos leidos.

       if(is.null(read_data())){

       }else{


         ##---De aquí hacia abajo no se ha cambiado nada

         myData<- read_data()

         #Selección de la columna que contiene la variable de análisis seleccionada por el usuario.

         if(is.numeric(myData[,input$listVar])){


           datos <<- myData[,input$listVar]

           n=length(datos)
           n<-seq(1:n)

           serie<-cbind(n,datos)

           colnames(serie)<- c("t", "y")
           serie<-data.frame(serie) ##convirtiendo el vector en df para que pueda ser leido por la función de regresión

           t_tras<-log(serie$t) ##En un ajuste logarítimo, transpongo la variable dependiente

           reg_log<-lm(y~t_tras,data=serie)
           summary(reg_log)

           Y_teor<-reg_exp$fitted.values

           v_Error2_log<<-sum( (serie$y-Y_teor)^2 )


           #plot(datos$X,datos$H_1,xlab='X(m)',ylab='H_1(%)',main='Ajuste exponencial')
           plot(serie$t, serie$y,  t='l')
           curve(reg_log$coefficient[1]+reg_log$coefficient[2]*log(x),add=T,col="orange")


         }else{


           h4("Select variable is not Numeric.")

         }

       }
     }

   })

 }
 )
 ##Fin

 ##FUNCIONES PARA OBTENER EL ERROR CUADRÁTICO DE LOS AJUSTES.

 ##Función que entrega el Error cuadrado del Ajuste Linea
 observeEvent(input$Lineal,{
   output$Error2_Lin <-renderText({
     if(is.null(read_data())){
       return()
     }else{
       myData<-read_data()

       if(is.numeric(myData[,input$listVar])){

         if(input$Lineal==TRUE){

           v_Error2_lin

         }else{

           v_Error2_lin=NULL

         }
       }else{
         return()
       }
     }
   }
   )
 })







 ##Función que entrega el Error cuadrado del Ajuste Cuadrática
 observeEvent(input$Quadratic,{
   output$Error2_Cua <-renderText({
     if(is.null(read_data())){
       return()
     }else{
       myData<-read_data()

       if(is.numeric(myData[,input$listVar])){

         if(input$Quadratic==TRUE){

           v_Error2_cua

         }else{

           v_Error2_cua=NULL

         }
       }else{
         return()
       }
     }
   }
   )
 })



 ##Función que entrega el Error cuadrado del Ajuste Cubica
 observeEvent(input$Cubic,{
   output$Error2_Cub <-renderText({
     if(is.null(read_data())){
       return()
     }else{
       myData<-read_data()

       if(is.numeric(myData[,input$listVar])){

         if(input$Cubic==TRUE){

           v_Error2_cub

         }else{

           v_Error2_cub=NULL

         }
       }else{
         return()
       }
     }
   }
   )
 })




 ##Función que entrega el Error cuadrado del Ajuste Exponencia
 observeEvent(input$Exponential,{
   output$Error2_Exp <-renderText({
     if(is.null(read_data())){
       return()
     }else{
       myData<-read_data()

       if(is.numeric(myData[,input$listVar])){

         if(input$Exponential==TRUE){

           v_Error2_exp

         }else{

           v_Error2_exp=NULL

         }
       }else{
         return()
       }
     }
   }
   )
 })



 ##Función que entrega el Error cuadrado del Ajuste Log
 observeEvent(input$logarithmic,{
   output$Error2_Log <-renderText({
     if(is.null(read_data())){
       return()
     }else{
       myData<-read_data()

       if(is.numeric(myData[,input$listVar])){

         if(input$logarithmic==TRUE){

           v_Error2_log

         }else{

           v_Error2_log=NULL

         }
       }else{
         return()
       }
     }
   }
   )
 })




}



