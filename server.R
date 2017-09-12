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


##3. Definición de la parte lógica:

myShinyServer <- function(input, output, session) {


  ##Función que valida si ya se seleccionó un archivo. Y si se seleccionó, entonce se lee.
  ##Esta función devuelve los datos "read_data()" si hay archivo seleccionado, sino return
  read_data <- reactive({


  if(is.null(input$file1)){

      print("ARCHIVO NULL")
      return()

  }else{

      print("Leyendo datos")

      file1<- input$file1
      read.table(file=file1$datapath, header=TRUE, sep=",", dec="." )

  }

  })


  ##Funcion principal que se llama en el Main del UI:
  ##FUNCIONALIDAD: Se pinta el histograma.
  output$distPlot <- renderPlot({


    ##Se valida si hay datos leidos.

    if(is.null(read_data())){
      print("DATA ES NULLL")
      h4("There is not loaded data.")

    }else{

      myData<- read_data()

      #Selección de la columna que contiene la variable de análisis seleccionada por el usuario.

      if(is.numeric(myData[,input$listVar])){

        x<-myData[,input$listVar]

        #Generar los bins:
        bins <- seq(min(x), max(x), length.out = input$bins + 1)


        #Pintar histograma:
        hist(x, main=input$listVar, breaks = bins, col = 'antiquewhite1', border = 'black', freq=FALSE)

        ##Se valida el Checkbock de Density, para pontar o no pintar la función de densidad.
        if(input$Density==TRUE){
          density<-density(x)
          lines(density, col = "red", lty=2, lwd = 3)
        }


        ##Se valida si el Checkbox de Distribuciones está activo.
        ##Si no está activo, no se hace nada, de lo contrario, se valida cuál esta acitvo.
        if (is.null(input$Distribution)){

        ##NORMAL:
        }else if (input$Distribution == "norm"){

          ##Ajustar la variable a la distribución normal:
          curve(dnorm(x, mean(x), sd(x)), col = "blue", lty=2, lwd = 2, add = TRUE)

        ##UNIFORME:
        }else if (input$Distribution == "unif"){

          ##Ajustar la variable a la distribución uniforme:
          curve(dunif(x,min=min(x), max=max(x),  log=FALSE), col="green",lty=2, lwd = 2,  add = TRUE)

        ##L-NORMAL:
        }else if (input$Distribution =="lnorm"){

          ##Ajustar la variable a la distribución LogNormal
          curve(dlnorm(x, mean(x), sd(x)),col=25, lty=2, lwd = 2, add = TRUE )

        ##EXPONENCIAL:
        }else if (input$Distribution =="exp"){
          curve(dexp(x), col=30, add= TRUE, lty=2, lwd = 2)

        }
    }else{


      h4("Select variable is not Numeric.")

    }

  }

  })


  ##FUNCIONALIDAD: MODA.
  output$median <-renderText({

    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        median(x)
      }else{
        return()
      }
    }
  }
  )



  ##FUNCIONALIDAD: Desviación Standar
  output$desv <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        sd(x)
      }else{
        return()
      }
    }
  }
  )

  ##FUNCIONALIDAD: Varianza.
  output$var <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        var(x)
      }else{
        return()
      }

    }
  }
  )



  ##FUNCIONALIDAD: Kurtosis---> Aquí lo que quiero es calcular la curtosis, y concluirle al negocio, qué se concluye con la curtosis
  output$kurtosis <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        kurtosis(x, na.rm = TRUE)
      }else{
        return()

      }
    }
  }
  )

  ##FUNCIONALIDAD: Sesgo---> Aquí lo que quiero es calcular la sesgo, y concluirle al negocio, qué se concluye con la curtosis
  output$sesgo <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        skewness(x, na.rm = TRUE)
      }else{
        return()
      }

    }
  }
  )

  ##FUNCIONALIDAD: QUANTILES
  output$quantil <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        quantile(x, input$quantile)
      }else{
        return()
      }

    }
  })


  ##FUNCIONALIDAD: Número de Observaciones
  output$records <-renderText({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        length(x)
      }else{
        return()
      }
    }
  }
  )


  ##FUNCIONALIDAD: Generate a summary of the dataset ----
  output$summary <- renderPrint({
    if(is.null(read_data())){
      return()
    }else{
      myData<-read_data()

      if(is.numeric(myData[,input$listVar])){
        x<-myData[,input$listVar]
        summary(x)
      }else{
        return()
      }
    }
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
  
  
  
  output$Correlation <- eventReactive(input$bottonCorrelation, {
    
    print("pasa por aqui")
    
    if(is.null(read_data())){
      return()
      
    }else{
      myData<-read_data()
    
      if(is.numeric(myData[,input$listVar]) & is.numeric(myData[,input$listvarCo]) ){
      
        print("pasa por aqui 2222")
      
        matrix1 = cbind(myData[,input$listVar], myData[,input$listvarCo])
      
        colnames(matrix1) <- c(input$listVar, input$listvarCo)
    
        cor(matrix1)
      
        #Need revew
        print(cor(matrix1))
      
      }else{
      
        return()
      }
  }
  })
  




}


