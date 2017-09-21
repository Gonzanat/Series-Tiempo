#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##2. INTERFACE DE USUARIO: Se asignan los elementos de visualización y se le asignan los parámetros
##Definidos en el Server que se quieren mostrar por pantalla.

library(shiny)
library(e1071)

##1. Declaración variables: Recibir lo del usuario final
myData <- c()
x<-c()

##data<-read.csv(file.choose(),header=TRUE, sep=",", dec=".")

##data<- read.csv('C:/Users/gonzanat/Desktop/Especialización Analítica/Toma de decisiones bajo incertidumbre/Entregas Semanales/Primera Semana/Supermarket Transactions .csv', header=TRUE, sep=",", dec="." );


##Asignar a variable global los datos leídos:
##myData <<-data

myFluidPage <- fluidPage(

  # Application title
  titlePanel("Playing with your variable"),


  ##Definición delLayout para asignar los componentes de la UI:
  sidebarLayout(
    sidebarPanel(


      ##Opción para cargar el archivo de variables de análisis:
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
      tags$hr(),

      ##Lista desplegable de variables que contiene el dataset, para que el usuario pueda seleccionar la que quiere analizar:
      selectInput("listVar", "Variables:",
                  choices=colnames(myData)),


      selectInput("listvarCo", "Variable for Correlation:",
                 choices=colnames(myData)),

      tags$hr(),




      ##Barra para definir número de divisiones del histograma:
      sliderInput("bins",
                  "Número de bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      tags$hr(),



      ##Control para recibir el valor del quantil que se quiere calcular:
      numericInput("quantile", "Quantil value:", value=0.05),
      tags$hr(),

      ##Control para recibir el número de observaciones que quiere visualizar el usuario:
      numericInput("obs", "Observations:", value=5),


      # Include clarifying text ----
      helpText("Note: while the data view will show only the specified",
               "number of observations, the Summary will still be based",
               "on the full dataset."),
      tags$hr(),

      ##Permitir pintar la gráfica de Densidad de los datos:
      checkboxInput("Density", "Density", FALSE),

      ##Permitir obtener otros datos Stadísticos adicionales:
      checkboxInput("Statistics", "Statistics", 0),
      tags$hr(),


      ##Control para seleccionar la distribución teórica a la que se quiere ajustar los datos:
      checkboxGroupInput("Distribution", "Distribution Type:",
                         c("Normal" = "norm",
                           "Uniform" = "unif",
                           "Log-normal" = "lnorm",
                           "Exponential" = "exp")),

      ##2017-09-17Control para seleccionar el estadístico sobre el que se va a aplicr
      ##bootstrapping
      checkboxGroupInput("Boots", "Apply Bootstrapping:",
                         c("Mean" = "mean",
                           "Median" = "median",
                           "Desviation" = "desviation",
                           "Variance" = "variance"))

    ),





    # Panel para desplegar los resultados:
    mainPanel(


      tabsetPanel(
        tabPanel("Exploration",



          ##Mostrar el histograma:
          plotOutput("distPlot"), ##En el server las variables distPlot y displot2, se definen como de salida.


          conditionalPanel(
            condition = "input.Statistics==1",

          wellPanel(
            ##Mostrar los estadísticos de los datos:
            h4("Summary"),
            verbatimTextOutput("summary"),

            ##Otros Stadísticos.
            h6("# Records:"),
            verbatimTextOutput("records"),
            h6("Standard Dev:"),
            verbatimTextOutput("desv"),
            h6("Variance:"),
            verbatimTextOutput("var"),
            h6("Kurtosis:"),
            verbatimTextOutput("kurtosis"),
            h6("Skewness:"),
            verbatimTextOutput("sesgo")
          )
        ),

      #Mostrar quantil:
      h4("Selected Quantil:"),
      textOutput("quantil"),

      #Mostar los datos que entraron por parámetro.
      h4("Observations"),
      tableOutput("Data"),

      #Mostrar la correlaciln entre las variables seleccionadas
      h4("Correlation:"),
      plotOutput("Correlation")

    ),

    ##Panel para Series de Tiempo

    tabPanel("Time Series",

             navbarPage("Play with Time Series",

                        navbarMenu("Menu",
                                   tabPanel("Graph natural data",

                                            ##Gráfica de los datos de la variable
                                            ##Sin ninguna transforación en serie de tiempos.
                                            plotOutput("NaturalSerie")


                                   ),

                                   tabPanel("Define",

                                            sidebarPanel(
                                              numericInput("Start_Year", "Start Year:", value=2001),
                                              numericInput("Periods", "Periods", value=1),
                                              numericInput("Frequency", "Frequency", value=12)

                                            ),



                                             print("Entrando a definicion"),

                                             textOutput("TS_data")


                                   ),


                                   tabPanel("Print data Time Series",

                                            plotOutput("Graph_TS")

                                            ),


                                   tabPanel("Time Series Graph "),

                                   tabPanel("Compare periods"),
                                   tabPanel("Get unit time"),
                                   tabPanel("Forecast Time Series"),
                                   tabPanel("Graph Components"),

                                   tabPanel("Trend Graph"),
                                   tabPanel("Season Graph "),
                                   tabPanel("Residual Graph "),
                                   tabPanel("Get a stablish Series Time (Transform )"),

                                   tabPanel("Auto variance"),
                                   tabPanel("Auto Correlation")
                                   )

                       ## plot(gas)


            )
    )

    #tabPanel("Table", tableOutput("table"))

  )

  )

  )
)
