library(ggplot2)
library(Rtsne)
library(shinythemes)
library(ggpubr)
library(pheatmap)
library(shiny)
library(rsconnect)
data<- read.csv("Dados.csv", sep = ";")
saldo<- read.csv("saldo.csv", sep = ";")
datalog<- read.csv("DadosLOG.csv")
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = shinytheme("sandstone"),  
  titlePanel("Laboratório em Bioestatística"),
  
  navbarPage("Shiny App",
             tabPanel("GGPlot",           
                      
                      
                      sidebarLayout(
                        sidebarPanel("",
                                     selectInput(inputId = "graf",
                                                 label = "Tipo de gráfico",
                                                 choices = c("Linhas","Pontos"),
                                                 selected = "Pontos"),
                                     
                                     selectInput(inputId = "log",
                                                 label = "Dados logaritmizados ou não",
                                                 choices = c("Normais", "Logaritmizados"),
                                                 selected = "Normais"),
                                     
                                     
                                     selectInput(inputId = "x_select", 
                                                 label = "Eixo dos xx", 
                                                 choices = list("Anos" ="Anos", 
                                                                "Natalidade" = "Natal",
                                                                "Esperança média de vida" = "Long",
                                                                "PIB" = "PIB",
                                                                "Óbitos infantis" = "Obit",
                                                                "População com ensino superior" ="Sup",
                                                                "População sem ano de escolaridade" = "S_esc",
                                                                "Médicos/100k habitantes" = "Med"), 
                                                 selected = "Anos"),
                                     
                                     
                                     selectInput(inputId = "y_select", 
                                                 label = "Eixo dos yy",
                                                 choices = list("Anos" ="Anos", 
                                                                "Natalidade" = "Natal",
                                                                "Esperança média de vida" = "Long",
                                                                "PIB" = "PIB",
                                                                "Óbitos infantis" = "Obit",
                                                                "População com ensino superior" ="Sup",
                                                                "População sem ano de escolaridade" = "S_esc",
                                                                "Médicos/100k habitantes" = "Med"),
                                                 selected = "Natal")
                        ),
                        mainPanel("Gráficos",
                                  plotOutput("my_plot")
                        )
                      )
                      
             ),
#---------------------------------------------------------------------------------------
             tabPanel("Valores por ano",
                      sidebarLayout(
                        sidebarPanel(
                          verticalLayout(
                            sliderInput(inputId = "sli1", 
                                        label = "Anos", 
                                        min = 1961, 
                                        max = 2021, 
                                        value = 1985))
                        ),
                        mainPanel(
                          sidebarLayout(
                            sidebarPanel(
                              h3("Nascimentos"),textOutput("nat"),tags$head(tags$style("#nat{
                                 color: green;
                                 font-size: 20px;
                                 font-style: bold;}")),
                              h3("Óbitos"), textOutput("obi"),tags$head(tags$style("#obi{
                                 color: red;
                                 font-size: 20px;
                                 font_style: bold;}")),
                              h3("Saldo Natural"), textOutput("sna"),tags$head(tags$style("#sna{
                                 color: orange;
                                 font-size: 20px;
                                 font_style: bold;}")),
                              h3("% com ensino superior"), textOutput("sup"),tags$head(tags$style("#sup{
                                 color: orange;
                                 font-size: 20px;
                                 font_style: bold;}")),
                              h3("% sem nível de escolaridade"), textOutput("sec"),tags$head(tags$style("#sec{
                                 color: orange;
                                 font-size: 20px;
                                 font_style: bold;}")),
                              h3("Esperança média de vida"), textOutput("lon"),tags$head(tags$style("#lon{
                                 color: orange;
                                 font-size: 20px;
                                 font_style: bold;}"))
                            ),
                            mainPanel("Variação do número de nascimentos e de óbitos ao longo dos anos",
                                plotOutput("var")    
                              
                            )
                          )
                        )
                        
                        
                      )         
                      
                      
                      
                      
                      
             )
  )
)

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------



server <- function(input, output, session) {
  output$my_plot <- renderPlot({
    if (input$graf=="Linhas"){
      if (input$log =="Normais"){
        a<- ggplot(data, aes(x=data[,input$x_select], y=data[,input$y_select])) + 
          geom_line() + labs(x= input$x_select,
                             y= input$y_select)+ geom_smooth(method=lm , color="red", se=FALSE)}
      if (input$log=="Logaritmizados"){
        a<- ggplot(datalog, aes(x=datalog[,input$x_select], y=datalog[,input$y_select])) + 
          geom_line() + labs(x= input$x_select,
                             y= input$y_select)+ geom_smooth(method=lm , color="red", se=FALSE)}
    }
    
    if (input$graf=="Pontos"){
      if (input$log =="Normais"){
        a<- ggplot(data, aes(x=data[,input$x_select], y=data[,input$y_select])) + 
          geom_point() + labs(x= input$x_select,
                             y= input$y_select) + geom_smooth(method=lm , color="red", se=FALSE)}
      if (input$log=="Logaritmizados"){
        a<- ggplot(datalog, aes(x=datalog[,input$x_select], y=datalog[,input$y_select])) + 
          geom_point() + labs(x= input$x_select,
                             y= input$y_select) + geom_smooth(method=lm , color="red", se=FALSE)}
    }
    a
  })
  #-----------------------------------------------------------------------------
  output$Anos <- {(renderText(input$sli1))}
  output$nat <- {(renderText(saldo[match(input$sli1,saldo$Anos),2]))}
  output$obi <- {(renderText(saldo[match(input$sli1,saldo$Anos),3]))}
  output$sna <- {(renderText(saldo[match(input$sli1,saldo$Anos),2] - saldo[match(input$sli1,saldo$Anos),3]))}
  output$sup <- {(renderText(data[match(input$sli1,data$Anos),4]))}
  output$sec <- {(renderText(data[match(input$sli1,data$Anos),3]))}
  output$lon <- {(renderText(data[match(input$sli1,data$Anos),6]))}
  
  
  output$var <-renderPlot(ggplot(saldo, aes(Anos)) + labs(y= "Indivíduos") +
                            geom_line(size=2, aes(y = nascimentos, colour = "Nascimentos")) +
                            geom_line(size=2, aes(y = obitos, colour = "Óbitos")))
}

#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)