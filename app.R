
library(shiny)
library(DT)
library(shinydashboard)
library('e1071')
library('dplyr')
library(xgboost)
library(shinydashboardPlus)

shinyApp(
  ui = dashboardPage(
    
    dashboardHeader(
      #title = img(src = "Dell.jpg", height = 80, width = 80),
      #titleWidth = 850
    ),
    ## Sidebar content
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        menuItem("Laptop Recommendation Service", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Revenue", tabName = "widgets", icon = icon("th"))
      )
    ),
    
    dashboardBody(
      
      # Also add some custom CSS to make the title background area the same
      # color as the rest of the header.
      tags$head(tags$style(HTML('
                                .skin-blue .main-header .logo {
                                background-color: "#B71C1C";
                                }
                                
                                
                                
                                h1 {
                                font-weight: 500;
                                color: #0D47A1;
                                text-align: center;
                                
                                }
                                
                                '))),
      
      
      
      
      
      # App title ----
      #headerPanel(
      
      # h1( img(src = "Dell.jpg", height = 80, width = 80) , " Support and Services" )
      #),
      #theme = "bootstrap.css",
      
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                
                headerPanel(
                  
                  h1( img(src = "Dell.jpg", height = 80, width = 80) , " Support and Services" )
                ),
                
                br(),br(),br(),br(),br(),br(),br(),
                
                
                
                fluidRow(
                  
                  box(
                    
                    # Input: Selector for variable to plot against mpg ----
                    title = "Inputs",
                    status = "warning",
                    color = "lime",
                    solidHeader = TRUE,
                    width = 4,
                    #height = 400,
                    #background = "black",
                    
                    selectInput("variable", "Laptop Type:", 
                                c("Home" = "home",
                                  "Business " = "office",
                                  "Student" = "student",
                                  "Gaming" = "gaming")),
                    
                    conditionalPanel(
                      condition = "input.variable == 'student'  ",
                      selectInput(
                        "student_model", "Student Model",
                        c("S1" = "S1",
                          "S2" = "S2",
                          "S3" = "S3"))
                      
                    ),
                    
                    conditionalPanel(
                      condition = "input.variable == 'home'",
                      selectInput(
                        "home_model", "Home Model",
                        c("H1" = "H1",
                          "H2" = "H2",
                          "H3" = "H3"))
                      
                    ),
                    
                    conditionalPanel(
                      condition = "input.variable == 'gaming'",
                      selectInput(
                        "gaming_model", "Gaming Model",
                        c("G1" = "G1",
                          "G2" = "G2",
                          "G3" = "G3"))
                      
                    ),
                    
                    conditionalPanel(
                      condition = "input.variable == 'office'",
                      selectInput(
                        "office_model", "Business Model",
                        c("O1" = "O1",
                          "O2" = "O2",
                          "O3" = "O3"))
                      
                      
                      
                    ),
                    
                    selectInput("dailyuse", "Average Daily Use:", 
                                c("1" = "1","2" = "2","3" = "3","4" = "4","5" = "5","6" = "6","7" = "7","8" = "8",
                                  "9" = "9","10" = "10","11" = "11","12" = "12"))
                    
                    
                    
                    
                    
                    
                    # Input: Checkbox for whether outliers should be included ----
                    #checkboxInput("outliers", "Show outliers", FALSE)
                    
                    #textOutput("result")
                    
                  ),
                  
                  
                  fluidRow(
                    conditionalPanel(
                      condition = "input.home_model == 'H1' && input.variable == 'home' ",
                      img(src = "dellh1.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.home_model == 'H2' && input.variable == 'home' ",
                      img(src = "dellh2.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.home_model == 'H3'  && input.variable == 'home' ",
                      img(src = "dellh3.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.office_model == 'O1' && input.variable == 'office'",
                      img(src = "dello1.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.office_model == 'O2'  && input.variable == 'office' ",
                      img(src = "dello2.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.office_model == 'O3' && input.variable == 'office' ",
                      img(src = "dello3.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.student_model == 'S1' && input.variable == 'student' ",
                      img(src = "dells1.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.student_model == 'S2' && input.variable == 'student' ",
                      img(src = "dells2.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.student_model == 'S3' && input.variable == 'student' ",
                      img(src = "dells3.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.gaming_model == 'G1' && input.variable == 'gaming' ",
                      img(src = "dellg1.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.gaming_model == 'G2' && input.variable == 'gaming' ",
                      img(src = "dellg2.png", height = 280, width = 400)
                    ),
                    
                    conditionalPanel(
                      condition = "input.gaming_model == 'G3' && input.variable == 'gaming' ",
                      img(src = "laptop-g-series-g7-15-7590-non-touch-notebook-pdp-mod-5.jpg",height = 280, width = 400 )
                    )
                    
                    
                    
                    
                    
                    
                    
                    
                  ),
                  
                  
                  
                  br() , br(),br(),
                  
                  fluidRow(
                    
                    
                    box(
                      width = 2,
                      height = 180,
                      title = "Battery",
                      align = "center",
                      status="info",
                      solidHeader = TRUE,
                      footer = textOutput("battery"),
                      img(src = "battery.png",height = 70, width = 70 )
                      
                      
                    ),
                    
                    box(
                      width = 2,
                      height = 180,
                      title = "Hard Disk",
                      align = "center",
                      status="info",
                      solidHeader = TRUE,
                      img(src = "harddrive.png",height = 70, width = 70),
                      footer = textOutput("hdd")
                      
                      
                    ),
                    
                    box(
                      width = 2,
                      height = 180,
                      title = "Mother Board",
                      align = "center" ,
                      status="info",
                      solidHeader = TRUE,
                      img(src = "motherboad.png",height = 70, width = 70),
                      footer = textOutput("mb")
                      
                      
                    ),
                    
                    box(
                      width = 2,
                      height = 180,
                      title = "Cooling Fan",
                      align = "center" ,
                      status="info",
                      solidHeader = TRUE,
                      img(src = "fan.png",height = 70, width = 70),
                     footer = textOutput("cf")
                      
                      
                    )
                    
                    
                    
                    
                    
                    
                  ),
                  
                  
                  
                  
                  
                  box(
                    title = "Warranty Duration [ in months ] ",
                    width = 4, 
                    align = "center" ,
                    status="success",
                    solidHeader = TRUE,
                    textOutput("result4")
                  ),
                  
                  box(
                    title = "Warranty Price",
                    width = 4, 
                    align = "center" ,
                    status="success",
                    solidHeader = TRUE,
                    textOutput("ccfw")
                    
                  )
                  
                  
                  
                  
                  
                  
                  
                )
                
                
                
                
                
        ),
        
        # Second tab content
        tabItem(tabName = "widgets",
               # h2("Widgets tab content"),
                
                box(
                  title = "Total Cost Incurred",
                  width = 4, 
                  align = "center" ,
                  status="success",
                  solidHeader = TRUE,
                  textOutput("ccfwee")
                  
                ),
                
                box(
                  title = "Sales Revenue",
                  width = 4, 
                  align = "center" ,
                  status="success",
                  solidHeader = TRUE,
                  textOutput("ccfweedc")
                  
                ),
                
                box(
                  title = "Profit",
                  width = 4, 
                  align = "center" ,
                  status="success",
                  solidHeader = TRUE,
                  textOutput("ccfweegrf")
                  
                )
                
                
        )
      )
      
      
      
      )
      ),
  
  
  
  
  server <- function(input, output) {
    
    third <<- 0
    motherboard <<- 0
    battery <<- 0
    harddisk <<- 0
    coolingfan <<- 0 
    
    batteryprice <<- 6000
    harddiskprice <<- 6000
    motherboardprice <<- 6000
    coolingfanprice <<- 6000
    
    Cool <<- 0
    H <<- 0 
    P <<- 0 
    
    output$result4 <- renderText({
      
      
      if(input$variable =='student'){
        delldell = input$student_model
      }
      if(input$variable=='home'){
        delldell = input$home_model
      }
      if(input$variable=='office'){
        delldell = input$office_model
      }
      if(input$variable=='gaming'){
        delldell = input$gaming_model
      }
      
      print(delldell)
      
      
      if(input$variable =='student'){
        value1 = input$dailyuse
      }
      if(input$variable=='home'){
        value1 = input$dailyuse
      }
      if(input$variable=='office'){
        value1 = input$dailyuse
      }
      if(input$variable=='gaming'){
        value1 = input$dailyuse
      }
      
      #BATTERY----------------------------------------------------------------------------
      Bookb <- read.csv("D:\\DOWNLOADS\\DELLDELLDELLULTIMATEFINAL11.csv")
      battery1<-subset(Bookb,MODEL == delldell)
      #print(input$student_model)
      print(battery1)
      xgb1 <- data.frame(battery1$BATTERYLIFE,battery1$dailyuse)
      btrain <- xgb.DMatrix(data = as.matrix(xgb1$battery1.dailyuse), label = as.matrix(xgb1$battery1.BATTERYLIFE))
      
      value <- as.numeric(value1)
      print(delldell)
      print(input$dailyuse)
      #print(input$variable)
      a <-xgb.DMatrix(data= as.matrix(value), label=as.matrix(1))
      xgb1 <- xgb.train ( data = btrain, nrounds = 79)
      BATTERY <- predict (xgb1,a)
      #paste("battery " ,BATTERY)
      B<<-round(BATTERY)
      dellexper1 <-data.frame(battery1$BATTERYLIFE)
      dellexper2 = mutate(dellexper1, percentile_rank = ntile(dellexper1$battery1.BATTERYLIFE,100))
      
      #COOLINGFAN------------------------------
      Book3 <- read.csv("D:\\DOWNLOADS\\DELLDELLDELLULTIMATEFINAL11.csv")
      bar<-subset(Book3,MODEL== delldell)
      # #print(input$student_model)
      xgb1 <- data.frame(bar$COOLINGFANLIFE,bar$dailyuse)
      ctrain <- xgb.DMatrix(data = as.matrix(xgb1$bar.dailyuse), label = as.matrix(xgb1$bar.COOLINGFANLIFE))
      #value1 <- input$dailyuse
      value <- as.numeric(value1)
      # #print(9)
      # #print(input$variable)
      a <-xgb.DMatrix(data= as.matrix(value), label=as.matrix(1))
      xgb1 <- xgb.train ( data = ctrain, nrounds = 79)
      COOLINGFAN <- predict (xgb1,a)
      paste("cooling fan " ,COOLINGFAN)
      # 
      Cool <<-round(COOLINGFAN)
      dellexper3 <-data.frame(bar$COOLINGFANLIFE)
      dellexper4 = mutate(dellexper3, percentile_rank = ntile(dellexper3$bar.COOLINGFANLIFE,100))
      
      
      #PROCESSOR------------------------------
      BookP <- read.csv("D:\\DOWNLOADS\\DELLDELLDELLULTIMATEFINAL11.csv")
      processor1<-subset(BookP,MODEL== delldell)
      #print(input$student_model)
      xgb2 <- data.frame(processor1$PROCESSORLIFE,processor1$dailyuse)
      ptrain <- xgb.DMatrix(data = as.matrix(xgb2$processor1.dailyuse), label = as.matrix(xgb2$processor1.PROCESSORLIFE))
      #value1 <- input$dailyuse
      value <- as.numeric(value1)
      #print(9)
      #print(input$variable)
      a <-xgb.DMatrix(data= as.matrix(value), label=as.matrix(1))
      xgb1 <- xgb.train ( data = ptrain, nrounds = 79)
      PROCESSOR <- predict (xgb1,a)
      paste("mother board " ,PROCESSOR)
      
      P<<-round(PROCESSOR)
      dellexper5 <-data.frame(processor1$PROCESSORLIFE)
      dellexper6 = mutate(dellexper5, percentile_rank = ntile(dellexper5$processor1.PROCESSORLIFE,100))
      
      #HARDDISK-------------------------------------------------------------------------------
      # 
      Bookh <- read.csv("D:\\DOWNLOADS\\DELLDELLDELLULTIMATEFINAL11.csv")
      harddisk1 <<-subset(Bookh,MODEL== delldell)
      #print(input$student_model)
      xgb1 <- data.frame(harddisk1$HARDDISKLIFE,harddisk1$dailyuse)
      htrain <- xgb.DMatrix(data = as.matrix(xgb1$harddisk1.dailyuse), label = as.matrix(xgb1$harddisk1.HARDDISKLIFE))
      #value1 <- input$dailyuse
      value <- as.numeric(value1)
      #print(9)
      #print(input$variable)
      a <-xgb.DMatrix(data= as.matrix(value), label=as.matrix(1))
      xgb1 <- xgb.train ( data = htrain, nrounds = 79)
      HARDDISK <- predict (xgb1,a)
      paste("hard disk " ,HARDDISK)
      
      H<<-round(HARDDISK)
      dellexper7 <-data.frame(harddisk1$HARDDISKLIFE)
      dellexper8 = mutate(dellexper7, percentile_rank = ntile(dellexper7$harddisk1.HARDDISKLIFE,100))
      
      if(input$variable =='student'){
        FINAL= PROCESSOR*0.35 + BATTERY*0.25 + HARDDISK*0.2 + COOLINGFAN*0.2
        FINAL=round(FINAL)
        third <<- FINAL
        print(third)
      }
      if(input$variable=='home'){
        FINAL= PROCESSOR*0.25 + BATTERY*0.25 + HARDDISK*0.25 + COOLINGFAN*0.25
        FINAL=round(FINAL)
        third <<- FINAL
        print(third)
      }
      if(input$variable=='office'){
        FINAL= PROCESSOR*0.25 + BATTERY*0.2 + HARDDISK*0.3 + COOLINGFAN*0.25
        FINAL=round(FINAL)
        third <<- FINAL
        print(third)
      }
      if(input$variable=='gaming'){
        FINAL= PROCESSOR*0.1 + BATTERY*0.35 + HARDDISK*0.2 + COOLINGFAN*0.35
        FINAL=round(FINAL)
        third <<- FINAL
        print(third)
      }
      
      COOLINGFAN_PERCENTILE <-subset(dellexper4,bar.COOLINGFANLIFE == FINAL)
       coolingfan <<- COOLINGFAN_PERCENTILE[1,2]
       #print(coolingfan)
      
      PROCESSOR_PERCENTILE <-subset(dellexper6,processor1.PROCESSORLIFE == FINAL)
      #print( PROCESSOR_PERCENTILE[1,2])
      #print(100-PROCESSOR_PERCENTILE[1,2])
      motherboard <<- 100-PROCESSOR_PERCENTILE[1,2]
      #print(motherboard)
      
      
      HARDDISK_PERCENTILE <-subset(dellexper8,harddisk1.HARDDISKLIFE == FINAL)
      #print( HARDDISK_PERCENTILE[1,2])
      #print(100-HARDDISK_PERCENTILE[1,2])
      harddisk <<- 100-HARDDISK_PERCENTILE[1,2]
      #print(harddisk)
      
      
      BATTERY_PERCENTILE <-subset(dellexper2,battery1.BATTERYLIFE ==FINAL)
      #print( BATTERY_PERCENTILE[1,2])
      #print(100-BATTERY_PERCENTILE[1,2])
      battery <<- 100-BATTERY_PERCENTILE[1,2]
      #print(battery)
      
      #second <<- COOLINGFAN_PERCENTILE[1,2]
      #third <<- 100-BATTERY_PERCENTILE[1,2]
      print(third)
      
      
  
      
    })
    
    
    output$result3 <- renderText({
      paste("Parts covered are " , " 1. BATTERY " , "2.HARD DISK " , "3.COOLING FAN " ,  third )
    })
    
    
    output$result <- renderText({
      paste("You chose ",input$variable , " Laptop")
    })
    
    output$battery <- renderText({
      ininininin <- input$dailyuse
      paste(" Life Time :-  " , B)
      #print(B)
    })
    
    output$hdd <- renderText({
      ininininin <- input$dailyuse
      paste(" Life Time : - " , H)
      #print(H)
    })
    
    output$mb <- renderText({
      ininininin <- input$dailyuse
      paste(" Life Time : - " , P)
      #print(P)
    })
    
    output$cf <- renderText({
      ininininin <- input$dailyuse
      paste(" Life Time  :-  " , Cool)
      #print(Cool)
    })
    
    
    
    output$ccfw <- renderText({
      
      
      if(input$variable=='student' && input$student_model == 'S3')
      {
        costincur <- (3500*battery + 4150*motherboard + 600*coolingfan + 8000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='student' && input$student_model == 'S2')
      {
        costincur <- (2500*battery + 3500*motherboard + 500*coolingfan + 7000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='student' && input$student_model == 'S1')
      {
        costincur <- (1500*battery + 2500*motherboard + 350*coolingfan + 4500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      
      if(input$variable=='home' && input$home_model == 'H3')
      {
        costincur <- (4500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='home' && input$home_model == 'H2')
      {
        costincur <- (3500*battery + 3200*motherboard + 400*coolingfan + 4500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='home' && input$home_model == 'H1')
      {
        costincur <- (2500*battery + 2200*motherboard + 300*coolingfan + 3000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      
      if(input$variable=='office' && input$office_model == 'O3')
      {
        costincur <- (4500*battery + 5500*motherboard + 600*coolingfan + 6500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='office' && input$office_model == 'O2')
      {
        costincur <- (3800*battery + 4400*motherboard + 450*coolingfan + 5000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='office' && input$office_model == 'O1')
      {
        costincur <- (3000*battery + 3700*motherboard + 350*coolingfan + 4000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      
      
      if(input$variable=='gaming' && input$gaming_model == 'G3'){
        costincur <- (7500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G2'){
        costincur <- (6500*battery + 6600*motherboard + 700*coolingfan + 8500*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G1'){
        costincur <- (5000*battery + 5200*motherboard + 500*coolingfan + 6000*harddisk)
        price <-1.15 *costincur*0.01
        print(price)
      }
      
      print(price)
      
      
     
      
    })
    
    output$ccfwee <- renderText({
      if(input$variable=='student' && input$student_model == 'S3')
      {
        costincur <- (3500*battery + 4150*motherboard + 600*coolingfan + 8000*harddisk)
        print(costincur)
      }
      if(input$variable=='student' && input$student_model == 'S2')
      {
        costincur <- (2500*battery + 3500*motherboard + 500*coolingfan + 7000*harddisk)
        print(costincur)
      }
      if(input$variable=='student' && input$student_model == 'S1')
      {
        costincur <- (1500*battery + 2500*motherboard + 350*coolingfan + 4500*harddisk)
        print(costincur)
      }
      
      if(input$variable=='home' && input$home_model == 'H3')
      {
        costincur <- (4500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        print(costincur)
      }
      if(input$variable=='home' && input$home_model == 'H2')
      {
        costincur <- (3500*battery + 3200*motherboard + 400*coolingfan + 4500*harddisk)
        print(costincur)
      }
      if(input$variable=='home' && input$home_model == 'H1')
      {
        costincur <- (2500*battery + 2200*motherboard + 300*coolingfan + 3000*harddisk)
        print(costincur)
      }
      
      if(input$variable=='office' && input$office_model == 'O3')
      {
        costincur <- (4500*battery + 5500*motherboard + 600*coolingfan + 6500*harddisk)
        print(costincur)
      }
      if(input$variable=='office' && input$office_model == 'O2')
      {
        costincur <- (3800*battery + 4400*motherboard + 450*coolingfan + 5000*harddisk)
        print(costincur)
      }
      if(input$variable=='office' && input$office_model == 'O1')
      {
        costincur <- (3000*battery + 3700*motherboard + 350*coolingfan + 4000*harddisk)
        print(costincur)
      }
      
      
      if(input$variable=='gaming' && input$gaming_model == 'G3'){
        costincur <- (7500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        print(costincur)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G2'){
        costincur <- (6500*battery + 6600*motherboard + 700*coolingfan + 8500*harddisk)
        print(costincur)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G1'){
        costincur <- (5000*battery + 5200*motherboard + 500*coolingfan + 6000*harddisk)
        print(costincur)
      }
      
      print(costincur)
      
      
    })
    
    output$ccfweedc  <- renderText({
      
      if(input$variable=='student' && input$student_model == 'S3')
      {
        costincur <- (3500*battery + 4150*motherboard + 600*coolingfan + 8000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='student' && input$student_model == 'S2')
      {
        costincur <- (2500*battery + 3500*motherboard + 500*coolingfan + 7000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='student' && input$student_model == 'S1')
      {
        costincur <- (1500*battery + 2500*motherboard + 350*coolingfan + 4500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      
      if(input$variable=='home' && input$home_model == 'H3')
      {
        costincur <- (4500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='home' && input$home_model == 'H2')
      {
        costincur <- (3500*battery + 3200*motherboard + 400*coolingfan + 4500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='home' && input$home_model == 'H1')
      {
        costincur <- (2500*battery + 2200*motherboard + 300*coolingfan + 3000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      
      if(input$variable=='office' && input$office_model == 'O3')
      {
        costincur <- (4500*battery + 5500*motherboard + 600*coolingfan + 6500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='office' && input$office_model == 'O2')
      {
        costincur <- (3800*battery + 4400*motherboard + 450*coolingfan + 5000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='office' && input$office_model == 'O1')
      {
        costincur <- (3000*battery + 3700*motherboard + 350*coolingfan + 4000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      
      
      if(input$variable=='gaming' && input$gaming_model == 'G3'){
        costincur <- (7500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G2'){
        costincur <- (6500*battery + 6600*motherboard + 700*coolingfan + 8500*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G1'){
        costincur <- (5000*battery + 5200*motherboard + 500*coolingfan + 6000*harddisk)
        price <-1.15 *costincur
        print(price)
      }
      
      print(price)
    })
    
    output$ccfweegrf <- renderText({
      
      if(input$variable=='student' && input$student_model == 'S3')
      {
        costincur <- (3500*battery + 4150*motherboard + 600*coolingfan + 8000*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='student' && input$student_model == 'S2')
      {
        costincur <- (2500*battery + 3500*motherboard + 500*coolingfan + 7000*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='student' && input$student_model == 'S1')
      {
        costincur <- (1500*battery + 2500*motherboard + 350*coolingfan + 4500*harddisk)
        print(0.15*costincur)
      }
      
      if(input$variable=='home' && input$home_model == 'H3')
      {
        costincur <- (4500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='home' && input$home_model == 'H2')
      {
        costincur <- (3500*battery + 3200*motherboard + 400*coolingfan + 4500*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='home' && input$home_model == 'H1')
      {
        costincur <- (2500*battery + 2200*motherboard + 300*coolingfan + 3000*harddisk)
        print(0.15*costincur)
      }
      
      if(input$variable=='office' && input$office_model == 'O3')
      {
        costincur <- (4500*battery + 5500*motherboard + 600*coolingfan + 6500*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='office' && input$office_model == 'O2')
      {
        costincur <- (3800*battery + 4400*motherboard + 450*coolingfan + 5000*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='office' && input$office_model == 'O1')
      {
        costincur <- (3000*battery + 3700*motherboard + 350*coolingfan + 4000*harddisk)
        print(0.15*costincur)
      }
      
      
      if(input$variable=='gaming' && input$gaming_model == 'G3'){
        costincur <- (7500*battery + 4300*motherboard + 550*coolingfan + 5500*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G2'){
        costincur <- (6500*battery + 6600*motherboard + 700*coolingfan + 8500*harddisk)
        print(0.15*costincur)
      }
      if(input$variable=='gaming' && input$gaming_model == 'G1'){
        costincur <- (5000*battery + 5200*motherboard + 500*coolingfan + 6000*harddisk)
        print(0.15*costincur)
      }
      
      print(0.15*costincur)
      
      
    })
    
    
   
    
    
  }
      )

