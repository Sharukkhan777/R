#========================================
#========================================
#------------HELPERS FUNCTION------------
#========================================
#========================================

#========================================
#----------------TIMER-------------------
# Adding zero for single digit number
add_zero <- function(x){
  x <- as.character(x)
  if (nchar(x) == 1) {
    ans_str <- paste("0", x, sep = "")    
  }else{
    ans_str <- x
  }
  return(ans_str)   # if the value is 1, it returns 01
}
#========================================
# Make a timer string
timer_str <- c("02 : 00")
min <- 1
sec <- 59
for (i in 1:120) {
  ans_str <- paste(add_zero(min)," : ",add_zero(sec),sep = "")
  timer_str <- c(timer_str,ans_str)
  sec <- sec - 1
  if (sec == -1) {
    min <- min - 1 
    sec <- 59
  }
}
timer_str <- rev(timer_str)
#========================================
#------------QUESTION ASK----------------
# Ask random math question to user
# by using the below function 
ask_ques <- function(){
  # random number 1
  randnum1 <- as.character(round(runif(1, min = 1, max = 15)))
  # random number 2
  randnum2 <- as.character(round(runif(1, min = 1, max = 15)))
  # random operation 
  randsym <- sample(c("+","*"),1)
  # calculate the answer
  ans_str <- paste(randnum1,randsym, randnum2)
  ques_str <<- paste(randnum1, " ", randsym, " ", randnum2, " = ")
  ans <<- as.character(eval(parse(text=ans_str)))
}

#========================================
#------------OVERALL SCORE---------------
time_taken_percentage <- function(cumsum_time){
  x <- round(((270 - cumsum_time)/121)*100,1)
  if(x > 100) {
    x <- 100
  }
  return(x)
}




#========================================
#========================================
#---------------APPLICATION--------------
#========================================
#========================================
library(lubridate)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)
#-----------------UI---------------------
# dashboarding properties

body <- dashboardBody(
  
  tags$head(tags$style(HTML('
        .content-wrapper, .right-side {
        background-color: #22272e;
        }
    '))),
  h2("MISSION STATUS!"),
  fluidRow(
    valueBoxOutput("mission")
  ),
  hr(),
  h2("Score Board"),
  
  fluidRow(
    valueBoxOutput("overall_percentage_box"),
    valueBoxOutput("total_question_ask_box"),
    valueBoxOutput("total_question_ans_box")
  ),
  hr(),
  h2("Highlights"),
  fluidRow(
    valueBoxOutput("max_speed_box"),
    valueBoxOutput("distance_reached_box"),
    valueBoxOutput("time_taken_box")
  )
)
css <- "
.nav li a.disabled {
cursor: not-allowed !important;
border-color: #aaa !important;
}"
#-----UI-----
ui <- fluidPage(theme = shinytheme("darkly"),
                shinyjs::inlineCSS(css),
                shinyjs::useShinyjs(),
                includeCSS("www/style.css"),
                
                tabsetPanel(id = "inTabset",
                #------Page 1 Intro--------    
                tabPanel("Intro",
                         img(class="mainpageeagle", src="introeagle.png",align = "center"),
                         br(),
                         HTML('&emsp;&emsp;&emsp;'),
                         actionButton(inputId = "startGame",class="startGame",label = "Start", align = "center")),
                #------Page 2 Game-------- 
                tabPanel("Game",
                         br(),
                         sidebarLayout(
                           #------SCORE AND TIMER---
                           sidebarPanel(verticalLayout(splitLayout(p("Timer"),
                                                                   textOutput(outputId = "timer"),
                                                                   p("Miles"),
                                                                   textOutput(outputId = "distance")),
                                                       div(id = "signal", style="background-color:#2bedff;",
                                                           h1("ANSWER STATUS", id = "Signal", align = "center", style="color: #0059ff; ")))),
                           
                           #------GAME STARTS-------
                           mainPanel(verticalLayout(# also we can use some multiline HTML 
                                                    HTML('
                                                         <div id="myDiv" class="myDiv">
                                                         <img src="nav_eagle.png" id="nav_eagle" class="nav_eagle" style="margin-left: 0%">
                                                         <img src="earth_cropped.png" class="earth_crop" style="max-width: 100%;">
                                                         <img class="upframe" src="upframe.png" style="max-width: 100%;">
                                                         
                                                         
                                                         <h3 class="ans_txt" id="ans_txt">1 + 2 =</h3>
                                                         &emsp;&emsp;&emsp;&emsp;&emsp;
                                                         <img class="bt" src="bubbleTextImage.png" alt="bubbletext" width="150" height="90">
                                                         <br>
                                                         <img class="eagle" src="eagleman.gif" alt="EagleFly" width="250" height="250">
                                                         <br>
                                                         <br>
                                                         <br>
                                                         <br>
                                                         <br>
                                                         <div class="inscreen">
                                                          <h6 id="speed" style="color:#70F503">&emsp;SPEED = 10 miles/min</h6>
                                                          <h6 id="atmos" style="color:#70F503">&emsp;ATMOSPHERE = Tropo&emsp;</h6>
                                                         </div>
                                                         </div>')
                                                    ),
                                                    p("",style="font-size:0.5px;"),
                                                    div(style="display: inline-block;vertical-align:top; width: 90px;",actionButton(inputId = "disable_buttons",label = "ANSWER : ")),
                                                    div(style="display: inline-block;vertical-align:top; width: 50px;",textOutput(outputId = "ans")),
                                                   
                                                    #-------BUTTONS------
                           
                           fluidRow(column(width = 5,
                                           p("",style="font-size:1px;"),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "one",class = "no_button", style = "width:50px",label = "1")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "two",class = "no_button",style = "width:50px",label = "2")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "three",class = "no_button",style = "width:50px",label = "3")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "four",class = "no_button",style = "width:50px",label = "4")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "five",class = "no_button",style = "width:50px",label = "5")),
                                           br(),
                                           p("",style="font-size:1px;"),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "six",class = "no_button",style = "width:50px",label = "6")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "seven",class = "no_button",style = "width:50px",label = "7")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "eight",class = "no_button",style = "width:50px",label = "8")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "nine",class = "no_button",style = "width:50px",label = "9")),
                                           div(style="display: inline-block;vertical-align:top; width: 50px;",actionButton(inputId = "zero",class = "no_button",style = "width:50px",label = "0")),
                                           p("",style="font-size:1px;")
                           ),
                           column(width = 4,offset = 0,
                                  div(style="display: inline-block;vertical-align:top; width: 70px;",actionButton(class = "del",inputId = "del",label = "DEL",style = "width:70px")),
                                  div(style="display: inline-block;vertical-align:top; width: 170px;",actionButton(class = "enter",inputId = "enter",label = "Enter",style = "width:170px")))
                           ))
                         
                          )
                        ),
                tabPanel("Stats", dashboardPage(
                  dashboardHeader(disable = TRUE),
                  dashboardSidebar(disable = TRUE),
                  body
                )),
                tabPanel("About", 
                         div(id="about",
                             fluidPage(
                               # About content here
                               
                               h1(class = "contents4","ABOUT GAME"),
                               HTML('
                               
                                <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
                                Math Eagle aims to reach outer space
                                <br><br>
                                <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
                                He can fly at an unbelievable speed and need your mathematical skills to achieve his aim
                                <br><br>
                                <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
                                Speed of the eagle increases with each correct answer and decreases on each incorrect answer. Within a time limit of 2 minutes, The player has to make the math eagle reach outer space by boosting his speed.
                                <br><br>'
                               ),
                               
                               h1(class = "contents4","ABOUT APPLICATION"),
                               HTML('
                                <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
                                The app has been developed by Sharuk khan, the app is powererd by Shiny and packages like shiny, shinydashboard, shinythemes, shinyjs.
                                
                                <br>&emsp;&emsp;
                                <i class="far fa-arrow-alt-circle-right"></i>&emsp;
                                Shiny is mainly used in this application to build.
                                
                                <br>&emsp;&emsp;
                                <i class="far fa-arrow-alt-circle-right"></i>&emsp;
                                Shinydashboard is used to display the scores.
                                
                                <br>&emsp;&emsp;
                                <i class="far fa-arrow-alt-circle-right"></i>&emsp;
                                Shinythemes is used to set the themes.
                                
                                <br>&emsp;&emsp;
                                <i class="far fa-arrow-alt-circle-right"></i>&emsp;
                                Shinyjs has been used to accomplish the functions behind the app.
                                
                                <br><br> 
                                <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
                                The Math Eagle character as well as other images created by author himself. This app aims to improve your mathematical skills and gives the player Overall Performance Score.
                                <br><br> 
                                '
                               )
                               #-------------------
                               )
                             )
                         )))


#-------SERVER--------
server <- function(input, output, session){
  showModal(modalDialog(
    title = "Welcome to Math Eagle !!!",
    HTML(
    '
    <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
    Math Eagle aims to reach outer space
    <br><br>
    <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
    He can fly at an unbelievable speed and need your mathematical skills to achieve his aim
    <br><br>
    <i class="fas fa-arrow-alt-circle-right"></i>&emsp;
    Speed of the eagle increases with each correct answer and decreases on each incorrect answer. Within a time limit of 2 minutes, The player has to make the math eagle reach outer space by boosting his speed.
    <br><br>
    All Set To Gooo !!!!!'
    ),
    footer = modalButton("Ready !!!"),
    size = c("m"),
    easyClose = FALSE,
    fade = TRUE
  ))
  output$ans <- renderText({""})
  
  # Page 2 by clicking the start button on Page 1
  observeEvent(input$startGame,{
    updateTabsetPanel(session, "inTabset", selected = "Game")
    
  })
  
  # Initialize the timer
  timer_count <- reactiveVal(121)
  time_taken <- reactiveVal()
  active <- reactiveVal(FALSE)
  output$timer <- renderText({timer_str[timer_count()]})
  cumsum_time <- 0
  data_time_taken_percentage <<- 0
  # change timer for every second
  observe({
    invalidateLater(1000, session)
    isolate({
      if (active()) 
      {
        timer_count(timer_count()-1)
        cumsum_time <<- cumsum_time + 1
        # data to show in dashboard
        time_taken(timer_str[timer_count()])
        data_time_taken_percentage <<- time_taken_percentage(cumsum_time)
        if (timer_count()<1) {
          active(FALSE)
          # data to show in dashboard
          time_taken("00 : 00")
          output$mission <- renderValueBox({
            valueBox(
              paste0(mission()), "Mission Status", icon = icon("fas fa-user-alt"),
              color = "purple"
            )
          })
          updateTabsetPanel(session,"inTabset", selected = "Stats")
        }
      }
    })
  })
  # Game starts
  observeEvent(input$startGame, {active(TRUE)})
  # SPEED AND DISTANCE
  speed <- reactiveVal(10)
  distance <- reactiveVal(0)
  atmosphere <- reactiveVal("")
  active <- reactiveVal(FALSE)
  mission <- reactiveVal("FAILED")
  overall_percentage <- reactiveVal()
  # OUTPUT DISTANCE
  output$distance <- renderText({paste(round(distance(),0),"/440",sep="")})
  # OUTPUT SPEED
  data_distance_reached <<- 0
  observe({
    invalidateLater(100,session)
    isolate({
      if (active()) 
      {
        distance(distance()+(speed()*0.01))
        data_distance_reached <<- round(distance(),0)
        runjs(paste('nav_eagle.style,marginLeft = "',((distance()/440)*100),'%"'))
        # atmosphere types
        if (distance() < 7) {
          runjs(paste0('document.getElementById("atmos").innerHTML = "&emsp; ATMOSPHERE = TROPO &emsp;"'))
        }else if (distance() >=7 & distance() <31) {
          runjs(paste0('document.getElementById("atmos").innerHTML = "&emsp; ATMOSPHERE = STRATO &emsp;"'))
        }else if (distance() >=31 & distance() <50) {
          runjs(paste0('document.getElementById("atmos").innerHTML = "&emsp; ATMOSPHERE = MESO &emsp;"'))
        }else if (distance() >=50 & distance() <440) {
          runjs(paste0('document.getElementById("atmos").innerHTML = "&emsp; ATMOSPHERE = THERMO &emsp;"'))
        }else {
          runjs(paste0('document.getElementById("atmos").innerHTML = "&emsp; ATMOSPHERE = EXO &emsp;"'))
        }
        
        
        if (distance()>440 || distance()==0 || speed()==0) 
        {
          active(FALSE)
          
          if (distance()>440){
          mission("PASSED!!!")
            output$mission <- renderValueBox({
              valueBox(
                paste0(mission()), "Mission Status", icon = icon("fas fa-user-alt"),
                color = "purple"
              )
            })
            runjs(paste('document.getElementById("Signal").innerHTML = "GAME OVER !!!";'))
            runjs(paste('Signal.style.color = "#8d00ad";'))
            runjs(paste('signal.style.backgroundColor = "#e46eff"'))
          }
          updateTabsetPanel(session, "inTabset",selected = "Stats")
        }
      }
    })
  })
  
  observeEvent(input$startGame, {active(TRUE)})
  # EAGLE NAVIGATION
  observe({
   runjs(paste('nav_eagle.style.marginLeft = "',((distance()/480)*100),'%"',sep="")) 
    runjs(paste('myDiv.style.backgroundColor = "rgb(0,',round(((440-distance())/440)*153,0),',',round(((440-distance())/440)*255,0),')"',sep = ""))
    # -------Calculate overall score-------
    # value 1 time based
    val1 <- data_time_taken_percentage
    # value 2 distance based
    val2 <- data_distance_reached
    # value 3 question based
    val3 <- round((data_question_ans()/data_question_ask())*100,0)
    if (data_question_ans() > 0) {
      overall_percentage(round(((val1+((val2/440)*100)+val3)/3)/10,1))
    }else{
      overall_percentage(0)
    }
  })
  # MATH CALCULATION GAME ASKING QUESTION
  ask_ques()
  runjs(paste('document.getElementById("ans_txt").innerHTML = "', ques_str,'";'))
  output$ans_txt <- renderText({ques_str})
  print(ques_str)
  print(ans)
  ans_str_entry <- ""
  
  observeEvent(input$del, {
    ans_str_entry <<- ""
    output$ans <- renderText({""})
  })
  
  observeEvent(input$one, {ans_str_entry <<- paste(ans_str_entry,"1",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$two, {ans_str_entry <<- paste(ans_str_entry,"2",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$three, {ans_str_entry <<- paste(ans_str_entry,"3",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$four, {ans_str_entry <<- paste(ans_str_entry,"4",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$five, {ans_str_entry <<- paste(ans_str_entry,"5",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$six, {ans_str_entry <<- paste(ans_str_entry,"6",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$seven, {ans_str_entry <<- paste(ans_str_entry,"7",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$eight, {ans_str_entry <<- paste(ans_str_entry,"8",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$nine, {ans_str_entry <<- paste(ans_str_entry,"9",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  observeEvent(input$zero, {ans_str_entry <<- paste(ans_str_entry,"0",sep="")
  output$ans <- renderText({ans_str_entry})
  })
  
  # TAKING THE SPEED DATA
  data_speed_vector <<- c(5)
  # QUESTION DATA
  data_question_ask <- reactiveVal(0)
  data_question_ans <- reactiveVal(0)
  
  
  
  observeEvent(input$enter, {
    output$ans <- renderText({""})
    data_question_ask(data_question_ask()+1)
    # data_question_ask <<- data_question_ask + 1 
    
    if(ans_str_entry == ans){
      runjs(paste('document.getElementById("Signal").innerHTML = "CORRECT  ~SPEED+5";'))
      runjs(paste('Signal.style.color = "#008709";'))
      runjs(paste('signal.style.backgroundColor = "#18f553"'))
      speed(speed()+5)
      data_question_ans(data_question_ans()+1)
      # data_question_ans <<- data_question_ans + 1
      data_speed_vector <<- c(data_speed_vector,speed())
      runjs(paste0('document.getElementById("speed").innerHTML = "&emsp; SPEED = ',speed(),' miles/min"'))
    }else{
      runjs(paste('document.getElementById("Signal").innerHTML = "WRONG  ~SPEED-5";'))
      runjs(paste('Signal.style.color = "#820023";'))
      runjs(paste('signal.style.backgroundColor = "#ff3d77"'))
      speed(speed()-5)
      data_speed_vector <<- c(data_speed_vector,speed())
      runjs(paste0('document.getElementById("speed").innerHTML = "&emsp; SPEED = ',speed(),' miles/min"'))
    }
    ans_str_entry <<- ""
    ask_ques()
    runjs(paste('document.getElementById("ans_txt").innerHTML = "', ques_str, '";'))
    output$ans_txt <- renderText({ques_str})
    output$ans_txt <- renderText({""})
  })

  #-------------------------
  max_speed <- reactiveVal(max(data_speed_vector))
  distance_reached <- reactiveVal(data_distance_reached)
  #-------------------------
  # Question 
  total_question_ask <- reactiveVal(data_question_ask)
  total_question_ans <- reactiveVal(data_question_ans)
  
  
  output$mission <- renderValueBox({
    valueBox(
      "...", "Mission Status", icon = icon("fas fa-user-alt"),
      color = "purple"
    )
  })
  output$overall_percentage_box <- renderValueBox({
    valueBox(
      paste0(overall_percentage(), "/10"), "Overall Performance", icon = icon("check-square"),
      color = "fuchsia"
    )
  })                    
  
  output$max_speed_box <- renderValueBox({
    valueBox(
      paste0(max(data_speed_vector), " miles/min"), "Max Speed", icon = icon("fas fa-rocket"),
      color = "red"
    )
  })
  
  output$distance_reached_box <- renderValueBox({
    valueBox(
      paste0(round(distance(),0), " miles"), "Distance Reached", icon = icon("fas fa-flag-checkered"),
      color = "orange"
    )
  })
  
  output$time_taken_box <- renderValueBox({
    valueBox(
      paste0(time_taken()), "End Time", icon = icon("fas fa-stopwatch"),
      color = "yellow"
    )
  })
  
  output$total_question_ask_box <- renderValueBox({
    valueBox(
      paste0(data_question_ask()), "Total questions", icon = icon("fas fa-comment"),
      color = "teal"
    )
  })
  
  output$total_question_ans_box <- renderValueBox({
    valueBox(
      paste0(data_question_ans()), "Answered Correctly", icon = icon("fas fa-comment-dots"),
      color = "lime"
    )
  })
  
}
shinyApp(ui, server)



















