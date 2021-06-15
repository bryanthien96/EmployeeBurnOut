library(shiny)
library(randomForest)
library(shinythemes)
library(data.table)
library(ggplot2)
library(shinyWidgets)

setBackgroundImage(src = NULL, shinydashboard = FALSE)

shiny_background <- base64enc::dataURI(file="www/background.png", mime="image/png")
intro_background <- base64enc::dataURI(file="www/Intro.png", mime="image/png")

#read the dataset for EDA
dfclean <- read.csv("dfclean.csv")

#read the RF model
model <- readRDS("model.rds")

# Define UI
ui <- fluidPage(setBackgroundImage(src=shiny_background),
                theme = shinytheme("journal"),
                navbarPage(
                  theme = "journal",  
                  "Employee Burnout Prediction", 
                  tabPanel("Introduction", icon = icon("book"), img(src=(intro_background), height=630, width=1470)),
                  tabPanel("Exploratory Data Analysis", icon = icon("bar-chart"),
                           sidebarPanel(
                             h3('Select the Category'),
                             varSelectInput("eda", "Frequency of Burnout Category by Group:", dfclean),
                             checkboxInput("side", "Create a Side-By-Side Bar Chart"),
                             h5("Designation:"),
                             h5(code("0 - Interns/Trainee/Junior Executive")),
                             h5(code("1 - Senior Executive/Team Leader/Assistant Manager")),
                             h5(code("2 - Senior Manager/Branch Manager/Regional Manager")),
                             h5(code("3 - Diretor/Assistant Director")),
                             h5(code("4 - Vice President/Assistant Vice President")),
                             h5(code("5 - President/CEO/CFO/CMO/Chairman"))),
                           mainPanel(plotOutput("plot"))),
                  tabPanel("Mental Fatigue Scores Quiz", icon = icon("file-alt"), 
                           sidebarPanel(
                             h2('Mental Fatigue Scores Quiz'),
                             h5('Kindly take this quiz first before you proceed to do the burnout test.'),
                             h5('This quiz is rated in a scale of 1 to 5 by which:'),
                             h5(div('1 - Not at all', style = "color:red")),
                             h5(div('2 - A little bit', style = "color:red")),
                             h5(div('3 - Moderately', style = "color:red")),
                             h5(div('4 - Quite a bit', style = "color:red")),
                             h5(div('5 - Extremely', style = "color:red")),
                             sliderInput('Q1', 'Have you felt fatigue in the past month?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q2', 'Do you find it difficult to start things?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q3', 'Does your brain become fatigued quickly when you have to think hard?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q4', 'Do you take a long time to recover after you have worked âuntil you dropâ or are no longer able to concentrate?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q5', 'Do you find it difficult to gather your thoughts and concentrate?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q6', 'Do you forget things more often than before?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q7', 'Do you feel that it takes an unusually long time to conclude a train of thought or solve a task that requires mental effort?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q8', 'Do you find it difficult to cope with stress that is, doing several things at the same time while under time pressure?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q9', 'Do you find that you cry more easily than previously?', 3, min = 1, max = 5, step = 1),
                             sliderInput('Q10', 'Are you unusually short-tempered or irritable about things that previously did not bother you?', 3, min = 1, max = 5, step = 1),
                             width=20),
                           mainPanel(
                             h3('Results of Mental Fatigue Scale (MFS)'),
                             h4('Your mental fatigue score is'),
                             verbatimTextOutput('ValueMFS'),
                             width = 20
                           )),
                  
                  tabPanel("Prediction", icon = icon("cogs"), 
                           sidebarPanel(
                             tags$label(h3('Input Parameters')),
                             
                             selectInput("Gender", label = "Gender:", 
                                         choices = list("Female" = "Female", "Male" = "Male"), 
                                         selected = "Female"),
                             selectInput("Company.Type", label = "Company Type:",
                                         choices = list("Product" = "Product", "Service" = "Service"), 
                                         selected = "Product"),
                             selectInput("WFH.Setup.Available", label = "Work From Home Setup Availability:",
                                         choices = list("No" = "No", "Yes" = "Yes"), 
                                         selected = "No"),
                             numericInput("Designation", label = "Designation:", 
                                          min = 0, max = 5,
                                          value = 0),
                             numericInput("Resource.Allocation", label = "Resource Allocation:", 
                                          min = 1, max = 24,
                                          value = 1),
                             numericInput("Mental.Fatigue.Score", label = "Mental Fatigue Score:", 
                                          min = 0, max =10,
                                          value = 0),
                             
                             actionButton("submitbutton", "Submit", class = "btn btn-primary")
                           ),
                           
                           mainPanel(
                             tags$label(h1('Stage of Burnout :')), # Status/Output Text Box
                             verbatimTextOutput('status'), # Prediction results
                             verbatimTextOutput('contents'),
                             h3(":: User's Guide ::"),
                             h4("1. Input the necessary parameters to run the prediction."),
                             h4("2. Designation is ranked from 0 to 5 as below:"),
                             h5(code("0 - Interns/Trainee/Junior Executive")),
                             h5(code("1 - Senior Executive/Team Leader/Assistant Manager")),
                             h5(code("2 - Senior Manager/Branch Manager/Regional Manager")),
                             h5(code("3 - Diretor/Assistant Director")),
                             h5(code("4 - Vice President/Assistant Vice President")),
                             h5(code("5 - President/CEO/CFO/CMO/Chairman")),
                             h4("3. Resource Allocation represents the number of hours you work in a day."),
                             h4("4. Mental Fatigue Score is the score you obtained from the quiz in the previous tab."),
                             h4('5. Click <Submit> button to get the result!')
                           ))
                  
                ) # navbarPage
) # fluidPage




# Define server function  
server<- function(input, output, session) {
  # Input Data
  MFS <- function(Q1,Q2,Q3,Q4,Q5,Q6,Q7,Q8,Q9,Q10) (Q1+Q2+Q3+Q4+Q5+Q6+Q7+Q8+Q9+Q10)/5
  
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Gender",
               "Company.Type",
               "WFH.Setup.Available",
               "Designation", 
               "Resource.Allocation",
               "Mental.Fatigue.Score"),
      Value = as.character(c(input$Gender,
                             input$Company.Type,
                             input$WFH.Setup.Available,
                             input$Designation,
                             input$Resource.Allocation,
                             input$Mental.Fatigue.Score)),
      stringsAsFactors = FALSE)
    
    Status <- "Status"
    df <- rbind(df, Status)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE, stringsAsFactors = TRUE)
    
    test$Gender <- factor(test$Gender, levels = c("Female", "Male"))
    test$Company.Type <- factor(test$Company.Type, levels = c("Product", "Service"))
    test$WFH.Setup.Available <- factor(test$WFH.Setup.Available, levels = c("No", "Yes"))
    
    Output <- predict(model,test)
    if (Output == "mild"){
      paste("You are either having no burn out or at the mild stage of burn out right now.
At this stage, you may begin to experience the anticipated stresses of the iniatitive you are undertaking.
Thus, you are recommended to start developing positive coping strategies, such as trying a relaxing activity or getting some exercises to take your mind off work whenever necessary.")
    } else if (Output == "moderate") {
      print("You are in the middle stage of burnout right now.
You may find your optimism waning, as well as notice common stress symptoms affecting you physically, mentally, or emotionally.
Hence, please reach out to a support network of people you trust who might help you cope before your condition worsens to the last stage.")
    } else {
      paste("CAUTION! You are in the most critical stage of burn out right now!
You may feel that your mind is never quiet and peace seems unattainable at this point.
Therefore, you are highly encouraged to seek for external support (counselling service/psychologist/psychiatrist) for a better recovery process.")
    }
  })
  
  # Status/Output Text Box
  output$plot <- renderPlot({
    if(input$side){
      ggplot(dfclean, aes(!!input$eda, fill=Burn_Out_Status)) +
        geom_bar(position = position_dodge(), width=0.5) + theme_minimal()
    } else{
      ggplot(dfclean, aes(!!input$eda, fill=Burn_Out_Status)) +
        geom_bar(width = 0.5) + theme_minimal()
    }})
  output$ValueMFS <- renderPrint({MFS(input$Q1,input$Q2,input$Q3,input$Q4,input$Q5,input$Q6,input$Q7,input$Q8,input$Q9,input$Q10)})
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Your prediction is completed.") 
    } else {
      return("It is ready for prediction.")
    }
  })
  
  
  # Prediction results
  output$status <- renderText({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}


# Create Shiny object
shinyApp(ui = ui, server = server)
