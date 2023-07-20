library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(DT)
library(tidyr)
library(ggplot2)
library(reshape2)
library(scales)
library(ggrepel)
library(dplyr)
library(class)
library(sqldf)
db <- dbConnect(RSQLite::SQLite(), dbname = "data.sqlite")
df<-dbGetQuery(db,"select Name from data")
find_similar_players <- function(input_data, k) {
  # Select the attributes that will be used to find similar players
  attributes <- c("Kicking", "PenaltyTaking", "Freekicks", "NaturalFitness")

  # Extract the selected attributes from the player data
  player_data <- players[, attributes]

  # Standardize the data by subtracting the mean and dividing by the standard deviation
  player_data <- scale(player_data)

  # Use the KNN algorithm to find the k nearest neighbors to the input data
  neighbors <- knn(train = player_data, test = input_data, cl = players$Name, k = k)

  # Extract the names of the k nearest neighbors
  similar_players <- players[neighbors, "Name"]

  # Return the list of similar players
  return(similar_players)
}

ui <- fluidPage(
  useShinyjs(),
  h1("Welcome to Football Players Records!", align = "center"),
  fluidRow(
    tabsetPanel(
      tabPanel("Home",
             fluidRow(
               tags$div(
    class = "row",
    # Add the image to a column
    tags$div(
      class = "col-md-4",
      tags$img(src = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTenFKXobc9cLoFI9iQlslKYX-jRsChXeoKsw&usqp=CAU", width = "100%")
    ),
    # Add the rest of the content to another column
    tags$div(
      class = "col-md-8",
      h3('Welcome to the Football Players Records website, where you can easily manage and
      maintain the records of your favorite football players. Our website offers a user-friendly
      interface that allows you to insert, delete, view, and update the records of players
      with just a few clicks.'),
      h3('Our website is designed to cater to football enthusiasts who are looking to
      keep track of their favorite players stats. With our efficient database management
      system, you can easily add new players, update their information, or delete their
      records whenever required.'),
      h3(' Our website also provides an option to view the complete list of players and
      their stats for easy reference.Overall, our website offers a comprehensive solution
      for managing football player records, making it an essential tool for all football
      enthusiasts.So, come and explore the world
      of football with our user-friendly and
      efficient Football Players Records website!',align='left')
    )
  ))
    ),
    tabPanel("Insert",
             fluidRow(
               column(4,offset=2,
                        div(class = "some-box-class",
                            h3("UID"),
                            h4(textOutput("uid"))),
                        textInput("name", "Name", ""),
                        numericInput("age", "Age", 37, min = 12, max = 40),
                        numericInput("height", "Height (cm)", 180,min=100,max=220),
                        numericInput("weight", "Weight (kg)", 82,min=30,max=150)
               ),
               column(4,
                      sliderInput("kicking", "Kicking Score", min = 1, max = 20, value = 10, step = 1),
                      sliderInput("fitness", "Fitness Score", min = 1, max = 20, value = 10, step = 1),
                      sliderInput("freekicks", "Free Kick Score", min = 1, max = 20, value = 5, step = 1),
                      sliderInput("penalty", "Penalty Kick Score", min = 1, max = 20, value = 15, step = 1)
               ),
               column(6,offset=5,
                      actionButton('insert',"Insert"),
               verbatimTextOutput("result"))
             )
    ),
    tabPanel("Update",
             fluidRow(
               column(4,offset=4,
                      textInput("uidu", "UID", ""),
                      textInput("nameu","Name",""),
                        sliderInput("kickingu", "Kicking Score", min = 1, max = 20, value = 10, step = 1),
                      sliderInput("fitnessu", "Fitness Score", min = 1, max = 20, value = 10, step = 1),
                      sliderInput("freekicksu", "Free Kick Score", min = 1, max = 20, value = 5, step = 1),
                      sliderInput("penaltyu", "Penalty Kick Score", min = 1, max = 20, value = 15, step = 1)
               ),
               column(6,offset=5,
                      actionButton('update',"Update"))
             )
    ),
      tabPanel("Delete",
               fluidRow(
                 column(width = 4, offset = 4,
                        textInput("uidd", "UID", "1173")
                 ),
               column(6,offset=5,
                      actionButton('delete',"Delete")),
                 column(6,offset=2,
                        dataTableOutput("players_tabled"))
               )),
      tabPanel("View",
               fluidRow(
                 column(width = 4, offset = 4,
                        textInput("uidv", "UID", "1158")
                 ),
               column(6,offset=5,
                      actionButton('view',"View"),br(),br()),
                 column(6,offset=2,
                        dataTableOutput("players_table"))
               )
      ),
      tabPanel("KNN Model For Football Players",
             fluidRow(
               sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "player", label = "Player",
                  choices = dbGetQuery(db, "SELECT DISTINCT Name FROM data ORDER BY Name ASC")$Name),
      sliderInput("k", "Number of nearest neighbours:", min = 1, max = 10, value = 5),
      actionButton("submit", "Find similar players!"),br(),
      tableOutput("similar_players")
    ),
    mainPanel(
      plotOutput("similarity_plot1"),
      plotOutput("similarity_plot2"),
      plotOutput("similarity_plot3"),
      plotOutput("similarity_plot4")
    )
  ))),
    tabPanel("Dashboard",
             fluidRow(
               column(width = 12,offset = 0,
                      sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "player1", label = "Player 1",
                  choices = dbGetQuery(db, "SELECT DISTINCT Name FROM data ORDER BY Name ASC")$Name),
      dataTableOutput('play1'),
      selectInput(inputId = "player2", label = "Player 2",
                  choices = dbGetQuery(db, "SELECT DISTINCT Name FROM data ORDER BY Name ASC")$Name),
      dataTableOutput('play2')
    ),
    mainPanel(
      plotOutput(outputId = "player_plot")
    )
  )),
                         column(width=5,offset=1,
                               selectInput("feature", "Select Feature:",
                  choices = c("Kicking", "PenaltyTaking", "Freekicks", "NaturalFitness"),
                  selected = "Kicking"),

      # Pie chart
      plotOutput(outputId = "pie_chart")),
               column(width = 5,offset=0,
                      plotOutput(outputId = "donut_chart"))

             )

    )
      ))
)

server <- function(input, output) {
  output$uid<- renderText({
    query<-paste('select UID from data order by UID desc limit 1')
    value<-dbGetQuery(db,query)
    val<-as.numeric(value)
    paste(val+1)
  })
  observeEvent(input$insert, {
    query<-paste('select UID from data order by UID desc limit 1')
    value<-dbGetQuery(db,query)
    val<-as.numeric(value)
    age <- input$age;uid<-as.character(val+1)
    height <- input$height
    weight <- input$weight
    name<-as.character(input$name)
    kicking<-as.character(input$kicking);penaly<-as.character(input$penalty);
    freekick<-as.character(input$freekicks);fit<-as.character(input$fitness)
    flag<-TRUE
    if (age < 12 || age > 40) {
      flag<-FALSE
      shinyjs::alert("Invalid age")
    }
    if (height < 100 || height > 220) {
      flag<-FALSE
      shinyjs::alert("Invalid height")
    }
    if (weight < 30 || weight > 150) {
      flag<-FALSE
      shinyjs::alert("Invalid weight")
    }
    if (nchar(name)==0){
      flag<-FALSE
      shinyjs::alert('Please enter Name!!!')
    }
    if(flag){
      dbExecute(db,"INSERT INTO data (UID,Name,Age,Height,Weight,Kicking,Freekicks,PenaltyTaking,NaturalFitness) VALUES (?,?,?,?,?,?,?,?,?)",params=list(uid,name,age,height,weight,kicking,freekick,penaly,fit))
      shinyjs::alert('Record Inserted Successfully!')
      shinyjs::refresh()
    }
  })
  observeEvent(input$update, {
    uidu<-input$uidu;nameu<-input$nameu
    query<-paste0('select * from data where UID=',uidu," and Name='",nameu,"'")
    uidl<-dbGetQuery(db,query)
    flag<-TRUE
    if(nrow(uidl)==0){
      flag<-FALSE
      shinyjs::alert('Invalid UID or Name')
    }
    if(flag){
      up<-paste('UPDATE data SET Kicking=',input$kickingu,", Freekicks=",input$freekicksu,
                ", PenaltyTaking=",input$penaltyu,", NaturalFitness=",input$fitnessu,
                " WHERE UID=",uidu)
      dbExecute(db,up)
      shinyjs::alert('Record Updated Successfully!!')
      shinyjs::refresh()
    }
  })
  observeEvent(input$delete ,{
    uidd<-input$uidd
    output$players_tabled <- renderDataTable({
      query<-paste("select * from data where UID=",uidd)
    table<-dbGetQuery(db,query)
      if(nrow(table)==0){
        shinyjs::alert("UID not found")
      } else {
        datatable(table)
        query1<-paste('delete from data where UID=',input$uidd)
        dbExecute(db,query1)
        shinyjs::alert('Record Deleted Successfully!!')
        shinyjs::refresh()
      }
  })
})
  observeEvent(input$view ,{
    uidv<-input$uidv
    output$players_table <- renderDataTable({
      query<-paste("select * from data where UID=",uidv)
    table<-dbGetQuery(db,query)
      if(nrow(table)==0){
        shinyjs::alert("UID not found")
      } else {
      datatable(table)
      }
  })
})
  query1<-paste('select Name,Kicking,NaturalFitness,PenaltyTaking from data')
  player_data<-dbGetQuery(db,query1)
model <- lm(PenaltyTaking ~ Kicking + NaturalFitness, data = player_data)

  # Create the plot
  output$plot <- renderPlot({
    plot(PenaltyTaking ~ Kicking, data = player_data, pch = 19, col = "blue",
         main = "Penalty Score vs Kicking Score",
         xlab = "Kicking Score", ylab = "Penalty Score")
    abline(model, col = "red")
  })

  # Predict the penalty score and display the result
  output$prediction <- renderText({
    input$predict_button
    predict(model, newdata = data.frame(kicking_score = input$kicking_score,
                                        fitness_score = input$fitness_score))
  })
  output$play1<-renderDataTable({
    dbGetQuery(db,paste0("select Age,Height,Weight from data where Name='",input$player1,"'"))
  },options = list(searching = FALSE, lengthChange = FALSE,info = FALSE,paging = FALSE,ordering = FALSE,dom = 't'))
  output$play2<-renderDataTable({
    dbGetQuery(db,paste0("select Age,Height,Weight from data where Name='",input$player2,"'"))
  },options = list(searching = FALSE, lengthChange = FALSE,info = FALSE,paging = FALSE,ordering = FALSE,dom = 't'))
  output$player_plot <- renderPlot({
    # Get data for selected players
    player1_data <- dbGetQuery(db, paste0("SELECT Kicking,PenaltyTaking,Freekicks,NaturalFitness FROM data WHERE Name = '", input$player1, "'"))
    player2_data <- dbGetQuery(db, paste0("SELECT Kicking,PenaltyTaking,Freekicks,NaturalFitness FROM data WHERE Name = '", input$player2, "'"))

    # Combine data into a single data frame
    comparison_data <- rbind(player1_data, player2_data)
    comparison_data$player_name <- c(rep(input$player1, nrow(player1_data)), rep(input$player2, nrow(player2_data)))

    # Melt data for plotting
    comparison_data_melted <- melt(comparison_data, id.vars = "player_name")

    # Create bar chart
    ggplot(comparison_data_melted, aes(x = variable, y = value, fill = player_name)) +
      geom_bar(stat = "identity", position = position_dodge()) +
      labs(x = "Features", y = "Score", fill = "Player") +
      theme_bw()
  })
  feature_freq <- reactive({
    sql_query <- paste0("SELECT ", input$feature, ", COUNT(*) as count FROM data GROUP BY ", input$feature)
    dbGetQuery(db,sql_query)
  })
  output$donut_chart <- renderPlot({
    ggplot(feature_freq(), aes(x=input$feature, y=count, fill=get(input$feature))) +
      geom_bar(stat="identity", position="stack") +
      labs(title="Stacked Bar Chart",
         x="Feature",
         y="Count",
         fill=input$feature)+
      theme_classic()+
    geom_text(aes(label=paste0(round(count/sum(count)*100, 1), "%")), position=position_stack(vjust=0.5))
  },height=900)

  players <- reactive({
    dbGetQuery(db, "SELECT * FROM data")
  })

  # Define the distance metric function for KNN
  euclidean_distance <- function(x, y) {
    x<-as.numeric(x);y<-as.numeric(y)
    sqrt(sum((x - y) ^ 2))
  }

  # Create a KNN model function
  knn_model <- function(dataset, k, target_player) {
    distances <- apply(dataset[, -1], 1, euclidean_distance, target_player[-1])
    sorted_index <- order(distances)
    top_k_index <- sorted_index[1:k]
    top_k_data <- dataset[top_k_index, ]
    return(top_k_data)
  }

  # Create a reactive function to find similar players
  similar_players <- eventReactive(input$submit, {
    target_player <- players() %>% filter(Name == input$player)
    knn_model(players() %>% select(Name, Kicking, Freekicks, PenaltyTaking, NaturalFitness), input$k, target_player %>% select(Kicking, Freekicks, PenaltyTaking, NaturalFitness))
  })

  # Display the similar players' records in a table
  output$similar_players <- renderTable({
    similar_players()
  })
  output$similarity_plot1 <- renderPlot({
    ggplot(similar_players(), aes(x = Name, y = Kicking)) +
      geom_area() +
      geom_point(data = similar_players(), color = "black", size = 2) +
      labs(title = "Kicking",x = "Name", y = "Feature") +
      theme_minimal()
  })
  output$similarity_plot2 <- renderPlot({
    ggplot(similar_players(), aes(x = Name, y = PenaltyTaking)) +
      geom_area() +
      geom_point(data = similar_players(), color = "red", size = 2) +
      labs(title = "Penalty Taking",x = "Name", y = "Feature") +
      theme_minimal()
  })
  output$similarity_plot3 <- renderPlot({
    ggplot(similar_players(), aes(x = Name, y = Freekicks)) +
      geom_area() +
      geom_point(data = similar_players(), color = "blue", size = 2) +
      labs(title="Freekicks",x = "Name", y = "Feature") +
      theme_minimal()
  })
  output$similarity_plot4 <- renderPlot({
    ggplot(similar_players(), aes(x = Name, y = NaturalFitness)) +
      geom_area() +
      geom_point(data = similar_players(), color = "orange", size = 2) +
      labs(title = "Natural Fitness",x = "Name", y = "Feature") +
      theme_minimal()
  })
  output$pie_chart <- renderPlot({
    ggplot(feature_freq(), aes(x="", y=count, fill=get(input$feature))) +
      geom_bar(stat="identity", width=1) +
      coord_polar("y", start=0) +
      labs(fill=input$feature) +
      theme_void()
  })

}

shinyApp(ui=ui, server=server)
