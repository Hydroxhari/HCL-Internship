library(shiny)
library(shinyjs)
library(DBI)
library(RSQLite)
library(dplyr)
library(ggplot2)
library(tidytext)   
library(sentimentr) 


db <- dbConnect(SQLite(), "data/feedback.sqlite")


dbExecute(db, "CREATE TABLE IF NOT EXISTS Users (
  username TEXT PRIMARY KEY,
  password TEXT
)")


dbExecute(db, "CREATE TABLE IF NOT EXISTS Feedback (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  username TEXT,
  comment TEXT,
  score INTEGER,
  sentiment TEXT
)")


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "custom.js")  
  ),
  useShinyjs(),  
  div(id = "main-container",
      uiOutput("pageContent")
  )
)


server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)


  output$pageContent <- renderUI({
    if (!is.null(current_user())) {
      dashboardUI()
    } else {
      loginPageUI()
    }
  })


  loginPageUI <- function() {
    div(class = "center-container",
        div(class = "login-container",
            h2("Login to Your Account"),
            div(class = "input-container",
                textInput("username", "Username"),
                passwordInput("password", "Password")
            ),
            div(class = "button-container",
                actionButton("login_btn", "Login"),
                actionButton("new_user_btn", "New User")
            ),
            uiOutput("login_message"),
            p(class = "quote", "The only way to do great work is to love what you do. – Steve Jobs")
        )
    )
  }


  dashboardUI <- function() {
    div(class = "center-container",
        div(class = "dashboard-container",
            div(class = "header",
                h1("Activity Center"),
                span(class = "profile-container",
                     span(class = "profile", textOutput("user_profile")),
                     div(class = "logout-menu",
                         actionButton("logout_btn", "Logout")
                     )
                )
            ),
            div(class = "dashboard-content",
                div(class = "dashboard-item",
                    img(src = "images/feedback.png", class = "dashboard-image"),
                    h3("Submit Feedback"),
                    actionButton("submit_feedback", "Submit Feedback")
                ),
                div(class = "dashboard-item",
                    img(src = "images/analysis.png", class = "dashboard-image"),
                    h3("Sentiment Analysis"),
                    actionButton("sentiment_analysis", "Sentiment Analysis")
                ),
                div(class = "dashboard-item",
                    img(src = "images/database.png", class = "dashboard-image"),
                    h3("Database Review"),
                    actionButton("database_review", "View Database")
                )
            )
        )
    )
  }


  observeEvent(input$login_btn, {
    username <- input$username
    password <- input$password

    if (!is.null(username) && !is.null(password) && username != "" && password != "") {
      query <- dbSendQuery(db, "SELECT * FROM Users WHERE username = ? AND password = ?", list(username, password))
      result <- dbFetch(query)
      dbClearResult(query)

      if (nrow(result) > 0) {
        current_user(username)
        session$sendCustomMessage("showNotification", list(
          type = "success",
          message = "Login successful! Welcome to the Dashboard."
        ))
      } else {
        session$sendCustomMessage("showNotification", list(
          type = "error",
          message = "Incorrect username or password!"
        ))
      }
    } else {
      session$sendCustomMessage("showNotification", list(
        type = "error",
        message = "Please enter both username and password."
      ))
    }
  })


  observeEvent(input$new_user_btn, {
    output$pageContent <- renderUI({
      registerPageUI()
    })
  })


registerPageUI <- function() {
  div(class = "center-container",
      div(class = "register-container",
          h2("Register a New Account"),
          div(class = "input-container",
              textInput("new_username", "New Username"),
              passwordInput("new_password", "New Password")
          ),
          div(class = "button-container",
              actionButton("register_btn", "Register")
          ),
          uiOutput("register_message"),
          p(class = "quote", "Success is not final, failure is not fatal: It is the courage to continue that counts. – Winston Churchill")
      )
  )
}


observeEvent(input$register_btn, {
  new_username <- input$new_username
  new_password <- input$new_password

  if (!is.null(new_username) && !is.null(new_password) && new_username != "" && new_password != "") {
    query <- dbSendQuery(db, "SELECT * FROM Users WHERE username = ?", list(new_username))
    result <- dbFetch(query)
    dbClearResult(query)

    if (nrow(result) == 0) {
      dbExecute(db, "INSERT INTO Users (username, password) VALUES (?, ?)", list(new_username, new_password))
      session$sendCustomMessage("showNotification", list(
        type = "success",
        message = "Registration successful! Logging you in..."
      ))
      

      current_user(new_username)


      shinyjs::delay(2000, {
        output$pageContent <- renderUI({
          dashboardUI()
        })
      })
    } else {
      session$sendCustomMessage("showNotification", list(
        type = "error",
        message = "Username already exists. Please choose a different one."
      ))
    }
  } else {
    session$sendCustomMessage("showNotification", list(
      type = "error",
      message = "Please fill in both fields."
    ))
  }
})


  observeEvent(input$logout_btn, {
    current_user(NULL)
    session$sendCustomMessage("showNotification", list(
      type = "success",
      message = "You have been logged out."
    ))


    shinyjs::delay(1000, {
      output$pageContent <- renderUI({
        loginPageUI()
      })
    })
  })
  

  output$user_profile <- renderText({
    current_user()
  })
  
sentimentAnalysisUI <- function() {
  feedback_summary <- getFeedbackSummary()
  div(class = "center-container",
      div(class = "sentiment-analysis-container", style = "width: 800px;",  
          h2(style = "font-weight: bold; font-size: 28px;", "Sentiment Analysis"),
          plotOutput("sentiment_plot", width = "100%"),  
          verbatimTextOutput("sentiment_summary"),
          actionButton("next_feedback_overview", "Next"),  
          actionButton("back_to_dashboard", "Back to Dashboard")
      )
  )
}



  getFeedbackSummary <- function() {
    feedback_data <- dbGetQuery(db, "SELECT * FROM Feedback")
    if (nrow(feedback_data) > 0) {
      feedback_summary <- feedback_data %>%
        mutate(sentiment = ifelse(sentiment == "Positive", 1, ifelse(sentiment == "Negative", -1, 0))) %>%
        group_by(sentiment) %>%
        summarise(count = n(), .groups = 'drop')
      return(feedback_summary)
    }
    return(NULL)
  }



analyzeSentiment <- function() {
  feedback_data <- dbGetQuery(db, "SELECT * FROM Feedback")

  if (nrow(feedback_data) > 0) {

    feedback_data$sentiment_score <- sentiment(feedback_data$comment)$sentiment

    feedback_summary <- feedback_data %>%
      mutate(sentiment = ifelse(sentiment_score >= 0.05, "Positive",
                                 ifelse(sentiment_score <= -0.05, "Negative", "Neutral"))) %>%
      group_by(sentiment) %>%
      summarise(count = n(), .groups = 'drop')


    output$sentiment_plot <- renderPlot({
      ggplot(feedback_summary, aes(x = "", y = count, fill = sentiment)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y") +
        scale_fill_manual(values = c("Positive" = "#4CAF50", "Negative" = "#F44336", "Neutral" = "#FFC107"),
                          labels = c("Positive", "Negative", "Neutral")) +
        theme_void() +
        labs(title = "Sentiment Analysis of Feedback",
             fill = "Sentiment") +
        geom_text(aes(label = scales::percent(count / sum(count), accuracy = 0.1)),
                  position = position_stack(vjust = 0.5), color = "white")
    })


    output$sentiment_summary <- renderText({
      paste("Total Feedback Received: ", nrow(feedback_data),
            "\nPositive Feedback: ", sum(feedback_summary$count[feedback_summary$sentiment == "Positive"]),
            "\nNegative Feedback: ", sum(feedback_summary$count[feedback_summary$sentiment == "Negative"]),
            "\nNeutral Feedback: ", sum(feedback_summary$count[feedback_summary$sentiment == "Neutral"]))
    })

    output$suggestions <- renderText({
      "Suggestions for Improvement:\n- Enhance communication\n- Address common issues raised in feedback."
    })
  } else {
    output$sentiment_plot <- renderPlot({ NULL })
    output$sentiment_summary <- renderText({ "No feedback available for analysis." })
    output$suggestions <- renderText({ "" })
  }
}



  observeEvent(input$sentiment_analysis, {
    output$pageContent <- renderUI({
      sentimentAnalysisUI()
    })
    analyzeSentiment()
  })


  observeEvent(input$next_feedback_overview, {
    output$pageContent <- renderUI({
      feedbackOverviewUI()
    })
  })


feedbackOverviewUI <- function() {
  feedback_data <- dbGetQuery(db, "SELECT * FROM Feedback")

  if (nrow(feedback_data) == 0) {
    return(div("No feedback available for analysis."))
  }

  total_feedback <- nrow(feedback_data)
  positive_feedback <- sum(feedback_data$sentiment == "Positive")
  negative_feedback <- sum(feedback_data$sentiment == "Negative")
  neutral_feedback <- sum(feedback_data$sentiment == "Neutral")


  negative_comments <- feedback_data$comment[feedback_data$sentiment == "Negative"]


  negative_counts <- table(negative_comments)
  negative_counts_df <- as.data.frame(negative_counts)


  top_negative_comments <- negative_counts_df %>%
    arrange(desc(Freq)) %>%
    head(5)


  common_issues <- c()
  suggestions <- c()


  for (comment in top_negative_comments$negative_comments) {
    if (grepl("not good|bad|poor|worst", comment, ignore.case = TRUE)) {
      common_issues <- unique(c(common_issues, "Quality of service was mentioned as poor."))
      suggestions <- unique(c(suggestions, "Consider improving the quality of service provided."))
    }
    if (grepl("support|help|assistance", comment, ignore.case = TRUE)) {
      common_issues <- unique(c(common_issues, "Lack of support was mentioned."))
      suggestions <- unique(c(suggestions, "Enhance customer support and provide timely assistance."))
    }
    if (grepl("look better|appearance|design", comment, ignore.case = TRUE)) {
      common_issues <- unique(c(common_issues, "Feedback on design and appearance."))
      suggestions <- unique(c(suggestions, "Consider redesigning for a more appealing look."))
    }
  }


  output_ui <- div(class = "center-container",
    div(class = "feedback-overview-container",
      h2("Feedback Overview"),
      p(paste("Total Feedback Submitted: ", total_feedback)),
      p(paste("Positive Feedback: ", positive_feedback)),
      p(paste("Negative Feedback: ", negative_feedback)),
      p(paste("Neutral Feedback: ", neutral_feedback)),
      h3("Top Negative Feedback Comments:"),
      if (nrow(top_negative_comments) > 0) {
        tags$ul(lapply(1:nrow(top_negative_comments), function(i) {
          tags$li(paste(top_negative_comments$negative_comments[i], 
                        "(Count: ", top_negative_comments$Freq[i], ")"))
        }))
      } else {
        "No negative feedback provided."
      },
      h3("Common Issues Identified:"),
      if (length(common_issues) > 0) {
        tags$ul(lapply(common_issues, function(x) tags$li(x)))
      } else {
        "No common issues identified."
      },
      h3("Suggestions for Improvement:"),
      if (length(suggestions) > 0) {
        tags$ul(lapply(suggestions, function(x) tags$li(x)))
      } else {
        "No specific suggestions available."
      },
      actionButton("back_to_sentiment_analysis", "Back to Sentiment Analysis", class = "back-button")
    )
  )
  
  return(output_ui)
}

observeEvent(input$back_to_sentiment_analysis, {
  output$pageContent <- renderUI({
    sentimentAnalysisUI()
  })
})

  observeEvent(input$back_to_dashboard, {
    output$pageContent <- renderUI({
      dashboardUI()
    })
  })


  observeEvent(input$submit_feedback, {
    output$pageContent <- renderUI({
      feedbackSubmissionUI()
    })
  })


  feedbackSubmissionUI <- function() {
    div(class = "center-container",
        div(class = "feedback-submission-container",
            h2("Submit Your Feedback"),
            textAreaInput("feedback_comment", "Your Feedback", rows = 5),
            sliderInput("feedback_score", "Rate Your Feedback:", 
                        min = 1, max = 5, value = 3, step = 1),
            actionButton("submit_feedback_btn", "Submit"),
            actionButton("back_to_dashboard_feedback", "Back to Dashboard"),
            uiOutput("feedback_message")
        )
    )
  }


  observeEvent(input$submit_feedback_btn, {
    feedback_comment <- input$feedback_comment
    feedback_score <- input$feedback_score  

    if (nchar(feedback_comment) > 0) {

      dbExecute(db, "INSERT INTO Feedback (username, comment, score) VALUES (?, ?, ?)", 
                 list(current_user(), feedback_comment, feedback_score))
      

      sentiment_score <- sentiment(feedback_comment)$sentiment
      sentiment_label <- ifelse(sentiment_score >= 0.05, "Positive",
                                 ifelse(sentiment_score <= -0.05, "Negative", "Neutral"))
      
      dbExecute(db, "UPDATE Feedback SET sentiment = ? WHERE id = last_insert_rowid()", 
                 list(sentiment_label))

      session$sendCustomMessage("showNotification", list(
        type = "success",
        message = "Feedback submitted successfully!"
      ))
      

      updateTextAreaInput(session, "feedback_comment", value = "") 
      updateSliderInput(session, "feedback_score", value = 3)  


      output$pageContent <- renderUI({
        dashboardUI()
      })
    } else {
      output$feedback_message <- renderUI({
        div(class = "alert alert-danger", "Feedback cannot be empty.")
      })
    }
  })


  observeEvent(input$back_to_dashboard_feedback, {
    output$pageContent <- renderUI({
      dashboardUI()
    })
  })


databaseReviewUI <- function() {
  feedback_data <- dbGetQuery(db, "SELECT * FROM Feedback")

  if (nrow(feedback_data) == 0) {
    return(div("No feedback available in the database."))
  }


  num_records <- reactiveVal(5)  

  search_term <- reactiveVal("")

  sentiment_filter <- reactiveVal("All")  


  unique_sentiments <- c("All", unique(feedback_data$sentiment))


  filtered_feedback <- reactive({
    data <- feedback_data
    if (sentiment_filter() != "All") {
      data <- data[data$sentiment == sentiment_filter(), ]
    }
    if (search_term() != "") {
      data <- data[grepl(search_term(), data$comment, ignore.case = TRUE), ]
    }
    return(data)
  })


  output$feedback_table <- renderTable({
    filtered_feedback()[1:num_records(), ]
  })


  output$pagination <- renderUI({
    tagList(
      numericInput("num_records_input", "Number of records to show:", value = num_records(), min = 1, max = nrow(feedback_data), step = 1),
      actionButton("update_records", "Update")
    )
  })


  output$search_bar <- renderUI({
    textInput("search_feedback", "Search Feedback Comments:", value = search_term())
  })


  output$sentiment_filter <- renderUI({
    selectInput("sentiment_select", "Filter by Sentiment:", choices = unique_sentiments, selected = sentiment_filter())
  })


  observeEvent(input$update_records, {
    num_records(input$num_records_input)
  })


  observeEvent(input$search_feedback, {
    search_term(input$search_feedback)
  })

  observeEvent(input$sentiment_select, {
    sentiment_filter(input$sentiment_select)
  })

  div(class = "center-container",
      div(class = "database-review-container",
          h2("Database Feedback Review"),
          uiOutput("search_bar"),        
          uiOutput("sentiment_filter"),   
          uiOutput("pagination"),         
          tableOutput("feedback_table"),
          actionButton("back_to_dashboard_database", "Back to Dashboard")
      )
  )
}


  observeEvent(input$database_review, {
    output$pageContent <- renderUI({
      databaseReviewUI()  
    })
  })

  observeEvent(input$back_to_dashboard_database, {
    output$pageContent <- renderUI({
      dashboardUI()
    })
  })

  onStop(function() {
    dbDisconnect(db)
  })
}

shinyApp(ui = ui, server = server)
