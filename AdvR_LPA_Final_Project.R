### LINKEDIN PROFILE ANALYZER

library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(rvest)
library(pdftools)
library(stringr)

# Define UI 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(to right, #EA8D8D,#A890FE);
        color: #f7f7f7;
        font-family: 'Arial', sans-serif;
        border-radius: 15px;
      }
      .container-fluid {
        padding: 10px;
        height: 100px;
        box-shadow: 0 16px 32px 0 rgba(0,0,0,0.2);
        border-radius: 30px;
        margin:5px;
      }
      .well {
        background-color: rgba(255, 255, 255, 0.9);
        border-radius: 15px;
        border: none;
        box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);

      }
      .btn-primary {
        background-color: #4CAF50;
        border-color: #3e8e41;
      }
      .btn-primary:hover {
        background-color: #45a049;
      }
      # .nav-tabs {
      #   border-bottom: 2px solid #dee2e6;
      #   border-color: grey
      # }
      .nav-link.active {
        background-color: #fff;
        border-color: #dee2e6 #dee2e6 #fff;
        color: #495057;
      }
      .nav-tabs .nav-item {
        margin-bottom: -1px;

      }
      .nav-link {
        border: 1px solid transparent;
        border-top-left-radius: 0.25rem;
        border-top-right-radius: 0.25rem;
        padding: 10px;
        color: #f8f9fa;
        background-color: #6c757d;
      }
      .nav-link:hover {
        border-color: #e9ecef #e9ecef #dee2e6;
      }
      .titlepanel-text {
        color: black; /* Set the title panel text color to black */
    }
    "))
  ),
  theme = shinytheme("flatly"),
  titlePanel(div(img(src="LPA.png", height = 50,style = "padding-right:10px;padding-bottom:5px"),HTML("<b>LinkedIn Profile Analyzer</b>"), style = "display:inline-block;width:100%;text-align: center;text-shadow: 2px 2px grey;color: #000000; padding-bottom: 50px;")),
  style = "background-color:#D3D3D3;",
  sidebarLayout(
    sidebarPanel(
      fileInput("profile_pdf", div("Upload LinkedIn Profile PDF:", style = "color: #000000;text-shadow: 1px 1px grey;"), accept = c(".pdf"), buttonLabel = div("Choose File", style = "color: #000000")),
      tags$style("
             .well {  
             background-color: #D3D3D3;
             
             }

             .progress-bar {
             background-color: green;
             
             }
             
             "),
      
      div(style="display:inline-block;width: 100%;text-align: center;",actionButton("submit", "Run Analysis", icon("paper-plane"), 
                   style="color: #fff; background-color: #089305; border-color: #089305; box-shadow: 0 8px 16px 0 rgba(0,0,0,0.2);")),
      width = 4,
    ),

    
    mainPanel(
      tabsetPanel(
        tabPanel(div("Score", style = "; color: #000000;text-shadow: .25px .25px grey;"), plotlyOutput("dynamic_score", height = 'auto',width = 'auto')),  # Updated to use plotlyOutput
        tabPanel(div("Evaluation", style = "color: #000000;text-shadow: .25px .25px grey;"), uiOutput("dynamic_evaluation",style = "padding: 20px; color: black;" )),
        id = "tabs"
      ),

      style = "background-color: #D3D3D3;box-shadow: 0 16px 32px 0 rgba(0,0,0,0.2); border-radius: 10px;padding: 10px;"
    ),

  )
)

# Define server logic
server <- function(input, output) {
  profile_data <- eventReactive(input$submit, {
    req(input$profile_pdf)
    # Extract text from PDF
    pdf_text <- pdf_text(input$profile_pdf$datapath)
    # Concatenate all pages
    full_text <- paste(pdf_text, collapse = " ")
    full_text
  })
  
  evaluation_results <- reactive({
    evaluate_profile(profile_data())
  })
  
  # Render the score using plotly for a dynamic gauge visualization
  output$dynamic_score <- renderPlotly({
    score_data <- evaluation_results()
    gauge_value <- score_data$score
    plot_ly(
      domain = list(x = c(0, 1), y = c(0, 1)),
      value = gauge_value,
      title = list(text = "Score"),
      type = "indicator",
      mode = "gauge+number",
      gauge = list(
        axis = list(range = c(0, 100)),
        bar = list(color = "darkblue"),
        steps = list(
          list(range = c(0, 50), color = "red"),
          list(range = c(50, 75), color = "yellow"),
          list(range = c(75, 100), color = "green")
        )
      )
    ) %>% 
      layout(margin = 1) %>% 
      layout(plot_bgcolor='#D3D3D3') %>% 
      layout(paper_bgcolor="#D3D3D3")
  })
  output$dynamic_evaluation <- renderUI({
    eval_data <- evaluation_results()
    details <- paste("Evaluation Details:", unlist(eval_data$details), collapse = "<br>")
    HTML(details)
  })
}

# Set the URL
url <- "https://everyonesocial.com/blog/linkedin-profile-optimization/"
# Read HTML content from the webpage
page <- read_html(url)
# Keywords to check in the content
keywords <- c("Profile", "Background", "Banner", "Headline", "Summary", "Experience", "Education", "Skills", "Certifications")
# Extract the section of interest
content <- page %>%
  html_nodes(xpath = "//h2[contains(., 'LinkedIn Profile Optimization Tips for Maximum Exposure')]/following-sibling::node()[not(self::h2)]") %>%
  html_text() %>%
  paste(collapse = "\n")
# Function to check for keywords presence
check_keywords <- function(content, keywords) {
  results <- sapply(keywords, function(keyword) {
    if (grepl(keyword, content, ignore.case = TRUE)) {
      paste(keyword, ": Present")
    } else {
      paste(keyword, ": Not Present")
    }
  })
  return(results)
}
# Check for keywords in the content
keyword_results <- check_keywords(content, keywords)
# Print the content and keyword check results
cat("Extracted Content:\n", content, "\n\n")
cat("Keyword Presence Check:\n")
print(keyword_results)


# Function to evaluate the profile
evaluate_profile <- function(profile_text) {
  score <- 0
  details <- list()
  
  if (str_detect(profile_text, "Profile")) {
    score <- score + 10
    details$photo <- "Profile picture is set (+10 points)."
  } else {
    details$photo <- "No profile picture found (0 points)."
  }
  
  if (str_detect(profile_text, "Background|Banner")) {
    score <- score + 10
    details$background <- "Background photo is set (+10 points)."
  } else {
    details$background <- "No background photo found (0 points)."
  }
  
  if (str_detect(profile_text, "Headline")) {
    score <- score + 10
    details$headline <- "Headline is well-crafted (+10 points)."
  } else {
    details$headline <- "Headline is missing or poorly crafted (0 points)."
  }
  
  if (str_detect(profile_text, "Summary")) {
    score <- score + 10
    details$about <- "About section is well-crafted (+10 points)."
  } else {
    details$about <- "About section is missing or incomplete (0 points)."
  }
  
  if (str_detect(profile_text, "Experience")) {
    score <- score + 20
    details$experience <- "Relevant work experience is listed, try to add bullet points where you add relavent experience and skills you got use in that job role (+20 points)."
  } else {
    details$experience <- "Work experience section is incomplete (0 points)."
  }
  
  if (str_detect(profile_text, "Education") && str_detect(profile_text, "Skills")) {
    score <- score + 30
    details$education_skills <- "Education and skills are well documented try to keep it updated with relavent job description (+30 points)."
  } else {
    details$education_skills <- "Education or skills are missing which are very crucial (0 points)."
  }
  
  if (str_detect(profile_text, "Certifications|Publications")) {
    score <- score + 10
    details$url <- "Certifications and Publications are well documented, keep it updated (+10 points)."
  } else {
    details$url <- "Certifications and Publications are missing (0 points)."
  }
  
  return(list(score = score, details = details))
}

# Run the application
shinyApp(ui = ui, server = server)
