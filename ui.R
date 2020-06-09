fluidPage(
  theme = "layout.css", 
  fluidRow(
    tags$head(
      tags$script('
      $(document).on("keydown", function (e) {
         Shiny.onInputChange("keypress", e.keyCode || e.which, {priority: "event"});
      });
    ')
    ),
    actionButton("start", "Start"),
    uiOutput("main.display"),
    verbatimTextOutput('transitionNumber'),
    verbatimTextOutput('otherinfo'),
    tableOutput("responseTable")
    
    
  )
)