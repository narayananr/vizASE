library(shiny)

# Define UI
shinyUI(pageWithSidebar(
  # Application title
  headerPanel("NOD x PWK Allele/Gene Level Expression Plots"),
  
  # Sidebar with inputs
  sidebarPanel(   
    textInput(inputId="name",label="Gene Name:",value="Igf2r"),
    # textInput(inputId="log",label="standard deviation:",value=1),
    radioButtons(inputId="allele",label="Allele-Level or Gene-level:",
                 choices=list("Allele","Gene"),
                 selected="Allele"),
    radioButtons(inputId="mainEff",label="Collapse One of the Main Effects",
                 choices=list("Age", "Diet", "Cross"),
                 selected="Cross"),
    radioButtons(inputId="log",label="Expression Scale:",
                 choices=list("log2(Cnt+1)","Cnt"),
                 selected="Cnt")
    # sliderInput(inputId="height",label="Plot height:",min=200,max=1200,value=600)
    #textInput(inputId="yaxis",label="y-axis height:",value=0)
    #  selectInput(inputId="alternative",label="One or Two sided (\"two.sided\",\"one.sided\"):",choices=list("two.sided","one.sided"),selected="two.sided")
  ),
  
  
  # Show the caption and plot of the requested variable against mpg
  mainPanel(
    plotOutput(outputId="result")
  )  

  ))