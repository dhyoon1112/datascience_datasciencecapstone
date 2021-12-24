library(shiny)
library(shinydashboard)
library(stringi)
library(stringr)
library(tm)
library(gsubfn)
library(RWeka)
library(ggplot2)

#file <- readRDS(file="./cleanfile")
file <- readRDS(gzcon(url("https://github.com/dhyoon1112/datasciencecapstone/blob/main/cleanfile.rds?raw=true")))

list_vectors <- function(x,y) {
    
    #1. Find all strings with the caption
    #Find all vector positions with argument x in them
    positions <- grepl(x, file)
    
    #Return those vectors, where "positions" is true
    bnt_file <- file[positions]

    #2. Retrieve the subtext within each vector that begins with the argument
    #x is the string to search for
    for (i in 1:length(bnt_file)) {
        #Retrieve the starting position of the argument (string)
        str_start <- regexpr(paste("^",x), bnt_file[i], ignore.case=TRUE)[1]
        
        #Retrieve the total length of the argument
        str_end <- nchar(bnt_file[i])
        
        #Substring the vector, for anything that starts with argument x 
        list[i] <- substr(bnt_file[i], str_start, str_end)
    }

    # #Retrieve the top 10 n-grams with the predicted next word 
    corpus <- VCorpus(VectorSource(bnt_file))
    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = y[1] + 1, max = y[1] + 1))
    Matrix <- TermDocumentMatrix(corpus, control = list(tokenize = Tokenizer))
    Corpus <- findFreqTerms(Matrix, lowfreq = 10)
    CorpusCount <- rowSums(as.matrix(Matrix[Corpus,]))
    CorpusCount <- sort(CorpusCount, decreasing=TRUE)
    CorpusCount <- na.omit(CorpusCount[1:10])
    CorpusCount <- data.frame(text = names(CorpusCount), frequency = CorpusCount)
    predictedword <- " "
    CorpusCount <- cbind.data.frame(CorpusCount,predictedword)
    for (i in 1:nrow(CorpusCount)){
        CorpusCount[i,3] <- tail(strsplit(CorpusCount[i,1],split=" ")[[1]],1)
        
    return(CorpusCount)
    }
}

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "What is the Next Word?",
                          titleWidth = 600)

sidebar <- dashboardSidebar(
    sidebarMenu(
        
        textInput("caption", "Type in a phrase:", value = "input text here")
        
    )
)

body <- dashboardBody(
    
    h2("You typed: "),
    textOutput("inputvalue"),
    h2("and your possible phrases with the predicted words are... "),
    h3("Please allow a minute for the algorithm to run."),
    textOutput("value1")    
)

ui <- dashboardPage(header,
                    sidebar,
                    body
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    observe({
        #argument for shiny app later on
        x_length <- sapply(strsplit(input$caption," "), length)
        print("breakpoint1")
        
        # #Retrieve the list of vectors with the argument string
        string_list <- reactive({list_vectors(input$caption, x_length)})
        print("breakpoint2")

        output$inputvalue <- renderText({
            toString(input$caption)
        })
        output$value1 <- renderText({
            tryCatch({
                toString(string_list()[1,1])}
                ,error = function(err) 
                {print(paste("Prediction not available"))
            })
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
