library(shiny)
library(magrittr)
library(here)
library(rvest)
library(showtext)
library(rvest)
library(stringr)
library(purrr)

font_add(family = "lato",
         regular = "Lato-Regular.ttf",
         bold = "Lato-Bold.ttf",
         italic = "Lato-Italic.ttf",
         bolditalic = "Lato-BoldItalic.ttf")
font_add(family = "Mr De Haviland",
         regular = "MrDeHaviland-Regular.ttf")

showtext_auto()

#---- UI ----

ui <- fluidPage(
    
    # where to get CSS
    includeCSS(path = here::here("www", "styles.css")),
    # Application title
    titlePanel("And The Winner Is!"),
    
    div(
        class = "flex-outer-container", # container for everything except title and footer
        
        div(
            class = "main-content",
            
            div(
                class = "winner",
                uiOutput("name_link")
            ),
            div(
                class = "tree",
                img(src = "lit_tree_transparent2.png")
            ),
            div(
                class = "button",
                actionButton(
                    inputId = "button",
                    label = "Pick Winner"
                ),
                actionButton(
                    inputId = "title_button",
                    label = "Reveal Their Movie Title"
                )
            ),
            div(
                class = "plot-intro",
                uiOutput("plot_intro")
            ),
            div(
                class = "plot",
                uiOutput("plot")
            ),
            div(
                class = "plot",
                uiOutput("my_title")
            )
        )
        
    ) # outer-most container ends
    
) # ui ends

#---- SERVER ----

server <- function(input, output) {

    team <- sort(c(
        
        "Julia",
        "Mara",
        "Jenny",
        "Jen",
        "Hannah",
        "Alison",
        "Jesse",
        "Kelly",
        "Toni",
        "Sarah",
        "Sigrid",
        "Kaitlyn",
        "Jim",
        "Winston",
        "Joe",
        "JJ",
        "Garrett",
        "Hadley",
        "Max",
        "Sean",
        "Tom",
        "Thomas",
        "Josiah",
        "Carson",
        "Jeremy"
    ))

    my_vals <- reactiveValues()
    
    observeEvent(input$button, {
        
        # pick a winner
        winner <- sample(x = team, size = 1)
        
        my_url <- paste0("https://www.imdb.com/search/title-text/?plot=",
                         winner,
                         "&ref_=fn_pl")
        
        output$name_link <- renderUI({

            tags$h3(winner)
            
        })
        
        message(my_url)
        
        plot_summaries <- read_html(my_url)
        
        plot_summary <- plot_summaries %>% 
            html_nodes(".lister-item-content") %>%
            html_text() %>%
            map(str_remove_all, pattern = "\\s{2,}") %>% 
            map(discard, is.na) %>%
            compact() %>%
            sample(size = 1) %>% 
            pluck(1) %>% 
            str_remove(pattern = "See All\\s.")
        
        my_vals$title <- plot_summary %>% 
            str_remove(pattern = "^[0-9]{0,3}\\.?") %>% 
            str_extract(pattern = "^.*\\([0-9].*\\)")
        
        plot_summary <- plot_summary %>% 
            str_extract(pattern = "(?<=Plot\n\\([0-9]{1,3}\\))(.*$)")
            #str_extract(pattern = paste0(winner, ".*$"))
        
        output$plot_intro <- renderUI({

          paste0(winner, " in a random IMDB film plot summary. Can you name it?")
            
        })
        
        output$plot <- renderUI({
            
            paste0(plot_summary)
            
        })
        
        output$my_title <- renderUI({
            tags$h4("SGLJK")
        })
        
        
    }) # observer 1 ends
    
    observeEvent(input$title_button, {
        
        req(my_vals)
        
        output$my_title <- renderUI({
            
            div(
                class = "movie-title",
                my_vals$title
            )
            
        })
        
    })
    
} #server ends

# Run the application 
shinyApp(ui = ui, server = server)
