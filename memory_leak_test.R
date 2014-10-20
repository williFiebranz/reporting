## shiny observer memory leak example
rm(list=ls())

library(shiny)

new.storage.object <- function() {
                        list(
                          data = matrix(1, 10^3*2, 10^3*2), # something to store
                          flag = FALSE                      # flag to change
                                                            #  by observer
                        )
                      }

server <- function(input, output) {

  # set up something with some visible memory footprint
  storage        <- reactiveValues()
  storage[["1"]] <- new.storage.object()
  storage[["2"]] <- new.storage.object()
  
  # create observer to change just the flag, but leads to memory leak
  observe({
    
    if (!is.null(input$flag)) isolate({
      
      storage[[input$storageselect]][["flag"]] <- input$flag
      
      print("observer was triggered")
    })
  })

  
  # UI element 1
  observe({
    
    if (length(names(storage)) > 0) isolate({  
      output$ui.storageselect <- renderUI({
        selectInput("storageselect", 
                    "Select one list element of reactive value object:",
                    names(storage),
                    selectize = FALSE)
      })
    })
  })
  
  
  # UI element 2
  observe({
    
    if (!is.null(input$storageselect)) isolate({  
      output$ui.flag <- renderUI({
        if (length(names(storage)) > 0) {
          
          checkboxInput("flag", "Flag in storage (Click to trigger observer)", 
                        value = storage[[input$storageselect]][["flag"]])
          
        } else {
          NULL
        }
      })      
    })
  })
  
  # UI element 2
  observe({
    
    if (!is.null(input$storageselect)) isolate({  
      output$ui.flag <- renderUI({
        if (length(names(storage)) > 0) {
          
          checkboxInput("flag", "Flag in storage (Click to trigger observer)", 
                        value = storage[[input$storageselect]][["flag"]])
          
        } else {
          NULL
        }
      })      
    })
  })
  
  # UI element 3 (with memory leak)
  observe({

    if (!is.null(input$storageselect)) {
      
      backup <- reactiveValuesToList(storage) # !!! THIS CAUSES THE MEMORY LEAK
      
      # ... but I could not find any other way to keep the backup file up to 
      # date since the downloadbutton itself has no input value I could us to 
      # trigger this observer context.
    
      isolate({
        
        output$backup <- downloadHandler(
          
          filename = "memory_leak_test_backup.dat",
          content = function(file) {
            save(backup, file = file)
          }
        )
        
        output$ui.backupbutton  <- renderUI({
          
           if (length(names(storage)) > 0) {
             downloadButton("backup", "Backup")
           } else {
             NULL
           }
                   
        })
      })
    }
  })
  
}


### UI

ui <- shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      div(
        uiOutput("ui.storageselect"),
        uiOutput("ui.flag"),
        uiOutput("ui.backupbutton")
      )
    ),
    mainPanel(h5(
      "Hello, thanks for your interest and help.\n
       As you can see (on Windows), everytime your update the flag checkbox\n
       you lose memory that is not recoverable through garbage collection.\n
       I assume this qulifies as a memory leak, since it can make your\n
       system completely run out of memory")
  ))
))

shinyApp(ui = ui, server = server)

