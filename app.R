if (!require('shiny')) install.packages('shiny'); library('shiny')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('rstatix')) install.packages('rstatix'); library('rstatix')
if (!require('ggpubr')) install.packages('ggpubr'); library('ggpubr')
if (!require('openxlsx')) install.packages('openxlsx'); library('openxlsx')

# Define UI for data upload app ----
ui <- fluidPage(
  navbarPage("ICP OES Analyser",
             tabPanel("Input Data",
                      # App title ----
                      titlePanel("Uploading Files"),
                      sidebarLayout(
                      sidebarPanel(
                               # Input: Select a file ----
                               fileInput("file_rawdata", "Choose the Text File containing measurement data",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    ".csv")),
                               
                               

                               # Horizontal line ----
                               tags$hr(),
                               
                               
                               # Input: Checkbox if file has header ----
                               checkboxInput("default", "Use default standards", TRUE),
                               
                               
                               # Horizontal line ----
                               tags$hr(), 
                      
                              tableOutput("standards_table")
                      ),
                      
                               mainPanel(
                                      tableOutput("raw_table")
                        )
                      )
                      ),
                          
             tabPanel("Calibration Curves",
                      titlePanel("Calibration Curves"),
                      
                      # Sidebar layout with input and output definitions ----
                      fluidRow(
                        # Main panel for displaying outputs ----
                        column(12,
                        mainPanel(
                          #tableOutput("processed_plot")
                          plotOutput("processed_plot")

                          
                        )
                        )
                      )
                      ),
             tabPanel("Download",
                      titlePanel("Download Excel with Concentration Data"),
                      fluidRow(
                        column(3,
                               downloadButton("downloadData", "Download")
                               
                        )))
             )
)

# Define server logic to read selected file ----
server <- function(input, output) {
  
  output$raw_table <- renderTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    
    req(input$file_rawdata)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        df <- read_delim(input$file_rawdata$datapath, na = c("--", "NA", "####")) %>%  slice(-1)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(df)
    
    
    })
  
  output$standards_table <- renderTable({
    
    req(input$default)
    if (input$default == TRUE){
      label=c("Blank", "Standard 1", "Standard 2", "Standard 3", "Standard 4", "Standard 5", "Standard 6", "Standard 7")
      std_concentration=c(0, 0.001, 0.01, 0.1, 0.5, 1, 5, 10)
      standards <- data.frame(label, std_concentration)
    }
    
    return(standards)
    
    
  })
  
    output$processed_plot <- renderPlot({
      req(input$file_rawdata)

      df <- read_delim(input$file_rawdata$datapath, na = c("--", "NA", "####"))

      
      
      req(input$default)
      if (input$default == TRUE){
        label=c("Blank", "Standard 1", "Standard 2", "Standard 3", "Standard 4", "Standard 5", "Standard 6", "Standard 7")
        std_concentration=c(0, 0.001, 0.01, 0.1, 0.5, 1, 5, 10)
        standards <- data.frame(label, std_concentration)
      }
  
      df %>%
        slice(-1) %>% 
        mutate_all(funs(gsub("!", "", .))) %>% 
        mutate(across(c(-1,-2), as.numeric)) %>% 
        rename(label = `Solution Label`) %>%
        pivot_longer(data=., cols=c(-1, -2), names_to="element") -> df



      df %>%
        filter(str_detect(`Rack:Tube`, "^S") |
                 str_detect(label, "^S") |
                 str_detect(label, "^Blank")) %>%
        left_join(standards , ., by="label") -> df_standards
      

      df_standards %>%
        ggscatter(x="std_concentration", y="value", add = "reg.line") +
        stat_cor(method = "pearson",
                 aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))
                 , p.accuracy = 0.1, r.digits=1) +
        facet_wrap(~element, scales="free_y")
      })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("ICP_Concentration.xlsx")
      },
      content = function(file)
      {
        
      req(input$file_rawdata)
      
      df <- read_delim(input$file_rawdata$datapath, na = c("--", "NA", "####"))
      
      df_original <- df
      
      req(input$default)
      if (input$default == TRUE){
        label=c("Blank", "Standard 1", "Standard 2", "Standard 3", "Standard 4", "Standard 5", "Standard 6", "Standard 7")
        std_concentration=c(0, 0.001, 0.01, 0.1, 0.5, 1, 5, 10)
        standards <- data.frame(label, std_concentration)
      }
      
      df %>%
        slice(-1) %>% 
        mutate_all(funs(gsub("!", "", .))) %>% 
        mutate(across(c(-1,-2), as.numeric)) %>% 
        rename(label = `Solution Label`) %>%
        pivot_longer(data=., cols=c(-1, -2), names_to="element") -> df
      
      
      
      df %>%
        filter(str_detect(`Rack:Tube`, "^S") |
                 str_detect(label, "^S") |
                 str_detect(label, "^Blank")) %>%
        left_join(standards , ., by="label") -> df_standards
      
      df_standards %>%
        group_by(element) %>%
        cor_test(., vars=value, var2=std_concentration) -> correlations


      df_standards %>%
        group_by(element) %>%
        do(mod = lm(value ~ std_concentration, data = .)) -> models

      models %>%
        do(data.frame(
          element=.$element,
          var = names(coef(.$mod)),
          coef(summary(.$mod)))) -> model_results
      
      models %>% 
        do(data.frame(
          element=.$element, 
          var = names(coef(.$mod)),
          coef(summary(.$mod)))) %>% 
          dplyr::select(1:3) %>% 
          pivot_wider(names_from=var, values_from=Estimate) %>% 
          left_join(., df, by="element") %>% 
          mutate(concentration=(value -`(Intercept)`)/std_concentration) %>% 
          dplyr::select(-std_concentration, -`(Intercept)`) %>% 
          rename(intensity=value, rack_tube=`Rack:Tube`) -> df_concentrations
      
      
      # Make Table Pretty (Excel) -------------------------------------------------------------
      df_concentrations %>% 
        pivot_wider(., id_cols=c(rack_tube, label), names_from=element, values_from = concentration) %>% 
        mutate(across(where(is.numeric), ~ case_when(.x < -0  ~ 0,
                                                     TRUE ~ as.numeric(.x)))) -> output 
      
      excel_sheets <- list("raw_data"=df_original, "processed_concentrations"=output)
      #write_csv(output,"./processed_data/results.csv")
      write.xlsx(excel_sheets, file=file, overwrite=TRUE)
    }
    )
}

# Create Shiny app ----
shinyApp(ui, server)