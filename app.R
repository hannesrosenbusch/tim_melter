

library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(tibble)
library(DT)

# Define UI
ui <- fluidPage(theme = "cerulean",
                tags$style(type="text/css", "body {padding-top: 65px;}"),
                
                #navbarpage includes all tabpanels of the UI
                navbarPage(
                  "Melted Data",
                  position = "fixed-top",
                  
                  #Instruction panel
                  tabPanel("Instructions", useShinyjs() ,"This app is for transforming appinio data into a long format that is compatible with other softwares. If you have questions please message Hannes or Tim.",
                           fileInput(inputId = "analyzer_csvdf", label = "Upload .CSV data", multiple = FALSE, accept = c(".csv")),
                           textOutput("checkinputs"),
                          
                          ###LOGIN AUTHENTICATION (UNCOMMENT WHEN PUSHING TO SHINYAPPS.io)
                          # tags$div(
                          #    id = "login-basic",
                          #    style = "width: 500px; max-width: 100%; margin: 0 auto;",
                          # 
                          #    tags$div(
                          #      class = "well",
                          #      h4(class = "text-center", "Please login"),
                          #      p(class = "text-center",
                          #        tags$small("First approach login form")
                          #      ),
                          # 
                          #      textInput(
                          #        inputId     = "ti_user_name_basic",
                          #        label       = tagList(icon("user"),
                          #                              "User Name"),
                          #        placeholder = "Enter user name"
                          #      ),
                          # 
                          #      passwordInput(
                          #        inputId     = "ti_password_basic",
                          #        label       = tagList(icon("unlock-alt"),
                          #                              "Password"),
                          #        placeholder = "Enter password"
                          #      ),
                          # 
                          #      tags$div(
                          #        class = "text-center",
                          #        actionButton(
                          #          inputId = "ab_login_button_basic",
                          #          label = "Log in",
                          #          class = "btn-primary"
                          #        )
                          #      )
                          #    )
                          #  ),
                           tags$br(),
                           tags$br(),
                           tags$br(),
                           textOutput("login_feedback"),



                  ),
                 

                  tabPanel("Next step", "Select which variables will be kept (left side; often used for splitting), and which ones should be stacked into one column (right side).",
                           splitLayout(cellWidths = c("25%", "25%", "40%", "10%"),
                               ###SOMETHING TO SELECT VARIABLES
                               conditionalPanel(
                                 condition = "output.analyzer_csv_provided",
                                 uiOutput("id_selector"),
                               ),
                               conditionalPanel(
                                 condition = "output.analyzer_csv_provided",
                                 uiOutput("stack_selector"),
                               ),
                           
                               DT::dataTableOutput("df_long"),
                               downloadButton("dl","Export in Excel"))
                           
                           )
                )#end of navbarPage
)#end of UI / fluidPage




#server 
server <- function(input, output) {
  
  ### FUNCTIONS THAT DO THE CRUCIAL STEPS SHOULD BE CONTAINED IN OTHER .R FILE FROM WHERE THEY CAN ALSO BE TESTED
  source('hanneshelpers.R')
  
  
  ####login authentication, can leave as is until incl "...feedback()})"
  user_base_basic_tbl <- tibble(
    user_name = "appinio",
    password  = "7umxJgk2Cd" #hardcoded bc all data is not persistent anyways, but do not put in public repos
  )
  validate_password_basic <- eventReactive(input$ab_login_button_basic, {
    validate <- FALSE
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password )
    {validate <- TRUE}
  })
  observeEvent(validate_password_basic(), {
    shinyjs::hide(id = "login-basic")
  })
  feedback <- eventReactive(input$ab_login_button_basic, {
    req(input$ti_user_name_basic)
    req(input$ti_password_basic )
    if (input$ti_user_name_basic == user_base_basic_tbl$user_name &&
        input$ti_password_basic == user_base_basic_tbl$password ){
      "Login succeeded"
    }else{"Login failed"}
  })
  output$login_feedback <- renderText({
    feedback()})
  
  
  ### check if data was uploaded
  output$analyzer_csv_provided <- reactive({
    #req(validate_password_basic()) #uncomment for shinyapps.io version
    isTruthy(input$analyzer_csvdf)})
  
  ### make uploaded data useable anywhere in script through df_analyzer_csv()
  df_analyzer_csv = reactive({d = data.table::fread(input$analyzer_csvdf$datapath, data.table = FALSE)
                  colnames(d) = sapply(colnames(d), function(x){gsub("([^A-Za-z ])+", "", x)} )
                  d})

  ### check user inputs and return informative error messages that RC can solve
  output$checkinputs = renderText({
    req(input$analyzer_csvdf)
    df_a = df_analyzer_csv()
    if(nrow(df_a) < 5){stop("Your upload has less than 5 rows")}
    "Uploads seem valid :)"
  })
  
  ###select variables for columns
  output$stack_selector = renderUI({
    req(input$analyzer_csvdf)
    analyzer_csv = df_analyzer_csv()
    checkboxGroupInput("stack_vars","Select variables to stack",choices=colnames(analyzer_csv))})
  
  ###select variables for rows
  output$id_selector = renderUI({
    req(input$analyzer_csvdf)
    analyzer_csv = df_analyzer_csv()
    checkboxGroupInput("id_vars","Select variables for  splits",choices=colnames(analyzer_csv))})
  
  id = eventReactive(input$id_vars, {input$id_vars})
  stack = eventReactive(input$stack_vars, {input$stack_vars})
  
  output$df_long = DT::renderDataTable({
    req(input$id_vars)
    req(input$stack_vars)

    df_long = df_analyzer_csv() %>% 
      mutate(across(everything(), as.character)) %>%
      select(c(id(), stack())) %>%
      pivot_longer(., cols = stack(), names_to = "variable", values_to = "value")

    DT::datatable(df_long, 
              extensions = 'Buttons',
              options = list(buttons = c('copy', "csv"), dom = "lfti")) #B
  })

  
  output$dl <- downloadHandler(
    filename = function() {
      paste0("melted_data.xlsx")
    },
    content = function(file){
      
      df_long = df_analyzer_csv() %>% 
        mutate(across(everything(), as.character)) %>%
        select(c(id(), stack())) %>%
        pivot_longer(., cols = stack(), names_to = "variable", values_to = "value")

      writexl::write_xlsx(df_long, path = file) # saving the file
    }
  ) 

  
  ####WRITE NOTIFICATION POP UPS FROM ANY POINT IN THIS SCRIPT
  ####BY INSERTING rv$popup = rv$popup + 1
  rv = reactiveValues(popup = 0)
  observeEvent(rv$dl_count, {
    req(input$propdf)
    showNotification(unlist(data_with_weights()[["convergence"]]) )# 
  })
  
  
  ###PREVENT CONDITIONAL PANEL FROM EXECUTING UNTIL DATA UPLOADED
  outputOptions(output, 'analyzer_csv_provided', suspendWhenHidden = FALSE)
  
  
  ###create dynmaic number of plots
  # output$plots <- renderUI({
  #   req(input$columns)
  #   my_vars = paste0("Plot_", input$columns)
  #   plot_output_list <- lapply(my_vars, function(target) {
  #     output[[target]] <- renderPlot({
  #     orig_target = gsub("Plot_", "", target)
  #     comparison_plot(orig_target, df_analyzer_csv(), df_prop())
  #     })
  #     plotOutput(target) 
  #   })
  #   do.call(tagList, plot_output_list)
  # })
  
  
  ###create content for ppt download
  # output$plt_dl <- downloadHandler(
  #   filename = function() {
  #     paste0("comparison_plots.pptx")
  #   },
  #   content = function(file){
  #     pp_download <- read_pptx()
  #     for (target in input$columns){
  #       
  #       pl = reactive({comparison_plot(target, df_analyzer_csv(), df_prop())})
  #       current_plot = rvg::dml(ggobj = pl())
  #       pp_download = add_slide(pp_download, layout = "Title and Content") %>%
  #         ph_with(location = ph_location_type(type = "title"), value = target) %>%
  #         ph_with(location = ph_location_type(type = "body"), value = current_plot)
  #       }
  #     
  #     print(pp_download, target = file)
  #   }
  # )
  
  
 } #end  server

shinyApp(ui = ui, server = server)