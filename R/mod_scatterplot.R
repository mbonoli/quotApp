#' scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_scatterplot_ui <- function(id, add_controls = NULL){
  ns <- NS(id)
  # tagList(
  # 
  # )
  tabPanel(
    "Scatterplot",
    sidebarPanel(
      add_controls,
      # uiOutput(ns("helpfile")),
      selectInput(
        inputId = ns("sel_mod_scatterplot_X"),
        label = "X",
        choices = NULL,
        selected = NULL) %>% 
        shinyhelper::helper(
          icon = "question-circle",
          colour = "green",
          type = "markdown",
          content = "help_vars"),
      selectInput(
        inputId = ns("sel_mod_scatterplot_Y"),
        label = "Y",
        choices = NULL,
        selected = NULL),
      selectInput(
        inputId = ns("sel_mod_scatterplot_Color"),
        label = "Color",
        choices = NULL,
        selected = NULL),
      selectInput(
        inputId = ns("sel_mod_scatterplot_Size"),
        label = "Size",
        choices = NULL,
        selected = NULL),
      selectInput(
        inputId = ns("sel_mod_scatterplot_Filter"),
        label = "Filter",
        choices = NULL,
        multiple = TRUE,
        selected = NULL),
      sliderInput(
        inputId = ns("sel_mod_scatterplot_Alpha"),
        label = "Alpha",
        min = 0, max = 1,
        value = .8,
        step = .1)),
    mainPanel(
      plotly::plotlyOutput(ns("mod_scatterplot_plot")
      )
    )
  )
}
    
#' scatterplot Server Functions
#'
#' @noRd 
mod_scatterplot_server <- function(id,
                                   datos, 
                                   X_vars, X_default = NULL,
                                   Y_vars, Y_default = NULL,
                                   color_vars = NULL, color_default = NA,
                                   size_vars = NULL, size_default = NULL,
                                   filter_var = NULL,
                                   min_x = NULL, max_x = NULL,
                                   label_var = "site"){
  
  moduleServer( id, function(input, output, session){

    # browser()
    ns <- session$ns
    req(datos)
    browser()
    if (nrow(datos) != 0){
      
      browser()
      updateSelectInput(
        session,
        inputId = "sel_mod_scatterplot_X",
        choices =  X_vars,
        selected = X_default)
      updateSelectInput(
        session,
        inputId = "sel_mod_scatterplot_Y",
        choices =  Y_vars,
        selected = Y_default)
      updateSelectInput(
        session,
        inputId = "sel_mod_scatterplot_Color",
        choices =  c("NONE", color_vars),
        selected = ifelse(!is.na(color_default), color_default, "NONE"))
      updateSelectInput(
        session,
        inputId = "sel_mod_scatterplot_Size",
        choices =  c("NONE", size_vars),
        selected = "NONE")
      updateSelectInput(
        session,
        inputId = "sel_mod_scatterplot_Filter",
        choices =  c("NONE", filter_var),
        selected = "NONE")
      
      x_lims <- reactiveValues()
      x_lims$min <- min_x
      x_lims$max <- max_x
      
      datos$label <- paste0("id: ", datos[[label_var]])
    }
    
    output$helpfile <- renderUI({
      # browser()
      
      shinyhelper::helper(
        shiny_tag = "tabs",
        icon = "question-circle",
        colour = "green",
        type = "markdown",
        content = "help_vars")
      
    })
    
    output$mod_scatterplot_plot <- plotly::renderPlotly({
      
      if (nrow(datos) != 0){
        
        require(ggplot2)
        require(plotly)
        
        req(datos)
        req(input$sel_mod_scatterplot_X)
        req(input$sel_mod_scatterplot_Y)
        
        req(input$sel_mod_scatterplot_Alpha)

        
        
        if (input$sel_mod_scatterplot_Filter != "NONE"){
          datos <- datos %>% 
            mutate(site = as.character(site)) %>% 
            filter(site %in% input$sel_mod_scatterplot_Filter)}
        
        dyn_aes <- aes_(x = as.name(input$sel_mod_scatterplot_X),
                        y = as.name(input$sel_mod_scatterplot_Y))
        if (input$sel_mod_scatterplot_Color != "NONE")
          dyn_aes <- modifyList(
            dyn_aes, aes_(color = as.name(input$sel_mod_scatterplot_Color)))
        if (input$sel_mod_scatterplot_Size != "NONE")
          dyn_aes <- modifyList(
            dyn_aes, aes_(size = as.name(input$sel_mod_scatterplot_Size)))
        # browser()
        p <- ggplot(data = datos, mapping = dyn_aes)+
          geom_point(aes(text = label), 
                     alpha = input$sel_mod_scatterplot_Alpha)+
          theme_bw()
        
        # if ()
        #   xlim(x_lims$min, x_lims$max)
        
        ggplotly(p)
      }
    })
  })
}
    
## To be copied in the UI
# mod_scatterplot_ui("scatterplot_ui_1")
    
## To be copied in the server
# mod_scatterplot_server("scatterplot_ui_1")
