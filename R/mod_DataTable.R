#' DataTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DataTable_ui <- function(id){
  ns <- NS(id)
  mainPanel(
    hr(),
    DT::dataTableOutput(ns("mod_DataTable_data")) # %>% withSpinner()
  )
}
    
#' DataTables Server Functions
#'
#' @noRd 
mod_DataTable_server <- function(id,
                                 datos,
                                 visible_vars = NULL,
                                 formatR = NULL,
                                 formatP = NULL){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    require(writexl)
    
    # browser()
    
    output$mod_DataTable_data <- DT::renderDataTable({
      
      invisible_ids <- 
        if(is.null(visible_vars)) {
          NA
        } else {
          # -1 supongo que es porque no muestro los rownames
          which(!(names(datos) %in% visible_vars)) - 1
        }
      
      dt <- DT::datatable(datos, filter = 'top', 
                          rownames = FALSE,
                          extensions = c('Buttons', 'ColReorder', 'FixedHeader'),
                          options = list(
                            colReorder = TRUE,
                            dom = 'Bfrtip',
                            buttons = c('copy', 'csv', 'excel', 'pdf', 'colvis'),
                            pageLength = min(nrow(datos), 500), 
                            fixedHeader = TRUE,
                            columnDefs = 
                              list(list(visible=FALSE, targets=invisible_ids))),
                          class = 'cell-border stripe') 
      # browser()
      if (!is.null(formatR)){
        for(f in 1:length(formatR)){
          dt <- DT::formatRound(
            table = dt,
            columns = formatR[[f]]$columns, 
            digits = formatR[[f]]$digits)
        }
      }
      
      if (!is.null(formatP)){
        for(f in 1:length(formatP)){
          dt <- DT::formatPercentage(
            table = dt,
            columns = formatP[[f]]$columns, 
            digits = formatP[[f]]$digits)
        }
      }
      
      dt
      
    })
 
  })
}
    
## To be copied in the UI
# mod_DataTables_ui("DataTables_ui_1")
    
## To be copied in the server
# mod_DataTables_server("DataTables_ui_1")
