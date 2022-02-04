#' DataTables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_DataTables_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' DataTables Server Functions
#'
#' @noRd 
mod_DataTables_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_DataTables_ui("DataTables_ui_1")
    
## To be copied in the server
# mod_DataTables_server("DataTables_ui_1")
