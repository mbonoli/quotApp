#' DataTable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_CalendarPlot_ui <- function(id){
  ns <- NS(id)
  plotOutput(ns("mod_calendarPlot"))
}
    
#' DataTables Server Functions
#'
#' @noRd 
mod_CalendarPlot_server <- function(id,
                                    datos,
                                    wdays = NULL,
                                    h = 600, w = 800){
  
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$mod_calendarPlot <- renderPlot({
      
      # browser()
      library(RColorBrewer)
      require(ggplot2)
      # browser()
      
      datos <- datos %>% 
        mutate(wd = lubridate::wday(delivery_date, week_start = 1),
               date = delivery_date)
      
      if (!is.null(wdays)){
        
        
        datos <- datos %>% 
          filter(wd %in% wdays)
      }
      
      mydate <- datos$date
      myfills <- datos$n_trucks 
      mp <- mean(myfills, na.rm = T)
      result <- ggcal::ggcal(mydate, myfills) +
        scale_fill_gradient2(low="green", 
                             mid="white", 
                             high="red", 
                             midpoint=mp)
      result
    }, height = h, width = w)
 
  })
}
    
## To be copied in the UI
# mod_DataTables_ui("DataTables_ui_1")
    
## To be copied in the server
# mod_DataTables_server("DataTables_ui_1")
