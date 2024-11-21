# library(jsonlite)
# library(fs)
# library(cowplot)
# library(ggplot2)

# #
# # Manage upload and new project
# #

# #
# # Public view
# #

# show_projects <- function(){

#   return(list(
#     "demo" = "demo",
#     "PRJNA709129_data" = "PRJNA709129_data",
#     "nodata" = "nodata"
#   ))
# }

# ui_project_selection <- function(id = "ID_SELECTOR_MODULE") {
#   ns <- NS(id)
#   fluidRow(
#     selectInput(
#       ns("project_selector"), "Project Id",
#       show_projects()
#     ),
#     textOutput(ns("project_id")),
#   )
# }

# sv_project_selection <- function(id = "ID_SELECTOR_MODULE", project_id) {

#   moduleServer(id, function(input, output, session) {

#     output$project_id <- renderText({
#       input$project_selector
#     })

#     observeEvent(input$project_selector, {
#       session$userData$project_id <- input$project_selector
#       log_info("session: {session$token}: project selected: {input$project_selector}")
#       newQueryString <- paste0("?project=", input$project_selector)
#       # updateQueryString(newQueryString, "replace")
#       project_id(input$project_selector)
#     })

#     # return(reactive({input$project_selector}))
#   })
# }

# ui_viewer <- function(id = "ID_VIEWER_MODULE"){
#   ns <- NS(id)
#   fluidPage(
#     textOutput(ns("hidden_tab_text"))
#   )
# }

# sv_viewer <- function(id = "ID_VIEWER_MODULE", project_id){

#     moduleServer(
#         id,
#         function(input, output, session) {

#             output$hidden_tab_text <- renderText({

#                 # log_info("Attemp to render viewr for project: {proj_id}")
#                 if (checkProject(project_id())){
#                     paste0("Project:", project_id())
#                 } else {
#                     paste0("Project:", project_id(), " does not exist")
#                 }
#             })

#         }
#     )
# }