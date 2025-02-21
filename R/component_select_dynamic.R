PLACE_HOLDER_CHOICE <- "Select an option"


#' @param id, project_id
#' @import shiny
#' @noRd

# Module for Select Input with Dynamic Choices
ui_select_metadata <- function(id) {
    ns <- NS(id)
    selectInput(
        inputId = ns("select_metadata"),
        label = "", # Label will be set externally
        choices = NULL,
        multiple = FALSE,
        selectize = FALSE
    )
}

sv_select_metadata <- function(id, project_obj, label_text) {
    moduleServer(
        id,
        function(input, output, session) {

            selected_value <- reactiveVal(PLACE_HOLDER_CHOICE)

            observeEvent(project_obj(), {
                metadata_choices <- setdiff(colnames(project_obj()$get_filtered_metadata()$meta), "ID_sample")
                metadata_choices <- c(PLACE_HOLDER_CHOICE, metadata_choices)

                updateSelectInput(
                    session = session,
                    inputId = "select_metadata",
                    label = label_text,
                    choices = metadata_choices,
                    selected = selected_value() # Use current value
                )
            })

            observeEvent(input$select_metadata, {
                selected_value(input$select_metadata)
            })

            selected_value # Return the reactiveVal
        }
    )
}


# Module for Select Input with Dynamic Choices (Taxa Version)
ui_select_taxa <- function(id) {
    ns <- NS(id)
    selectInput(
        inputId = ns("select_taxa_level"),
        label = "", # Label will be set externally
        choices = NULL,
        multiple = FALSE,
        selectize = FALSE
    )
}

sv_select_taxa <- function(id, project_obj, label_text) {
    moduleServer(
        id,
        function(input, output, session) {

            selected_value <- reactiveVal(PLACE_HOLDER_CHOICE)

            observeEvent(project_obj(), {
                taxa_names <- c(PLACE_HOLDER_CHOICE, project_obj()$get_taxa_name())
                updateSelectInput(
                    session = session,
                    inputId = "select_taxa_level",
                    label = label_text, # Set the label here
                    choices = taxa_names,
                    selected = selected_value()
                )
            })

            observeEvent(input$select_taxa_level, {
                selected_value(input$select_taxa_level)
            })

            selected_value # Return the reactiveVal
        }
    )
}