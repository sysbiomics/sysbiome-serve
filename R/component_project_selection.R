# In app_ui.R

project_selection_ui <- function(id) {
    ns <- NS(id)

    tagList(
        fluidRow(
            column(
                6,
                selectizeInput(
                    inputId = ns("project_select"),
                    label = "Select or search for a project",
                    choices = NULL,
                    options = list(
                        placeholder = "Type to search for a project",
                        onInitialize = I('function() { this.setValue(""); }')
                    )
                )
            ),
            column(
                6,
                h2(textOutput(ns("current_project")), align = "right")
            )
        ),
    )
}

project_selection_server <- function(id, tower) {
    moduleServer(id, function(input, output, session) {
        # Reactive value for available projects
        available_projects <- reactive({
            tower$project_search()
        })

        # Update selectize input choices
        observe({
            projects <- available_projects()
            updateSelectizeInput(session, "project_select",
                choices = projects,
                selected = "demo", # Set initial selection to "demo"
                server = TRUE
            )
        })

        # Reactive value for selected project, initialize with "demo"
        selected_project <- reactiveVal("demo")

        # Update selected project when input changes
        observeEvent(input$project_select, {
            if (!is.null(input$project_select) && input$project_select != "") {
                showPageSpinner(caption = (div(strong("Loading project"), br(), em("Please wait"))))
                Sys.sleep(2)
                selected_project(input$project_select)
                hidePageSpinner()
            }
        })

        # Display current project
        output$current_project <- renderText({
            paste("Current Project:", selected_project())
        })

        # Return the selected project so it can be used in other parts of the app
        return(selected_project)
    })
}
