# In app_ui.R
project_selection_ui <- function(id) {
    ns <- NS(id)

    tagList(
        selectizeInput(
            inputId = ns("project_select"),
            label = "Select or search for a project",
            choices = NULL,
            options = list(
                placeholder = "Type to search for a project",
                onInitialize = I('function() { this.setValue(""); }')
            )
        ),
        h3(textOutput(ns("current_project")))

    )
}

project_selection_server <- function(id, tower) {
    moduleServer(id, function(input, output, session) {

        # Reactive value for selected project, initialize with "demo"
        selected_project <- reactiveVal("demo")

        # Update selectize input choices
        observeEvent(tower$num_projects(), {
            projects <- tower$project_search()
            updateSelectizeInput(session, "project_select",
                choices = projects,
                # selected = "demo", # Set initial selection to "demo"
                server = TRUE
            )
        })

        # Update selected project when input changes
        observeEvent(input$project_select, {
            req(input$project_select) # Ensure input is not NULL or empty
            showPageSpinner(caption = (div(strong("Loading project"), br(), em("Please wait"))))
            Sys.sleep(1)
            selected_project(input$project_select)
            hidePageSpinner()
        })

        # Display current project
        output$current_project <- renderText({
            paste("Current Project:", selected_project())
        })

        # Return the selected project so it can be used in other parts of the app
        return(selected_project)
    })
}