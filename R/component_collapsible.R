
# Define the reusable collapse component
#' @import shiny
collapse_compo <- function(...) {
    tagList(
        # CSS for styling
        tags$style(HTML("
      .box {
        margin-bottom: 10px;
        padding: 0;
      }
      .box-header {
        cursor: pointer;
        background-color: #f7f7f7;
        padding: 10px;
        font-weight: bold;
        border-bottom: 1px solid #ddd;
        color: #333; /* Default color */
        transition: color 0.3s ease;
      }
      .box-header.active {
        color: #007BFF; /* Highlight color when active */
      }
      .box-header::before {
         content: '►';
         display: inline-block;
         margin-right: 8px;
         transition: transform 0.2s;
      }
     .box-header.active::before {
        transform: rotate(90deg);
      }
      .box-content {
        display: none;
        padding: 10px;
      }
    ")),

        # JavaScript for toggle functionality
        tags$script(HTML("
      $(document).on('click', '.box-header', function() {
        const header = $(this);
        $(this).next('.box-content').slideToggle();
        header.toggleClass('active'); // Toggle active class for color change
      });
    ")),

        # Create the collapsible boxes with the passed content and headers
        fluidRow(
            lapply(list(...), function(box) {
                div(
                    class = "box",
                    div(class = "box-header", box$header),
                    div(class = "box-content", box$content)
                )
            })
        )
    )
}

# #
# # Usage
# #
# library(shiny)
# # Example Shiny app using the collapse_compo
# ui <- fluidPage(
#     collapse_compo(
#         list(header = "Box 1 Header", content = h1("Content of Box 1")),
#         list(header = "Box 2 Header", content = h2("Content of Box 2")),
#         list(header = "Box 3 Header", content = h3("Content of Box 3"))
#     )
# )

# server <- function(input, output, session) {}

# shinyApp(ui, server)
