#' Collapsible Component for Shiny Applications
#'
#' A reusable Shiny UI component that creates collapsible boxes with headers and content.
#' Users can click on the headers to expand or collapse the content within each box.
#'
#' @param ... A list of boxes where each box is a list containing:
#'   - `header`: A character string or HTML element for the box header.
#'   - `content`: A Shiny tag or HTML element for the box content.
#'
#' @details This function generates a set of collapsible boxes styled with custom CSS 
#'   and uses JavaScript for toggle functionality. Clicking a box header toggles 
#'   the visibility of its corresponding content.
#'
#' @return A Shiny tag list representing the collapsible UI component.
#'
#' @examples
#' library(shiny)
#'
#' ui <- fluidPage(
#'     collapse_compo(
#'         list(header = "Box 1 Header", content = h1("Content of Box 1")),
#'         list(header = "Box 2 Header", content = h2("Content of Box 2")),
#'         list(header = "Box 3 Header", content = h3("Content of Box 3"))
#'     )
#' )
#'
#' server <- function(input, output, session) {}
#'
#' shinyApp(ui, server)
#'
#' @import shiny
#' @noRd
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
        const content = $(this).next('.box-content');
        
        // Close all other boxes
        $('.box-header').not(header).removeClass('active');
        $('.box-content').not(content).slideUp();
        
        // Toggle the clicked box
        content.slideToggle();
        header.toggleClass('active');
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