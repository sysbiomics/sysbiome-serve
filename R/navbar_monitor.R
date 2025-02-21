# # Shiny module for viewing all folders inside a specific projects


# #' convert vector of objects to data frame
# #' @importFrom DT DTOutput
# ui_monitor <- function(id = "MONITOR_UI") {
#     ns <- NS(id)

#     # Create helloworld message in h1
#     fluidPage(
#         # includeCSS("www/icons.css"),
#         DTOutput(ns('tbl'))
#     )
# }

# #' @import data.table
# #' @importFrom DT renderDT JS
# sv_monitor <- function(id = "MONITOR_UI", tower) {
#     moduleServer(
#         id,
#         function(input, output, session) {
#             # Logging the session
#             log_info("Going into monitor mode")
#             dat <- tower$get_projects_mock()

#             # output$tbl <- renderTable(
#             #     {
#             #         checkBoxColumns <- paste0('<input type="checkbox" name="row', dat$launchId, '" value="', dat$launchId, '">', "")
#             #         userColumns <- paste(icon("user", "fa-solid"), dat$userName)
#             #         dat$status <- case_when(
#             #             dat$status == "SUCCEEDED" ~ paste(icon("circle-check", "green-icon"), dat$status),
#             #             dat$status == "FAILED" ~ paste(icon("circle-xmark", "red-icon"), dat$status),
#             #             TRUE ~ paste(icon("circle", "yellow-icon"), dat$status)
#             #         )
#             #         dat$userName <- userColumns
#             #         dat <- dat %>% mutate(launchId = checkBoxColumns, .before = 1)
#             #         dat
#             #     },
#             #     sanitize.text.function = function(x) x,
#             #     rownames = FALSE
#             # )

#             output$tbl <- renderDT(
#                 {
#                     checkBoxColumns <- paste0('<input type="checkbox" name="row', dat$launchId, '" value="', dat$launchId, '">', "")
#                     userColumns <- paste(icon("user", "fa-solid"), dat$userName)
#                     # If status is SUCCEEDED then use green, else use red.
#                     dat$status <- case_when(
#                         dat$status == "SUCCEEDED" ~ paste(icon("circle-check", "green-icon"), dat$status),
#                         dat$status == "FAILED" ~ paste(icon("circle-xmark", "red-icon"), dat$status),
#                         TRUE ~ paste(icon("circle", ".yellow-icon"), dat$status)
#                     )
#                     # Replace user column with icon
#                     dat$userName <- userColumns
#                     # Add checkBox
#                     dat <- dat %>% mutate(launchId = checkBoxColumns, .before = 1)
#                     dat
#                 },
#                 rownames = FALSE,
#                 escape = FALSE,
#                 options = list(
#                     pageLength = 20,
#                     info = FALSE, paging = FALSE, dom = "Bfrtip",
#                     buttons = list("copy", "csv", "excel", "pdf"),
#                     search = list(regex = TRUE, caseInsensitive = FALSE),
#                     headerCallback = DT::JS(
#                         "function(thead, data, start, end, display){",
#                         "  $(thead).remove();",
#                         "}"
#                     ) # removed the header
#                     # buttons = c('copy', 'csv', 'excel', 'pdf')
#                 )
#             )
#         }
#     )
# }


# NAMES <- c("John", "Mary", "Peter", "Paul", "Jane", "Anne", "Lucy", "Mark", "Lisa", "Kate")
# WORKFLOWS <- c("16s", "wgs", "ITS")
# STATUS <- c("SUBMITTED", "RUNNING", "SUCCEEDED", "FAILED", "CANCELLED")

# generate_sample <- function(seed = NA) {
#     if (!is.na(seed)) set.seed(seed)

#     list(
#         launchId = digest::digest(as.character(Sys.time()), algo = "md5", serialize = FALSE),
#         userName = sample(NAMES, 1, replace = T),
#         status = sample(STATUS, 1, replace = T),
#         start = generate_time() # Random number from 0 to 20,
#     )
# }

# # A function that generate a random time from 50 seconds to 2 hours.
# # Generate time in a format of Feb 20, 2021 12:00:00
# generate_time <- function() {
#     a_time <- sample(c(50:7200), 1)
#     format(Sys.time() - a_time, "%b %d, %Y %H:%M:%S")
# }
