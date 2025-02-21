# #' homepage
# #' @noRd
# #' @
# homePage <- fluidPage(
#     tags$head(
#         tags$style(HTML("
#       .header {
#         background-color: #003E4F; /* Dark greenish background */
#         color: white;             /* White text */
#         text-align: center;       /* Center alignment */
#         padding: 20px;            /* Padding around text */
#         font-size: 24px;          /* Font size */
#         font-family: 'Arial', sans-serif; /* Font family */
#       }
#       .header a {
#         color: #00BFFF;           /* Link color (sky blue) */
#         text-decoration: none;    /* Remove underline */
#         font-weight: bold;        /* Bold links */
#       }
#       .header a:hover {
#         text-decoration: underline; /* Underline on hover */
#       }
#     "))
#     ),
#     div(
#         class = "header",
#         HTML("<div>SYSBIOMICS Platform</div>"),
#         HTML("<div>Maintained by <a href='mailto:preecha.pa@ku.th'>Preecha Patumcharoenpol</a> • <a href='https://github.com/sysbiomics/SYSMIOME/' target='_blank'>Code on GitHub</a> ❤</div>"),
#         HTML("<div>Citation: <a href='doi'>doi...</a></div>")
#     ),
#     # Rest of your app content
# )

#' homepage
#' @noRd
#' @
homePage <- fluidPage(
 tags$head(
        tags$style(HTML("
            .header {
                background-color: #8B0000; /* Dark red */
                color: white;
                text-align: center;
                padding: 30px;
                font-size: 30px; /* Increased font size */
                font-family: 'Arial', sans-serif;
            }
            .header a {
                color: #FF4500; /* Orange */
                text-decoration: none;
                font-weight: bold;
            }
            .header a:hover {
                text-decoration: underline;
            }
            .welcome-section {
                text-align: center;
                margin-top: 20px;
                padding: 30px;
                font-size: 22px; /* Increased font size */
                max-width: 900px;
                margin-left: auto;
                margin-right: auto;
            }
            .methods-container {
                display: flex;
                justify-content: center;
                gap: 30px;
                flex-wrap: wrap;
                padding: 40px;
            }
            .method-card {
                width: 400px; /* Made cards bigger */
                text-align: center;
                border: 1px solid #ddd;
                border-radius: 12px;
                overflow: hidden;
                box-shadow: 3px 3px 12px rgba(0, 0, 0, 0.1);
                background: white;
                transition: transform 0.2s;
                padding-bottom: 20px;
            }
            .method-card:hover {
                transform: scale(1.08);
            }
            .method-card img {
                width: 100%; /* Ensures the image fills the card width */
                height: auto; /* Keeps aspect ratio */
                max-height: 250px; /* Prevents images from becoming too tall */
                object-fit: contain; /* Prevents cropping */
                display: block;
                margin: 0 auto;
            }
            .method-card h4 {
                margin: 20px 0;
                font-size: 22px;
            }
            .method-card .btn {
                margin-top: 10px;
                font-size: 18px; /* Larger buttons */
                padding: 12px 20px;
            }
        "))
    ),
    
    # Header Section
    div(
        class = "header",
        HTML("<div>SYSBIOMICS Toolbox</div>"),
        HTML("<div>Maintained by <a href='mailto:preecha.pa@ku.th'>Preecha Patumcharoenpol</a> • <a href='https://github.com/sysbiomics/SYSMIOME/' target='_blank'>Code on GitHub</a> ❤</div>"),
        HTML("<div>Citation: <a href='doi'>doi...</a></div>")
    ),

    # Welcome Section
    div(
        class = "welcome-section",
        h2("Welcome to the SYSBIOMICS Toolbox"),
        p("SYSBIOMICS is an interactive bioinformatics platform designed for microbiome research. 
           It enables researchers to upload and visualize genomic data efficiently."),
        p("Get started by uploading your dataset or exploring existing research data in the Viewer.")
    ),
    
    # Methods Section
    div(
        class = "methods-container",
        
        # Upload Section
        div(
            class = "method-card",
            img(src = "www/img/upload.png", alt = "Upload Image"),
            h4("Upload"),
            actionButton("go_upload", "See more", class = "btn btn-primary")
        ),
        
        # Viewer Section
        div(
            class = "method-card",
            img(src = "www/img/viewer.png", alt = "Viewer Image"),
            h4("Viewer"),
            actionButton("go_viewer", "See more", class = "btn btn-secondary")
        )
    )
)