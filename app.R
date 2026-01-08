library(shiny)
library(bslib)

ui = page_sidebar(
    title = "RGamble",
    tags$style(HTML("
        .box.box-solid.box-primary>.box-header {
            color:#fff;
            background:#000000
        }

        .box.box-solid.box-primary {
            border-bottom-color:#000000;
            border-left-color:#000000;
            border-right-color:#000000;
            border-top-color:#000000;
        }

        .box.box-primary>.box-header {
            color:#000000;
            background:#fff
        }

        .box.box-primary {
            border-bottom-color:#000000;
            border-left-color:#000000;
            border-right-color:#000000;
            border-top-color:#000000;
        }

        .skin-red .main-sidebar {
            background-color: #000000;
        }
        "
    )),
    nav_panel(title = "One",p("sixseven")),
    fluidRow(
        actionButton("start",label="Start",width="300"),
        actionButton("start",label="Start",width="300"),
        actionButton("start",label="Start",width="300"),
    ),
    fluidRow(
        imageOutput("testCard")
    )

    
)

server = function(input,output) {

}

shinyApp(ui = ui, server = server)

renderCard = function(cardElement,cardVal) {
    return(renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile <- tempfile(fileext = '.png')

    # Generate the PNG
    png(outfile, width = 400, height = 300)
    hist(rnorm(input$obs), main = "Generated in renderImage()")
    dev.off()

    # Return a list containing the filename
    list(src = outfile,
         contentType = 'image/png',
         width = 400,
         height = 300,
         alt = "This is alternate text")
  }, deleteFile = TRUE))
}