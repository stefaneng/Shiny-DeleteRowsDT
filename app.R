library(shiny)

ui <- fluidPage(
  titlePanel("Delete rows"),
  mainPanel(
    selectInput('datasetSelect',
                label = 'Select data',
                choices = c("mtcars", "iris", "faithful")),
    uiOutput('undoUI'),
    DT::dataTableOutput("dtable")
  )
)

server <- function(input, output) {
    rv <- reactiveValues(
        data = NULL,
        deletedRows = NULL,
        deletedRowIndices = list()
    )

    observeEvent(input$datasetSelect, {
        rv$data <- switch (input$datasetSelect,
                            "mtcars" = mtcars,
                            "iris" = iris,
                            "faithful" = faithful
        )

        # Clear the previous deletions
        rv$deletedRows <- NULL
        rv$deletedRowIndices = list()
    })

    observeEvent(input$deletePressed, {
      rowNum <- parseDeleteEvent(input$deletePressed)
      dataRow <- rv$data[rowNum,]

      # Put the deleted row into a data frame so we can undo
      # Last item deleted is in position 1
      rv$deletedRows <- rbind(dataRow, rv$deletedRows)
      rv$deletedRowIndices <- append(rv$deletedRowIndices, rowNum, after = 0)

      # Delete the row from the data frame
      rv$data <- rv$data[-rowNum,]
    })

    observeEvent(input$undo, {
      if(nrow(rv$deletedRows) > 0) {
        row <- rv$deletedRows[1, ]
        rv$data <- addRowAt(rv$data, row, rv$deletedRowIndices[[1]])

        # Remove row
        rv$deletedRows <- rv$deletedRows[-1,]
        # Remove index
        rv$deletedRowIndices <- rv$deletedRowIndices[-1]
      }
    })

    # Disable the undo button if we have not deleted anything
    output$undoUI <- renderUI({
      if(!is.null(rv$deletedRows) && nrow(rv$deletedRows) > 0) {
        actionButton('undo', label = 'Undo delete', icon('undo'))
      } else {
        actionButton('undo', label = 'Undo delete', icon('undo'), disabled = TRUE)
      }
    })

    output$dtable <- DT::renderDataTable(
      # Add the delete button column
      deleteButtonColumn(rv$data, 'delete_button')
    )
}

#' Adds a row at a specified index
#'
#' @param df a data frame
#' @param row a row with the same columns as \code{df}
#' @param i the index we want to add row at.
#' @return the data frame with \code{row} added to \code{df} at index \code{i}
addRowAt <- function(df, row, i) {
  # Slow but easy to understand
  if (i > 1) {
    rbind(df[1:(i - 1), ], row, df[-(1:(i - 1)), ])
  } else {
    rbind(row, df)
  }

}

#' A column of delete buttons for each row in the data frame for the first column
#'
#' @param df data frame
#' @param id id prefix to add to each actionButton. The buttons will be id'd as id_INDEX.
#' @return A DT::datatable with escaping turned off that has the delete buttons in the first column and \code{df} in the other
deleteButtonColumn <- function(df, id, ...) {
  # function to create one action button as string
  f <- function(i) {
    # https://shiny.rstudio.com/articles/communicating-with-js.html
    as.character(actionButton(paste(id, i, sep="_"), label = NULL, icon = icon('trash'),
                              onclick = 'Shiny.setInputValue(\"deletePressed\",  this.id, {priority: "event"})'))
  }

  deleteCol <- unlist(lapply(seq_len(nrow(df)), f))

  # Return a data table
  DT::datatable(cbind(delete = deleteCol, df),
                # Need to disable escaping for html as string to work
                escape = FALSE,
                options = list(
                  # Disable sorting for the delete column
                  columnDefs = list(list(targets = 1, sortable = FALSE))
                ))
}

#' Extracts the row id number from the id string
#' @param idstr the id string formated as id_INDEX
#' @return INDEX from the id string id_INDEX
parseDeleteEvent <- function(idstr) {
  res <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (! is.na(res)) res
}

# Run the application
shinyApp(ui = ui, server = server)
