library(shiny)
library(shinydashboard)
library(googlesheets4)
library(googledrive)
library(rsconnect)
library(purrr)
library(agricolae) # used for Latin Square Design
questions <- readRDS("data/questions.Rds")


getInputID <- function(input){
  if(!inherits(input, "shiny.tag")){
    return()
  }
  c(
    if(!is.null(input$attribs$id)){list(list(id=input$attribs$id, type = input$name))}else{NULL},
    do.call("c", map(input$children, getInputID))
  )
}

# Define server logic ---
shinyServer(
  function(input, output, session){

    # check connection to google sheet

    sheet <- tryCatch({

      gs4_auth(
        cache = ".secrets",
        email = "abab0012@student.monash.edu",
        token = "data/authentication.rds"
      )
      #gs4_auth(email = "kjin7@student.monash.edu",
               #scopes = "https://www.googleapis.com/auth/spreadsheets")
      sheet <- gs4_get("1JcmIB5dOi7qkArfGawTkOIVFyKMOU-xxHZRuV_7Kzlw")
    }, error = function(e){
      message("Access has not been granted, please try again in 5 minutes.")
      return(NULL)
    })

    # unique identifier function
    create_unique_id <- function(char_len = 7){
      set.seed(Sys.time())

      pool <- c(letters, LETTERS, 0:9)

      this_res <- paste0(sample(pool, char_len, replace = T), collapse = "")
      this_res
    }

    identifier <- create_unique_id()



    seed=sample(1000:2000,1)
    set.seed(seed)
    repl <- sample(1:5,1)

    # load survey image
    image_list <- c(
                    paste0("www/images/sleepstudy/v", "1",repl,  "_1" ,".png"),
                    paste0("www/images/sleepstudy/v", "2",repl,  "_2" ,".png"),
                    paste0("www/images/sleepstudy/v", "1",repl, "_3",  ".png"),
                    paste0("www/images/sleepstudy/v", "2",repl, "_4", ".png"),
                    paste0("www/images/sleepstudy/v", "2",repl,  "_1" ,".png"),
                    paste0("www/images/sleepstudy/v", "1",repl,  "_2" ,".png"),
                    paste0("www/images/sleepstudy/v", "2",repl, "_3",  ".png"),
                    paste0("www/images/sleepstudy/v", "1",repl, "_4", ".png"))

    #image.list <- list.files(paste0("www/images"), full.names = T)
    image_list <- sample(image_list, length(image_list))

    v <- reactiveValues(
      imageNum = 1,
      responses = list(),
      start.time = Sys.time(),
      finish.time = Sys.time(),
      plot.order = 1,
      submitted = F
    )

    current_img <- reactive({
      image_list[v$imageNum]
    })

    output$out_img <- renderImage({
      list(src = current_img(), id = "taipan_current_img")
    }, deleteFile = F)

    sceneInputs <- getInputID(questions$scene)
    demographicsInputs <- getInputID(questions$demographics)

    scene_vals <- reactive({
      vals <- map(sceneInputs, function(id){input[[id$id]]})
      names(vals) <- map_chr(sceneInputs, "id")
      vals
    })

    demographic_vals <- reactive({
      vals <- map(demographicsInputs, function(id){input[[id$id]]})
      names(vals) <- map_chr(demographicsInputs, "id")
      vals
    })

    output$ui_d_save <- renderUI({
      actionLink(
        "btn_saveInfo",
        box(
          "Save Info",
          width = 12,
          background = "purple"
        )
      )
    })

    output$ui_btn_next <- renderUI({
      if (v$imageNum != length(image_list)) {
        actionLink(
          "btn_next",
          box(
            "Next Image",
            width = 12,
            background = "green"
          )
        )
      }
      else {
        div(
          p("Thank you for your participation"),
          p("Please click the SUBMIT botton at the top of the window.")
        )
      }
    })

    # update the scene values when image changes
    observeEvent(current_img(), {
      updateSelectInput(session = session, "choice", selected = 0)
      updateSelectInput(session = session, "reason", selected = 0)
      updateSelectInput(session = session, "certainty", selected = 3)
    })

    observeEvent(scene_vals(), {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
    })

    observeEvent(input$btn_next, {
      v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
      v$responses[[basename(current_img())]][["scene"]]$plot.order <- match(current_img(), image_list)
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      v$responses[[basename(current_img())]][["demographic"]]$identifier <- identifier

      # time calculations
      v$responses[[basename(current_img())]][["scene"]]$time_taken <- (Sys.time() - v$start.time)
      v$responses[[basename(current_img())]][["scene"]]$start.time <- v$start.time
      v$responses[[basename(current_img())]][["scene"]]$end.time <- Sys.time()
      # update time for next plot
      v$start.time <- Sys.time()

      v$imageNum <- pmin(length(image_list), v$imageNum +1)
    })

    output$out_img_info <- renderText({
      sprintf("Image: (%i/%i)",
              v$imageNum,
              length(image_list))
    })

    observeEvent(input$btn_saveInfo, {
      v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
      if (input$consent == "Consented"){
        # Switch to the survey tab
        updateTabItems(session = session, inputId = "tabs", selected = "Questions")
        v$start.time <- Sys.time()
      } else {
        showNotification(h3("Consent must be given before you can proceed to questions."),
                         type = "error", duration = 1)
      }
    })

    # validation
    output$validation <- renderUI({
      box(
        div(
          p("Thank you for your participation!"),
          p("Now you can close the page.")
        ),
        width = 12,
        background = "orange"
      )
    })

    # change this to upload rows to survey google spreadsheet
    observeEvent(input$btn_export, {
      if (v$imageNum < 12) {
        showNotification(h3("Please complete the survey before submitting."),
                         type = "message", duration = 3, closeButton = T)
      } else {
        # switch to thank you tab
        updateTabItems(session = session, inputId = "tabs", selected = "Thank_you")

        v$responses[[basename(current_img())]][["scene"]] <- scene_vals()
        v$responses[[basename(current_img())]][["scene"]]$plot.order <- match(current_img(), image_list)
        v$responses[[basename(current_img())]][["demographic"]] <- demographic_vals()
        v$responses[[basename(current_img())]][["demographic"]]$identifier <- identifier
        # time calculations
        v$responses[[basename(current_img())]][["scene"]]$time_taken <- (Sys.time() - v$start.time)
        v$responses[[basename(current_img())]][["scene"]]$start.time <- v$start.time
        v$responses[[basename(current_img())]][["scene"]]$end.time <- Sys.time()

        out <- ""
        out <- suppressWarnings(
          v$responses %>%
            imap_dfr(
              function(img, image_name){
                scene_vals <- img$scene %>%
                  map(paste0, collapse = ", ")
                demographic_vals <- img$demographic %>%
                  map(paste0, collapse = ", ")

                df <- as.data.frame(c(demographic_vals,
                                      image_name = image_name,
                                      scene_vals))
                return(df)
              }
            )
        )
        sheet_append(ss = sheet, data = out, sheet = 1)

        # change submitted to true
        v$submitted <-  T
      }
    })

  }
)
