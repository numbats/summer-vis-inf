library(shiny)
library(shinydashboard)
library(googlesheets4)
# Load survey questions
source("questions.R")

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(
    id="tabs",
    menuItem("About you", tabName = "About_you", icon = icon("id-card")),
    menuItem("Questions", tabName = "Questions", icon = icon("images")),
    menuItem("Thank you", tabName = "Thank_you", icon = icon("smile-wink"))
  ),
  hr(),
  conditionalPanel(
    condition = "input.tabs == 'Questions'",
    fluidRow(
      column(width = 1),
      column(width = 12,
             h4("Questions:"),
             hr(),
             questions$scene,
             div(
               uiOutput("ui_btn_next")
             ),
             status = "info")
    )
  )

)

shinyUI(
  dashboardPage(
    dashboardHeader(
      title = "Visual Inference Study : Reading Plots",
      tags$li(class = "dropdown", actionLink("btn_export", span(icon("save"), "Submit Survey")))
    ),
    sidebar,
    dashboardBody(
      tabItems(
        tabItem(tabName = "About_you",
                box(
                  title = "Demographics",
                  questions$demographics,
                  div(uiOutput("ui_d_save")),
                  width = 14,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
        ),

        tabItem(tabName = "Questions",
                includeCSS("www/taipan.css"),
                includeScript("www/img_size.js"),
                box(
                  title = textOutput("out_img_info"),
                  div(class = "taipan_image_div",
                      imageOutput("out_img",
                                  inline = T)),
                  width = NULL,
                  status = "primary",
                  solidHeader = TRUE,
                  collapsible = F
                )),

        tabItem(tabName = "Thank_you",
                box(
                  title = "Thank you",
                  div(
                    uiOutput("validation")),
                  width = 12,
                  status = "info",
                  solidHeader = TRUE,
                  collapsible = TRUE
                )
        )

      )
    )
  )
)
