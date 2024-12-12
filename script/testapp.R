library(shiny)
library(bslib)
library(readxl)
library(writexl)
library(here)
library(bsicons)
library(shinyWidgets)
library(dplyr)
library(shinyjs)
library(DT)


infos <- read_excel(here("data", "infos.xlsx"))

pop <- read_excel(here("data", "adjusted.xlsx"), sheet = 4, skip = 4) |> janitor::clean_names()


pop <- pop[, c("province", "district", "couverture_ecv")]

ui <- page_sidebar(
  useShinyjs(),

  # Shiny example
  theme = bs_theme(
    bootswatch = "litera",
    heading_font = font_google("Poppins"),
    base_font = font_google("IBM Plex Serif"),
  ),
  sidebar = sidebar(
    h5("Telechargez le fichier excel à remplir"),
    downloadBttn("info", "Telechargez le fichier à remplir", icon = icon("download")),
    h5("Lire le fichier rempli dans l'application"),
    fileInput("info_update", "Lire le fichier rempli", multiple = F, accept = ".xlsx", placeholder = "No file"),
    h5("Cliquez sur le boutton si vous avez rempli les données de l'ECV"),
    prettySwitch(inputId = "ecv", label = "", fill = TRUE, status = "success", value = FALSE, bigger = TRUE),
    h5("Lancez l'ajustement en appuyant sur le boutton"),
    input_task_button("go", label = "Ajuster", icon = icon("play"), type = "secondary", label_busy = "Ajustement des population...")
  ),
  card(
    card_header(
      h2("Tableau des populations ajustées"),
      downloadBttn("adjusted", "Telechargez les populations ajustées", icon = icon("download"))
    ),
    card_body(
      DTOutput("tab_adjusted")
    )
  )
)







server <- function(input, output, session) {
  output$info <- downloadHandler(
    filename = function() {
      paste0("infos.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(infos, file)
    }
  )


  data <- reactiveValues(infos = NULL, merged = NULL)

  observeEvent(input$go, {
    # Print to console for debugging
    print("Go button pressed")#
    if (is.null(input$info_update)) {
      show_alert(
        title = "Pas de fichier",
        text = "Veuillez lire le fichier excel",
        type = "warning"
      )
      return()
    }

    req(input$info_update)

    tryCatch({
      data$infos <- readxl::read_excel(input$info_update$datapath)


    if (input$ecv) {
      data$merged <- data$infos
    } else {
      data$infos <- select(data$infos, -couverture_ecv)
      data$merged <- merge(data$infos, pop, by.x = "district", by.y = "district")
    }
      print("Merge completed")
      print(head(data$merged))  # Print first few rows

      },
    error = function(e) {
      print(paste("Error in file processing:", e$message))
  })})


  tab <- reactive({
    req(data$merged)

    print("Starting tab calculation")



    tryCatch({
      var <- c("survivants_adj", "naissances_adj")



      results <-  data$merged |>
        mutate(
          pop_ecv = dtc1_vaccinated / couverture_ecv
        ) |>
        mutate(
          variation = (survivants_routine - pop_ecv) / survivants_routine
        ) |>
        mutate(
          survivants_adj = case_when(

            between(variation, -0.1, 0.1) ~ survivants_routine,
            variation < -0.1 & variation > -0.2 ~ survivants_routine + abs(variation) * survivants_routine,
            variation <= -0.2 ~ survivants_routine + 0.2 * survivants_routine,
            variation > 0.1 & variation < 0.2 ~ survivants_routine - abs(variation) * survivants_routine,
            variation > 0.2 ~ survivants_routine - 0.2 * survivants_routine,
            .default = survivants_routine + abs(variation) * survivants_routine
          )
        ) |>
        mutate(survivants_adj = ifelse(survivants_adj <= dtc1_vaccinated, dtc1_vaccinated, survivants_adj)) |>
        mutate(
          naissances_adj = survivants_adj + (55 / 1000) * survivants_adj
        ) |>
        mutate(
          across(c(survivants_adj, naissances_adj), ~ .x * 1.03, .names = "{var}_annee1")
        ) |>
        mutate(
          across(ends_with("annee1"), ~ .x * 1.03, .names = "{var}_annee2")
        )|>
        filter(!is.na(dtc1_vaccinated))




      return(results)

      }, error = function(e) {
        print(paste("Error in tab calculation:", e$message))
        return(NULL)
      })


  })






  output$adjusted <- downloadHandler(
    filename = function() {
      paste0("pop_ajusted.xlsx")
    },
    content = function(file) {
      writexl::write_xlsx(tab(), file)
    }
  )


  output$tab_adjusted <- renderDT({

    req(tab())
    tab()
  })




}

shinyApp(ui, server)

