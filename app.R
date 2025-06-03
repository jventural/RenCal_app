# app.R

# 1) Forzar tanto LANG como LC_CTYPE a UTF-8
Sys.setenv(LANG = "es_PE.UTF-8")
Sys.setlocale("LC_CTYPE", "es_PE.UTF-8")

# Verificar si devtools está instalado y cargarlo
if (!require("devtools", quietly = TRUE)) {
  install.packages("devtools")
  library(devtools)
}

# Lista de paquetes necesarios
paquetes <- c(
  "shiny", "shinydashboard", "rvest", "tidyverse",
  "readxl", "stringi", "DT", "curl", "httr2"
)

# Bucle para instalar y cargar cada paquete si es necesario
for (pkg in paquetes) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Carga de librerías
library(shiny)
library(shinydashboard)
library(rvest)
library(tidyverse)
library(readxl)
library(stringi)
library(DT)
library(curl)
library(httr2)
library(dplyr)


# ----------------------------
# Función extraer_tabla() (quita tildes)
# ----------------------------
extraer_tabla <- function(page, texto_seccion) {
  tryCatch({
    raw_table <- page %>%
      html_node(xpath = paste0(
        "//*[contains(text(), '", texto_seccion, "')]/following::table[1]"
      )) %>%
      html_table(fill = TRUE)
    
    if (!is.null(raw_table) && nrow(raw_table) > 0) {
      # Primera fila son nombres de columna
      colnames(raw_table) <- as.character(raw_table[1, ])
      raw_table <- raw_table[-1, ]
      
      # Quitar tildes de nombres de columna
      colnames(raw_table) <- stringi::stri_trans_general(
        colnames(raw_table), "Latin-ASCII"
      )
      
      # Quitar tildes de cada celda de tipo carácter
      raw_table <- raw_table %>%
        mutate(across(
          where(is.character),
          ~ stringi::stri_trans_general(.x, "Latin-ASCII")
        ))
    }
    
    raw_table
  }, error = function(e) {
    data.frame(Mensaje = paste0("No se encontró la tabla para: ", texto_seccion))
  })
}


# -----------------------------------------------
# Función restaurar_tildes() (reemplaza palabras clave sin tilde por su forma correcta)
# -----------------------------------------------
restaurar_tildes <- function(df) {
  # Mapeo: sin tilde -> con tilde
  mapping <- c(
    "Ano"            = "Año",
    "Publicacion"    = "Publicación",
    "Asesoria"       = "Asesoría",
    "Formacion"      = "Formación",
    "Produccion"     = "Producción",
    "Pais"           = "País",
    "Titulo"         = "Título",
    "Tesis"          = "Tesis",           # se deja igual
    "Grado"          = "Grado"            # se deja igual
  )
  
  # 1) Ajustar nombres de columna
  col_names <- names(df)
  for (key in names(mapping)) {
    col_names <- gsub(paste0("\\b", key, "\\b"), mapping[[key]], col_names)
  }
  names(df) <- col_names
  
  # 2) Ajustar valores en columnas de tipo carácter
  df <- df %>%
    mutate(across(where(is.character), ~ {
      text <- .x
      for (key in names(mapping)) {
        text <- gsub(paste0("\\b", key, "\\b"), mapping[[key]], text)
      }
      text
    }))
  
  df
}


# ========================================
# UI: interfaz con shinydashboard
# ========================================
ui <- dashboardPage(
  dashboardHeader(title = "RenCal", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("Información", tabName = "info", icon = icon("info-circle")),
      menuItem("Scraping",    tabName = "scraping", icon = icon("table")),
      menuItem("Producción Científica", tabName = "produccion", icon = icon("file-alt")),
      menuItem("Puntajes",    tabName = "puntajes", icon = icon("calculator")),
      menuItem("Acerca del autor", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side { background-color: #ecf0f5; }
        .skin-blue .main-header .logo { background-color: #3c8dbc; color: white; }
        .skin-blue .main-header .navbar { background-color: #3c8dbc; }
        .box { border-top: 3px solid #3c8dbc; }
        .sidebar-menu > li.active > a { background-color: #3c8dbc; color: white; }
      "))
    ),
    tabItems(
      
      # Pestaña Información RENACYT
      tabItem(tabName = "info",
              fluidRow(
                box(
                  width = 12, title = "Acerca del programa", status = "primary", solidHeader = TRUE,
                  p("RenCal es una calculadora diseñada para determinar los puntajes otorgados por el Registro Nacional Científico, Tecnológico y de Innovación Tecnológica (RENACYT)."),
                  p("Esta herramienta facilita la obtención de la calificación de un investigador o candidato, automatizando los criterios más complejos."),
                  p("El proceso se centra en analizar el nombre de la revista en la que se publicó un artículo para detectar su cuartil y asignar el puntaje correspondiente, combinando información de Scimago y Scielo."),
                  p("Los criterios adicionales, que pueden ser menos complicados, se pueden ingresar manualmente."),
                  p("Además, se incorpora información extraída de la Ficha CTI Vitae del investigador para complementar el análisis mediante técnicas de webscraping.")
                )
              ),
              fluidRow(
                box(
                  width = 12, title = "Normativas RENACYT", status = "primary", solidHeader = TRUE,
                  p("Para obtener la normativa completa haga ",
                    a(href = "http://resoluciones.concytec.gob.pe/subidos/sintesis/RP-090-2021-CONCYTEC-P.pdf",
                      "click aquí.", target = "_blank")),
                  p("A continuación se muestra el Anexo Nº 1:"),
                  imageOutput("image1", height = "750px", width = "950px")
                )
              )
      ),
      
      # Pestaña Scraping
      tabItem(tabName = "scraping",
              box(
                width = 12, title = "Scraping de CTIVITAE", status = "primary", solidHeader = TRUE,
                textInput("url_invest", "URL Investigador", value = ""),
                actionButton("run", "Ejecutar Análisis"),
                br(), br(),
                tabsetPanel(
                  tabPanel("Asesoría",            tableOutput("asesor_table")),
                  tabPanel("Formación Académica", tableOutput("formacion_table")),
                  tabPanel("Producción científica", tableOutput("produccion_table")),
                  tabPanel("Derechos de Propiedad Intelectual", tableOutput("dpi_table"))
                )
              )
      ),
      
      # Pestaña Producción Científica
      tabItem(tabName = "produccion",
              box(
                width = 12, title = "Resumen de Publicaciones", status = "primary", solidHeader = TRUE,
                DT::dataTableOutput("df_final_table"),
                br(),
                h4("Puntaje total de Artículos Científicos:"),
                verbatimTextOutput("total_valor")
              )
      ),
      
      # Pestaña Puntajes
      tabItem(tabName = "puntajes",
              box(
                width = 12, title = "Cálculo de Puntajes RENACYT", status = "primary", solidHeader = TRUE,
                fluidRow(
                  column(6,
                         h4("Grado Académico (Max. 10 puntos)"),
                         verbatimTextOutput("grado_academico")
                  ),
                  column(6,
                         h4("Artículos Científicos"),
                         verbatimTextOutput("articulos_cientificos")
                  )
                ),
                fluidRow(
                  column(6,
                         h4("Registro de Propiedad Intelectual"),
                         verbatimTextOutput("registro_propiedad_calculado")
                  ),
                  column(6,
                         numericInput("libros_capitulos",
                                      "Libros y Capítulos (Max. 10 puntos)",
                                      value = 0, min = 0, max = 10, step = 1
                         )
                  )
                ),
                fluidRow(
                  column(6,
                         selectInput("indice_h",
                                     "Índice H (>=10)",
                                     choices = c("No", "Sí"),
                                     selected = "No"
                         )
                  ),
                  column(6,
                         h4("Asesorías de tesis (Max. 10 puntos)"),
                         verbatimTextOutput("asesoria_tesis")
                  )
                ),
                br(),
                fluidRow(
                  column(6,
                         h4("Puntaje Total RENACYT"),
                         tags$div(
                           style = "font-size: 20px; font-weight: bold; color: #3c8dbc;",
                           textOutput("total_renacyt_puntaje")
                         )
                  ),
                  column(6,
                         h4("Calificación"),
                         tags$div(
                           style = "font-size: 20px; font-weight: bold; color: #3c8dbc;",
                           textOutput("renacyt_calificacion")
                         )
                  )
                )
              )
      ),
      
      # Pestaña Acerca del autor
      tabItem(tabName = "about",
              box(
                width = 12, title = "Acerca del autor", status = "primary", solidHeader = TRUE,
                tags$p("José Ventura-León es Doctor en Psicología y Magister en Psicología Educativa. Actualmente es Docente Investigador a tiempo completo en la UPN."),
                tags$p("Más información en: ", tags$a(href = "https://joseventuraleon.com/", "joseventuraleon.com", target = "_blank")),
                tags$p("Para consultas o reportar errores, escriba a: info@joseventuraleon.com")
              )
      )
    )
  )
)


# ========================================
# SERVER
# ========================================
server <- function(input, output, session) {
  
  # Render de la imagen en Información RENACYT
  output$image1 <- renderImage({
    filename <- normalizePath(file.path("www", "anexo1.png"))
    list(
      src = filename,
      contentType = 'image/png',
      width = 950,
      height = 750,
      alt = "Anexo Nº 1 RENACYT"
    )
  }, deleteFile = FALSE)
  
  # Función para el cálculo de puntaje y calificación RENACYT
  GetPuntajeSum <- function(Grado = 0, Articulos = 0, Patentes = 0, Libros = 0, Asesorias = 0) {
    Grado + Articulos + Patentes + Libros + Asesorias
  }
  
  # Función actualizada para obtener la calificación incluyendo la verificación de producción total
  Getcalificacion <- function(value = 0, IndiceH = "No", prod_total = 0) {
    if (prod_total < 6) {
      "No califica: no tiene 6 puntos en producción total"
    } else if (value == 0) {
      "No califica: Requiere al menos un ítem en Producción"
    } else if (value == 1) {
      "No califica: Estudiantes requieren 9 en producción"
    } else if (value > 1 && value < 6) {
      "No califica: Requiere al menos 6 en producción"
    } else if (value < 10) {
      "No califica: Requiere al menos 10 en puntaje total"
    } else if (value <= 24) {
      "Sí califica: Nivel VII"
    } else if (value <= 34) {
      "Sí califica: Nivel VI"
    } else if (value <= 49) {
      "Sí califica: Nivel V"
    } else if (value <= 69) {
      "Sí califica: Nivel IV"
    } else if (value <= 99) {
      "Sí califica: Nivel III"
    } else if (value <= 159) {
      "Sí califica: Nivel II"
    } else if (value <= 199) {
      "Sí califica: Nivel I"
    } else if (IndiceH == "Sí") {
      "Investigador Distinguido"
    } else {
      "Sí califica: Nivel I"
    }
  }
  
  # Evento principal: al pulsar "Ejecutar Análisis"
  analysisData <- eventReactive(input$run, {
    withProgress(message = "Realizando análisis...", value = 0, {
      
      incProgress(0.1, detail = "Extrayendo datos de CTIVITAE")
      
      url_invest <- ifelse(
        input$url_invest == "",
        "https://ctivitae.concytec.gob.pe/appDirectorioCTI/VerDatosInvestigador.do?id_investigador=74018",
        input$url_invest
      )
      page <- tryCatch({
        read_html(url_invest)
      }, error = function(e) {
        cat("Error al cargar la página:", e$message, "\n")
        return(NULL)
      })
      
      if (!is.null(page)) {
        asesor_raw    <- extraer_tabla(page, "Experiencia como Asesor de Tesis")
        formacion_raw <- extraer_tabla(page, "Formación Académica (Fuente: SUNEDU)")
        produccion_raw <- extraer_tabla(page, "Producción científica")
        dpi_raw        <- extraer_tabla(page, "Derechos de Propiedad Intelectual")
        
        # -------------------------
        # Restaurar tildes en tablas
        # -------------------------
        asesor    <- if (!is.null(asesor_raw))    restaurar_tildes(asesor_raw)    else asesor_raw
        formacion <- if (!is.null(formacion_raw)) restaurar_tildes(formacion_raw) else formacion_raw
        
        # Para producción, primero quitamos tildes en raw; luego restauramos para mostrar
        produccion <- if (!is.null(produccion_raw)) {
          restaurar_tildes(produccion_raw)
        } else {
          produccion_raw
        }
        
        dpi <- if (!is.null(dpi_raw)) restaurar_tildes(dpi_raw) else dpi_raw
        
        # Procesar DPI para calcular puntaje
        registro_propiedad_calculado <- 0
        if (!is.null(dpi) && "Tipo de PI" %in% names(dpi)) {
          dpi <- dpi %>%
            mutate(Puntuacion = case_when(
              `Tipo de PI` %in% c(
                "Patente de invención", "Certificado de Obtentor",
                "Paquete tecnológico", "Registro de certificado de obtentor"
              ) ~ 3L,
              `Tipo de PI` %in% c(
                "Patente de modelo de utilidad",
                "Certificado de derecho de autor por software"
              ) ~ 1L,
              TRUE ~ 0L
            ))
          
          registro_propiedad_calculado <- sum(dpi$Puntuacion, na.rm = TRUE)
        }
        
        # Imprimir resultados en consola
        cat("Tabla de Asesoría:\n");    print(asesor)
        cat("\nTabla de Formación Académica:\n"); print(formacion)
        cat("\nTabla de Producción científica:\n"); print(produccion)
        cat("\nTabla de Derechos de Propiedad Intelectual:\n"); print(dpi)
      } else {
        asesor <- data.frame(Mensaje = "No se pudo cargar la página.")
        formacion <- asesor
        produccion <- asesor
        dpi <- asesor
        registro_propiedad_calculado <- 0
      }
      
      incProgress(0.2, detail = "Leyendo archivos Excel")
      df_scopus   <- read_excel("df_scopus.xlsx")
      Scielo_Data <- read_excel("Scielo_Data.xlsx")
      
      incProgress(0.2, detail = "Procesando y normalizando datos")
      # Para normalizar, quitamos tildes de "Revista" antes de unir
      produccion_norm <- produccion %>%
        mutate(Revista_norm = tolower(stri_trans_general(Revista, "Latin-ASCII")))
      
      df_scopus_norm <- df_scopus %>%
        mutate(Revista_norm = tolower(stri_trans_general(Revista, "Latin-ASCII")))
      
      data_joined <- produccion_norm %>%
        left_join(df_scopus_norm, by = "Revista_norm", relationship = "many-to-many") %>%
        filter(!( `Tipo Producción` %in% c(
          "DoctoralThesis", "MasterThesis", "Note",
          "Editorial", "Letter", "Journal - Meeting Abstract"
        ))) %>%
        na.omit()
      
      resumen <- data_joined %>%
        select(
          Revista_norm,
          `Año de Producción`,
          `Título`,
          `Cuartil de ScimagoJR o JCR*`,
          Cuartil, Valor
        ) %>%
        distinct(`Título`, .keep_all = TRUE)
      
      data_joined2 <- resumen %>%
        mutate(
          AnioProd = as.numeric(`Año de Producción`),
          join_year = case_when(
            AnioProd %in% c(2024, 2025) ~ 2024,
            TRUE                       ~ AnioProd
          )
        ) %>%
        left_join(
          df_scopus_norm %>% rename(join_year = year),
          by = c("Revista_norm", "join_year"),
          relationship = "many-to-many"
        )
      
      df_final <- data_joined2 %>%
        select(
          Revista_norm,
          `Año de Producción`,
          `Título`,
          `Cuartil de ScimagoJR o JCR*`,
          Cuartil.y, Valor.y
        ) %>%
        distinct(`Título`, .keep_all = TRUE)
      
      Scielo_Data <- Scielo_Data %>%
        mutate(
          Revista = tolower(Revista),
          Revista = gsub("[[:punct:]]", "", Revista),
          Revista = trimws(Revista)
        )
      
      scielo_counts <- Scielo_Data %>%
        group_by(Revista) %>%
        summarise(n_matches = n(), .groups = "drop")
      
      df_final <- df_final %>%
        left_join(scielo_counts, by = c("Revista_norm" = "Revista")) %>%
        mutate(
          n_matches = if_else(is.na(n_matches), 0L, n_matches),
          Valor.y = if_else(Cuartil.y == "No Cuartil", pmin(n_matches, 10L), Valor.y)
        ) %>%
        select(-n_matches) %>%
        rename(Cuartil = Cuartil.y, Value = Valor.y)
      
      incProgress(0.2, detail = "Calculando puntajes")
      total_suma_valor <- sum(df_final$Value, na.rm = TRUE)
      
      formacion_scores <- formacion %>%
        mutate(score = case_when(
          str_detect(Grado, regex("DOCTOR", ignore_case = TRUE))   ~ 10,
          str_detect(Grado, regex("MAGISTER", ignore_case = TRUE)) ~ 6,
          str_detect(Grado, regex("LICENCIADO", ignore_case = TRUE)) ~ 4,
          str_detect(Grado, regex("BACHILLER", ignore_case = TRUE)) ~ 2,
          str_detect(Grado, regex("CONSTANCIA DE MATRICULA", ignore_case = TRUE)) ~ 1,
          TRUE ~ 0
        ))
      
      puntaje_final <- formacion_scores %>%
        summarise(puntaje = max(score, na.rm = TRUE)) %>%
        pull(puntaje)
      
      puntaje_total2 <- asesor %>%
        mutate(
          score = case_when(
            str_detect(Tesis, regex("Doctorado", ignore_case = TRUE)) ~ 2,
            str_detect(Tesis, regex("Magister", ignore_case = TRUE))  ~ 1,
            str_detect(Tesis, regex("Bachiller|Título Profesional|Licenciado / Título", ignore_case = TRUE)) ~ 0.5,
            TRUE ~ 0
          )
        ) %>%
        summarise(total = sum(score, na.rm = TRUE)) %>%
        mutate(total = if_else(total > 10, 10, total)) %>%
        pull(total)
      
      incProgress(0.1, detail = "Finalizando análisis")
      list(
        asesor            = asesor,
        formacion         = formacion,
        produccion        = produccion,
        dpi               = dpi,
        registro_propiedad_calculado = registro_propiedad_calculado,
        df_final          = df_final,
        total_suma_valor  = total_suma_valor,
        puntaje_formacion = puntaje_final,
        puntaje_asesor    = puntaje_total2
      )
    })
  })
  
  # Reactivo para el puntaje total RENACYT
  total_renacyt_puntaje <- reactive({
    req(analysisData())
    GetPuntajeSum(
      Grado     = analysisData()$puntaje_formacion,
      Articulos = analysisData()$total_suma_valor,
      Patentes  = analysisData()$registro_propiedad_calculado,
      Libros    = input$libros_capitulos,
      Asesorias = analysisData()$puntaje_asesor
    )
  })
  
  # Reactivo para calcular la producción total
  production_total <- reactive({
    req(analysisData())
    analysisData()$total_suma_valor +
      analysisData()$registro_propiedad_calculado +
      input$libros_capitulos
  })
  
  # Reactivo para la calificación RENACYT
  renacyt_calificacion <- reactive({
    req(total_renacyt_puntaje(), production_total())
    Getcalificacion(
      value       = total_renacyt_puntaje(),
      IndiceH     = input$indice_h,
      prod_total  = production_total()
    )
  })
  
  # -----------------------------
  # Salidas pestaña "Scraping"
  # -----------------------------
  output$asesor_table <- renderTable({
    req(analysisData())
    analysisData()$asesor
  })
  
  output$formacion_table <- renderTable({
    req(analysisData())
    analysisData()$formacion
  })
  
  output$produccion_table <- renderTable({
    req(analysisData())
    analysisData()$produccion
  })
  
  output$dpi_table <- renderTable({
    req(analysisData())
    analysisData()$dpi
  })
  
  # -----------------------------
  # Salidas pestaña "Producción Científica"
  # -----------------------------
  output$df_final_table <- DT::renderDataTable({
    req(analysisData())
    # Antes de mostrar, restaurar tildes en nombres de columna y valores
    restaurar_tildes(analysisData()$df_final)
  })
  
  output$total_valor <- renderPrint({
    req(analysisData())
    analysisData()$total_suma_valor
  })
  
  # -----------------------------
  # Salidas pestaña "Puntajes"
  # -----------------------------
  output$grado_academico <- renderPrint({
    req(analysisData())
    analysisData()$puntaje_formacion
  })
  
  output$articulos_cientificos <- renderPrint({
    req(analysisData())
    analysisData()$total_suma_valor
  })
  
  output$asesoria_tesis <- renderPrint({
    req(analysisData())
    analysisData()$puntaje_asesor
  })
  
  output$registro_propiedad_calculado <- renderPrint({
    req(analysisData())
    analysisData()$registro_propiedad_calculado
  })
  
  output$total_renacyt_puntaje <- renderText({
    total_renacyt_puntaje()
  })
  
  output$renacyt_calificacion <- renderText({
    renacyt_calificacion()
  })
}

# Lanzar la aplicación
shinyApp(ui, server)
