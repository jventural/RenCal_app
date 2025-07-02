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
  "readxl", "stringi", "DT", "curl", "httr2", "plotly", "openxlsx"
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
library(dplyr)  # Para mutate, across, etc.
library(plotly)
library(openxlsx)


# -------------------------------------------------------------
# Función extraer_tabla() modificada para quitar tildes
# -------------------------------------------------------------
extraer_tabla <- function(page, texto_seccion) {
  tryCatch({
    raw_table <- page %>%
      html_node(xpath = paste0(
        "//*[contains(text(), '", texto_seccion, "')]/following::table[1]"
      )) %>%
      html_table(fill = TRUE)
    
    if (!is.null(raw_table) && nrow(raw_table) > 0) {
      # Primera fila: nombres de columna
      colnames(raw_table) <- as.character(raw_table[1, ])
      raw_table        <- raw_table[-1, ]
      
      # Eliminar tildes de nombres de columna
      colnames(raw_table) <- stringi::stri_trans_general(
        colnames(raw_table),
        "Latin-ASCII"
      )
      
      # Eliminar tildes en cada celda de tipo character
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

# -------------------------------------------------------------
# Función para extraer nombre del investigador
# -------------------------------------------------------------
extraer_nombre_investigador <- function(page) {
  tryCatch({
    # Usar el selector CSS específico proporcionado
    nombre <- page %>%
      html_node(".tituloNombreFicha2 span") %>%
      html_text() %>%
      str_trim()
    
    # Si no encuentra con el selector específico, intentar alternativas
    if (is.null(nombre) || nombre == "" || is.na(nombre)) {
      nombre <- page %>%
        html_node("h3") %>%
        html_text() %>%
        str_trim()
    }
    
    if (is.null(nombre) || nombre == "" || is.na(nombre)) {
      nombre <- page %>%
        html_node("title") %>%
        html_text() %>%
        str_extract("(?<=Investigador: ).*?(?=\\s*-|$)") %>%
        str_trim()
    }
    
    # Si aún no encuentra nombre, usar un identificador genérico
    if (is.null(nombre) || nombre == "" || is.na(nombre)) {
      nombre <- "Investigador no identificado"
    }
    
    return(nombre)
  }, error = function(e) {
    return(paste("Error al extraer nombre:", e$message))
  })
}

# -------------------------------------------------------------
# Función para procesar un investigador individual
# -------------------------------------------------------------
procesar_investigador <- function(url_investigador, df_scopus, Scielo_Data) {
  tryCatch({
    page <- read_html(url_investigador)
    
    # Extraer nombre
    nombre <- extraer_nombre_investigador(page)
    
    # Extraer datos
    asesor <- extraer_tabla(page, "Experiencia como Asesor de Tesis")
    formacion <- extraer_tabla(page, "Formación Académica (Fuente: SUNEDU)")
    produccion_raw <- extraer_tabla(page, "Producción científica")
    derechos_propiedad_intelectual <- extraer_tabla(page, "Derechos de Propiedad Intelectual")
    
    # Recodificación UTF-8 a la tabla produccion_raw
    if (!is.null(produccion_raw) && nrow(produccion_raw) > 0) {
      produccion <- produccion_raw %>%
        { names(.) <- enc2utf8(names(.)); . } %>%
        mutate(across(where(is.character), enc2utf8))
    } else {
      produccion <- produccion_raw
    }
    
    # Calcular puntaje de Derechos de PI
    registro_propiedad_calculado <- 0
    if (!is.null(derechos_propiedad_intelectual) &&
        "Tipo de PI" %in% colnames(derechos_propiedad_intelectual)) {
      derechos_propiedad_intelectual <- derechos_propiedad_intelectual %>%
        mutate(Puntuacion = case_when(
          `Tipo de PI` %in% c(
            "Patente de invencion", "Certificado de Obtentor",
            "Paquete tecnologico", "Registro de certificado de obtentor"
          ) ~ 3L,
          `Tipo de PI` %in% c(
            "Patente de modelo de utilidad",
            "certificado de derecho de autor por software"
          ) ~ 1L,
          TRUE ~ 0L
        ))
      registro_propiedad_calculado <-
        sum(derechos_propiedad_intelectual$Puntuacion, na.rm = TRUE)
    }
    
    # Procesar producción científica
    df_scopus_norm <- df_scopus %>%
      mutate(Revista_norm = tolower(stri_trans_general(Revista, "Latin-ASCII")))
    
    # Normalizar produccion para quitar acentos y pasar a minúsculas
    produccion_norm <- produccion %>%
      mutate(Revista_norm = tolower(stri_trans_general(Revista, "Latin-ASCII")))
    
    data_joined <- produccion_norm %>%
      left_join(df_scopus_norm,
                by = "Revista_norm",
                relationship = "many-to-many") %>%
      filter(
        !(
          `Tipo Produccion`
          %in% c(
            "DoctoralThesis", "MasterThesis", "Note",
            "Editorial", "Letter", "Journal - Meeting Abstract"
          )
        )
      ) %>%
      na.omit()
    
    resumen <- data_joined %>%
      select(
        Revista_norm, `Ano de Produccion`, Titulo,
        `Cuartil de ScimagoJR o JCR*`, Cuartil, Valor
      ) %>%
      distinct(Titulo, .keep_all = TRUE)
    
    data_joined2 <- resumen %>%
      mutate(
        AnioProd = as.numeric(`Ano de Produccion`),
        join_year = case_when(
          AnioProd %in% c(2024, 2025) ~ 2024,
          TRUE ~ AnioProd
        )
      ) %>%
      left_join(
        df_scopus_norm %>% rename(join_year = year),
        by = c("Revista_norm", "join_year"),
        relationship = "many-to-many"
      )
    
    df_final <- data_joined2 %>%
      select(
        Revista_norm, `Ano de Produccion`, Titulo,
        `Cuartil de ScimagoJR o JCR*`, Cuartil.y, Valor.y
      ) %>%
      distinct(Titulo, .keep_all = TRUE)
    
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
      left_join(scielo_counts,
                by = c("Revista_norm" = "Revista")) %>%
      mutate(
        n_matches = if_else(is.na(n_matches), 0L, n_matches),
        Valor.y = if_else(Cuartil.y == "No Cuartil",
                          pmin(n_matches, 10L),
                          Valor.y)
      ) %>%
      select(-n_matches) %>%
      rename(
        Cuartil = Cuartil.y,
        Value = Valor.y
      )
    
    # Calcular puntajes
    total_suma_valor <- sum(df_final$Value, na.rm = TRUE)
    
    # Puntaje de formación académica
    formacion_scores <- formacion %>%
      mutate(score = case_when(
        str_detect(Grado, regex("DOCTOR", ignore_case = TRUE))  ~ 10,
        str_detect(Grado, regex("MAGISTER", ignore_case = TRUE)) ~ 6,
        str_detect(Grado, regex("LICENCIADO", ignore_case = TRUE)) ~ 4,
        str_detect(Grado, regex("BACHILLER", ignore_case = TRUE)) ~ 2,
        str_detect(Grado, regex("CONSTANCIA DE MATRICULA", ignore_case = TRUE)) ~ 1,
        TRUE ~ 0
      ))
    
    puntaje_formacion <- formacion_scores %>%
      summarise(puntaje = max(score, na.rm = TRUE)) %>%
      pull(puntaje)
    
    # Puntaje de asesorías
    puntaje_asesor <- asesor %>%
      mutate(score = case_when(
        str_detect(Tesis, regex("Doctorado", ignore_case = TRUE)) ~ 2,
        str_detect(Tesis, regex("Magister", ignore_case = TRUE)) ~ 1,
        str_detect(Tesis, regex("Bachiller|Titulo Profesional|Licenciado / Titulo", ignore_case = TRUE)) ~ 0.5,
        TRUE ~ 0
      )) %>%
      summarise(total = sum(score, na.rm = TRUE)) %>%
      mutate(total = if_else(total > 10, 10, total)) %>%
      pull(total)
    
    # Contar publicaciones por cuartil
    cuartiles_count <- df_final %>%
      count(Cuartil, name = "cantidad") %>%
      replace_na(list(Cuartil = "Sin cuartil"))
    
    return(list(
      nombre = nombre,
      url = url_investigador,
      puntaje_formacion = puntaje_formacion,
      puntaje_articulos = total_suma_valor,
      puntaje_propiedad = registro_propiedad_calculado,
      puntaje_asesor = puntaje_asesor,
      total_publicaciones = nrow(df_final),
      cuartiles = cuartiles_count,
      df_final = df_final,
      success = TRUE
    ))
    
  }, error = function(e) {
    return(list(
      nombre = "Error al procesar",
      url = url_investigador,
      error = e$message,
      success = FALSE
    ))
  })
}


# -------------------------------------------------------------
# INTERFAZ de usuario (UI)
# -------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "RenCal", titleWidth = 300),
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      menuItem("📋 Información", tabName = "info", icon = icon("info-circle")),
      menuItem("🧮 Calculadora RENACYT", tabName = "calculadora", icon = icon("calculator")),
      menuItem("👨‍🔬 Acerca del autor", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    # CSS personalizado moderno
    tags$head(
      tags$style(HTML("
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700&display=swap');
        
        /* Variables CSS */
        :root {
          --primary-color: #667eea;
          --primary-gradient: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          --secondary-color: #f093fb;
          --secondary-gradient: linear-gradient(135deg, #f093fb 0%, #f5576c 100%);
          --success-color: #4ecdc4;
          --success-gradient: linear-gradient(135deg, #4ecdc4 0%, #44a08d 100%);
          --warning-color: #ffeaa7;
          --warning-gradient: linear-gradient(135deg, #ffeaa7 0%, #fab1a0 100%);
          --dark-color: #2d3748;
          --light-color: #f8fafc;
          --card-shadow: 0 10px 25px rgba(0,0,0,0.1);
          --card-hover-shadow: 0 15px 35px rgba(0,0,0,0.15);
        }
        
        /* Body general */
        body, .content-wrapper, .right-side { 
          background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
          font-family: 'Inter', -apple-system, BlinkMacSystemFont, sans-serif;
          color: var(--dark-color);
        }
        
        /* Header moderno */
        .skin-blue .main-header .logo { 
          background: var(--primary-gradient);
          color: white;
          font-weight: 600;
          border: none;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        .skin-blue .main-header .navbar { 
          background: var(--primary-gradient);
          border: none;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        
        /* Sidebar moderno */
        .skin-blue .main-sidebar {
          background: linear-gradient(180deg, #2d3748 0%, #1a202c 100%);
          box-shadow: 2px 0 15px rgba(0,0,0,0.1);
        }
        
        .sidebar-menu > li > a {
          color: #e2e8f0 !important;
          border-left: 3px solid transparent;
          transition: all 0.3s ease;
          font-weight: 500;
        }
        
        .sidebar-menu > li > a:hover {
          background: rgba(255,255,255,0.05) !important;
          border-left-color: var(--primary-color);
          transform: translateX(5px);
        }
        
        .sidebar-menu > li.active > a { 
          background: var(--primary-gradient) !important;
          color: white !important;
          border-left-color: #ffffff;
          box-shadow: 0 3px 10px rgba(102, 126, 234, 0.3);
        }
        
        /* Boxes modernas */
        .box { 
          border: none !important;
          border-radius: 15px !important;
          box-shadow: var(--card-shadow) !important;
          background: white;
          transition: all 0.3s ease;
          overflow: hidden;
        }
        
        .box:hover {
          box-shadow: var(--card-hover-shadow) !important;
          transform: translateY(-2px);
        }
        
        .box-header {
          background: var(--primary-gradient) !important;
          color: white !important;
          border: none !important;
          font-weight: 600;
          padding: 20px;
        }
        
        .box-header.with-border {
          border-bottom: none !important;
        }
        
        .box-title {
          font-size: 1.2em;
          font-weight: 600;
        }
        
        .box-body {
          padding: 25px;
          background: white;
        }
        
        /* Cards de métricas mejoradas */
        .comparative-card { 
          background: white;
          border-radius: 15px;
          padding: 25px;
          margin: 15px 0;
          box-shadow: var(--card-shadow);
          border-left: 4px solid var(--primary-color);
          transition: all 0.3s ease;
          position: relative;
          overflow: hidden;
        }
        
        .comparative-card::before {
          content: '';
          position: absolute;
          top: 0;
          left: 0;
          right: 0;
          height: 4px;
          background: var(--primary-gradient);
        }
        
        .comparative-card:hover {
          transform: translateY(-3px);
          box-shadow: var(--card-hover-shadow);
        }
        
        .metric-value { 
          font-size: 2.5em;
          font-weight: 700;
          background: var(--primary-gradient);
          -webkit-background-clip: text;
          -webkit-text-fill-color: transparent;
          background-clip: text;
          margin-bottom: 8px;
        }
        
        .metric-label { 
          font-size: 0.95em;
          color: #64748b;
          font-weight: 500;
          text-transform: uppercase;
          letter-spacing: 0.5px;
        }
        
        /* Botones modernos */
        .btn {
          border-radius: 25px;
          font-weight: 500;
          padding: 12px 30px;
          border: none;
          transition: all 0.3s ease;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          font-size: 0.9em;
        }
        
        .btn-primary {
          background: var(--primary-gradient);
          box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3);
        }
        
        .btn-primary:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
        }
        
        .btn-success {
          background: var(--success-gradient);
          box-shadow: 0 4px 15px rgba(78, 205, 196, 0.3);
        }
        
        .btn-success:hover {
          transform: translateY(-2px);
          box-shadow: 0 6px 20px rgba(78, 205, 196, 0.4);
        }
        
        /* Inputs modernos */
        .form-control {
          border: 2px solid #e2e8f0;
          border-radius: 10px;
          padding: 12px 15px;
          transition: all 0.3s ease;
          font-weight: 500;
        }
        
        .form-control:focus {
          border-color: var(--primary-color);
          box-shadow: 0 0 0 3px rgba(102, 126, 234, 0.1);
        }
        
        /* Labels mejorados */
        label {
          font-weight: 600;
          color: var(--dark-color);
          margin-bottom: 8px;
        }
        
        /* Tablas modernas */
        .dataTables_wrapper {
          border-radius: 15px;
          overflow: hidden;
          box-shadow: var(--card-shadow);
        }
        
        .table {
          margin-bottom: 0;
        }
        
        .table thead th {
          background: var(--primary-gradient) !important;
          color: white !important;
          border: none !important;
          font-weight: 600;
          padding: 15px;
        }
        
        .table tbody tr {
          transition: all 0.2s ease;
        }
        
        .table tbody tr:hover {
          background-color: #f8fafc !important;
          transform: scale(1.01);
        }
        
        .table tbody td {
          padding: 12px 15px;
          border-color: #e2e8f0;
        }
        
        /* Progress bars */
        .progress {
          height: 8px;
          border-radius: 10px;
          background-color: #e2e8f0;
          overflow: hidden;
        }
        
        .progress-bar {
          background: var(--primary-gradient);
          border-radius: 10px;
        }
        
        /* Select modernos */
        select.form-control {
          background-image: linear-gradient(45deg, transparent 50%, var(--primary-color) 50%), 
                           linear-gradient(135deg, var(--primary-color) 50%, transparent 50%);
          background-position: calc(100% - 20px) calc(1em + 2px), calc(100% - 15px) calc(1em + 2px);
          background-size: 5px 5px, 5px 5px;
          background-repeat: no-repeat;
        }
        
        /* Tabs modernos */
        .nav-tabs {
          border: none;
          background: #f8fafc;
          border-radius: 15px;
          padding: 5px;
        }
        
        .nav-tabs > li > a {
          border: none !important;
          border-radius: 10px !important;
          color: var(--dark-color);
          font-weight: 500;
          transition: all 0.3s ease;
        }
        
        .nav-tabs > li.active > a {
          background: var(--primary-gradient) !important;
          color: white !important;
          box-shadow: 0 3px 10px rgba(102, 126, 234, 0.3);
        }
        
        /* Alertas modernas */
        .alert {
          border: none;
          border-radius: 15px;
          padding: 20px;
          font-weight: 500;
        }
        
        /* Responsive design */
        @media (max-width: 768px) {
          .comparative-card {
            margin: 10px 0;
            padding: 20px;
          }
          
          .metric-value {
            font-size: 2em;
          }
          
          .box-body {
            padding: 15px;
          }
        }
        
        /* Animaciones sutiles */
        @keyframes fadeInUp {
          from {
            opacity: 0;
            transform: translateY(30px);
          }
          to {
            opacity: 1;
            transform: translateY(0);
          }
        }
        
        .box {
          animation: fadeInUp 0.6s ease-out;
        }
        
        /* Loading spinner */
        .shiny-spinner-output-container {
          background: rgba(255,255,255,0.9);
          border-radius: 15px;
        }
        
        /* Mejoras para plotly */
        .plotly {
          border-radius: 15px;
          overflow: hidden;
          box-shadow: var(--card-shadow);
        }
      "))
    ),
    tabItems(
      # -------------------------------------------------------------
      # Pestaña: Información RENACYT
      # -------------------------------------------------------------
      tabItem(tabName = "info",
              fluidRow(
                box(
                  width = 12, title = "ℹ️ Acerca del programa",
                  status = "primary", solidHeader = TRUE,
                  div(style = "padding: 10px;",
                      div(style = "background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); 
                                 border-radius: 15px; padding: 25px; margin-bottom: 20px;",
                          div(style = "display: flex; align-items: center; gap: 20px; margin-bottom: 20px;",
                              div(style = "width: 80px; height: 80px; background: linear-gradient(135deg, #667eea, #764ba2); 
                                         border-radius: 20px; display: flex; align-items: center; justify-content: center;",
                                  tags$span("🧮", style = "font-size: 3em;")
                              ),
                              div(
                                h3("RenCal", style = "color: #2d3748; font-weight: 700; margin: 0; font-size: 2.5em;"),
                                p("Calculadora RENACYT Inteligente", style = "color: #4a5568; font-size: 1.2em; margin: 5px 0 0 0;")
                              )
                          ),
                          
                          div(style = "display: grid; grid-template-columns: repeat(auto-fit, minmax(300px, 1fr)); gap: 20px;",
                              div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                  div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 15px;",
                                      div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #4ecdc4, #44a08d); 
                                                 border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                          tags$span("🎯", style = "font-size: 1.2em;")
                                      ),
                                      h5("Objetivo", style = "color: #2d3748; font-weight: 600; margin: 0;")
                                  ),
                                  p("Determina automáticamente los puntajes otorgados por el Registro Nacional Científico, Tecnológico y de Innovación Tecnológica (RENACYT).", 
                                    style = "color: #4a5568; line-height: 1.6; margin: 0;")
                              ),
                              
                              div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                  div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 15px;",
                                      div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #f093fb, #f5576c); 
                                                 border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                          tags$span("⚡", style = "font-size: 1.2em;")
                                      ),
                                      h5("Automatización", style = "color: #2d3748; font-weight: 600; margin: 0;")
                                  ),
                                  p("Facilita la obtención de la calificación de investigadores automatizando los criterios más complejos de evaluación.", 
                                    style = "color: #4a5568; line-height: 1.6; margin: 0;")
                              ),
                              
                              div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                  div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 15px;",
                                      div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #ffeaa7, #fab1a0); 
                                                 border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                          tags$span("🔬", style = "font-size: 1.2em;")
                                      ),
                                      h5("Metodología", style = "color: #2d3748; font-weight: 600; margin: 0;")
                                  ),
                                  p("Analiza el nombre de las revistas para detectar su cuartil y asignar puntajes, combinando información de Scimago y Scielo.", 
                                    style = "color: #4a5568; line-height: 1.6; margin: 0;")
                              ),
                              
                              div(style = "background: white; border-radius: 12px; padding: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                                  div(style = "display: flex; align-items: center; gap: 12px; margin-bottom: 15px;",
                                      div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #667eea, #764ba2); 
                                                 border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                          tags$span("🌐", style = "font-size: 1.2em;")
                                      ),
                                      h5("Web Scraping", style = "color: #2d3748; font-weight: 600; margin: 0;")
                                  ),
                                  p("Extrae automáticamente información de las Fichas CTI Vitae de los investigadores mediante técnicas avanzadas de web scraping.", 
                                    style = "color: #4a5568; line-height: 1.6; margin: 0;")
                              )
                          )
                      )
                  )
                )
              ),
              fluidRow(
                box(
                  width = 12, title = "📋 Normativas RENACYT",
                  status = "primary", solidHeader = TRUE,
                  div(style = "padding: 10px;",
                      div(style = "background: linear-gradient(135deg, #e3f2fd 0%, #bbdefb 100%); 
                                 border-radius: 15px; padding: 20px; margin-bottom: 20px; text-align: center;",
                          h5("📄 Documentación Oficial", style = "color: #01579b; font-weight: 600; margin-bottom: 15px;"),
                          p("Para obtener la normativa completa, consulta el documento oficial:", style = "color: #0277bd; margin-bottom: 15px;"),
                          a(href = "http://resoluciones.concytec.gob.pe/subidos/sintesis/RP-090-2021-CONCYTEC-P.pdf",
                            "📖 Resolución de Presidencia Nº 090-2021-CONCYTEC-P", 
                            target = "_blank",
                            style = "background: linear-gradient(135deg, #1976d2, #1565c0); color: white; 
                                   padding: 12px 25px; border-radius: 25px; text-decoration: none; 
                                   font-weight: 600; display: inline-block; box-shadow: 0 4px 15px rgba(25, 118, 210, 0.3);")
                      ),
                      div(style = "text-align: center;",
                          p("A continuación se muestra el Anexo Nº 1:", style = "color: #2d3748; font-weight: 500; margin-bottom: 20px;"),
                          imageOutput("image1", height = "750px", width = "950px")
                      )
                  )
                )
              )
      ),
      
      # -------------------------------------------------------------
      # Pestaña: Calculadora RENACYT (antes Análisis Comparativo)
      # -------------------------------------------------------------
      tabItem(tabName = "calculadora",
              fluidRow(
                box(
                  width = 12, title = "🧮 Calculadora RENACYT - Análisis Individual y Comparativo",
                  status = "primary", solidHeader = TRUE,
                  div(style = "padding: 10px;",
                      div(style = "background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); 
                                   border-radius: 15px; padding: 25px; margin-bottom: 20px;",
                          h4("🚀 ¡Bienvenido a RenCal!", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                          p("Analiza de forma automática los puntajes RENACYT de investigadores peruanos.", 
                            style = "color: #4a5568; font-size: 1.1em; margin-bottom: 15px;"),
                          div(style = "display: flex; gap: 20px; flex-wrap: wrap;",
                              div(style = "display: flex; align-items: center; gap: 10px;",
                                  div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #667eea, #764ba2); 
                                             border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                      tags$span("1", style = "color: white; font-weight: bold;")),
                                  div("Para análisis individual: ingresa una sola URL", style = "color: #2d3748; font-weight: 500;")
                              ),
                              div(style = "display: flex; align-items: center; gap: 10px;",
                                  div(style = "width: 40px; height: 40px; background: linear-gradient(135deg, #4ecdc4, #44a08d); 
                                             border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                      tags$span("2", style = "color: white; font-weight: bold;")),
                                  div("Para análisis comparativo: ingresa múltiples URLs (una por línea)", style = "color: #2d3748; font-weight: 500;")
                              )
                          )
                      ),
                      
                      div(style = "background: white; border-radius: 15px; padding: 25px; box-shadow: 0 10px 25px rgba(0,0,0,0.1);",
                          h5("📋 URLs de Investigadores CTIVITAE", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                          textAreaInput(
                            "urls_multiple", 
                            label = NULL,
                            value = "", 
                            rows = 6, 
                            placeholder = "https://ctivitae.concytec.gob.pe/appDirectorioCTI/VerDatosInvestigador.do?id_investigador=XXXXX"
                          ),
                          
                          div(style = "text-align: center; margin-top: 20px;",
                              actionButton("run_comparative", 
                                           HTML("🔍 Ejecutar Análisis"), 
                                           class = "btn btn-primary",
                                           style = "font-size: 1.1em; padding: 15px 40px;")
                          )
                      )
                  ),
                  br(), br(),
                  
                  conditionalPanel(
                    condition = "output.comparative_results_ready",
                    tabsetPanel(
                      tabPanel(HTML("📊 Resumen Comparativo"), 
                               DTOutput("comparative_summary_table"),
                               br(),
                               h4("📊 Estadísticas Generales", style = "color: #2d3748; font-weight: 600; margin-bottom: 25px;"),
                               fluidRow(
                                 column(3,
                                        div(class = "comparative-card",
                                            style = "text-align: center;",
                                            div(style = "font-size: 3em; margin-bottom: 10px;", "👥"),
                                            div(class = "metric-value", textOutput("total_investigators")),
                                            div(class = "metric-label", "Investigadores Analizados")
                                        )
                                 ),
                                 column(3,
                                        div(class = "comparative-card",
                                            style = "text-align: center;",
                                            div(style = "font-size: 3em; margin-bottom: 10px;", "📚"),
                                            div(class = "metric-value", textOutput("avg_publications")),
                                            div(class = "metric-label", "Promedio de Publicaciones")
                                        )
                                 ),
                                 column(3,
                                        div(class = "comparative-card",
                                            style = "text-align: center;",
                                            div(style = "font-size: 3em; margin-bottom: 10px;", "🏆"),
                                            div(class = "metric-value", textOutput("top_score")),
                                            div(class = "metric-label", "Puntaje Máximo")
                                        )
                                 ),
                                 column(3,
                                        div(class = "comparative-card",
                                            style = "text-align: center;",
                                            div(style = "font-size: 3em; margin-bottom: 10px;", "📈"),
                                            div(class = "metric-value", textOutput("avg_score")),
                                            div(class = "metric-label", "Puntaje Promedio")
                                        )
                                 )
                               )
                      ),
                      tabPanel(HTML("🏅 Calificación RENACYT"),
                               fluidRow(
                                 box(
                                   width = 12, title = "⚙️ Configuración de Puntajes Adicionales",
                                   status = "info", solidHeader = TRUE,
                                   div(style = "background: linear-gradient(135deg, #e0f2fe 0%, #b3e5fc 100%); 
                                              border-radius: 10px; padding: 20px; margin-bottom: 15px;",
                                       p("✨ Para cada investigador, configura los valores de Índice H y Libros/Capítulos para obtener la calificación RENACYT completa.", 
                                         style = "color: #01579b; font-weight: 500; margin: 0;")
                                   ),
                                   uiOutput("renacyt_inputs_ui")
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12, title = "🎯 Calificaciones RENACYT Comparativas",
                                   status = "primary", solidHeader = TRUE,
                                   DTOutput("renacyt_comparative_table"),
                                   br(),
                                   div(style = "text-align: center;",
                                       actionButton("update_renacyt", 
                                                    HTML("🔄 Actualizar Calificaciones"), 
                                                    class = "btn btn-primary",
                                                    style = "font-size: 1.1em; padding: 12px 30px;")
                                   )
                                 )
                               ),
                               fluidRow(
                                 box(
                                   width = 12, title = "📈 Gráfico de Calificaciones RENACYT",
                                   status = "success", solidHeader = TRUE,
                                   plotlyOutput("renacyt_levels_plot", height = "500px")
                                 )
                               )
                      ),
                      tabPanel(HTML("📊 Gráfico Comparativo"), 
                               plotlyOutput("comparative_plot", height = "600px")
                      ),
                      tabPanel(HTML("🎯 Distribución por Cuartiles"), 
                               plotlyOutput("quartile_plot", height = "600px")
                      ),
                      tabPanel(HTML("👤 Detalles por Investigador"),
                               div(style = "background: white; border-radius: 15px; padding: 20px; margin-bottom: 20px;",
                                   fluidRow(
                                     column(8,
                                            h5("🔍 Selecciona un investigador:", style = "color: #2d3748; font-weight: 600; margin-bottom: 10px;"),
                                            selectInput("selected_researcher", 
                                                        label = NULL,
                                                        choices = NULL)
                                     ),
                                     column(4,
                                            br(),
                                            div(style = "text-align: center;",
                                                downloadButton("download_researcher", 
                                                               HTML("📁 Descargar Excel"), 
                                                               class = "btn btn-success",
                                                               style = "font-size: 1em; padding: 12px 25px;")
                                            )
                                     )
                                   )
                               ),
                               DTOutput("researcher_detail_table")
                      )
                    )
                  )
                )
              )
      ),
      
      # -------------------------------------------------------------
      # Pestaña: Acerca del autor
      # -------------------------------------------------------------
      tabItem(tabName = "about",
              box(
                width = 12, title = "👨‍🔬 Acerca del autor",
                status = "primary", solidHeader = TRUE,
                div(style = "padding: 10px;",
                    div(style = "background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%); 
                               border-radius: 15px; padding: 30px; text-align: center;",
                        div(style = "display: inline-block; width: 120px; height: 120px; 
                                   background: linear-gradient(135deg, #667eea, #764ba2); 
                                   border-radius: 50%; display: flex; align-items: center; 
                                   justify-content: center; margin-bottom: 25px;",
                            tags$span("👨‍🎓", style = "font-size: 4em;")
                        ),
                        
                        h3("Dr. José Ventura-León", style = "color: #2d3748; font-weight: 700; margin-bottom: 10px;"),
                        p("Doctor en Psicología | Magíster en Psicología Educativa", 
                          style = "color: #4a5568; font-size: 1.2em; margin-bottom: 25px; font-weight: 500;"),
                        
                        div(style = "background: white; border-radius: 12px; padding: 25px; 
                                   box-shadow: 0 10px 25px rgba(0,0,0,0.1); margin-bottom: 25px;",
                            div(style = "display: flex; align-items: center; justify-content: center; gap: 15px; margin-bottom: 20px;",
                                div(style = "width: 50px; height: 50px; background: linear-gradient(135deg, #4ecdc4, #44a08d); 
                                           border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                                    tags$span("🏢", style = "font-size: 1.5em;")
                                ),
                                h5("Universidad Privada del Norte (UPN)", style = "color: #2d3748; font-weight: 600; margin: 0;")
                            ),
                            p("Docente Investigador a tiempo completo, especializado en metodología de investigación y análisis estadístico aplicado a las ciencias sociales.", 
                              style = "color: #4a5568; line-height: 1.6; margin: 0;")
                        ),
                        
                        div(style = "display: flex; gap: 20px; justify-content: center; flex-wrap: wrap;",
                            a(href = "https://joseventuraleon.com/", 
                              target = "_blank",
                              style = "background: linear-gradient(135deg, #667eea, #764ba2); color: white; 
                                     padding: 15px 30px; border-radius: 25px; text-decoration: none; 
                                     font-weight: 600; display: flex; align-items: center; gap: 10px;
                                     box-shadow: 0 4px 15px rgba(102, 126, 234, 0.3); 
                                     transition: all 0.3s ease;",
                              onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 6px 20px rgba(102, 126, 234, 0.4)';",
                              onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(102, 126, 234, 0.3)';",
                              HTML("🌐 Sitio Web Personal")
                            ),
                            
                            tags$a(href = "mailto:info@joseventuraleon.com",
                                   style = "background: linear-gradient(135deg, #4ecdc4, #44a08d); color: white; 
                                         padding: 15px 30px; border-radius: 25px; text-decoration: none; 
                                         font-weight: 600; display: flex; align-items: center; gap: 10px;
                                         box-shadow: 0 4px 15px rgba(78, 205, 196, 0.3);
                                         transition: all 0.3s ease;",
                                   onmouseover = "this.style.transform='translateY(-2px)'; this.style.boxShadow='0 6px 20px rgba(78, 205, 196, 0.4)';",
                                   onmouseout = "this.style.transform='translateY(0)'; this.style.boxShadow='0 4px 15px rgba(78, 205, 196, 0.3)';",
                                   HTML("📧 Contacto")
                            )
                        ),
                        
                        div(style = "margin-top: 30px; padding-top: 25px; border-top: 2px solid #e2e8f0;",
                            p("💡 Para consultas, reportar errores o sugerencias de mejora", 
                              style = "color: #64748b; font-style: italic; margin: 0;")
                        )
                    )
                )
              )
      )
    )
  )
)


# -------------------------------------------------------------
# LÓGICA del servidor
# -------------------------------------------------------------
server <- function(input, output, session) {
  
  # Render de la imagen en Información RENACYT
  output$image1 <- renderImage({
    filename <- normalizePath(file.path("www", "anexo1.png"))
    list(
      src         = filename,
      contentType = 'image/png',
      width       = 950,
      height      = 750,
      alt         = "Anexo Nº 1 RENACYT"
    )
  }, deleteFile = FALSE)
  
  # Función para el cálculo de puntaje y calificación RENACYT
  GetPuntajeSum <- function(Grado = 0, Articulos = 0, Patentes = 0, Libros = 0, Asesorias = 0) {
    Grado + Articulos + Patentes + Libros + Asesorias
  }
  
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
  
  
  # -------------------------------------------------------------
  # ANÁLISIS COMPARATIVO - FUNCIONALIDAD PRINCIPAL
  # -------------------------------------------------------------
  comparativeData <- eventReactive(input$run_comparative, {
    req(input$urls_multiple)
    
    urls <- str_split(input$urls_multiple, "\n")[[1]] %>%
      str_trim() %>%
      .[. != ""]
    
    if (length(urls) == 0) {
      return(NULL)
    }
    
    withProgress(message = "Analizando investigadores...", value = 0, {
      
      # Cargar datos de referencia
      df_scopus   <- read_excel("df_scopus.xlsx")
      Scielo_Data <- read_excel("Scielo_Data.xlsx")
      
      resultados <- list()
      
      for (i in seq_along(urls)) {
        incProgress(1/length(urls), detail = paste("Procesando investigador", i, "de", length(urls)))
        
        resultado <- procesar_investigador(urls[i], df_scopus, Scielo_Data)
        resultados[[i]] <- resultado
      }
      
      return(resultados)
    })
  })
  
  # Valores reactivos para almacenar configuraciones RENACYT
  renacyt_values <- reactiveValues()
  
  # Inicializar valores RENACYT cuando se cargan nuevos datos
  observeEvent(comparativeData(), {
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) > 0) {
      for (i in seq_along(successful_results)) {
        renacyt_values[[paste0("indice_h_", i)]] <- "No"
        renacyt_values[[paste0("libros_", i)]] <- 0
      }
    }
  })
  
  # UI dinámica para inputs RENACYT
  output$renacyt_inputs_ui <- renderUI({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(div("No hay investigadores procesados exitosamente."))
    }
    
    input_list <- list()
    
    for (i in seq_along(successful_results)) {
      researcher <- successful_results[[i]]
      
      input_list[[length(input_list) + 1]] <- fluidRow(
        column(12,
               div(
                 class = "comparative-card",
                 style = "border-left: 4px solid #667eea; background: linear-gradient(135deg, #ffffff 0%, #f8fafc 100%);",
                 div(style = "display: flex; align-items: center; gap: 15px; margin-bottom: 20px;",
                     div(style = "width: 50px; height: 50px; background: linear-gradient(135deg, #667eea, #764ba2); 
                                border-radius: 50%; display: flex; align-items: center; justify-content: center;",
                         tags$span("👤", style = "font-size: 1.5em;")
                     ),
                     h5(strong(researcher$nombre), style = "color: #2d3748; margin: 0; font-weight: 600;")
                 ),
                 
                 fluidRow(
                   column(6,
                          div(style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                              h6("🏆 Índice H", style = "color: #667eea; font-weight: 600; margin-bottom: 10px;"),
                              selectInput(
                                paste0("indice_h_", i),
                                label = NULL,
                                choices = c("No", "Sí"),
                                selected = "No"
                              )
                          )
                   ),
                   column(6,
                          div(style = "background: white; border-radius: 10px; padding: 15px; box-shadow: 0 4px 6px rgba(0,0,0,0.05);",
                              h6("📚 Libros y Capítulos", style = "color: #667eea; font-weight: 600; margin-bottom: 10px;"),
                              numericInput(
                                paste0("libros_", i),
                                label = NULL,
                                value = 0, min = 0, max = 10, step = 1
                              )
                          )
                   )
                 ),
                 
                 div(style = "background: linear-gradient(135deg, #f0f4f8 0%, #e6f3ff 100%); 
                            border-radius: 10px; padding: 15px; margin-top: 15px;",
                     h6("📋 Puntajes Automáticos", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                     fluidRow(
                       column(4,
                              div(style = "text-align: center; padding: 10px;",
                                  div(style = "font-size: 1.5em; color: #667eea; font-weight: 700;", researcher$puntaje_formacion),
                                  div(style = "font-size: 0.85em; color: #64748b; font-weight: 500;", "🎓 Grado Académico")
                              )
                       ),
                       column(4,
                              div(style = "text-align: center; padding: 10px;",
                                  div(style = "font-size: 1.5em; color: #4ecdc4; font-weight: 700;", researcher$puntaje_articulos),
                                  div(style = "font-size: 0.85em; color: #64748b; font-weight: 500;", "📄 Artículos")
                              )
                       ),
                       column(4,
                              div(style = "text-align: center; padding: 10px;",
                                  div(style = "font-size: 1.5em; color: #f093fb; font-weight: 700;", researcher$puntaje_asesor),
                                  div(style = "font-size: 0.85em; color: #64748b; font-weight: 500;", "🎯 Asesorías")
                              )
                       )
                     )
                 )
               )
        )
      )
      
      input_list[[length(input_list) + 1]] <- br()
    }
    
    do.call(tagList, input_list)
  })
  
  # Tabla de calificaciones RENACYT
  output$renacyt_comparative_table <- renderDT({
    req(comparativeData())
    
    # Trigger para actualizar cuando se presiona el botón
    input$update_renacyt
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(data.frame(Mensaje = "No hay investigadores procesados"))
    }
    
    renacyt_data <- map_dfr(seq_along(successful_results), function(i) {
      res <- successful_results[[i]]
      
      # Obtener valores de los inputs
      indice_h <- input[[paste0("indice_h_", i)]]
      libros <- input[[paste0("libros_", i)]]
      
      if (is.null(indice_h)) indice_h <- "No"
      if (is.null(libros)) libros <- 0
      
      # Calcular puntaje total
      total_puntaje <- res$puntaje_formacion + res$puntaje_articulos + 
        res$puntaje_propiedad + res$puntaje_asesor + libros
      
      # Calcular producción total
      prod_total <- res$puntaje_articulos + res$puntaje_propiedad + libros
      
      # Obtener calificación
      calificacion <- Getcalificacion(
        value = total_puntaje,
        IndiceH = indice_h,
        prod_total = prod_total
      )
      
      data.frame(
        Investigador = res$nombre,
        `Grado Académico` = res$puntaje_formacion,
        `Artículos Científicos` = res$puntaje_articulos,
        `Propiedad Intelectual` = res$puntaje_propiedad,
        `Asesorías` = res$puntaje_asesor,
        `Libros/Capítulos` = libros,
        `Índice H` = indice_h,
        `Puntaje Total` = total_puntaje,
        `Calificación RENACYT` = calificacion,
        check.names = FALSE
      )
    })
    
    datatable(
      renacyt_data,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE
      )
    ) %>%
      formatStyle(
        columns = "Puntaje Total",
        backgroundColor = styleInterval(c(10, 25, 50, 100, 160), 
                                        c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff", "#ccccff", "#e6ccff"))
      ) %>%
      formatStyle(
        columns = "Calificación RENACYT",
        backgroundColor = styleEqual(
          c("Investigador Distinguido", "Sí califica: Nivel I", "Sí califica: Nivel II", 
            "Sí califica: Nivel III", "Sí califica: Nivel IV", "Sí califica: Nivel V",
            "Sí califica: Nivel VI", "Sí califica: Nivel VII"),
          c("#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", "#FFC107", "#FF9800", "#FF5722", "#9C27B0")
        )
      )
  })
  
  # Gráfico de niveles RENACYT
  output$renacyt_levels_plot <- renderPlotly({
    req(comparativeData())
    
    # Trigger para actualizar cuando se presiona el botón
    input$update_renacyt
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(plotly_empty())
    }
    
    plot_data <- map_dfr(seq_along(successful_results), function(i) {
      res <- successful_results[[i]]
      
      # Obtener valores de los inputs
      indice_h <- input[[paste0("indice_h_", i)]]
      libros <- input[[paste0("libros_", i)]]
      
      if (is.null(indice_h)) indice_h <- "No"
      if (is.null(libros)) libros <- 0
      
      # Calcular puntaje total
      total_puntaje <- res$puntaje_formacion + res$puntaje_articulos + 
        res$puntaje_propiedad + res$puntaje_asesor + libros
      
      # Calcular producción total
      prod_total <- res$puntaje_articulos + res$puntaje_propiedad + libros
      
      # Obtener calificación
      calificacion <- Getcalificacion(
        value = total_puntaje,
        IndiceH = indice_h,
        prod_total = prod_total
      )
      
      # Extraer solo el nivel para el gráfico (orden importante: del más específico al menos específico)
      nivel <- case_when(
        str_detect(calificacion, "Investigador Distinguido") ~ "Investigador Distinguido",
        str_detect(calificacion, "Nivel VII") ~ "Nivel VII",
        str_detect(calificacion, "Nivel VI") ~ "Nivel VI",
        str_detect(calificacion, "Nivel V") ~ "Nivel V",
        str_detect(calificacion, "Nivel IV") ~ "Nivel IV",
        str_detect(calificacion, "Nivel III") ~ "Nivel III",
        str_detect(calificacion, "Nivel II") ~ "Nivel II",
        str_detect(calificacion, "Nivel I") ~ "Nivel I",
        TRUE ~ "No Califica"
      )
      
      data.frame(
        Investigador = res$nombre,
        `Puntaje Total` = total_puntaje,
        Nivel = nivel,
        check.names = FALSE
      )
    })
    
    # Definir colores para cada nivel
    colors <- c(
      "Investigador Distinguido" = "#4CAF50",
      "Nivel I" = "#8BC34A",
      "Nivel II" = "#CDDC39", 
      "Nivel III" = "#FFEB3B",
      "Nivel IV" = "#FFC107",
      "Nivel V" = "#FF9800",
      "Nivel VI" = "#FF5722",
      "Nivel VII" = "#9C27B0",
      "No Califica" = "#9E9E9E"
    )
    
    p <- ggplot(plot_data, aes(x = reorder(Investigador, `Puntaje Total`), 
                               y = `Puntaje Total`, fill = Nivel)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colors) +
      labs(title = "Puntajes y Niveles RENACYT por Investigador",
           x = "Investigador", y = "Puntaje Total RENACYT") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      geom_hline(yintercept = c(10, 25, 35, 50, 70, 100, 160, 200), 
                 linetype = "dashed", alpha = 0.5, color = "gray")
    
    ggplotly(p)
  })
  
  # -------------------------------------------------------------
  # Reactivos para análisis comparativo
  # -------------------------------------------------------------
  
  # Indicador de que los resultados están listos
  output$comparative_results_ready <- reactive({
    !is.null(comparativeData())
  })
  outputOptions(output, "comparative_results_ready", suspendWhenHidden = FALSE)
  
  # Tabla resumen comparativa
  output$comparative_summary_table <- renderDT({
    req(comparativeData())
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(data.frame(Mensaje = "No se pudieron procesar los investigadores"))
    }
    
    summary_data <- map_dfr(successful_results, function(res) {
      total_puntaje <- res$puntaje_formacion + res$puntaje_articulos + 
        res$puntaje_propiedad + res$puntaje_asesor
      
      data.frame(
        Investigador = res$nombre,
        `Formación Académica` = res$puntaje_formacion,
        `Artículos Científicos` = res$puntaje_articulos,
        `Propiedad Intelectual` = res$puntaje_propiedad,
        `Asesorías` = res$puntaje_asesor,
        `Total Publicaciones` = res$total_publicaciones,
        `Puntaje Total` = total_puntaje,
        URL = res$url,
        check.names = FALSE
      )
    })
    
    datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth = TRUE,
        scrollX = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = which(names(summary_data) == "URL") - 1)
        )
      )
    ) %>%
      formatStyle(
        columns = "Puntaje Total",
        backgroundColor = styleInterval(c(10, 25, 50, 100), 
                                        c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff", "#ccccff"))
      )
  })
  
  # Estadísticas generales
  output$total_investigators <- renderText({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    length(successful_results)
  })
  
  output$avg_publications <- renderText({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) return("0")
    
    avg_pubs <- mean(sapply(successful_results, function(x) x$total_publicaciones))
    round(avg_pubs, 1)
  })
  
  output$top_score <- renderText({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) return("0")
    
    max_score <- max(sapply(successful_results, function(res) {
      res$puntaje_formacion + res$puntaje_articulos + res$puntaje_propiedad + res$puntaje_asesor
    }))
    max_score
  })
  
  output$avg_score <- renderText({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) return("0")
    
    avg_score <- mean(sapply(successful_results, function(res) {
      res$puntaje_formacion + res$puntaje_articulos + res$puntaje_propiedad + res$puntaje_asesor
    }))
    round(avg_score, 1)
  })
  
  # Gráfico comparativo
  output$comparative_plot <- renderPlotly({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(plotly_empty())
    }
    
    plot_data <- map_dfr(successful_results, function(res) {
      data.frame(
        Investigador = res$nombre,
        `Formación Académica` = res$puntaje_formacion,
        `Artículos Científicos` = res$puntaje_articulos,
        `Propiedad Intelectual` = res$puntaje_propiedad,
        `Asesorías` = res$puntaje_asesor,
        check.names = FALSE
      )
    })
    
    plot_data_long <- plot_data %>%
      pivot_longer(-Investigador, names_to = "Categoria", values_to = "Puntaje")
    
    p <- ggplot(plot_data_long, aes(x = Investigador, y = Puntaje, fill = Categoria)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Comparación de Puntajes por Categoría",
           x = "Investigador", y = "Puntaje") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(type = "qual", palette = "Set2")
    
    ggplotly(p)
  })
  
  # Gráfico de distribución por cuartiles
  output$quartile_plot <- renderPlotly({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0) {
      return(plotly_empty())
    }
    
    # Combinar datos de cuartiles de todos los investigadores
    all_cuartiles <- map_dfr(successful_results, function(res) {
      if (!is.null(res$cuartiles) && nrow(res$cuartiles) > 0) {
        res$cuartiles %>%
          mutate(Investigador = res$nombre)
      } else {
        data.frame()
      }
    })
    
    if (nrow(all_cuartiles) == 0) {
      return(plotly_empty())
    }
    
    p <- ggplot(all_cuartiles, aes(x = Investigador, y = cantidad, fill = Cuartil)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Distribución de Publicaciones por Cuartil",
           x = "Investigador", y = "Número de Publicaciones") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c("Q1" = "#2E8B57", "Q2" = "#4682B4", 
                                   "Q3" = "#DAA520", "Q4" = "#CD853F", 
                                   "Sin cuartil" = "#808080"))
    
    ggplotly(p)
  })
  
  # Actualizar choices del selector de investigador
  observeEvent(comparativeData(), {
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    choices <- setNames(
      seq_along(successful_results),
      sapply(successful_results, function(x) x$nombre)
    )
    
    updateSelectInput(session, "selected_researcher", choices = choices)
  })
  
  # Tabla de detalles por investigador
  output$researcher_detail_table <- renderDT({
    req(comparativeData(), input$selected_researcher)
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    if (length(successful_results) == 0 || is.null(input$selected_researcher)) {
      return(data.frame())
    }
    
    selected_idx <- as.numeric(input$selected_researcher)
    selected_researcher <- successful_results[[selected_idx]]
    
    if (!is.null(selected_researcher$df_final)) {
      detail_table <- selected_researcher$df_final %>%
        select(-Revista_norm) %>%
        rename(
          `Año de Publicación` = `Ano de Produccion`,
          `Título` = Titulo,
          `Cuartil Original` = `Cuartil de ScimagoJR o JCR*`,
          `Cuartil` = Cuartil,
          `Valor` = Value
        )
      
      datatable(
        detail_table,
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      )
    } else {
      data.frame(Mensaje = "No hay datos de publicaciones disponibles")
    }
  })
  
  # Handler para descarga de Excel del investigador seleccionado
  output$download_researcher <- downloadHandler(
    filename = function() {
      req(comparativeData(), input$selected_researcher)
      
      results <- comparativeData()
      successful_results <- results[sapply(results, function(x) x$success)]
      
      if (length(successful_results) > 0 && !is.null(input$selected_researcher)) {
        selected_idx <- as.numeric(input$selected_researcher)
        selected_researcher <- successful_results[[selected_idx]]
        researcher_name <- gsub("[^A-Za-z0-9_-]", "_", selected_researcher$nombre)
        paste0("RENACYT_", researcher_name, "_", Sys.Date(), ".xlsx")
      } else {
        paste0("RENACYT_Investigador_", Sys.Date(), ".xlsx")
      }
    },
    content = function(file) {
      req(comparativeData(), input$selected_researcher)
      
      results <- comparativeData()
      successful_results <- results[sapply(results, function(x) x$success)]
      
      if (length(successful_results) == 0 || is.null(input$selected_researcher)) {
        return()
      }
      
      selected_idx <- as.numeric(input$selected_researcher)
      selected_researcher <- successful_results[[selected_idx]]
      
      # Crear workbook
      wb <- createWorkbook()
      
      # Hoja 1: Resumen del investigador
      addWorksheet(wb, "Resumen RENACYT")
      
      # Obtener valores de los inputs para el cálculo completo
      indice_h <- input[[paste0("indice_h_", selected_idx)]]
      libros <- input[[paste0("libros_", selected_idx)]]
      
      if (is.null(indice_h)) indice_h <- "No"
      if (is.null(libros)) libros <- 0
      
      # Calcular puntaje total
      total_puntaje <- selected_researcher$puntaje_formacion + 
        selected_researcher$puntaje_articulos + 
        selected_researcher$puntaje_propiedad + 
        selected_researcher$puntaje_asesor + libros
      
      # Calcular producción total
      prod_total <- selected_researcher$puntaje_articulos + 
        selected_researcher$puntaje_propiedad + libros
      
      # Obtener calificación
      calificacion <- Getcalificacion(
        value = total_puntaje,
        IndiceH = indice_h,
        prod_total = prod_total
      )
      
      # Datos del resumen
      resumen_data <- data.frame(
        Categoría = c(
          "Investigador",
          "URL CTIVITAE",
          "Fecha de Análisis",
          "",
          "PUNTAJES RENACYT",
          "Grado Académico",
          "Artículos Científicos",
          "Propiedad Intelectual",
          "Asesorías de Tesis",
          "Libros y Capítulos",
          "Índice H (>=10)",
          "",
          "TOTALES",
          "Puntaje Total",
          "Producción Total",
          "Total Publicaciones",
          "",
          "CALIFICACIÓN RENACYT",
          "Resultado"
        ),
        Valor = c(
          selected_researcher$nombre,
          selected_researcher$url,
          as.character(Sys.Date()),
          "",
          "",
          paste(selected_researcher$puntaje_formacion, "puntos"),
          paste(selected_researcher$puntaje_articulos, "puntos"),
          paste(selected_researcher$puntaje_propiedad, "puntos"),
          paste(selected_researcher$puntaje_asesor, "puntos"),
          paste(libros, "puntos"),
          indice_h,
          "",
          "",
          paste(total_puntaje, "puntos"),
          paste(prod_total, "puntos"),
          paste(selected_researcher$total_publicaciones, "publicaciones"),
          "",
          "",
          calificacion
        )
      )
      
      writeData(wb, "Resumen RENACYT", resumen_data)
      
      # Estilo para el resumen
      headerStyle <- createStyle(fontSize = 12, textDecoration = "bold", 
                                 fgFill = "#3c8dbc", fontColour = "white")
      sectionStyle <- createStyle(fontSize = 11, textDecoration = "bold", 
                                  fgFill = "#ecf0f5")
      
      # Aplicar estilos de forma individual para evitar errores de longitud
      addStyle(wb, "Resumen RENACYT", headerStyle, rows = 1, cols = 1)
      addStyle(wb, "Resumen RENACYT", headerStyle, rows = 1, cols = 2)
      
      # Aplicar estilo a las secciones
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 5, cols = 1)
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 5, cols = 2)
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 13, cols = 1)
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 13, cols = 2)
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 18, cols = 1)
      addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 18, cols = 2)
      
      # Hoja 2: Detalle de publicaciones
      if (!is.null(selected_researcher$df_final) && nrow(selected_researcher$df_final) > 0) {
        addWorksheet(wb, "Publicaciones")
        
        detail_table <- selected_researcher$df_final %>%
          select(-Revista_norm) %>%
          rename(
            `Año de Publicación` = `Ano de Produccion`,
            `Título` = Titulo,
            `Cuartil Original` = `Cuartil de ScimagoJR o JCR*`,
            `Cuartil` = Cuartil,
            `Valor` = Value
          )
        
        writeData(wb, "Publicaciones", detail_table, headerStyle = headerStyle)
        
        # Auto ajustar ancho de columnas
        setColWidths(wb, "Publicaciones", cols = 1:ncol(detail_table), widths = "auto")
      }
      
      # Hoja 3: Distribución por cuartiles (si existe)
      if (!is.null(selected_researcher$cuartiles) && nrow(selected_researcher$cuartiles) > 0) {
        addWorksheet(wb, "Cuartiles")
        
        cuartiles_data <- selected_researcher$cuartiles %>%
          rename(
            `Cuartil` = Cuartil,
            `Cantidad de Publicaciones` = cantidad
          )
        
        writeData(wb, "Cuartiles", cuartiles_data, headerStyle = headerStyle)
        setColWidths(wb, "Cuartiles", cols = 1:2, widths = "auto")
      }
      
      # Auto ajustar ancho de columnas en resumen
      setColWidths(wb, "Resumen RENACYT", cols = 1:2, widths = c(25, 30))
      
      # Guardar archivo
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}


# -------------------------------------------------------------
# Lanzar la aplicación
# -------------------------------------------------------------
shinyApp(ui, server)