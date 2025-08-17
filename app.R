# app.R
# =========================================================================================
# RenCal 2.1 — Versión corregida con validación robusta de valores NULL
# =========================================================================================

# ===== CONFIGURACIÓN INICIAL =====
# Forzar configuración UTF-8
Sys.setenv(LANG = "es_PE.UTF-8")
Sys.setlocale("LC_CTYPE", "es_PE.UTF-8")

# ===== GESTIÓN DE PAQUETES =====
required_packages <- c(
  "shiny", "shinydashboard", "rvest", "tidyverse",
  "readxl", "stringi", "DT", "curl", "httr2",
  "plotly", "openxlsx", "dplyr", "purrr"
)

load_packages <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message(paste("Instalando", pkg, "..."))
      install.packages(pkg, dependencies = TRUE)
    }
    library(pkg, character.only = TRUE)
  }
}
load_packages(required_packages)

# Operador null-coalescing personalizado
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# ===== MÓDULO DE NORMALIZACIÓN Y DEDUPLICACIÓN =====

# Normalización avanzada de títulos para detección de duplicados
normalize_titles_advanced <- function(titles) {
  if (is.null(titles) || length(titles) == 0) {
    return(list(normalized = character(0), keys = character(0)))
  }
  
  normalized <- titles %>%
    as.character() %>%
    gsub("&nbsp;|&amp;|&lt;|&gt;|&quot;|&#39;", " ", .) %>%
    gsub("([a-záéíóúñ])([A-ZÁÉÍÓÚÑ])", "\\1 \\2", ., perl = TRUE) %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    gsub("[^a-z0-9]", " ", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
  
  keys <- gsub(" ", "", normalized)
  list(normalized = normalized, keys = keys)
}

# Detectar y eliminar duplicados en publicaciones
deduplicate_publications <- function(df,
                                     year_col = "Ano de Produccion",
                                     title_col = "Titulo",
                                     value_col = "Value") {
  
  if (is.null(df) || nrow(df) == 0) {
    return(list(
      data = df,
      duplicates_removed = 0,
      duplicate_details = NULL,
      original_count = 0,
      final_count = 0
    ))
  }
  
  original_size <- nrow(df)
  
  title_norm <- normalize_titles_advanced(df[[title_col]])
  
  df_work <- df %>%
    mutate(
      .title_key = title_norm$keys,
      .year = suppressWarnings(as.integer(.data[[year_col]])),
      .value = suppressWarnings(as.numeric(.data[[value_col]])),
      .original_index = dplyr::row_number()
    )
  
  duplicate_groups <- df_work %>%
    group_by(.year, .title_key) %>%
    filter(n() > 1) %>%
    arrange(.year, .title_key, desc(.value)) %>%
    mutate(duplicate_rank = dplyr::row_number()) %>%
    ungroup()
  
  duplicate_details <- NULL
  if (nrow(duplicate_groups) > 0) {
    duplicate_details <- duplicate_groups %>%
      select(all_of(c(year_col, title_col, value_col)), duplicate_rank) %>%
      filter(duplicate_rank > 1)
  }
  
  df_dedup <- df_work %>%
    arrange(desc(.value)) %>%
    distinct(.year, .title_key, .keep_all = TRUE) %>%
    arrange(.original_index) %>%
    select(-starts_with("."))
  
  duplicates_removed <- original_size - nrow(df_dedup)
  
  list(
    data = df_dedup,
    duplicates_removed = duplicates_removed,
    duplicate_details = duplicate_details,
    original_count = original_size,
    final_count = nrow(df_dedup)
  )
}

# ===== MÓDULO DE EXTRACCIÓN DE DATOS =====

extraer_tabla_robusta <- function(page, texto_seccion) {
  tryCatch({
    raw_table <- page %>%
      html_node(xpath = paste0(
        "//*[contains(text(), '", texto_seccion, "')]/following::table[1]"
      )) %>%
      html_table(fill = TRUE)
    
    if (is.null(raw_table) || nrow(raw_table) == 0) return(NULL)
    
    if (nrow(raw_table) > 0) {
      colnames(raw_table) <- as.character(raw_table[1, ])
      raw_table <- raw_table[-1, , drop = FALSE]
    }
    
    if (nrow(raw_table) > 0) {
      colnames(raw_table) <- stringi::stri_trans_general(colnames(raw_table), "Latin-ASCII")
      raw_table <- raw_table %>%
        mutate(across(where(is.character),
                      ~ stringi::stri_trans_general(.x, "Latin-ASCII")))
    }
    
    raw_table
  }, error = function(e) {
    message(paste("Error extrayendo tabla para:", texto_seccion, "-", e$message))
    NULL
  })
}

extraer_nombre_investigador_mejorado <- function(page) {
  selectors <- list(
    list(selector = ".tituloNombreFicha2 span", method = "css"),
    list(selector = "h3", method = "css"),
    list(selector = "//h2[@class='nombre-investigador']", method = "xpath"),
    list(selector = "title", method = "css", extract = "title")
  )
  
  for (sel in selectors) {
    tryCatch({
      if (sel$method == "css") {
        nombre <- page %>% html_node(sel$selector) %>% html_text() %>% str_trim()
        if (!is.null(sel$extract) && sel$extract == "title") {
          nombre <- str_extract(nombre, "(?<=Investigador: ).*?(?=\\s*-|$)")
        }
        if (!is.null(nombre) && !is.na(nombre) && nchar(nombre) > 0) {
          return(nombre)
        }
      } else if (sel$method == "xpath") {
        nombre <- page %>% html_node(xpath = sel$selector) %>% html_text() %>% str_trim()
        if (!is.null(nombre) && nchar(nombre) > 0) return(nombre)
      }
    }, error = function(e) NULL)
  }
  
  "Investigador no identificado"
}

# ===== MÓDULO DE PROCESAMIENTO DE INVESTIGADORES =====

procesar_investigador_mejorado <- function(url_investigador,
                                           df_scopus,
                                           Scielo_Data,
                                           enable_deduplication = TRUE) {
  
  if (is.null(url_investigador) || !grepl("^https?://", url_investigador)) {
    return(list(success = FALSE, error = "URL inválida", url = url_investigador))
  }
  
  tryCatch({
    page <- read_html(url_investigador)
    
    nombre <- extraer_nombre_investigador_mejorado(page)
    
    asesor <- extraer_tabla_robusta(page, "Experiencia como Asesor de Tesis")
    formacion <- extraer_tabla_robusta(page, "Formación Académica (Fuente: SUNEDU)")
    produccion_raw <- extraer_tabla_robusta(page, "Producción científica")
    derechos_propiedad <- extraer_tabla_robusta(page, "Derechos de Propiedad Intelectual")
    
    # Propiedad intelectual
    puntaje_propiedad <- 0
    if (!is.null(derechos_propiedad) && "Tipo de PI" %in% colnames(derechos_propiedad)) {
      derechos_propiedad <- derechos_propiedad %>%
        mutate(Puntuacion = case_when(
          `Tipo de PI` %in% c(
            "Patente de invencion", "Certificado de Obtentor",
            "Paquete tecnologico", "Registro de certificado de obtentor"
          ) ~ 3L,
          `Tipo de PI` %in% c(
            "Patente de modelo de utilidad", "certificado de derecho de autor por software"
          ) ~ 1L,
          TRUE ~ 0L
        ))
      puntaje_propiedad <- sum(derechos_propiedad$Puntuacion, na.rm = TRUE)
    }
    
    # Formación
    # Formación
    puntaje_formacion <- 0
    grado_label <- NA_character_
    
    if (!is.null(formacion) && nrow(formacion) > 0) {
      formacion_scores <- formacion %>%
        mutate(
          score = dplyr::case_when(
            str_detect(Grado, regex("DOCTOR", ignore_case = TRUE)) ~ 10,
            str_detect(Grado, regex("MAGISTER|MAESTR|MASTER", ignore_case = TRUE)) ~ 6,
            str_detect(Grado, regex("LICENCIAD|TITULO PROFESIONAL", ignore_case = TRUE)) ~ 4,
            str_detect(Grado, regex("BACHILLER", ignore_case = TRUE)) ~ 2,
            str_detect(Grado, regex("CONSTANCIA DE MATRICULA|ESTUD", ignore_case = TRUE)) ~ 1,
            TRUE ~ 0
          ),
          nivel = dplyr::case_when(
            str_detect(Grado, regex("DOCTOR", ignore_case = TRUE)) ~ "Doctor",
            str_detect(Grado, regex("MAGISTER|MAESTR|MASTER", ignore_case = TRUE)) ~ "Magíster",
            str_detect(Grado, regex("LICENCIAD|TITULO PROFESIONAL", ignore_case = TRUE)) ~ "Licenciado",
            str_detect(Grado, regex("BACHILLER", ignore_case = TRUE)) ~ "Bachiller",
            str_detect(Grado, regex("CONSTANCIA DE MATRICULA|ESTUD", ignore_case = TRUE)) ~ "Estudiante",
            TRUE ~ "—"
          )
        )
      puntaje_formacion <- max(formacion_scores$score, na.rm = TRUE)
      grado_label <- formacion_scores %>% dplyr::filter(score == puntaje_formacion) %>% dplyr::slice(1) %>% dplyr::pull(nivel)
    }
    
    # Asesorías
    puntaje_asesor <- 0
    if (!is.null(asesor) && nrow(asesor) > 0) {
      puntaje_asesor <- asesor %>%
        mutate(score = case_when(
          str_detect(Tesis, regex("Doctorado", ignore_case = TRUE)) ~ 2,
          str_detect(Tesis, regex("Magister", ignore_case = TRUE)) ~ 1,
          str_detect(Tesis, regex("Bachiller|Titulo Profesional|Licenciado", ignore_case = TRUE)) ~ 0.5,
          TRUE ~ 0
        )) %>%
        summarise(total = sum(score, na.rm = TRUE)) %>%
        mutate(total = pmin(total, 10)) %>%
        pull(total)
    }
    
    # Producción científica
    df_final <- NULL
    puntaje_articulos <- 0
    total_publicaciones <- 0
    cuartiles_count <- NULL
    dedup_info <- list(duplicates_removed = 0, original_count = 0, final_count = 0, duplicate_details = NULL)
    
    if (!is.null(produccion_raw) && nrow(produccion_raw) > 0) {
      # Normaliza strings
      produccion <- produccion_raw %>% mutate(across(where(is.character), enc2utf8))
      
      # Norma revistas y filtra tipos que NO son artículos/capítulos
      produccion_norm <- produccion %>%
        mutate(Revista_norm = tolower(stringi::stri_trans_general(Revista, "Latin-ASCII"))) %>%
        filter(!(`Tipo Produccion` %in% c(
          "DoctoralThesis", "MasterThesis", "Note", "Editorial",
          "Letter", "Journal - Meeting Abstract"
        )))
      
      # SCOPUS normalizado y desinflado a relación many-to-one (revista-año)
      df_scopus_norm <- df_scopus %>%
        mutate(Revista_norm = tolower(stringi::stri_trans_general(Revista, "Latin-ASCII"))) %>%
        group_by(Revista_norm, year) %>%
        slice_max(order_by = Valor, n = 1, with_ties = FALSE) %>%
        ungroup()
      
      if (nrow(produccion_norm) > 0) {
        # Resumen base (sin joins que dupliquen)
        resumen <- produccion_norm %>%
          select(Revista_norm, `Ano de Produccion`, Titulo, `Cuartil de ScimagoJR o JCR*`)
        
        # Empareja por revista + año (ajuste 2024/2025 → 2024)
        data_joined2 <- resumen %>%
          mutate(
            AnioProd  = suppressWarnings(as.numeric(`Ano de Produccion`)),
            join_year = if_else(AnioProd %in% c(2024, 2025), 2024, AnioProd)
          ) %>%
          left_join(
            df_scopus_norm %>% rename(join_year = year),
            by = c("Revista_norm", "join_year")  # many-to-one
          )
        
        # Tabla previa a deduplicar (SCOPUS ya aplicado)
        df_final_pre <- data_joined2 %>%
          select(Revista_norm, `Ano de Produccion`, Titulo,
                 `Cuartil de ScimagoJR o JCR*`, Cuartil, Valor) %>%
          # Completar NAs para no perder filas antes del fallback Scielo
          mutate(
            Cuartil = tidyr::replace_na(Cuartil, "No Cuartil"),
            Valor   = tidyr::replace_na(Valor, 0)
          )
        
        # --- Fallback SCIELO para "No Cuartil" ---
        Scielo_Data_norm <- Scielo_Data %>%
          mutate(
            Revista = tolower(Revista),
            Revista = gsub("[[:punct:]]", "", Revista),
            Revista = trimws(Revista)
          ) %>%
          count(Revista, name = "n_matches")
        
        df_final_pre <- df_final_pre %>%
          # Alinear normalización para el join con Scielo
          mutate(Revista_norm_join = gsub("[[:punct:]]", "", Revista_norm),
                 Revista_norm_join = trimws(Revista_norm_join)) %>%
          left_join(Scielo_Data_norm %>% rename(Revista_norm_join = Revista),
                    by = "Revista_norm_join") %>%
          mutate(
            n_matches = dplyr::coalesce(n_matches, 0L),
            Value     = if_else(Cuartil == "No Cuartil", pmin(n_matches, 10L), Valor),
            Value     = dplyr::coalesce(Value, 0)
          ) %>%
          select(-Revista_norm_join, -n_matches)
        
        # --- Deduplicación (mismo universo: antes=después) ---
        if (enable_deduplication && nrow(df_final_pre) > 0) {
          df_final_pre_unique <- df_final_pre %>% distinct()
          pre_dedup_count <- nrow(df_final_pre_unique)          # base correcta de "originales"
          
          dedup_result <- deduplicate_publications(
            df_final_pre_unique,
            year_col  = "Ano de Produccion",
            title_col = "Titulo",
            value_col = "Value"
          )
          
          df_final <- dedup_result$data
          
          dedup_info <- list(
            duplicates_removed = dedup_result$duplicates_removed, # usar el real
            original_count     = pre_dedup_count,                 # universo correcto
            final_count        = nrow(df_final),
            duplicate_details  = dedup_result$duplicate_details
          )
          
          if ((dedup_info$duplicates_removed %||% 0) > 0) {
            message(sprintf(
              "Investigador '%s': %d duplicados removidos de %d publicaciones (pre-dedup).",
              nombre,
              dedup_info$duplicates_removed,
              dedup_info$original_count
            ))
          }
        } else {
          df_final <- df_final_pre %>% distinct()
          dedup_info <- list(
            duplicates_removed = 0,
            original_count     = nrow(df_final),
            final_count        = nrow(df_final),
            duplicate_details  = NULL
          )
        }
        
        # Puntajes y agregados
        puntaje_articulos   <- sum(df_final$Value, na.rm = TRUE)
        total_publicaciones <- nrow(df_final)
        
        cuartiles_count <- df_final %>%
          count(Cuartil, name = "cantidad") %>%
          tidyr::replace_na(list(Cuartil = "Sin cuartil"))
      }
    }
    
    list(
      nombre = nombre,
      url = url_investigador,
      puntaje_formacion = puntaje_formacion,
      grado_label = grado_label,              # <--- NUEVO
      puntaje_articulos = puntaje_articulos,
      puntaje_propiedad = puntaje_propiedad,
      puntaje_asesor = puntaje_asesor,
      total_publicaciones = total_publicaciones,
      cuartiles = cuartiles_count,
      df_final = df_final,
      deduplication_info = dedup_info,
      success = TRUE
    )
    
    
  })
}

# ===== FUNCIONES DE CALIFICACIÓN RENACYT (CORREGIDAS) =====

GetPuntajeSum <- function(Grado = 0, Articulos = 0, Patentes = 0,
                          Libros = 0, Asesorias = 0) {
  # Asegurar que todos los valores sean numéricos y no NULL
  Grado <- if(is.null(Grado) || length(Grado) == 0 || is.na(Grado)) 0 else as.numeric(Grado)
  Articulos <- if(is.null(Articulos) || length(Articulos) == 0 || is.na(Articulos)) 0 else as.numeric(Articulos)
  Patentes <- if(is.null(Patentes) || length(Patentes) == 0 || is.na(Patentes)) 0 else as.numeric(Patentes)
  Libros <- if(is.null(Libros) || length(Libros) == 0 || is.na(Libros)) 0 else as.numeric(Libros)
  Asesorias <- if(is.null(Asesorias) || length(Asesorias) == 0 || is.na(Asesorias)) 0 else as.numeric(Asesorias)
  
  Grado + Articulos + Patentes + Libros + Asesorias
}

Getcalificacion <- function(value = 0, IndiceH = "No", prod_total = 0) {
  # Validación robusta de parámetros
  if(is.null(value) || length(value) == 0 || is.na(value)) {
    value <- 0
  } else {
    value <- as.numeric(value)
  }
  
  if(is.null(prod_total) || length(prod_total) == 0 || is.na(prod_total)) {
    prod_total <- 0
  } else {
    prod_total <- as.numeric(prod_total)
  }
  
  if(is.null(IndiceH) || length(IndiceH) == 0 || is.na(IndiceH)) {
    IndiceH <- "No"
  } else {
    IndiceH <- as.character(IndiceH)
  }
  
  # Lógica de calificación
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
  } else if (IndiceH == "Sí" && value >= 200) {
    "Investigador Distinguido"
  } else {
    "Sí califica: Nivel I"
  }
}

# ===== INTERFAZ DE USUARIO (UI) =====
ui <- dashboardPage(
  dashboardHeader(title = "RenCal 2.1", titleWidth = 300),
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
        
    /* Cards de métricas */
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
        
    /* Botones */
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
        
    /* Inputs */
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
        
    /* Labels */
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
        
    /* Select */
    select.form-control {
      background-image: linear-gradient(45deg, transparent 50%, var(--primary-color) 50%), 
                       linear-gradient(135deg, var(--primary-color) 50%, transparent 50%);
      background-position: calc(100% - 20px) calc(1em + 2px), calc(100% - 15px) calc(1em + 2px);
      background-size: 5px 5px, 5px 5px;
      background-repeat: no-repeat;
    }
        
    /* Tabs */
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
        
    /* Alertas */
    .alert {
      border: none;
      border-radius: 15px;
      padding: 20px;
      font-weight: 500;
    }
        
    /* Responsive */
    @media (max-width: 768px) {
      .comparative-card { margin: 10px 0; padding: 20px; }
      .metric-value      { font-size: 2em; }
      .box-body          { padding: 15px; }
    }
        
    /* Animaciones */
    @keyframes fadeInUp {
      from { opacity: 0; transform: translateY(30px); }
      to   { opacity: 1; transform: translateY(0); }
    }
        
    .box { animation: fadeInUp 0.6s ease-out; }
        
    /* Loading spinner */
    .shiny-spinner-output-container {
      background: rgba(255,255,255,0.9);
      border-radius: 15px;
    }
        
    /* Plotly */
    .plotly {
      border-radius: 15px;
      overflow: hidden;
      box-shadow: var(--card-shadow);
    }

    /* DataTables fixes */
    .dataTables_wrapper .dataTables_scrollHeadInner,
    .dataTables_wrapper .dataTables_scrollHeadInner table,
    .dataTables_wrapper .dataTables_scrollBody table,
    table.dataTable {
      width: 100% !important;
      margin: 0 !important;
    }

    .dataTables_scrollHead { 
      overflow: hidden !important; 
    }
  "))
    ),
    
    tabItems(
      # Pestaña: Información RENACYT
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
                                h3("RenCal 2.1", style = "color: #2d3748; font-weight: 700; margin: 0; font-size: 2.5em;"),
                                p("Calculadora RENACYT Inteligente - Versión Mejorada", style = "color: #4a5568; font-size: 1.2em; margin: 5px 0 0 0;")
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
                                      h5("Deduplicación Inteligente", style = "color: #2d3748; font-weight: 600; margin: 0;")
                                  ),
                                  p("Sistema avanzado de detección y eliminación de publicaciones duplicadas con normalización robusta de títulos.",
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
      
      # Pestaña: Calculadora RENACYT
      tabItem(tabName = "calculadora",
              fluidRow(
                box(
                  width = 12, title = "🧮 Calculadora RENACYT - Análisis Individual y Comparativo",
                  status = "primary", solidHeader = TRUE,
                  div(style = "padding: 10px;",
                      div(style = "background: linear-gradient(135deg, #f8fafc 0%, #e2e8f0 100%);
                                   border-radius: 15px; padding: 25px; margin-bottom: 20px;",
                          h4("🚀 ¡Bienvenido a RenCal 2.1!", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                          p("Analiza de forma automática los puntajes RENACYT de investigadores peruanos con deduplicación inteligente.",
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
                          ),
                          tags$div(
                            class = "alert alert-info",
                            style = "margin-top: 20px; background: linear-gradient(135deg, #8e7cc3 0%, #b29bf5 100%); 
                                     border: none; border-radius: 10px; padding: 15px;",
                            tags$strong("✨ Nuevo en v2.1"),
                            " Ahora el sistema detecta y elimina publicaciones duplicadas basándose en título normalizado y año."
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
                          checkboxInput(
                            "enable_dedup",
                            "🔄 Activar eliminación automática de duplicados",
                            value = TRUE
                          ),
                          div(style = "text-align: center; margin-top: 20px;",
                              actionButton("run_comparative",
                                           HTML("🔍 Ejecutar Análisis"),
                                           class = "btn btn-primary",
                                           style = "font-size: 1.1em; padding: 15px 40px;")
                          )
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
                             h4("📊 Estadísticas Generales", style = "color: #f8fafc; font-weight: 600; margin-bottom: 25px;"),
                             fluidRow(
                               column(3,
                                      div(class = "comparative-card", style = "text-align: center;",
                                          div(style = "font-size: 3em; margin-bottom: 10px;", "👥"),
                                          div(class = "metric-value", textOutput("total_investigators")),
                                          div(class = "metric-label", "Investigadores Analizados")
                                      )),
                               column(3,
                                      div(class = "comparative-card", style = "text-align: center;",
                                          div(style = "font-size: 3em; margin-bottom: 10px;", "📚"),
                                          div(class = "metric-value", textOutput("avg_publications")),
                                          div(class = "metric-label", "Promedio de Publicaciones")
                                      )),
                               column(3,
                                      div(class = "comparative-card", style = "text-align: center;",
                                          div(style = "font-size: 3em; margin-bottom: 10px;", "🏆"),
                                          div(class = "metric-value", textOutput("top_score")),
                                          div(class = "metric-label", "Puntaje Máximo")
                                      )),
                               column(3,
                                      div(class = "comparative-card", style = "text-align: center;",
                                          div(style = "font-size: 3em; margin-bottom: 10px;", "📈"),
                                          div(class = "metric-value", textOutput("avg_score")),
                                          div(class = "metric-label", "Puntaje Promedio")
                                      ))
                             ),
                             br(),
                             fluidRow(
                               column(12,
                                      div(class = "comparative-card",
                                          h5("📊 Resumen de Artículos Duplicados", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                                          uiOutput("deduplication_summary")
                                      ))
                             ),
                             fluidRow(
                               column(
                                 12,
                                 div(
                                   style = "text-align: center; margin-top: 10px;",
                                   actionButton("openDupModal", "👀 Ver duplicados (todos los investigadores)", class = "btn btn-success")
                                 )
                               )
                             )
                    ),
                    tabPanel(HTML("🏅 Calificación RENACYT"),
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
                                          selectInput("selected_researcher", label = NULL, choices = NULL)
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
                             div(
                               style = "text-align: center; margin: 10px 0 0 0;",
                               actionButton("openDupModalOne", "👀 Ver duplicados (solo este investigador)", class = "btn btn-success")
                             ),
                             DTOutput("researcher_detail_table"),
                             br(),
                             conditionalPanel(
                               condition = "input.selected_researcher",
                               div(class = "comparative-card",
                                   h5("📋 Información de Deduplicación", style = "color: #2d3748; font-weight: 600; margin-bottom: 15px;"),
                                   uiOutput("researcher_dedup_info")
                               )
                             )
                    )
                  )
                )
              )
      ),
      
      # Pestaña: Acerca del autor
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
                              style = "color: #64748b; font-style: italic; margin: 0;"),
                            p("Versión 2.1 - Mejoras en deduplicación y arquitectura",
                              style = "color: #64748b; font-size: 0.9em; margin-top: 10px;")
                        )
                    )
                )
              )
      )
    )
  )
)

# ===== LÓGICA DEL SERVIDOR =====
server <- function(input, output, session) {
  
  # Cache para datos de referencia
  reference_data <- reactiveValues(
    df_scopus = NULL,
    Scielo_Data = NULL,
    loaded = FALSE
  )
  
  # Cargar datos de referencia una sola vez
  observe({
    if (!reference_data$loaded) {
      tryCatch({
        reference_data$df_scopus <- readxl::read_excel("df_scopus.xlsx")
        reference_data$Scielo_Data <- readxl::read_excel("Scielo_Data.xlsx")
        reference_data$loaded <- TRUE
      }, error = function(e) {
        showNotification(
          paste("❌ Error cargando datos de referencia:", e$message),
          type = "error",
          duration = NULL
        )
      })
    }
  })
  
  # Render de la imagen en Información RENACYT
  output$image1 <- renderImage({
    path <- normalizePath(file.path("www", "anexo1.png"), mustWork = FALSE)
    if (!file.exists(path)) {
      validate(need(FALSE, "⚠️ No se encontró la imagen 'www/anexo1.png'. Coloca el archivo en la carpeta 'www/'."))
      return(NULL)
    }
    list(
      src = path,
      contentType = 'image/png',
      width = 950,
      height = 750,
      alt = "Anexo Nº 1 RENACYT"
    )
  }, deleteFile = FALSE)
  
  # ===== ANÁLISIS COMPARATIVO - FUNCIONALIDAD PRINCIPAL =====
  comparativeData <- eventReactive(input$run_comparative, {
    req(input$urls_multiple)
    req(reference_data$loaded)
    
    urls <- str_split(input$urls_multiple, "\n")[[1]] %>%
      str_trim() %>%
      .[. != ""]
    
    invalid_urls <- urls[!grepl("^https?://", urls)]
    if (length(invalid_urls) > 0) {
      showNotification(
        paste("❌ URLs inválidas detectadas:", paste(invalid_urls, collapse = ", ")),
        type = "error",
        duration = 10
      )
      return(NULL)
    }
    
    if (length(urls) == 0) {
      showNotification("⚠️ Por favor ingresa al menos una URL", type = "warning")
      return(NULL)
    }
    
    withProgress(message = "Analizando investigadores...", value = 0, {
      resultados <- list()
      total_dup_removed <- 0
      
      for (i in seq_along(urls)) {
        incProgress(1 / length(urls), detail = paste("Procesando investigador", i, "de", length(urls)))
        
        res <- procesar_investigador_mejorado(
          urls[i],
          reference_data$df_scopus,
          reference_data$Scielo_Data,
          enable_deduplication = input$enable_dedup
        )
        
        if (!is.null(res$deduplication_info)) {
          total_dup_removed <- total_dup_removed + (res$deduplication_info$duplicates_removed %||% 0)
        }
        
        resultados[[i]] <- res
      }
      
      if (input$enable_dedup && total_dup_removed > 0) {
        showNotification(
          paste("✅ Se removieron", total_dup_removed, "publicaciones duplicadas en total"),
          type = "message",
          duration = 8
        )
      }
      
      resultados
    })
  })
  
  # Resumen de deduplicación
  output$deduplication_summary <- renderUI({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) isTRUE(x$success))]
    
    if (length(successful_results) == 0)
      return(p("No hay datos disponibles"))
    
    # Tomar SIEMPRE los números del módulo de deduplicación
    total_original <- sum(sapply(successful_results, function(x) {
      x$deduplication_info$original_count %||% 0
    }))
    
    total_final <- sum(sapply(successful_results, function(x) {
      x$deduplication_info$final_count %||% 0
    }))
    
    total_removed <- sum(sapply(successful_results, function(x) {
      x$deduplication_info$duplicates_removed %||% 0
    }))
    
    pct <- ifelse(total_original > 0, (total_removed / total_original) * 100, 0)
    
    if (isTRUE(input$enable_dedup)) {
      tagList(
        p(style = "color: #2d3748;", tags$strong("Estado:"), " Activado ✅"),
        p(style = "color: #4a5568;", tags$strong("Publicaciones originales totales:"), total_original),
        p(style = "color: #4a5568;", tags$strong("Publicaciones finales totales:"), total_final),
        p(style = "color: #e53e3e; font-weight: 600;",
          tags$strong("Total de duplicados eliminados:"), total_removed,
          sprintf(" (%.1f%% del total)", pct))
      )
    } else {
      tagList(
        p(style = "color: #2d3748;", tags$strong("Estado:"), " Desactivado ⚠️"),
        p(style = "color: #4a5568;",
          "Los duplicados no se eliminan automáticamente. Los conteos mostrados corresponden al estado sin deduplicación.")
      )
    }
  })
  
  
  # === REACTIVE con duplicados globales para el modal (todos)
  dups_global <- reactive({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0) return(NULL)
    
    dfs <- lapply(successful_results, function(res) {
      dd <- res$deduplication_info$duplicate_details
      if (!is.null(dd) && is.data.frame(dd) && nrow(dd) > 0) {
        dd$Investigador <- res$nombre
        dd[, c("Investigador", setdiff(names(dd), "Investigador")), drop = FALSE]
      } else {
        NULL
      }
    })
    dplyr::bind_rows(dfs)
  })
  
  # Modal "👀 Ver duplicados (todos los investigadores)"
  observeEvent(input$openDupModal, {
    showModal(modalDialog(
      title = "Duplicados eliminados — Todos los investigadores",
      size = "l",
      easyClose = TRUE,
      div(style = "margin-bottom: 10px;",
          p("Lista completa de filas eliminadas por la deduplicación. (Criterio: título normalizado + año, se conserva el de mayor 'Valor').")
      ),
      DTOutput("dups_table"),
      footer = modalButton("Cerrar")
    ))
  })
  
  output$dups_table <- DT::renderDT({
    df <- dups_global()
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "No hubo duplicados eliminados."),
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      ))
    }
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
  })
  
  # Información de deduplicación por investigador
  output$researcher_dedup_info <- renderUI({
    req(comparativeData(), input$selected_researcher)
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0 || is.null(input$selected_researcher)) return(NULL)
    
    selected_idx <- as.numeric(input$selected_researcher)
    selected_researcher <- successful_results[[selected_idx]]
    
    if (is.null(selected_researcher$deduplication_info)) {
      return(p(style = "color: #718096;", "No hay información de deduplicación disponible"))
    }
    
    dedup_info <- selected_researcher$deduplication_info
    
    tagList(
      p(style = "color: #4a5568;",
        tags$strong("Publicaciones originales:"),
        dedup_info$original_count %||% selected_researcher$total_publicaciones
      ),
      p(style = "color: #4a5568;",
        tags$strong("Publicaciones finales:"),
        dedup_info$final_count %||% selected_researcher$total_publicaciones
      ),
      p(style = "color: #e53e3e; font-weight: 600;",
        tags$strong("Duplicados removidos:"),
        dedup_info$duplicates_removed %||% 0
      ),
      if (!is.null(dedup_info$duplicate_details) &&
          is.data.frame(dedup_info$duplicate_details) &&
          nrow(dedup_info$duplicate_details) > 0) {
        tagList(
          hr(),
          p(tags$strong("Ejemplos de duplicados eliminados:"),
            style = "color: #2d3748; margin-bottom: 10px;"),
          tags$ul(
            lapply(seq_len(min(3, nrow(dedup_info$duplicate_details))), function(i) {
              dup <- dedup_info$duplicate_details[i, ]
              titulo <- if ("Titulo" %in% names(dup)) {
                substr(as.character(dup$Titulo), 1, 50)
              } else {
                "Título no disponible"
              }
              ano <- if ("Ano de Produccion" %in% names(dup)) {
                as.character(dup$`Ano de Produccion`)
              } else {
                "N/A"
              }
              valor <- if ("Value" %in% names(dup)) {
                as.character(dup$Value)
              } else {
                "N/A"
              }
              tags$li(
                style = "color: #718096; font-size: 0.9em;",
                sprintf("'%s' (Año: %s, Valor: %s)", titulo, ano, valor)
              )
            })
          )
        )
      }
    )
  })
  
  # Modal "👀 Ver duplicados (solo este investigador)"
  observeEvent(input$openDupModalOne, {
    req(comparativeData(), input$selected_researcher)
    showModal(modalDialog(
      title = "Duplicados eliminados — Investigador seleccionado",
      size = "l",
      easyClose = TRUE,
      DTOutput("dups_table_one"),
      footer = modalButton("Cerrar")
    ))
  })
  
  output$dups_table_one <- DT::renderDT({
    req(comparativeData(), input$selected_researcher)
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0) {
      return(DT::datatable(data.frame(Mensaje = "No hay datos disponibles"), rownames = FALSE))
    }
    idx <- as.numeric(input$selected_researcher)
    sel <- successful_results[[idx]]
    df <- sel$deduplication_info$duplicate_details
    if (is.null(df) || nrow(df) == 0) {
      return(DT::datatable(
        data.frame(Mensaje = "Este investigador no tuvo duplicados eliminados."),
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      ))
    }
    DT::datatable(
      df,
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
  })
  
  # Reactivo para indicar si los resultados están listos
  output$comparative_results_ready <- reactive({
    !is.null(comparativeData())
  })
  outputOptions(output, "comparative_results_ready", suspendWhenHidden = FALSE)
  
  # Tabla resumen comparativa
  output$comparative_summary_table <- DT::renderDT({
    req(comparativeData())
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0) {
      return(data.frame(Mensaje = "No se pudieron procesar los investigadores"))
    }
    
    summary_data <- purrr::map_dfr(successful_results, function(res) {
      dup_info <- ""
      if (!is.null(res$deduplication_info) && res$deduplication_info$duplicates_removed > 0) {
        dup_info <- paste0(" (", res$deduplication_info$duplicates_removed, " dup. eliminados)")
      }
      data.frame(
        Investigador            = res$nombre,
        `Formación Académica`   = res$grado_label %||% "-",
        `Artículos Científicos` = res$total_publicaciones %||% 0,  # cantidad (no puntaje)
        `Propiedad Intelectual` = res$puntaje_propiedad,
        `Asesorías`             = res$puntaje_asesor,
        `Total Publicaciones`   = paste0(res$total_publicaciones %||% 0, dup_info),
        URL                     = res$url,
        check.names = FALSE
      )
    })
    
    # Índices 0-based para DataTables
    idx_url <- which(names(summary_data) == "URL") - 1
    center_targets <- setdiff(1:(ncol(summary_data) - 1), idx_url) # todas menos col 0 (Investigador) y URL
    
    DT::datatable(
      summary_data,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        autoWidth  = TRUE,
        scrollX    = TRUE,
        columnDefs = list(
          list(visible = FALSE, targets = idx_url),         # ocultar URL
          list(className = 'dt-left',   targets = 0),       # Investigador a la izquierda
          list(className = 'dt-center', targets = center_targets)  # resto centradas
        ),
        initComplete = DT::JS(
          "function(settings, json) {
           $(this.api().table().container()).css('background-color', 'white');
           $(this.api().table().node()).css('background-color', 'white');
           $(this.api().table().body()).css('background-color', 'white');
         }"
        )
      )
    ) %>%
      DT::formatStyle(
        columns = 1:ncol(summary_data),
        backgroundColor = 'white'
      ) %>%
      # Refuerzo por si algún CSS externo pisa los className
      DT::formatStyle(
        columns = setdiff(names(summary_data), c("Investigador", "URL")),
        textAlign = 'center'
      ) %>%
      DT::formatStyle(columns = "Investigador", textAlign = 'left')
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
    max(sapply(successful_results, function(res) {
      res$puntaje_formacion + res$puntaje_articulos + res$puntaje_propiedad + res$puntaje_asesor
    }))
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
  
  # === TABLA: Calificaciones RENACYT con inputs en columnas (DINÁMICA Y REACTIVA) ===
  # === TABLA: Calificaciones RENACYT con inputs en columnas (VERSIÓN CORREGIDA) ===
  # === TABLA: Calificaciones RENACYT con inputs en columnas (VERSIÓN CORREGIDA) ===
  output$renacyt_comparative_table <- DT::renderDT({
    req(comparativeData())
    
    # Hacer la tabla reactiva a TODOS los inputs
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    n <- length(successful_results)
    if (n == 0) return(data.frame(Mensaje = "No hay investigadores procesados"))
    
    # Forzar reactividad para todos los inputs
    for(i in seq_len(n)) {
      input[[paste0("libros_", i)]]
      input[[paste0("indice_h_", i)]]
    }
    
    # También reaccionar al botón de actualizar
    input$update_renacyt
    
    # Generar columnas con inputs
    libros_inputs <- character(n)
    indice_inputs <- character(n)
    
    for(i in seq_len(n)) {
      # Obtener valor actual o default
      libros_val <- input[[paste0("libros_", i)]]
      if(is.null(libros_val)) libros_val <- 0
      
      indice_val <- input[[paste0("indice_h_", i)]]
      if(is.null(indice_val)) indice_val <- "No"
      
      # Crear los inputs HTML
      libros_inputs[i] <- as.character(
        numericInput(
          paste0("libros_", i), 
          label = NULL, 
          value = libros_val,
          min = 0, 
          max = 100, 
          step = 1, 
          width = "100px"
        )
      )
      
      indice_inputs[i] <- as.character(
        selectInput(
          paste0("indice_h_", i), 
          label = NULL, 
          choices = c("No", "Sí"),
          selected = indice_val, 
          width = "110px"
        )
      )
    }
    
    # Calcular totales usando valores actuales de inputs con validación robusta
    renacyt_rows <- purrr::map_dfr(seq_len(n), function(i) {
      res <- successful_results[[i]]
      
      # Obtener valores actuales de los inputs con validación robusta
      libros_val <- input[[paste0("libros_", i)]]
      libros <- if(is.null(libros_val) || length(libros_val) == 0 || is.na(libros_val)) {
        0
      } else {
        val <- suppressWarnings(as.numeric(libros_val))
        if(is.na(val)) 0 else val
      }
      
      indice_h_val <- input[[paste0("indice_h_", i)]]
      indice_h <- if(is.null(indice_h_val) || length(indice_h_val) == 0) {
        "No"
      } else {
        as.character(indice_h_val)
      }
      
      # Asegurar que los puntajes no sean NULL
      puntaje_formacion <- if(is.null(res$puntaje_formacion) || is.na(res$puntaje_formacion)) {
        0
      } else {
        res$puntaje_formacion
      }
      
      puntaje_articulos <- if(is.null(res$puntaje_articulos) || is.na(res$puntaje_articulos)) {
        0
      } else {
        res$puntaje_articulos
      }
      
      puntaje_propiedad <- if(is.null(res$puntaje_propiedad) || is.na(res$puntaje_propiedad)) {
        0
      } else {
        res$puntaje_propiedad
      }
      
      puntaje_asesor <- if(is.null(res$puntaje_asesor) || is.na(res$puntaje_asesor)) {
        0
      } else {
        res$puntaje_asesor
      }
      
      # Calcular totales
      total_puntaje <- puntaje_formacion + puntaje_articulos + puntaje_propiedad + puntaje_asesor + libros
      prod_total <- puntaje_articulos + puntaje_propiedad + libros
      
      # Obtener calificación
      calificacion <- Getcalificacion(value = total_puntaje, IndiceH = indice_h, prod_total = prod_total)
      
      # Crear fila de datos
      data.frame(
        Investigador = res$nombre,
        `Grado Académico` = puntaje_formacion,
        `Artículos Científicos` = puntaje_articulos,
        `Propiedad Intelectual` = puntaje_propiedad,
        `Asesorías` = puntaje_asesor,
        `Libros/Capítulos` = libros_inputs[i],
        `Índice H` = indice_inputs[i],
        `Puntaje Total` = total_puntaje,
        `Calificación RENACYT` = calificacion,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    })
    
    # Crear la tabla con opciones mejoradas para hacerla dinámica
    DT::datatable(
      renacyt_rows,
      escape = FALSE,  # Necesario para que se muestren los inputs HTML
      rownames = FALSE,
      selection = 'none',  # Desactivar selección de filas
      extensions = c('ColReorder', 'FixedColumns'),  # Extensiones para funcionalidad adicional
      options = list(
        pageLength = 15,
        autoWidth = TRUE,
        scrollX = TRUE,
        scrollY = "500px",
        
        # Configuración del DOM sin botones
        dom = 'lfrtip',
        
        # Habilitar ordenamiento
        ordering = TRUE,
        
        # Configurar columnas ordenables (todas excepto las que tienen inputs)
        columnDefs = list(
          list(orderable = FALSE, targets = c(5, 6)),  # Libros/Capítulos e Índice H no ordenables
          list(className = 'dt-center', targets = c(1, 2, 3, 4, 7)),  # Centrar números
          list(className = 'dt-left', targets = 0),  # Alinear nombres a la izquierda
          list(type = 'num', targets = c(1, 2, 3, 4, 7))  # Especificar que son números para ordenamiento correcto
        ),
        
        # Configuración de reordenamiento de columnas
        colReorder = TRUE,
        
        # Configuración de columnas fijas (opcional)
        fixedColumns = list(
          leftColumns = 1  # Fijar la primera columna (Investigador)
        ),
        
        # Opciones de búsqueda
        search = list(
          search = '',
          smart = TRUE,
          regex = FALSE,
          caseInsensitive = TRUE
        ),
        
        # Configuración de paginación mejorada
        pagingType = 'full_numbers',
        lengthMenu = list(
          c(10, 15, 25, 50, 100, -1),
          c('10', '15', '25', '50', '100', 'Todos')
        ),
        
        # Configuración de idioma en español (CORREGIDA - usando backticks)
        language = list(
          lengthMenu = "Mostrar _MENU_ registros",
          zeroRecords = "No se encontraron resultados",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 a 0 de 0 registros",
          infoFiltered = "(filtrado de _MAX_ registros totales)",
          search = "Buscar:",
          paginate = list(
            first = "Primero",
            last = "Último",
            `next` = "Siguiente",
            previous = "Anterior"
          ),
          processing = "Procesando...",
          loadingRecords = "Cargando...",
          emptyTable = "No hay datos disponibles en la tabla",
          thousands = ",",
          decimal = ".",
          buttons = list(
            copy = "Copiar",
            colvis = "Visibilidad de columnas",
            collection = "Colección",
            colvisRestore = "Restaurar columnas",
            copyKeys = "Presione ctrl o cmd + C para copiar los datos de la tabla al portapapeles.<br><br>Para cancelar, haga clic en este mensaje o presione escape.",
            copySuccess = list(
              `1` = "Copiada 1 fila al portapapeles",
              `_` = "Copiadas %d filas al portapapeles"
            ),
            copyTitle = "Copiar al portapapeles",
            pageLength = list(
              `-1` = "Mostrar todas las filas",
              `_` = "Mostrar %d filas"
            )
          )
        ),
        
        # Callbacks para manejar los inputs de Shiny
        drawCallback = DT::JS("
        function(settings) {
          // Re-vincular los inputs de Shiny después del redibujado
          Shiny.bindAll(this.api().table().node());
          
          // Aplicar estilos a los headers para indicar que son ordenables
          $(this.api().table().header()).find('th').each(function(index) {
            if(index !== 5 && index !== 6) {  // No para columnas con inputs
              $(this).css('cursor', 'pointer');
              $(this).hover(
                function() { $(this).css('background-color', '#f0f0f0'); },
                function() { $(this).css('background-color', ''); }
              );
            }
          });
        }
      "),
        
        initComplete = DT::JS(
          "function(settings, json) {
          // Estilos generales
          $(this.api().table().container()).css('background-color', 'white');
          $(this.api().table().node()).css('background-color', 'white');
          $(this.api().table().body()).css('background-color', 'white');
          $('.dataTables_wrapper').css('background-color', 'white');
          
          // Vincular inputs de Shiny
          Shiny.bindAll(this.api().table().node());
          
          // Añadir tooltip a las columnas ordenables
          $(this.api().table().header()).find('th').each(function(index) {
            if(index !== 5 && index !== 6) {
              $(this).attr('title', 'Click para ordenar');
            }
          });
        }"
        ),
        
        # Callback para manejar cambios de página
        preDrawCallback = DT::JS("
        function() {
          // Desvincular inputs antes de redibujar
          Shiny.unbindAll(this.api().table().node());
        }
      ")
      )
    ) %>%
      DT::formatStyle(
        columns = 1:ncol(renacyt_rows),
        backgroundColor = 'white'
      ) %>%
      DT::formatStyle(
        columns = c("Grado Académico", "Artículos Científicos", 
                    "Propiedad Intelectual", "Asesorías", "Puntaje Total"),
        textAlign = 'center'
      ) %>%
      DT::formatStyle(
        columns = "Puntaje Total",
        backgroundColor = DT::styleInterval(
          c(10, 25, 50, 100, 160),
          c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff", "#ccccff", "#e6ccff")
        ),
        fontWeight = 'bold'
      ) %>%
      DT::formatStyle(
        columns = "Calificación RENACYT",
        backgroundColor = DT::styleEqual(
          c("Investigador Distinguido", 
            "Sí califica: Nivel I", "Sí califica: Nivel II",
            "Sí califica: Nivel III", "Sí califica: Nivel IV", 
            "Sí califica: Nivel V", "Sí califica: Nivel VI", 
            "Sí califica: Nivel VII",
            "No califica: no tiene 6 puntos en producción total",
            "No califica: Requiere al menos un ítem en Producción",
            "No califica: Estudiantes requieren 9 en producción",
            "No califica: Requiere al menos 6 en producción",
            "No califica: Requiere al menos 10 en puntaje total"),
          c("#4CAF50", "#8BC34A", "#CDDC39", "#FFEB3B", 
            "#FFC107", "#FF9800", "#FF5722", "#9C27B0",
            "#ffcccc", "#ffcccc", "#ffcccc", "#ffcccc", "#ffcccc")
        ),
        fontWeight = 'bold'
      )
  })
  
  # Gráfico de niveles RENACYT (CORREGIDO)
  output$renacyt_levels_plot <- renderPlotly({
    req(comparativeData())
    input$update_renacyt
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0) return(plotly_empty())
    
    plot_data <- purrr::map_dfr(seq_along(successful_results), function(i) {
      res <- successful_results[[i]]
      
      # Validación robusta de valores igual que antes
      indice_h_val <- input[[paste0("indice_h_", i)]]
      indice_h <- if(is.null(indice_h_val) || length(indice_h_val) == 0) "No" else as.character(indice_h_val)
      
      libros_val <- input[[paste0("libros_", i)]]
      libros <- if(is.null(libros_val) || length(libros_val) == 0 || is.na(libros_val)) {
        0
      } else {
        suppressWarnings(as.numeric(libros_val))
      }
      if(is.na(libros)) libros <- 0
      
      # Asegurar que los puntajes no sean NULL
      puntaje_formacion <- if(is.null(res$puntaje_formacion)) 0 else res$puntaje_formacion
      puntaje_articulos <- if(is.null(res$puntaje_articulos)) 0 else res$puntaje_articulos
      puntaje_propiedad <- if(is.null(res$puntaje_propiedad)) 0 else res$puntaje_propiedad
      puntaje_asesor <- if(is.null(res$puntaje_asesor)) 0 else res$puntaje_asesor
      
      total_puntaje <- puntaje_formacion + puntaje_articulos + puntaje_propiedad + puntaje_asesor + libros
      prod_total <- puntaje_articulos + puntaje_propiedad + libros
      
      calificacion <- Getcalificacion(value = total_puntaje, IndiceH = indice_h, prod_total = prod_total)
      nivel <- dplyr::case_when(
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
  
  # Gráfico comparativo
  output$comparative_plot <- renderPlotly({
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    if (length(successful_results) == 0) return(plotly_empty())
    
    plot_data <- purrr::map_dfr(successful_results, function(res) {
      data.frame(
        Investigador = res$nombre,
        `Formación Académica` = res$puntaje_formacion,
        `Artículos Científicos` = res$puntaje_articulos,
        `Propiedad Intelectual` = res$puntaje_propiedad,
        `Asesorías` = res$puntaje_asesor,
        check.names = FALSE
      )
    })
    
    plot_data_long <- plot_data %>% pivot_longer(-Investigador, names_to = "Categoria", values_to = "Puntaje")
    
    p <- ggplot(plot_data_long, aes(x = Investigador, y = Puntaje, fill = Categoria)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Comparación de Puntajes por Categoría", x = "Investigador", y = "Puntaje") +
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
    if (length(successful_results) == 0) return(plotly_empty())
    
    all_cuartiles <- purrr::map_dfr(successful_results, function(res) {
      if (!is.null(res$cuartiles) && nrow(res$cuartiles) > 0) {
        res$cuartiles %>% mutate(Investigador = res$nombre)
      } else {
        data.frame()
      }
    })
    
    if (nrow(all_cuartiles) == 0) return(plotly_empty())
    
    p <- ggplot(all_cuartiles, aes(x = Investigador, y = cantidad, fill = Cuartil)) +
      geom_bar(stat = "identity", position = "stack") +
      labs(title = "Distribución de Publicaciones por Cuartil",
           x = "Investigador", y = "Número de Publicaciones") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_manual(values = c(
        "Q1" = "#2E8B57", "Q2" = "#4682B4", "Q3" = "#DAA520",
        "Q4" = "#CD853F", "Sin cuartil" = "#808080", "No Cuartil" = "#808080"
      ))
    
    ggplotly(p)
  })
  
  # Selector y tabla de detalles por investigador
  observeEvent(comparativeData(), {
    req(comparativeData())
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) x$success)]
    
    choices <- setNames(seq_along(successful_results),
                        sapply(successful_results, function(x) x$nombre))
    updateSelectInput(session, "selected_researcher", choices = choices)
  })
  
  # --- TABLA: Detalles por Investigador (todas las publicaciones, post-dedup) ---
  output$researcher_detail_table <- DT::renderDT({
    req(comparativeData(), input$selected_researcher)
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) isTRUE(x$success))]
    validate(need(length(successful_results) > 0, "No hay investigadores procesados."))
    
    idx <- as.numeric(input$selected_researcher)
    validate(need(!is.na(idx) && idx >= 1 && idx <= length(successful_results),
                  "Selecciona un investigador."))
    
    sel <- successful_results[[idx]]
    validate(need(!is.null(sel$df_final) && nrow(sel$df_final) > 0,
                  "No hay publicaciones para mostrar."))
    
    # Asegurar columnas y evitar que un missing rompa el render
    base <- sel$df_final
    needed <- c("Ano de Produccion", "Titulo",
                "Cuartil de ScimagoJR o JCR*", "Cuartil", "Value", "Revista_norm")
    missing <- setdiff(needed, names(base))
    if (length(missing)) base[missing] <- NA
    
    detail_table <- base %>%
      select(-Revista_norm) %>%
      transmute(
        `Año de Publicación` = `Ano de Produccion`,
        `Título`             = Titulo,
        `Cuartil Original`   = `Cuartil de ScimagoJR o JCR*`,
        `Cuartil`            = Cuartil,
        `Valor`              = Value
      )
    
    # Render de la tabla (todas las publicaciones detectadas, ya deduplicadas)
    DT::datatable(
      detail_table,
      rownames = FALSE,
      escape = TRUE,
      options = list(
        pageLength = 10,
        autoWidth  = TRUE,
        scrollX    = TRUE,
        initComplete = DT::JS(
          "function(settings, json) {
           $(this.api().table().container()).css('background-color', 'white');
           $(this.api().table().node()).css('background-color', 'white');
           $(this.api().table().body()).css('background-color', 'white');
           $('.dataTables_wrapper').css('background-color', 'white');
         }"
        )
      )
    ) %>%
      DT::formatStyle(
        columns = 1:ncol(detail_table),
        backgroundColor = 'white'
      ) %>%
      DT::formatStyle(
        columns = "Valor",
        backgroundColor = DT::styleInterval(c(1, 5, 10, 15),
                                            c("#f0f0f0", "#e6f3ff", "#cce7ff", "#99ccff", "#66b2ff"))
      )
  })
  
  # --- MODAL: Ver duplicados (solo los duplicados eliminados de ese investigador) ---
  observeEvent(input$openDupModalOne, {
    req(comparativeData(), input$selected_researcher)
    
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) isTRUE(x$success))]
    idx <- as.numeric(input$selected_researcher)
    sel <- successful_results[[idx]]
    
    showModal(modalDialog(
      title = "Duplicados eliminados — Investigador seleccionado",
      size = "l",
      easyClose = TRUE,
      DTOutput("dups_table_one"),
      footer = modalButton("Cerrar")
    ))
  })
  
  output$dups_table_one <- DT::renderDT({
    req(comparativeData(), input$selected_researcher)
    results <- comparativeData()
    successful_results <- results[sapply(results, function(x) isTRUE(x$success))]
    idx <- as.numeric(input$selected_researcher)
    sel <- successful_results[[idx]]
    
    df <- sel$deduplication_info$duplicate_details
    
    if (is.null(df) || !nrow(df)) {
      return(DT::datatable(
        data.frame(Mensaje = "Este investigador no tuvo duplicados eliminados."),
        rownames = FALSE,
        options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
      ))
    }
    
    # Normaliza y muestra sólo los campos relevantes del duplicado
    keep <- intersect(c("Ano de Produccion", "Titulo", "Value", "duplicate_rank"), names(df))
    DT::datatable(
      df[, keep, drop = FALSE] %>%
        dplyr::rename(
          `Año de Publicación` = `Ano de Produccion`,
          `Título`             = Titulo,
          `Valor`              = Value,
          `Orden del duplicado`= duplicate_rank
        ),
      rownames = FALSE,
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
  })
  
  
  # Descarga Excel de investigador seleccionado
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
      if (length(successful_results) == 0 || is.null(input$selected_researcher)) return()
      
      selected_idx <- as.numeric(input$selected_researcher)
      selected_researcher <- successful_results[[selected_idx]]
      
      wb <- openxlsx::createWorkbook()
      
      # Hoja 1: Resumen
      openxlsx::addWorksheet(wb, "Resumen RENACYT")
      
      # Obtener valores actuales con validación robusta
      indice_h_val <- input[[paste0("indice_h_", selected_idx)]]
      indice_h <- if(is.null(indice_h_val) || length(indice_h_val) == 0) "No" else as.character(indice_h_val)
      
      libros_val <- input[[paste0("libros_", selected_idx)]]
      libros <- if(is.null(libros_val) || length(libros_val) == 0 || is.na(libros_val)) {
        0
      } else {
        val <- suppressWarnings(as.numeric(libros_val))
        if(is.na(val)) 0 else val
      }
      
      total_puntaje <- selected_researcher$puntaje_formacion +
        selected_researcher$puntaje_articulos +
        selected_researcher$puntaje_propiedad +
        selected_researcher$puntaje_asesor + libros
      
      prod_total <- selected_researcher$puntaje_articulos +
        selected_researcher$puntaje_propiedad + libros
      
      calificacion <- Getcalificacion(value = total_puntaje, IndiceH = indice_h, prod_total = prod_total)
      
      dedup_info_text <- "No aplicada"
      if (!is.null(selected_researcher$deduplication_info)) {
        dedup_info_text <- sprintf(
          "%d duplicados eliminados de %d publicaciones originales",
          selected_researcher$deduplication_info$duplicates_removed %||% 0,
          selected_researcher$deduplication_info$original_count %||% selected_researcher$total_publicaciones
        )
      }
      
      resumen_data <- data.frame(
        Categoría = c(
          "Investigador", "URL CTIVITAE", "Fecha de Análisis", "",
          "DEDUPLICACIÓN", "Estado", "",
          "PUNTAJES RENACYT", "Grado Académico", "Artículos Científicos", "Propiedad Intelectual",
          "Asesorías de Tesis", "Libros y Capítulos", "Índice H (>=10)", "",
          "TOTALES", "Puntaje Total", "Producción Total", "Total Publicaciones", "",
          "CALIFICACIÓN RENACYT", "Resultado"
        ),
        Valor = c(
          selected_researcher$nombre,
          selected_researcher$url,
          as.character(Sys.Date()),
          "",
          "",
          dedup_info_text,
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
      
      openxlsx::writeData(wb, "Resumen RENACYT", resumen_data)
      
      headerStyle <- openxlsx::createStyle(fontSize = 12, textDecoration = "bold",
                                           fgFill = "#3c8dbc", fontColour = "white")
      sectionStyle <- openxlsx::createStyle(fontSize = 11, textDecoration = "bold",
                                            fgFill = "#ecf0f5")
      
      openxlsx::addStyle(wb, "Resumen RENACYT", headerStyle, rows = 1, cols = 1)
      openxlsx::addStyle(wb, "Resumen RENACYT", headerStyle, rows = 1, cols = 2)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 5, cols = 1)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 5, cols = 2)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 8, cols = 1)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 8, cols = 2)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 16, cols = 1)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 16, cols = 2)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 21, cols = 1)
      openxlsx::addStyle(wb, "Resumen RENACYT", sectionStyle, rows = 21, cols = 2)
      openxlsx::setColWidths(wb, "Resumen RENACYT", cols = 1:2, widths = c(25, 35))
      
      # Hoja 2: Publicaciones
      if (!is.null(selected_researcher$df_final) && nrow(selected_researcher$df_final) > 0) {
        openxlsx::addWorksheet(wb, "Publicaciones")
        detail_table <- selected_researcher$df_final %>%
          select(-Revista_norm) %>%
          rename(
            `Año de Publicación` = `Ano de Produccion`,
            `Título` = Titulo,
            `Cuartil Original` = `Cuartil de ScimagoJR o JCR*`,
            `Cuartil` = Cuartil,
            `Valor` = Value
          )
        openxlsx::writeData(wb, "Publicaciones", detail_table, headerStyle = headerStyle)
        openxlsx::setColWidths(wb, "Publicaciones", cols = 1:ncol(detail_table), widths = "auto")
      }
      
      # Hoja 3: Cuartiles
      if (!is.null(selected_researcher$cuartiles) && nrow(selected_researcher$cuartiles) > 0) {
        openxlsx::addWorksheet(wb, "Cuartiles")
        cuartiles_data <- selected_researcher$cuartiles %>%
          rename(`Cuartil` = Cuartil, `Cantidad de Publicaciones` = cantidad)
        openxlsx::writeData(wb, "Cuartiles", cuartiles_data, headerStyle = headerStyle)
        openxlsx::setColWidths(wb, "Cuartiles", cols = 1:2, widths = "auto")
      }
      
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# ===== LANZAR LA APLICACIÓN =====
shinyApp(ui, server)
