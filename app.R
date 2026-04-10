library(shiny)
library(bslib)
library(leaflet)
library(plotly)
library(arrow)
library(dplyr)
library(tidyr)
library(sf)


# --- CONFIGURACIÓN DE COLORES ---
col_hombres <- "#009EDB"
col_mujeres <- "#E5243B"

col_hombres_ocu <- "#009EDB"     
col_hombres_no  <- "#80CFF0"      
col_mujeres_ocu <- "#E5243B"      
col_mujeres_no  <- "#F28B99"
# CARGA
mapa <- readRDS("data/mapa_ready.rds")
datos <- read_parquet("data/bd_resumen.parquet")

ui <- page_sidebar(
  title = "Monitor Laboral y de Cuidados ",
  header = tags$style(HTML("
    .main-footer {
      padding: 10px 20px;
      margin-top: 20px;
      border-top: 1px solid #eee;
      color: #777;
      font-size: 0.85rem;
      text-align: center;
    }
  ")),
  sidebar = sidebar(
    selectInput("anio", "Año:", c("Todos los años" = "Todos", sort(unique(datos$anio)))),
    selectInput("entidad", "Entidad:", c("Todos los estados" = "Todos", sort(unique(datos$NOMGEO)))),
    hr(),
    actionButton("reset", "Reiniciar Filtros", icon = icon("refresh"), class = "btn-primary")
  ),
  
 # FILA DE KPIs 
  layout_columns(
    fill = FALSE,
    height = "110px", 
    
    value_box(
      title = span("Tasa de ocupación — hombres", style = "font-size: 0.9rem; color: #444;"),
      value = uiOutput("tasa_h"),
      uiOutput("diff_h")
    ),
    
    value_box(
      title = span("Tasa de ocupación — mujeres", style = "font-size: 0.9rem; color: #444;"),
      value = uiOutput("tasa_m"),
      uiOutput("diff_m")
    ),
    
    value_box(
      title = span("Brecha de Sexo", style = "font-size: 0.9rem; color: #444;"),
      value = uiOutput("brecha"),
      p("Diferencia hombres - mujeres", style = "font-size: 0.8rem; color: #666; margin-top: 5px;")
    ),
    
    value_box(
      title = span("Horas de cuidados — mujeres no ocupadas", style = "font-size: 0.9rem; color: #444;"),
      value = uiOutput("horas_m_no"),
      p("Promedio semanal", style = "font-size: 0.8rem; color: #666; margin-top: 5px;")
    )
  ),

  layout_columns(
    col_widths = c(4, 8),
    card(
      full_screen = TRUE, 
      card_header("Desigualdad por Entidad (Brecha H-M)"), 
      div(
        style = "max-height: 450px; overflow-y: auto; overflow-x: hidden;",
        plotlyOutput("barras", height = "1000px")
      )
    ),
    card(
      full_screen = TRUE,
      card_header("Mapa de Horas de cuidado (promedio semanal)"), 
      leafletOutput("mapa", height = 450)
    )
  ),
  
  layout_columns(
    col_widths = c(6, 6),
    card(
      full_screen = TRUE, 
      card_header("Evolución Temporal del Cuidado"), 
      plotlyOutput("serie")
    ),
    card(
      full_screen = TRUE,
      card_header("Tiempo de Cuidado por Grupo de Edad"),
      plotlyOutput("grafica_edad")
    )
  ),
  div(class = "main-footer",
      "Fuente: Encuesta Nacional de Ocupación y Empleo, 2005-1T a 2025-4T, INEGI."
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reset, {
    updateSelectInput(session, "anio", selected = "Todos")
    updateSelectInput(session, "entidad", selected = "Todos")
  })

  datos_reactivos <- reactive({
    d <- datos
    if (input$entidad != "Todos") d <- d %>% filter(NOMGEO == input$entidad)
    d_anio <- d
    if (input$anio != "Todos") d_anio <- d_anio %>% filter(anio == input$anio)
    list(general = d, filtrado = d_anio)
  })

  output$mapa <- renderLeaflet({
    leaflet(mapa) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -102.5, lat = 23.8, zoom = 3.5)
  })

  observe({
    # 1. Preparar datos del mapa
    mapa_vals <- datos_reactivos()$filtrado %>%
      group_by(CVEGEO, NOMGEO, sexo_lab) %>%
      summarise(prom_horas = sum(cuidados_total) / sum(peso_cuidadores), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = sexo_lab, values_from = prom_horas) %>%
      mutate(prom_general = (replace_na(Hombre, 0) + replace_na(Mujer, 0)) / 2)
    
    mapa_data <- mapa %>% left_join(mapa_vals, by = "CVEGEO")
    pal <- colorNumeric("Purples", mapa_data$prom_general, na.color = "transparent")
    
    # 2. Actualizar visualización con Proxy
    proxy <- leafletProxy("mapa", data = mapa_data) %>%
      clearShapes() %>%
      addPolygons(
        fillColor = ~pal(prom_general), 
        color = "white", 
        weight = 1, 
        fillOpacity = 0.7,
        label = ~paste0(
          "<strong>", NOMGEO.x, "</strong><br/>",
          "Mujeres: ", round(Mujer, 1), " hrs<br/>",
          "Hombres: ", round(Hombre, 1), " hrs"
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions(direction = "auto")
      )
    
    # --- BLOQUE DE ZOOM 
    if (input$entidad != "Todos") {
      # Filtrar el polígono específico para obtener sus coordenadas
      region <- mapa_data %>% filter(NOMGEO.x == input$entidad)
      bbox <- st_bbox(region) 
      
      proxy %>% flyToBounds(
        lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
        lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
      )
    } else {
      # Si elige "Todos", regresar a la vista nacional
      proxy %>% flyTo(lng = -102.5, lat = 23.8, zoom = 4)
    }
  })
 output$serie <- renderPlotly({
  df_serie <- datos_reactivos()$general %>% 
  group_by(anio, sexo_lab) %>%
  summarise(
    Ocupado = sum(cuidados_ocu) / sum(peso_cuid_ocu),
    `No Ocupado` = sum(cuidados_no_ocu) / sum(peso_cuid_no_ocu),
    .groups = "drop"
  ) %>%
  tidyr::pivot_longer(cols = c(Ocupado, `No Ocupado`), names_to = "Estado", values_to = "Horas") %>%
  mutate(
    # Ajustamos el género de la palabra Estado según sexo_lab
    Estado = ifelse(sexo_lab == "Mujer", 
                    gsub("Ocupado", "Ocupada", Estado), 
                    Estado),
    # Ahora Grupo y texto_hover usarán la versión corregida
    Grupo = paste(sexo_lab, Estado, sep = " - "),
    texto_hover = paste0(
      "<b>Año:</b> ", anio, "<br>",
      "<b>Grupo:</b> ", Grupo, "<br>",
      "<b>Horas:</b> ", round(Horas, 1), " hrs"
    )
  )
  # Creamos el gráfico con la leyenda configurada en theme()
  p <- ggplot(df_serie, aes(x = anio, y = Horas, color = Grupo, group = Grupo, text = texto_hover)) +
    geom_line(linewidth = 0.2) + 
    geom_point(size = 1) +
    scale_color_manual(values = c(
      "Hombre - Ocupado"    = col_hombres_ocu,
      "Hombre - No Ocupado" = col_hombres_no,
      "Mujer - Ocupada"     = col_mujeres_ocu,
      "Mujer - No Ocupada"  = col_mujeres_no
    )) +
    theme_minimal() +
    labs(title = "Hora promedio de Cuidado a la semana", y = "Horas", color = "Categoría") +
    theme(legend.position = "right", axis.title.x = element_blank())

  ggplotly(p, tooltip = "text") %>% 
    layout(
      legend = list(
        valign = "middle",
        tracegroupgap = 0  
      )
    )
 })
  output$barras <- renderPlotly({
    df_brecha <- datos_reactivos()$filtrado %>%
      group_by(NOMGEO, sexo_lab) %>%
      summarise(horas = sum(cuidados_total) / sum(peso_cuidadores), .groups = "drop") %>%
      tidyr::pivot_wider(names_from = sexo_lab, values_from = horas) %>%
      mutate(
        brecha = Mujer - Hombre,
        NOMGEO = case_when(
          NOMGEO == "Coahuila de Zaragoza" ~ "Coahuila",
          NOMGEO == "Veracruz de Ignacio de la Llave" ~ "Veracruz",
          NOMGEO == "Michoacán de Ocampo" ~ "Michoacán",
          TRUE ~ NOMGEO),
        texto_hover = paste0(
          "<b>", NOMGEO, "</b><br>",
          "Mujeres: ", round(Mujer, 1), " hrs<br>",
          "Hombres: ", round(Hombre, 1), " hrs<br>",
          "Brecha: ", round(brecha, 1), " hrs"
        )
      ) %>%
      arrange(brecha)
    
    if (input$entidad != "Todos") {
      df_brecha <- df_brecha %>% mutate(es_seleccionado = ifelse(NOMGEO == input$entidad, 1, 0)) %>% arrange(es_seleccionado, brecha)
    } else {
      df_brecha <- df_brecha %>% arrange(brecha)
    }
    
    df_brecha <- df_brecha %>% mutate(NOMGEO = factor(NOMGEO, levels = NOMGEO))

    p <- ggplot(df_brecha) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "gray70") +
      geom_segment(aes(y = NOMGEO, yend = NOMGEO, x = -Hombre, xend = Mujer), color = "#e0e0e0", size = 2) +
      geom_point(aes(x = -Hombre, y = NOMGEO, text = texto_hover), color = col_hombres, size = 3) +
      geom_point(aes(x = Mujer, y = NOMGEO, text = texto_hover), color = col_mujeres, size = 3) +
      geom_text(aes(x = 0, y = NOMGEO, label = NOMGEO), vjust = -1, size = 3, fontface = "bold") +
      geom_text(aes(x = -Hombre, y = NOMGEO, label = round(Hombre, 1)), nudge_x = -7, hjust = 1, size = 3.5, color = col_hombres) +
      geom_text(aes(x = Mujer, y = NOMGEO, label = round(Mujer, 1)), nudge_x = 7, hjust = 0, size = 3.5, color = col_mujeres) +
      theme_minimal() +
      expand_limits(x = c(max(df_brecha$Hombre, na.rm = TRUE) * -1.3, max(df_brecha$Mujer, na.rm = TRUE) * 1.3)) +
      labs(title = "Brecha de Cuidados: Horas Semanales", subtitle= "Dezliza hacia abajo", x = "Horas (Hombres ← | → Mujeres)", y = NULL) +
      theme(axis.text.y = element_blank(), plot.title = element_text(size = 10),
    plot.subtitle = element_text(size = 8),
    panel.grid.minor = element_blank())

    alt_dinamica <- if(input$entidad != "Todos") 300 else 1000
    ggplotly(p, tooltip = "text", height = alt_dinamica) %>% config(displayModeBar = FALSE)
  })

output$grafica_edad <- renderPlotly({
  df_edad <- datos_reactivos()$filtrado %>%
    group_by(grupo_edad, sexo_lab) %>%
    summarise(horas = sum(cuidados_total) / sum(peso_cuidadores), .groups = "drop") %>% 
    filter(grupo_edad != "No sabe / No contesta") %>%
    mutate(label_text = paste0("<b>", sexo_lab, "</b><br>", grupo_edad, "<br>", round(horas, 1), " hrs"))
  
  p <- ggplot(df_edad, aes(x = grupo_edad, y = horas, fill = sexo_lab, text = label_text)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(aes(y = horas + 1.2, label = round(horas, 1)), 
              position = position_dodge(width = 0.9), size = 3.2, fontface = "bold") +
    scale_fill_manual(values = c("Hombre" = col_hombres, "Mujer" = col_mujeres), name = NULL) +
    theme_minimal() +
    labs(title = "Cuidado por Grupo de Edad", x = "Grupo de Edad", y = "Horas Promedio") +
    theme( legend.title = element_blank())

  ggplotly(p, tooltip = "text") |> 
    config(displayModeBar = FALSE)
})
 indicadores <- reactive({
    df_actual <- datos_reactivos()$filtrado
    
    t_h <- (sum(df_actual$ocupados_total[df_actual$sexo == 1]) / sum(df_actual$peso_total_pob[df_actual$sexo == 1])) * 100
    t_m <- (sum(df_actual$ocupados_total[df_actual$sexo == 2]) / sum(df_actual$peso_total_pob[df_actual$sexo == 2])) * 100
    h_m_no <- sum(df_actual$cuidados_no_ocu[df_actual$sexo == 2]) / sum(df_actual$peso_cuid_no_ocu[df_actual$sexo == 2])
    
    # Lógica de comparativa (solo si hay un año seleccionado)
    diff_h_val <- 0
    diff_m_val <- 0
    
    if (input$anio != "Todos") {
      anio_previo <- as.numeric(input$anio) - 1
      df_previo <- datos %>% filter(anio == anio_previo)
      
      if (input$entidad != "Todos") df_previo <- df_previo %>% filter(NOMGEO == input$entidad)
      
      if (nrow(df_previo) > 0) {
        t_h_prev <- (sum(df_previo$ocupados_total[df_previo$sexo == 1]) / sum(df_previo$peso_total_pob[df_previo$sexo == 1])) * 100
        t_m_prev <- (sum(df_previo$ocupados_total[df_previo$sexo == 2]) / sum(df_previo$peso_total_pob[df_previo$sexo == 2])) * 100
        diff_h_val <- round(t_h - t_h_prev, 1)
        diff_m_val <- round(t_m - t_m_prev, 1)
      }
    }
    
    list(
      tasa_h = round(t_h, 1),
      tasa_m = round(t_m, 1),
      brecha = round(t_h - t_m, 1),
      horas_m_no = round(h_m_no, 1),
      diff_h = diff_h_val,
      diff_m = diff_m_val
    )
  })

  # Dentro del server:
  output$tasa_h <- renderUI({ span(paste0(indicadores()$tasa_h, "%"), style = paste0("color:", col_hombres, "; font-weight:bold; font-size: 2rem;")) })
  output$tasa_m <- renderUI({ span(paste0(indicadores()$tasa_m, "%"), style = paste0("color:", col_mujeres, "; font-weight:bold; font-size: 2rem;")) })
  output$brecha <- renderUI({ span(paste0(indicadores()$brecha, " pp"), style = "color: #B27C30; font-weight:bold; font-size: 2rem;") })
  output$horas_m_no <- renderUI({ span(paste0(indicadores()$horas_m_no, " h"), style = "color: #276751; font-weight:bold; font-size: 2rem;") })

  # Renderizado de los subtítulos
  output$diff_h <- renderUI({ p(paste0(ifelse(indicadores()$diff_h > 0, "+", ""), indicadores()$diff_h, " pp vs año anterior"), style = "font-size: 0.85rem; color: #666; margin: 0;") })
  output$diff_m <- renderUI({ p(paste0(ifelse(indicadores()$diff_m > 0, "+", ""), indicadores()$diff_m, " pp vs año anterior"), style = "font-size: 0.85rem; color: #666; margin: 0;") })
}

shinyApp(ui, server)