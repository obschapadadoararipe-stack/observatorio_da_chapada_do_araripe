library(shiny)
library(raster)
library(leaflet)
library(sf)
library(sp)
library(rsconnect)
library(plotly)
library(ggplot2)
# -------------------------
# Chapada do Araripe bbox
# -------------------------
bbox_ext <- extent(
  -41, -38.8,
  -8,  -6.8
)
#-41.1, -8.1, -38, -6.1
# -------------------------
# Raster files by year
# -------------------------
raster_files <- list.files(
  "MapBiomas",
  pattern = "^MapBiomas_LULC_\\d{4}_Araripe\\.tif$",
  full.names = TRUE
)

years <- sub(".*_(\\d{4})_.*", "\\1", raster_files)
years_num <- as.numeric(years)
names(raster_files) <- years_num

# -------------------------
# Shapefile Chapada
# -------------------------
chapada_do_araripe <- st_read("mapa_da_chapada/APA_chapada_araripe/APA_chapada_araripe.shp", quiet = TRUE)
chapada_do_araripe <- st_transform(chapada_do_araripe, 4326)

flona_chapada_do_araripe <- st_read("mapa_da_chapada/FLONA_araripe_apodi/FLONA_araripe_apodi.shp", quiet = TRUE)
flona_chapada_do_araripe <- st_transform(flona_chapada_do_araripe, 4326)


# garantir geometria válida (evita erros no difference)
chapada_do_araripe <- st_make_valid(chapada_do_araripe)
flona_chapada_do_araripe <- st_make_valid(flona_chapada_do_araripe)

# APA sem FLONA (o que você quer)
apa_sem_flona <- st_difference(chapada_do_araripe, flona_chapada_do_araripe)


# -------------------------
# Classes realmente presentes
# -------------------------
classes <- c(0,3,4,12,9,15,21,23,24,25,29,30,31,33,39,41,75)

natural_classes <- c(
  3,  # Formação Florestal
  4,  # Formação Savânica
  12  # Formação Campestre
)

farming_classes <- c(
  15, # Pastagem
  21, # Mosaico de Agricultura e Pastagem
  39, # Soja
  41  # Outras Lavouras Temporárias
)

urban_classes <- c(
  24 # Infraestrutura Urbana
)

no_veg_classes <- c(
  0,  # Sem dados
  9,  # Silvicultura / Floresta Plantada
  23, # Praia, Duna e Areal
  25, # Outras Áreas Não Vegetadas Naturais
  29, # Afloramento Rochoso
  31,  # Aquicultura
  30, # Mineração
  75  # Outras Áreas Não Vegetadas Antrópicas
  )

water_classes <- c(
  33  # Rios, Lagos e Oceano
)



group_df <- data.frame(
  from = classes,
  to = ifelse(classes %in% natural_classes, 1,
              ifelse(classes %in% farming_classes, 2,
                     ifelse(classes %in% urban_classes,   3,
                            ifelse(classes %in% no_veg_classes,  4,
                                   ifelse(classes %in% water_classes,   5, NA)))))
)

group_mat <- as.matrix(group_df)

# =========================
# UI
# =========================
ui <- fluidPage(
  
  tags$h1("Observatório da Chapada do Araripe", align="center"),
  
  fluidRow(
    column(10, offset=1,
           
           tabsetPanel(
             
             # =========================
             # TAB MAPA
             # =========================
             tabPanel("Mapas",
                      
                      fluidRow(
                        column(9,
                               # Title above the map
                               tags$h3("Atenção meu povo!", style="text-align:center;"),
                               tags$p(
                                 "O mapa mostra as mudanças de uso e cobertura do solo na Área de Proteção Ambiental (APA) da Chapada do Araripe e na Floresta Nacional do Araripe (FLONA) entre 2010 e 2024. Houve um aumento de cerca de 800 km² nas áreas de agricultura ou pastagem na APA nesse período. Use o controle abaixo para visualizar as alterações ao longo dos anos.",
                                 style="text-align:center; margin-bottom:15px;"
                               ),
                               
                               # 
                               div(style="text-align:center; margin-bottom:10px;",
                                   div(style="display:inline-block;width:60%;",
                                       sliderInput("year","",
                                                   min=min(years_num),
                                                   max=max(years_num),
                                                   value=max(years_num),
                                                   step=1, sep=""
                                       )
                                   )
                               ),
                               
                               # Map
                               leafletOutput("mapGrouped", height = 350)
                        ),
                        
                        column(3,
                               tags$div(
                                 style="border:1px solid #ccc;padding:20px;border-radius:8px;",
                                 tags$h4("Legenda"),
                                 tags$ul(
                                   tags$li(span(style="color:darkgreen;font-weight:bold;","■ "), "Vegetação"),
                                   tags$li(span(style="color:red;font-weight:bold;","■ "), "Agricultura ou pastagem"),
                                   tags$li(span(style="color:yellow;font-weight:bold;","■ "), "Área Urbana"),
                                   tags$li(span(style="color:blue;font-weight:bold;","■ "), "Água"),
                                   tags$li(span(style="color:gray;font-weight:bold;","■ "), "Outros"),

                                   
                                   # APA solid line
                                   tags$li(
                                     tags$span(
                                       style="display:inline-block;width:30px;border-top:4px solid black;margin-right:8px;"
                                     ),
                                     "APA"
                                   ),
                                   
                                   # FLONA dashed line
                                   tags$li(
                                     tags$span(
                                       style="display:inline-block;width:30px;border-top:4px dashed black;margin-right:8px;"
                                     ),
                                     "FLONA"
                                   )
                                 
                                 
                                  )
                               )
                        )
                      )
                      
             ),
             
             # =========================
             # Gráficos
             # =========================
             
             tabPanel("Gráficos",
                      tags$h3("Aumento anual nas áreas de Agricultura ou Pastagem"),
                      tags$p(
                        "Valores calculados para a Área de Proteção Ambiental (APA) da Chapada do Araripe (sem incluir a FLONA), comparados com 2010. Nossas análises iniciais indicam um aumento de aproximadamente 800 km² nas áreas utilizadas para agricultura ou pastagem entre 2011 e 2024.",
                        tags$strong("Um aumento de cerca de 110 mil campos de futebol."), 
                      ),
                      plotOutput("areaPlot", height = "400px"),
                      tags$h5("Estes resultados podem contribuir para o desenvolvimento de melhores políticas públicas de ocupação territorial na região. Mais análises e discussões são necessárias e estão sendo desenvolvidas. O Observatório da Chapada do Araripe está aberto a sugestões e colaborações com grupos, pessoas ou instituições interessadas. O gráficos e os mapas podem ser baixados para uso."),
                      
                    #  tags$h3("Distribuição de uso do solo"),
                    #  fluidRow(
                    #    column(6, plotOutput("pie2010", height = "350px")),
                    #    column(6, plotOutput("pie2024", height = "350px"))
                    #  ),
             ),
             
             # =========================
             # TAB DADOS
             # =========================
             tabPanel("Métodos e Referências",
              
                      tags$h3("Cite o nosso trabalho"),
                      tags$p(
                        "Caso você escreva sobre o Observatório da Chapada do Araripe, utilize nossas análises ou se inspire em nossas tecnologias, você pode nos citar da seguinte forma:",
                        tags$strong(" Felix, V. A. R. (2026). Observatório da Chapada do Araripe [Software]. Zenodo. DOI:10.5281/zenodo.18732472"),
                        ".Como utilizamos dados do MapBiomas neste trabalho, também sugerimos a citação correspondente descrita abaixo."
                      ),
                      tags$h3("Sobre os dados utilizados no Observatório"),
                      tags$p(
                        "Para as nossas análises iniciais sobre a Chapada do Araripe e áreas adjacentes, utilizamos dados do MapBiomas – Coleção 10 da série anual de Mapas de Cobertura e Uso da Terra do Brasil, baixados em uma escala espacial de aproximadamente 9 hectares (~300 m x 300 m) por pixel em 07/02/2026 por meio do link:",
                        tags$a(
                          href = "https://developers.google.com/earth-engine/datasets/catalog/projects_mapbiomas-public_assets_brazil_lulc_v1",
                          "Google Earth Engine – MapBiomas.",
                          target = "_blank"
                        ),
                        "Artigo sobre os dados disponível em (Souza et al, 2020): ",
                        tags$a(
                          href = "https://doi.org/10.3390/rs12172735",
                          "DOI: 10.3390/rs12172735",
                          target = "_blank"
                        )
                      ),
                      
                      tags$h4("Os dados foram agrupados da seguinte forma:"),
                      tags$ul(tags$li(tags$b("Vegetação Natural:"), 
                                      " Formação Florestal (3), Formação Savânica (4) ou Formação Campestre (12)"),
                              
                              tags$li(tags$b("Agricultura ou Pastagem:"), 
                                      " Pastagem (15), Mosaico de Agricultura e Pastagem (21), Soja (39) ou Outras Lavouras Temporárias (41)"),
                              
                              tags$li(tags$b("Área Urbana:"), 
                                      " Infraestrutura Urbana (24)"),
                              
                              tags$li(tags$b("Água:"), 
                                      " Rios e lagos (33)"),
                              
                              tags$li(tags$b("Outros:"), 
                                      "Mineração (30), Usina Fotovoltaica (75), Silvicultura (9), Aquicultura (31), Areal (23), Outras Áreas Não Vegetadas (25), Afloramento Rochoso (29) ou Sem Dados (0)")),
                      
                      tags$p("*Apenas classes presentes nos rasters da região foram consideradas."),
                      
                      
                      tags$h4("Mapas, Gráficos e Análises"),
                      tags$h5("Os mapas e os gráficos foram desenvolvidos com foco na Área de Proteção Ambiental (APA) da Chapada do Araripe. Para o gráfico sobre o aumento anual das áreas de agricultura ou pastagem, calculamos a área total dessas classes em cada ano e comparamos os valores com o total observado em 2010. As estimativas foram realizadas para toda a área da APA, sem incluir a área da FLONA. A área total de cada classe em cada ano foi estimada a partir da resolução espacial dos dados do MapBiomas utilizados, que foram de aproximadamente 9 hectares (~300 m x 300 m) por pixel."),
                      
                      tags$h3("Chapada do Araripe"),
                      tags$p(
                        "A Chapada do Araripe está delimitada pela Área de Proteção Ambiental (APA) e pela Floresta Nacional do Araripe (FLONA), de acordo com dados da ",
                      tags$a("Secretaria de Meio Ambiente e Mudança do Clima (SEMA)", 
                        href = "https://www.sema.ce.gov.br/cadastro-estadual-de-unidade-de-conservacao-ceuc/painel-cadastro-estadual-de-unidades-de-conservacao/downloads-de-decretos-e-poligonais-ceuc/unidades-de-conservacao-federais/", target = "_blank"),"."
                      ),
                      
                      tags$h3("Processamento de Dados"),
                      tags$p("Todo o processamento dos dados foi realizado utilizando a linguagem de programação ",
                             tags$a("R", href = "https://www.r-project.org/", target = "_blank"),
                             ". Além disso, os mapas foram elaborados com integração ao ",
                             tags$a("Leaflet", href = "https://leafletjs.com/", target = "_blank"),
                             ", ao ",
                             tags$a("OpenStreetMap", href = "https://www.openstreetmap.org/", target = "_blank"),
                             " e ao ",
                             tags$a("CARTO", href = "https://carto.com/", target = "_blank"),
                             ", permitindo visualizações interativas e dinâmicas.")
                      
             ),
             
             # =========================
             # TAB CONTRIBUA
             # =========================
             tabPanel("Sobre o Observatório",
                      
                      tags$h3("Um Software baseado em Ciência e Tecnologias Livres"),
                      tags$p(
                        "O Observatório da Chapada do Araripe está publicado como um software científico para análises ambientais e sociais baseado em ciência de dados e tecnologias livres. Isso facilita citações e a transparência metodológica, incluindo as referências utilizadas no desenvolvimento deste projeto. A publicação também permite o desenvolvimento progressivo do Observatório e a inclusão posterior de coautores em caso de colaborações significativas no aprimoramento dessa tecnologia.",
                        target = "_blank"
                      ),
                      tags$h4("Missão do Observatório", style="text-align:left;"),
                      tags$h5(
                        "Promover a produção, análise e divulgação de dados ambientais e sociais, bem como o monitoramento da Chapada do Araripe e cidades adjacentes, contribuindo para políticas públicas baseadas em evidências e para o desenvolvimento sustentável da região.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        "Visão do Observatório",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h5(
                        "Tornar-se um centro de excelência em análise de dados ambientais e sociais que possa apoiar políticas públicas para desenvolvimento regional.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        "Objetivos do Observatório",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h5(
                        "Apoiar políticas públicas e desenvolvimento sustentável; Fortalecer pesquisa científica regional;  Promover educação socioambiental; Contribuir com o monitoramento da Chapada do Araripe e cidades adjacentes.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
      
                      tags$h3(
                        "Entre em contato",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        HTML(
                          "Caso queira contribuir com o Observatório da Chapada do Araripe, você pode seguir a página no Instagram: 
    <a href='https://www.instagram.com/obschapadadoararipe/' target='_blank' style='font-weight:bold'>@obschapadadoararipe</a>, 
    enviar um email para 
    <a href='mailto:obschapadadoararipe@gmail.com' target='_blank' style='font-weight:bold'>obschapadadoararipe@gmail.com</a> 
    ou usar esse 
    <a href='https://forms.gle/JfxvYWxu4feohTy3A' target='_blank' style='font-weight:bold'>formulário.</a>",
                        ),
                        style="text-align:left; margin-bottom:15px;"
                      )
             ),
             
             
             # =========================
             # TAB equipe
             # =========================
             tabPanel("Equipe",
                      tags$h4(
                        "Victor Arraes Rocha Felix",
                        style="text-align:left; margin-bottom:2px;"
                      ),
                      tags$h5(
                        "Desenvolvedor & Pesquisador | Mestre em Geografia na Universidade da Georgia (EUA)",
                        style="text-align:left; margin-bottom:4px;"
                      )
             ),
             
             # =========================
             # Baixe os mapas
             # =========================
             tabPanel("Baixe os mapas e os gráficos",
                      
                      tags$h4("Mapas do Usos do Solo e Ocupação na Área de Proteção Ambiental (APA) e na Floresta Nacional do Araripe (FLONA) desenvolvidos pelo Observatório"),
                      
                      selectInput("year_download", "Escolha o ano do mapa:", choices = years_num, selected = max(years_num)),
                      downloadButton("downloadGgMap", "Baixar o mapa"),
                      
                      tags$hr(),
                      
                      tags$h4("Gráfico de aumento anual nas áreas de Agricultura ou Pastagem na Área de Proteção Ambiental (APA) da Chapada do Araripe entre 2011 e 2024"),
                      downloadButton("downloadAgroPlot", "Baixar o gráfico")
             )
           )
    )
  ),
  
  # =========================
  # FOOTER
  # =========================
  tags$footer(
    "Felix, V. A. R. (2026). Observatorio da Chapada do Araripe [Software]. Zenodo. DOI:10.5281/zenodo.18732472",
    style = "text-align:center; padding:5px; font-size:14px; color:gray; border-top:1px solid #ccc; margin-top:20px;"
  ),
  tags$footer(
    "",
    style = "text-align:center; padding:5px; font-size:14px; color:gray;"
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session){
  
  class_labels <- c(
    "1" = "Vegetação",
    "2" = "Agropecuária",
    "3" = "Área Urbana",
    "4" = "Outros",
    "5" = "Água"
  )
  
  class_colors <- c(
    "1" = "green4",
    "2" = "red",
    "3" = "yellow2",
    "4" = "gray",
    "5" = "blue"
  )
  
  
  
  
  current_raster <- reactive({
    req(input$year)
    crop(raster(raster_files[as.character(input$year)]), bbox_ext)
  })
  
  grouped_raster <- reactive({
    reclassify(current_raster(), group_mat)
  })
  
  # render once
  output$mapGrouped <- renderLeaflet({
    
    center_lng <- (-41.105201 + -38.971922)/2
    center_lat <- (-7.943965 + -6.999838)/2
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(center_lng, center_lat, zoom=9)
  })
  
  # update without resetting zoom
  observe({
    
    pal <- colorFactor(
      c("green4","red","yellow2","gray","blue"),
      domain = 1:5
    )
    
    leafletProxy("mapGrouped") %>%
      clearImages() %>%
      addRasterImage(grouped_raster(),
                     colors=pal,
                     opacity=1,
                     method="ngb") %>%
      clearShapes() %>%
      addPolygons(
        data = chapada_do_araripe$geometry,
        color="black",
        weight=4,
        opacity=1,
        fill=FALSE
      ) %>%
      addPolygons(
        data = flona_chapada_do_araripe$geometry,
        color = "black",
        weight = 3,
        opacity = 1,
        fill = FALSE,
        dashArray = "6,6"  
      )
    
  })

  apa_area_data <- reactive({
    
    pixel_area_km2 <- 0.09
    results <- data.frame()
    
    for (yr in years_num) {
      
      r <- raster(raster_files[as.character(yr)])
      r <- crop(r, bbox_ext)
      #r <- mask(r, as(chapada_do_araripe, "Spatial"))
      r <- mask(r, as(apa_sem_flona, "Spatial"))
      
      r_grouped <- reclassify(r, group_mat)
      
      freq_df <- as.data.frame(freq(r_grouped, useNA="no"))
      colnames(freq_df) <- c("class","count")
      
      freq_df$year <- yr
      freq_df$area_km2 <- freq_df$count * pixel_area_km2
      
      results <- rbind(results, freq_df)
    }
    
    return(results) 
  })
    
    class_labels <- c(
      "1" = "Vegetação",
      "2" = "Agropecuária",
      "3" = "Área Urbana",
      "4" = "Sem Vegetação",
      "5" = "Água"
    )
  
    output$areaPlot <- renderPlot({
      
      df <- apa_area_data()
      
      df_agro <- df[df$class == 2, ]
      first_year_area <- df_agro$area_km2[df_agro$year == 2010]
      df_agro$delta_area <- df_agro$area_km2 - first_year_area
      df_agro <- df_agro[df_agro$year != 2010, ]
      
      ggplot(df_agro, aes(x = year, y = delta_area)) +  # use numeric x
        geom_bar(stat = "identity", fill = "red") +
        labs(
          x = "",
          y = "Área em km²",
          title = ""
        ) +
        scale_x_continuous(breaks = seq(2012, max(df_agro$year), by = 2)) +  # every 2 years
        theme_minimal(base_size = 14)
    })
    
    # --- gráfico de agropecuária ---
    output$downloadGgMap <- downloadHandler(
      filename = function() {
        paste0("mapa_chapada_", input$year_download, ".png")
      },
      content = function(file) {
        req(input$year_download)
        
        bbox_ext <- extent(
          -41, -38.8,
          -8,  -6.8
        )
        
        r <- raster(raster_files[as.character(input$year_download)])
        r <- crop(r, bbox_ext)
        r <- mask(r, as(chapada_do_araripe, "Spatial"))
        r_grouped <- reclassify(r, group_mat)
        
        # cores correspondentes às classes
        class_colors <- c("green4","red","yellow2")
        class_labels <- c("Vegetação","Agricultura ou pastagem","Área Urbana")
        
        # abrir dispositivo gráfico
        png(file, width = 8, height = 7, units = "in", res = 400)
        
        par(mar = c(4, 4, 4, 2))  # bottom, left, top, right
        
        # plot raster
        plot(r_grouped, col = c("green4","red","yellow2","gray", "blue"), legend = FALSE,
             main = paste("Área de Proteção Ambiental (APA) da Chapada do Araripe em", input$year_download))
        
        # legenda das classes + FLONA
        legend(
          "topleft",
          legend = c(class_labels, "Floresta Nacional do Araripe (FLONA)"),
          fill = c(class_colors, NA),
          border = c(rep("black", length(class_labels)), NA),
          lty = c(NA, NA, NA, 2),
          col = c(NA, NA, NA, "black"),
          bty = "n",
          pt.cex = 2,
          lwd = 2
        )
        
        # contorno dos polígonos
        plot(st_geometry(chapada_do_araripe), add = TRUE, border = "black", lwd = 1)
        plot(st_geometry(flona_chapada_do_araripe), add = TRUE, border = "black", lwd = 2, lty = 2)
        
        # projeção abaixo da imagem
        mtext("DATUM: WGS 84 (Latitude/Longitude)",
              side = 1,       
              line = 2.5,    
              adj = 0.5,    
              cex = 0.8,
              font = 1) 
        
        # legenda da fonte
        legend(
          "bottomleft",
          legend = c(
            "Fonte: Dados do MapBiomas (Souza et al, 2020) analisados pelo",
            "Observatório da Chapada do Araripe (Felix, 2026)"
          ),
          bty = "n",
          cex = 0.8,
          text.col = "black",
          text.font = 2
        )
        
        # cores e labels restantes
        rest_colors <- c("blue", "gray")
        rest_labels <- c("Água", "Outros")
        
        # legenda das classes restantes
        legend(
          "topright",
          legend = rest_labels,
          fill   = rest_colors,
          border = "black",
          bty    = "n",
          pt.cex = 2,
          cex    = 0.8
        )
        
        
       
        dev.off()
      }
    )
    
    
    output$downloadAgroPlot <- downloadHandler(
      filename = function() {
        paste0("areaPlot_", Sys.Date(), ".png")
      },
      content = function(file) {
        df <- apa_area_data()
        df_agro <- df[df$class == 2, ]
        first_year_area <- df_agro$area_km2[df_agro$year == 2010]
        df_agro$delta_area <- df_agro$area_km2 - first_year_area
        df_agro <- df_agro[df_agro$year != 2010, ]
        
        png(file, width = 5, height = 4, units = "in", res = 400)
        
        # create plot
        p <- ggplot(df_agro, aes(x = year, y = delta_area)) +  
          geom_bar(stat = "identity", fill = "red") +
          labs(
            x = "",
            y = "Área em km²",
            title = "Aumento nas Áreas de Agricultura\nou Pastagem desde 2011",
            subtitle = "Dados para a APA da Chapada do Araripe",
            caption = "Fonte: Dados do MapBiomas (Souza et al., 2020) analisados pelo\nObservatório da Chapada do Araripe (Felix, 2026)"
          ) +
          scale_x_continuous(breaks = seq(2012, max(df_agro$year), by = 2)) +
          theme_minimal(base_size = 14) +
          theme(
            plot.title = element_text(face = "bold", hjust = 0.5),
            plot.subtitle = element_text(face = "bold", hjust = 0.5),
            plot.caption = element_text(size = 10, hjust = 0)
          )
        
        print(p) 
        
        dev.off()
      }
    )
    
}

# =========================
# Run App
# =========================
shinyApp(ui, server)
