library(shiny)
library(raster)
library(leaflet)
library(sf)
library(sp)
library(rsconnect)

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

# -------------------------
# Classes realmente presentes
# -------------------------
classes <- c(0,3,4,12,15,21,23,24,25,29,30,33,39,41,75)

natural_classes <- c(3,4,12)
farming_classes <- c(15,21,39,41)
urban_classes   <- c(24,30,75)
no_veg_classes  <- c(0,23,25,29)
water_classes   <- c(33)

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
             tabPanel("Chapada do Araripe",
                      
                      fluidRow(
                        column(9,
                               # Title above the map
                               tags$h3("Atenção meu povo!", style="text-align:center;"),
                               tags$p(
                                 "O mapa mostra as mudanças de uso e cobertura do solo na Area de Protecao Ambiental (APA) e Floresta Nacional do Araripe (FLONA) entre 2000 e 2024. Use o controle abaixo para ver as alarmantes alterações!",
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
                                   tags$li(span(style="color:red;font-weight:bold;","■ "), "Agropecuária"),
                                   tags$li(span(style="color:yellow;font-weight:bold;","■ "), "Área Urbana"),
                                   tags$li(span(style="color:gray;font-weight:bold;","■ "), "Sem Vegetação"),
                                   tags$li(span(style="color:blue;font-weight:bold;","■ "), "Água"),
                                   
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
             # TAB DADOS
             # =========================
             tabPanel("Métodos, Dados e Referências",
                      
                      tags$h3("Ciência e Tecnologias Livres"),
                      tags$p(
                        "O Observatório da Chapada do Araripe está publicado como um software científico para análises ambientais e sociais, baseado em ciência de dados e tecnologias livres. Isso facilita citações e a transparência metodológica, incluindo as referências utilizadas no desenvolvimento deste projeto. A publicação também permite o desenvolvimento progressivo do Observatório, como a inclusão posterior de coautores em caso de colaborações significativas no aprimoramento dessa tecnologia.",
                        target = "_blank"
                      ),
                      tags$h3("Cite o nosso trabalho"),
                      tags$p(
                        "Caso você escreva sobre o Observatório da Chapada do Araripe, utilize nossas análises ou se inspire em nossas tecnologias, você pode nos citar da seguinte forma: Felix, V. A. R. (2026). Observatorio da Chapada do Araripe [Software]. Zenodo.",
                          target = "_blank"
                        ),
                      tags$h3("Sobre os dados utilizados no Observatório"),
                      tags$p(
                        "Para as nossas análises iniciais sobre a Chapada do Araripe e áreas adjacentes, utilizamos dados do MapBiomas – Coleção 10 da série anual de Mapas de Cobertura e Uso da Terra do Brasil, baixados em uma escala espacial de 300 m² em 07/02/2026 por meio do link:",
                        tags$a(
                          href = "https://developers.google.com/earth-engine/datasets/catalog/projects_mapbiomas-public_assets_brazil_lulc_v1",
                          "Google Earth Engine – MapBiomas.",
                          target = "_blank"
                        ),
                        "Artigo sobre os dados disponível em: ",
                        tags$a(
                          href = "https://doi.org/10.3390/rs12172735",
                          "DOI: 10.3390/rs12172735",
                          target = "_blank"
                        )
                      ),
                      
                      tags$h4("Os dados foram agrupados da seguinte forma:"),
                      tags$ul(
                        tags$li(tags$b("Vegetação Natural:")," Formação Florestal, Formação Savânica ou Formação Campestre"),
                        tags$li(tags$b("Agropecuária:")," Pastagem, Mosaico de Usos, Soja ou Outras Lavouras Temporarias"),
                        tags$li(tags$b("Área Urbana ou Infraestrutura:"),"Área Urbana, Mineração ou Usina Fotovoltaica"),
                        tags$li(tags$b("Sem Vegetação:"),"Areal, Outras Áreas Não Vegetadas, Afloramento Rochoso ou sem dados"),
                        tags$li(tags$b("Água:")," Rios e lagos")
                      ),
                      
                      tags$p("*Apenas classes presentes nos rasters da região foram consideradas."),
                      
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
                      
                      tags$h3("Missão", style="text-align:left;"),
                      tags$h4(
                        "Promover a produção, análise e divulgação de dados ambientais e sociais, bem como o monitoramento da Chapada do Araripe e cidades adjacentes, contribuindo para políticas públicas baseadas em evidências e para o desenvolvimento sustentável da região.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h3(
                        "Visão",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        "Tornar-se um centro de excelência em dados ambientais e sociais, apoiando políticas públicas e desenvolvimento regional ordenado.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h3(
                        "Objetivos",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        "Fortalecer a pesquisa científica na região; Apoiar políticas públicas e desenvolvimento sustentável; Promover educação socioambiental; Contribuir com o monitoramento da Chapada do Araripe e cidades adjacentes.",
                        style="text-align:left; margin-bottom:15px;"
                      ),
                      tags$h4(
                        tagList(
                          tags$b("Contato:"), " obscapadadoararipe@gmail.com"
                        ),
                        style = "text-align:left; border-top:15px; margin-bottom:15px;"
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
                      ),
                      tags$h4(
                        "",
                        style="text-align:center; margin-bottom:2px;"
                      ),
                      tags$h5(
                        ""
                      ),
                      tags$h5(
                        ""
                        ,
                        style="text-align:center; margin-bottom:2px;"
                        ),
                      tags$a(
                        "Link para entrar em contato",
                        href="https://forms.gle/JfxvYWxu4feohTy3A",
                        target="_blank",
                        style="font-weight:bold;font-size:16px; display:block; text-align:center; margin-top:15px;"
                      )
             )
           )
    )
  ),
  
  # =========================
  # FOOTER
  # =========================
  tags$footer(
    "Felix, V. A. R. (2026). Observatório da Chapada do Araripe [Software]. Zenodo.",
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
  
}

# =========================
# Run App
# =========================
shinyApp(ui, server)
