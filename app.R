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
  -41.105201, -38.971922,
  -7.943965,  -6.999838
)

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
municipios_araripe <- st_read("mapa_da_chapada/chapada_above650.shp", quiet = TRUE)
municipios_araripe <- st_transform(municipios_araripe, 4326)

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
                                 "O mapa mostra as mudanças de uso e cobertura do solo entre 2000 e 2024. Use o controle abaixo e para ver as alarmantes alterações!",
                                 style="text-align:center; margin-bottom:15px;"
                               ),
                               
                               # 🔽 SLIDER MOVED HERE
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
                                   tags$li(span(style="color:blue;font-weight:bold;","■ "), "Água")
                                 )
                               )
                        )
                      )
                      
             ),
             
             # =========================
             # TAB DADOS
             # =========================
             tabPanel("Dados e Métodos",
                      
                      tags$h3("Fonte"),
                      tags$p(
                        "Dados do MapBiomas – Coleção 10 da série anual de Mapas de Cobertura e Uso da Terra do Brasil, acessado em 07/02/2026 através do link: ",
                        tags$a(
                          href = "https://developers.google.com/earth-engine/datasets/catalog/projects_mapbiomas-public_assets_brazil_lulc_v1",
                          "Google Earth Engine – MapBiomas",
                          target = "_blank"
                        ),
                        ". Artigo sobre os dados disponível em: ",
                        tags$a(
                          href = "https://doi.org/10.3390/rs12172735",
                          "DOI: 10.3390/rs12172735",
                          target = "_blank"
                        )
                      ),
                      
                      tags$h3("Agrupamento das Classes"),
                      tags$ul(
                        tags$li(tags$b("Vegetação Natural:")," Formação Florestal, Formação Savânica ou Formação Campestre"),
                        tags$li(tags$b("Agropecuária:")," Pastagem, Mosaico de Usos, Soja ou Outras Lavouras Temporarias"),
                        tags$li(tags$b("Área Urbana ou Infraestrutura:"),"Área Urbana, Mineração ou Usina Fotovoltaica"),
                        tags$li(tags$b("Sem Vegetação:"),"Areal, Outras Áreas Não Vegetadas, Afloramento Rochoso ou sem dados"),
                        tags$li(tags$b("Água:")," Rios e lagos")
                      ),
                      
                      tags$p("Apenas classes realmente presentes nos rasters da região foram consideradas."),
                      
                      tags$h3("Topografia (> 650 m)"),
                      tags$p(
                        "O mapa da Chapada do Araripe foi feito usando áreas acima de 650 metros de altitude e foi gerado em R a partir de dados de elevação obtidos via pacote ",
                        tags$a("elevatr", href="https://cran.r-project.org/package/elevatr", target="_blank"),
                        ", que acessa modelos digitais de elevação globais (DEM) com dados abertos."
                      ),
                      
                      tags$h3("Processamento de Dados"),
                      tags$p(
                        "Todo o processamento dos dados raster e vetoriais foi realizado em R, utilizando os pacotes ",
                        tags$b("raster"), ", ", tags$b("sf"), " e ", tags$b("elevatr"), 
                        ". O site interativo foi desenvolvido em R com o pacote ", tags$b("shiny"), " e utiliza ", tags$b("leaflet"), " para visualização dos mapas."
                      )
                      
             ),
             
             # =========================
             # TAB CONTRIBUA
             # =========================
             tabPanel("Participe",
                      
                      tags$h2("Participe!", style="text-align:center;"),
                      tags$h3(
                        "Queremos colaborar com as várias comunidades da Chapada do Araripe, incluindo agricultores, estudantes, gestores e pesquisadores que queiram construir meios para uma ocupação consciente deste nosso território ancestral.",
                        style="text-align:center; margin-bottom:15px;"
                      ),
                      tags$h3(
                        "Venham desenvolver juntos materiais, mapas e análises com informações relevantes e compartilhar no Observatório!",
                        style="text-align:center; margin-bottom:15px;"
                      ),
                      
                      
                      tags$a(
                        "Quer participar? Entre em contato!",
                        href="https://forms.gle/JfxvYWxu4feohTy3A",
                        target="_blank",
                        style="font-weight:bold;font-size:16px; display:block; text-align:center; margin-top:15px;"
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
                        "Pesquisador & Programador | Mestre em Geografia",
                        style="text-align:left; margin-bottom:4px;"
                      ),
                      tags$h4(
                        "",
                        style="text-align:left; margin-bottom:2px;"
                      ),
                      tags$h5(
                        "",
                        style="text-align:left; margin-bottom:2px;"
                      )
             )
           )
    )
  ),
  
  # =========================
  # FOOTER
  # =========================
  tags$footer(
    "Por um desenvolvimento ordenado na Chapada do Araripe. Contato:obschapadadoararipe@gmail.com",
    style = "text-align:center; padding:15px; font-size:14px; color:gray; border-top:1px solid #ccc; margin-top:20px;"
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
        data = municipios_araripe,
        color="black",
        weight=4,
        opacity=1,
        fill=FALSE
      )
    
  })
  
}

# =========================
# Run App
# =========================
shinyApp(ui, server)
