## app.R ##
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(gargoyle)
#library(DT)

source('soil-survey_functions_shiny.R')
uploadDir <- '/srv/shiny-server/soil-survey/uploads/'
polygonsDir <- '/srv/shiny-server/soil-survey/polygons/'
stagingDir <- '/srv/shiny-server/soil-survey/staging/'
projectsDir <- '/srv/shiny-server/soil-survey/projects/'


ui <- dashboardPage(
  dashboardHeader(title = "Soil Survey - EM-38"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data sources", tabName = "sources", icon = icon("file-upload")),
      menuItem("ECa maps", tabName = "ecamaps", icon = icon("map")), #,badgeLabel = "new", badgeColor = "green")
      menuItem("Management Zones", tabName = "MZs", icon = icon("adjust")),
      #menuItem("Soil sampling", tabName = "sampling", icon = icon("adjust")),
      menuItem("Variogram index", tabName = "variogram", icon = icon("flask")),
      menuItem("QC Sample design", tabName = "QC", icon = icon("wind")),
      menuItem("QC 2023", tabName = "QC2023", icon = icon("th"))
      # menuItem("Raw Results", tabName = "raw", icon = icon("th")),
      # menuItem("Correlation", tabName = "correlation", icon = icon("th")),
      # menuItem("Parameters by ID", tabName = "params", icon = icon("th"))
    ),
    shinyjs::useShinyjs(),
    tags$head(tags$script(src="app.js")),
    includeCSS("style.css")
  ),
  
  dashboardBody(
    tabItems(
      # Upload tab content
      tabItem(tabName = "sources",
              fluidRow(
                column(width = 12,
                       h2("ECa survey data"),
                       h2("")
                )
              ),
              fluidRow(
                column(width = 4,
                       box(title = "Upload EM38 file", status = "primary", width=12,
                           
                           box(width=12,solidHeader=F,
                               fileInput("file1", "Upload .xyz file containing EM-38 survey data",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    "text/comma-separated-values,text/plain",
                                                    c(".csv",".xyz"))),
                               
                               div(id="frename",
                                   #    tags$hr(),
                                   # Input: Checkbox if file has header ----
                                   #    checkboxInput("header", "First line is Header", TRUE),
                                   tags$hr(),
                                   textInput("fileName", "File Name", ""),
                                   actionButton("addFile", "Add file")
                                   # Input: Select separator ----
                                   # radioButtons("sep", "Separator",
                                   #              choices = c(Comma = "csv",
                                   #                          Tab = "tsv"),
                                   #              selected = "tsv")
                                   
                                   # radioButtons("quote", "Quote",
                                   #              choices = c('" (Double Quote)' = '"',
                                   #                          "' (Single Quote)" = "'",
                                   #                          "None" = ""),
                                   #              selected = '"')
                               ),
                               div(id="fadded",
                                   tags$hr(),
                                   h4("File added to folder.")
                               ),
                               div(id="message",""),
                               
                               div(
                                 h5("CRS: WGS84/UTM")
                               ),
                               div(
                                 
                                 h5('Example columns:'),
                                 tags$table(class="extable",
                                            tags$tr(
                                              tags$td("Northing"),tags$td("Easting"), tags$td("EcaH1"), tags$td("I"), tags$td("EcaH05"), tags$td("I"), tags$td("Elev.[m]")
                                            )
                                            # ,
                                            # tags$tbody(
                                            #   tags$tr(
                                            #     tags$td("740816.386"), tags$td("3674205.663"), tags$td("62.77"), tags$td("-1.49"), tags$td("44.49"), tags$td("0"), tags$td("75.882")
                                            #   ),
                                            #   tags$tr(
                                            #     tags$td("740817.235"), tags$td("3674205.526"), tags$td("62.93"), tags$td("-1.49"), tags$td("43.36"), tags$td("-0.01"), tags$td("75.856")
                                            #   ),
                                            #   tags$tr(
                                            #     tags$td("..."), tags$td(""), tags$td(""), tags$td(""), tags$td(""), tags$td(""), tags$td("...")
                                            #   )
                                            # )
                                 )
                               )
                           )
                           
                       ),
                       
                       box(title = "EM38 files", status = "primary", width=12, 
                           div(id="filelist",
                               # as.list(list.files(uploadDir))
                               HTML(getFiles(uploadDir))
                           )
                       )
                ),
                
                
                column(width = 4,
                       box(title = "Upload Polygon file - field perimeter", status = "info", width=12,
                           box(width=12,solidHeader=F,
                               fileInput("file2", "polygon .shp file",
                                         multiple = FALSE,
                                         accept = c("text/csv",
                                                    c(".shp"))),
                               div(id="frename2",
                                   tags$hr(),
                                   #textInput("fileName2", "File Name", ""),
                                   actionButton("addFile2", "Add file to list"),
                                   h2("")
                               ),
                               div(id="fadded2",
                                   tags$hr(),
                                   h5("shp file added to folder.")
                               ),
                               div(id="message2",""),
                               
                               box(title = "Shape files", status = "info", width=12, 
                                   div(id="filelist2",
                                       # as.list(list.files(uploadDir))
                                       HTML(getPolygonFiles(polygonsDir))
                                   )
                               )
                               
                               
                           )
                       )
                ),
                
                
                column(width = 4,
                       box(title = "Projects", status = "success", width=12,
                           box(width=12,solidHeader=F,
                               
                               div(id="message3",""),
                               
                               div(id="projectsList",
                                   HTML(getProjects(projectsDir))
                               )
                           ),
                           box(width=12,solidHeader=T,
                               actionButton("newProjButton", "New project")
                           ),
                           box(width=12,solidHeader=F,
                               div(id='newproj',
                                   textInput("newProjName", "Name", ""),
                                   actionButton("createProjButton", "Create project")
                               )
                           )
                       )
                ),
                
                #tableOutput("contents"),
                # Input: Select number of rows to display ----
                
                textInput("newFileName", "", ""),
                textInput("oldFileName", "", ""),
                textInput("stageDelete", "", ""),
                textInput("stageDownload", "", "")
              )
      ),
      
      
      # Raw Data tab
      tabItem(tabName = "ecamaps",
              fluidRow(
                column(width = 4,
                       h2("EM38 survey - ECa maps")
                )
              ),
              fluidRow(
                column(width = 2,
                       selectInput("rawfile", h4("Source dataset"),
                                   choices = list.files(path = uploadDir))
                ),
                column(width = 2,offset=1,
                       selectInput("compactBy", h4("Compact data factor"),
                                   choices = list("None"=1,"3"=3,"5"=5,"10"=10,"20"=20,"30"=30),
                                   selected = 30)
                ),
                column(width = 1,offset=1,
                       h4("Trim data:")
                ),
                column(width = 1,
                       selectInput("trimleft", h4("Lower bound"),
                                   choices = list("None"=0,"1%"=0.01,"2%"=0.02,"3%"=0.03,"5%"=0.05,"10%"=0.1),
                                   selected = 0)
                ),
                column(width = 1,
                       selectInput("trimright", h4("Upper bound"),
                                   choices = list("None"=1,"1%"=0.99,"2%"=0.98,"3%"=0.97,"5%"=0.95,"10%"=0.9),
                                   selected = 0)
                ),
                column(width = 1,
                       checkboxInput("translog", label = "log tranform", F)
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("Distribution")
                ),
                column(width = 12,
                       withSpinner(plotOutput("ecahistplot", height = 300),color="#9030B0")
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("ECa Points")
                ),
                column(width = 12,
                       withSpinner(plotOutput("ecapointsplot", height = 500),color="#30B090")
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("Variogram")
                ),
                column(width = 12,
                       withSpinner(plotOutput("ecavariogramplot", height = 400),color="#6070E0")
                )
              ),
              fluidRow(
                column(width = 12,
                       h3("Kriging")
                ),
                column(width = 3,
                       selectInput("polygonfile", h4("Polygon - field perimeter [optional]"),
                                   choices = c("None"="none",list.files(path = polygonsDir,pattern="*.shp"))),
                       actionButton("krigeButton", "Krige")
                ),
                column(width = 12,id="krigewrap",
                       div(id="krigeplot",
                           withSpinner(plotOutput("ecakrigplot", height = 1200),color="#C090B0")
                       ),
                       div(id="dlRasterBox",
                           box(width = 2,
                               downloadButton("dlRaster", "Download Rasters")
                           ),
                           box(width = 4,
                               actionButton("addRasterButton", "Add rasters to project"),
                               div(id="addToProjectBox",
                                   selectInput("destinationProject", p("Destination project"), 
                                               choices = as.list(list.dirs(projectsDir, recursive=FALSE,full.names = F))
                                   ),
                                   actionButton("addRaster", "Add to project")
                               ),
                               div(id="message4","")
                           )
                       )
                )
              )
      ),
      
      
      
      # Summary tab content
      tabItem(tabName = "MZs",
              fluidRow(
                column(width = 4,
                       h2("Management Zones")
                )
              ),
              
              fluidRow(
                column(width = 2,
                       selectInput("mzsProject", p("Project"), 
                                   choices = c("",list.dirs(projectsDir, recursive=FALSE,full.names = F))
                       )
                ),
                column(width = 10,
                       box(width = 12,solidHeader = T,
                           h4("Input layers"),
                           uiOutput("layersCheckbox")
                       )
                ),
                column(width = 12,
                       withSpinner(plotOutput("cSourcePlot", height = 400),color="#C09050")
                )
              ),
              
              fluidRow(
                box(width = 12,solidHeader = T,
                    hr(style="border-color: purple;"),
                    column(width = 12,
                           box(width = 4,solidHeader = T,
                               radioButtons("cSwitch", h4(strong("Select view")), inline = T,
                                            choices = list(
                                              "Clustering" = "clusters",
                                              "Cluster Validity Indices" = "indices"
                                            ),
                                            selected = "clusters")
                               #textOutput("console"),
                               
                               
                           ),
                           box(width = 2,solidHeader = T,id="ncl",
                               selectInput("nclust", h4("Number of clusters"),
                                           choices = list("None"=1,2,3,4,5,6,7,8),
                                           selected = 1)
                           )
                    ),
                    
                    fluidRow(    
                      column(width = 12,id="plotclust",
                             h4("Clusters"),
                             column(width = 8,
                                    withSpinner(plotOutput("cplot", height = 800),color="#90A0C0"),
                                    
                                    div(id="dlButtonCluster",
                                        downloadButton("dlRasterCluster", "Download Raster")
                                    ),
                                    pre(id = "console")
                             ),
                             column(width = 4,
                                    withSpinner(plotOutput("cboxplot", height = 540),color="#E0A050")
                             )
                      ),
                      
                      column(width = 12,id="plotindex",
                             h4("Cluster Validity Indices - number of zones"),
                             column(width = 12,
                                    withSpinner(plotOutput("cviplot", height = 1200),color="#90A0C0")
                             )
                      )
                    )
                )
              )
      ),
      
      
      
      # Soil Sampling tab content
      tabItem(tabName = "sampling",
              fluidRow(
                column(width = 2,
                       h2("Soil sample design")
                )
              ),
              
              fluidRow(
                column(width = 2,
                       selectInput("samplingProject", h4("Project"), 
                                   choices = c("",list.dirs(projectsDir, recursive=FALSE,full.names = F))
                       )
                ),
                column(width = 10,
                       box(width = 12,solidHeader = T,
                           h4("Input layers"),
                           uiOutput("samplingLayers")
                       )
                ),
                column(width = 12,
                       withSpinner(plotOutput("sSourcePlot", height = 400),color="#C09050")
                )
              ),
              
              fluidRow(
                h2(""),
                column(width = 2,
                       sliderInput("npoints", h4("Points range"), min = 7, max = 30, value = c(12, 18))
                ),
                column(width = 2,
                       radioButtons("stratification", h4("Method"),inline=T,
                                    choices = list("Quantiles" = "quantiles", "Clusters" = "clusters"), selected = "quantiles"),
                       div(id="oincluded",
                           checkboxInput("outlier.rm", label = "Include only 25%-75% percentile", T)
                       )
                ),
                column(width = 2,
                       radioButtons("seed", h4("Initialization"),
                                    choices = list("Deterministic" = "deterministic","Random seed" = "random"), selected = "deterministic")
                ),
                column(width = 2,
                       h2(""),
                       actionButton("sampleDesignButton", "Run sample design")
                )
              ),
              fluidRow(
                column(width = 12,id="sampleResults",
                       withSpinner(plotOutput("samplesplot", height = 3200),color="#603090")
                )
              ),
              
              
              fluidRow(id="sampleMetrics",
                       column(width = 12,
                              h2(""),
                              hr()
                              #actionButton("sampleMetricsButton", "Run samples metrics")
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("sampleMetricsPlot", height = 1200),color="#C060C0")
                       )
              ),
              
              fluidRow(id="sampleDownload",
                       column(width = 12,
                              h4("Download all sampling plans"),
                              p("CRS: WGS84 / UTM 36N"),
                              downloadButton("dlSamples", "Download Sampling plans")
                       )
              ),
              fluidRow(id="samplingPlan",
                       column(width = 12,
                              hr(),
                              h4("Plot individual sampling plan")
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("samplingSize", h5("Select sample size"), 
                                          choices = c("",seq(3,30,by=1))
                              )
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("samplingZones", h5("Number of Management Zones"), 
                                          choices = c(1:6)
                              )
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("samplingPlanPlot", height = 600),color="#9030E0")
                       ),
                       column(width=12,
                              downloadButton("dlSample", "Download this sampling plan")
                       )
              )
              
      ),
      
      
      
      
      
      # Variogram index tab content
      tabItem(tabName = "variogram",
              fluidRow(
                column(width = 2,
                       h2("Variogram index X"),
                       h5("[Glass, 2003]")
                )
              ),
              
              fluidRow(
                box(width = 12,solidHeader = T,
                    column(width = 4,
                           box(width = 6,solidHeader = T,
                               selectInput("vSamplingProject", h4("Project"), 
                                           choices = c("",list.dirs(projectsDir, recursive=FALSE,full.names = F))
                               )),
                           box(width = 12,solidHeader = T,
                               h4("Layer"),
                               uiOutput("vSamplingLayers")
                           )
                    ),
                    column(width = 6,
                           withSpinner(plotOutput("vSourcePlot", height = 400),color="#C09050")
                    )
                )
              ),
              
              fluidRow(
                
                column(width = 12,
                       h3("Variogram index by sample size",class="mt-5, text-center")
                ),
                box(width = 12,
                    column(width = 3,
                           radioButtons("vNpoints", h4("Max sample size"), inline = T,
                                        choices = list(120,180,240,320,480), selected = 240)
                    ),
                    # column(width = 3,
                    #        radioButtons("vSMethod", h4("Sampling method"), inline = T,
                    #                     choices = list("Grid"="grid","Centroids"="centroids"), selected = "grid")
                    # ),
                    
                    column(width = 3,
                           radioButtons("vIterations", h4("Number of runs:"), inline = T,
                                        choices = list(1,10,20,50), selected = 1)
                    ),
                    
                    
                    column(width = 6,
                           radioButtons("vData", h4("Data"), inline = T,
                                        choices = list("ECa"="eca","Sythetic (Uniform distribution)"="uniform","Sythetic (Normal distribution)"="normal"), selected = "eca")
                    )
                )
                
              ),
              
              fluidRow(
                # box(width = 12,solidHeader = T,
                #     column(width = 2,
                #            h4("Variogram parameters")
                #     ),
                #     column(width = 1,  
                #            selectInput("model", "Model",
                #                        choices = list("Auto"="auto","Sphere"="Sph","Matern"="Mat","Exponential"="Exp","Gaussian"="Gau"),
                #                        selected = "auto")
                #     ),
                #     column(width = 1,
                #            numericInput("nugget", "Nugget:", 0, min = 0, max = 1,step=0.1)
                #     ),
                #     column(width = 1,
                #            numericInput("sill", "Sill:", 1, min = 0, max = 1,step=0.1)
                #     ),
                #     column(width = 1,
                #            numericInput("range", "Range:", 10, min = 0, max = 500,step=10)
                #     )
                # )
              ),
              
              fluidRow(
                # column(width = 6, h4("Variogram model and variogram cloud", class="text-center mt-5") ),
                # column(width = 6, h4("Sample", class="text-center mt-5") ),
                # column(width = 12,
                #        withSpinner(plotOutput("variogramsPlot", height = 1400),color="#C060C0")
                # ),
                column(width = 12,h2(" "))
              ),
              fluidRow(
                column(width = 12, h4("Variogram index X vs sample size", class="text-center mt-5") ),
                column(width = 10, offset=1,
                       #withSpinner(plotOutput("XssPlot", height = 400),color="#805040")
                       withSpinner(plotOutput("XssMultiPlot", height = 600),color="#805040")
                )
              )
              
      ),
      
      
      # QC Sample design - tab content
      tabItem(tabName = "QC",
              fluidRow(
                column(width = 12,
                       h3("Quantile-Cluster method for soil sample design")
                )
              ),
              
              fluidRow(
                column(width = 2,
                       selectInput("QCsamplingProject", h4("Project"), 
                                   choices = c("",list.dirs(projectsDir, recursive=FALSE,full.names = F))
                       )
                ),
                column(width = 10,
                       box(width = 12,solidHeader = T,
                           h4("Input layers"),
                           uiOutput("QCsamplingLayers")
                       )
                ),
                column(width = 12,
                       withSpinner(plotOutput("QCSourcePlot", height = 300),color="#C09050")
                )
              ),
              
              fluidRow(
                h2(""),
                column(width = 2,
                       h4("Points range"),
                       #sliderInput("QCnpoints", h4("Points range"), min = 4, max = 30, value = c(6, 10))
                       box( width = 12,
                            column(width = 6,
                              selectInput("QC.min", h4("Min"), choices = list(4,6, 20, 30, 40), selected = 10) ),
                            column(width = 6, selectInput("QC.max", h4("Max"), choices = list(20, 30, 40, 50,60,100,200), selected = 20) )
                       )
                ),
                column(width = 2,
                       radioButtons("QCstratification", h4("Stratification Method"),inline=T,
                                    choices = list("Quantiles" = "quantiles", "Clusters" = "clusters"), selected = "quantiles"),
                       div(id="QCoincluded",
                           checkboxInput("QCoutlier.rm", label = "Include only 25%-75% percentile", T)
                       )
                ),
                column(width = 2,
                       radioButtons("QCseed", h4("Seed"),
                                    choices = list("Deterministic" = "deterministic","Random" = "random"), selected = "deterministic")
                ),
                
                # column(width = 2,
                #        numericInput("QCstep", "step size:", 1, min = 1, max = 10),
                # ),

                column(width = 2,
                       h2(""),
                       actionButton("QCsampleDesignButton", "Run sample design")
                )
              ),
              fluidRow(
                column(width = 12,id="QCsampleResults",
                       withSpinner(plotOutput("QCsamplesplot", height = 3200),color="#603090")
                )
              ),
              
              
              fluidRow(id="QCsampleMetrics",
                       column(width = 12,
                              h2(""),
                              hr()
                              #actionButton("sampleMetricsButton", "Run samples metrics")
                       ),
                       column(width = 8, offset = 2,
                              withSpinner(plotOutput("QCsampleMetricsPlot", height = 1200),color="#C060C0")
                       )
              ),
              
              fluidRow(id="QCsampleDownload",
                       column(width = 12,
                              h4("Download all sampling plans"),
                              p("CRS: WGS84/UTM36N"),
                              downloadButton("QCdlSamples", "Download Sampling plans")
                       )
              ),
              fluidRow(id="QCsamplingPlan",
                       column(width = 12,
                              hr(),
                              h4("Plot individual sampling plan")
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("QCsamplingSize", h5("Select sample size"), 
                                          choices = c("",seq(4,30,by=1))
                              )
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("QCsamplingZones", h5("Number of Management Zones"), 
                                          choices = c(1:8)
                              )
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("QCsamplingPlanPlot", height = 600),color="#9030E0")
                       ),
                       column(width=12,
                              downloadButton("QCdlSample", "Download this sampling plan")
                       )
              )
      ),
      # QC2023 - tab content
      tabItem(tabName = "QC2023",
              fluidRow(
                column(width = 12,
                       h4(strong("QC: a Quantile-Cluster method for soil sample design"))
                )
              ),
              
              fluidRow(
                column(width = 2,
                       selectInput("QC23samplingProject", h4("Project"), 
                                   choices = c("",list.dirs(projectsDir, recursive=FALSE,full.names = F))
                       )
                ),
                column(width = 10,
                       box(width = 12,solidHeader = T,
                           h4("Input layers"),
                           uiOutput("QC23samplingLayers")
                       )
                ),
                column(width = 12,
                       withSpinner(plotOutput("QC23SourcePlot", height = 300),color="#C09050")
                )
              ),
              
              fluidRow(
                h2(""),
                column(width = 2,
                       h4("Points range"),
                       #sliderInput("QCnpoints", h4("Points range"), min = 4, max = 30, value = c(6, 10))
                       box( width = 12,
                            column(width = 6,
                                   selectInput("QC23.min", h4("Min"), choices = list("-"=0,4,6,10, 20, 30, 40), selected = 0) ),
                            column(width = 6, selectInput("QC23.max", h4("Max"), choices = list("-"=0,20, 30, 40, 50,60,100,200), selected = 0) )
                       )
                ),
                column(width = 2,
                       radioButtons("QC23stratification", h4("Stratification method"),inline=T,
                                    choices = list("Quantiles" = "quantiles", "Clusters" = "clusters"), selected = "quantiles"),
                       div(id="QC23oincluded",
                           checkboxInput("QC23outlier.rm", label = "Include only 25%-75% percentile", T)
                       )
                ),
                column(width = 1,
                       radioButtons("QC23seed", h4("Seed"),
                                    choices = list("Deterministic" = "deterministic","Random" = "random"), selected = "deterministic")
                ),
                
                column(width = 2,
                  selectInput("QC23variance", h4("Filter by ECa variance"), choices = list("None"=0, "Lowest 10%"=10, "Lowest 15%"=15, "Lowest 20%"=20, "Lowest 50%"=50), selected = 15)
                ),
                
                column(width = 2,
                       h4("Extra points"),
                       checkboxInput("QC23extra", label = "Add 10% random points at close range", T)
                )
                
                # column(width = 2,
                #        numericInput("QCstep", "step size:", 1, min = 1, max = 10),
                # ),
                # 
                # column(width = 2,
                #        h2(""),
                #        actionButton("QC23sampleDesignButton", "Run sample design")
                # )
              ),
              fluidRow(
                column(width = 12,id="QC23sampleResults",
                       withSpinner(plotOutput("QC23samplesplot", height = 3200),color="#603090")
                )
              ),
              
              fluidRow(
                column(width = 12, 
                              h2(""),
                              hr(),
                              actionButton("QC23sampleMetricsButton", "Run samples metrics")
                       ),
                       column(width = 8, offset = 2, id="QC23sampleMetrics",
                              #uiOutput("QC23sampleMetricsPlot")
                              withSpinner(plotOutput("QC23sampleMetricsPlot", height = 1200),color="#C060C0")
                       )
              ),
              
              fluidRow(id="QC23sampleDownload",
                       column(width = 12,
                              h4("Download all sampling plans"),
                              p("CRS: WGS84/UTM36N"),
                              downloadButton("QC23dlSamples", "Download Sampling plans")
                       )
              ),
              fluidRow(id="QC23samplingPlan",
                       column(width = 12,
                              hr(),
                              h4("Plot individual sampling plan")
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("QC23samplingSize", h5("Select sample size"), 
                                          choices = c("",seq(4,30,by=1))
                              )
                       ),
                       column(width = 2,
                              h4(""),
                              selectInput("QC23samplingZones", h5("Number of Management Zones"), 
                                          choices = c(1:8)
                              )
                       ),
                       column(width = 12,
                              withSpinner(plotOutput("QC23samplingPlanPlot", height = 600),color="#9030E0")
                       ),
                       column(width=12,
                              downloadButton("QC23dlSample", "Download this sampling plan")
                       )
              )
      )
      
      
    )
  )
)


# 
# withConsoleRedirect <- function(containerId, expr) {
#   # Change type="output" to type="message" to catch stderr
#   # (messages, warnings, and errors) instead of stdout.
#   txt <- capture.output(results <- expr, type = "output")
#   if (length(txt) > 0) {
#     insertUI(paste0("#", containerId), where = "beforeEnd",
#              ui = paste0(txt, "\n", collapse = "")
#     )
#   }
#   results
# }


server <- function(input, output, session) {
  #  set.seed(122)
  #  histdata <- rnorm(500)
  
  
  
  
  
  output$contents <- renderDataTable({
    
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # # or all rows if selected, will be shown.
    # 
    # req(input$file1)
    # 
    # df <- loadECa(input$file1$datapath)
    #   # data.table(read.csv(input$file1$datapath,
    #   #              header = T, #input$header,
    #   #              sep = ',',  #input$sep,
    #   #              quote = '"' #input$quote
    #   #              ))     
    # 
    # #df.clean<-df[rowSums(is.na(df[,4])) == 0,]
    # 
    # if(input$disp == "head") {
    #   return(head(df))
    # }
    # else {
    #   return(df)
    # }
  },
  options = list(scrollX = TRUE,paging = FALSE)
  )
  
  # getData <- reactive({
  #   inFile <- input$file1
  #   if (is.null(input$file1))
  #     return(NULL)
  #     read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
  # })
  # 
  # output$contents <- renderTable(
  #   getData()
  # )
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() { 
  #     paste("data-", Sys.Date(), ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(getData(), file)
  #   })
  # 
  
  # EM38 file upload
  observe({
    if (is.null(input$file1)) {
      shinyjs::hide('frename')
      shinyjs::hide('fadded')
      #shinyjs::hide('tabledisp')
      return()
    }
    
    shinyjs::show('frename')
    #shinyjs::show('tabledisp')
    shinyjs::hide('fadded')
    updateTextInput(session, "fileName", value = substr(input$file1$name, 0, nchar(input$file1$name)-4))
  })
  
  
  observeEvent(input$addFile, {
    if (is.null(input$file1)) return()
    file.copy(input$file1$datapath, paste0(uploadDir, paste0(input$fileName,'.csv')))
    
    shinyjs::html("filelist", as.list(getFiles(uploadDir)))
    updateSelectInput(session, "rawfile", h4("Source data"),choices = list.files(path = uploadDir))
    #updateSelectInput(session, "tvfile", h4("Source data"),choices = list.files(path = uploadDir))
    shinyjs::hide('frename')
    shinyjs::show('fadded')
    #shinyjs::show('tabledisp')
  })
  
  
  
  # Polygon file upload
  observe({
    if (is.null(input$file2)) {
      shinyjs::hide('fadded2')
      shinyjs::hide('frename2')
      return()
    }
    
    shinyjs::hide('fadded2')
    shinyjs::show('frename2')
    #updateTextInput(session, "polygonFileName", value = substr(input$file2$name, 0, nchar(input$file2$name)-4))
  })
  
  
  observeEvent(input$addFile2, {
    if (is.null(input$file2)) return()
    file.copy(input$file2$datapath, paste0(polygonsDir, input$file2$name))
    #substr(input$file2$name, 0, nchar(input$file1$name)-4)
    
    shinyjs::html("filelist2", as.list(getPolygonFiles(polygonsDir)))
    updateSelectInput(session, "polygonfile", h4("Polygon - field perimeter [optional]"),choices = list.files(path = polygonsDir,pattern="*.shp"))
    shinyjs::hide('frename2')
    shinyjs::show('fadded2')
  })
  
  
  # new project
  observeEvent(input$newProjButton, {
    shinyjs::show('newproj')
  })
  
  observeEvent(input$createProjButton, {
    if (is.null(input$newProjName) & length(input$newProjName)>0) return()
    #file.copy(input$file1$datapath, paste0(uploadDir, paste0(input$fileName,'.csv')))
    dirPath<-(paste0(projectsDir,input$newProjName))
    
    if(dir.exists(dirPath)){
      html("message3","Project with this name already exists.")
      return()
    }
    
    dir.create(dirPath)
    Sys.chmod(dirPath, "777", use_umask = FALSE)
    
    shinyjs::html("projectsList", getProjects(projectsDir))
    updateSelectInput(session,"mzsProject", p("Project"), choices = as.list(list.dirs(projectsDir, recursive=FALSE,full.names = F)))
    updateSelectInput(session,"destinationProject", p("Destination project"), choices = as.list(list.dirs(projectsDir, recursive=FALSE,full.names = F)))
    updateCheckboxGroupInput(session,"sampleLayers","", choices = list.files(path = paste0(projectsDir,input$samplingProject)),inline = T)
    
    updateSelectInput(session,"vSampleLayers","", choices = list.files(path = paste0(projectsDir,input$vSamplingProject))) #,inline = T)
    
    shinyjs::hide('newproj')
    shinyjs::show('padded')
    #shinyjs::show('tabledisp')
  })
  
  
  
  # observeEvent(input$rawfile, {
  #   x.path <- paste0(uploadDir, input$rawfile)
  #   xdat <- data.table(read.csv(x.path,header = T,sep = ',',quote = '"'))
  #   x<-xdat[rowSums(is.na(xdat[,4])) == 0,]
  #   colnames(x)[2]<-'Well'
  #   
  #   updateSelectInput(session, "blankRows", h4("select blanks"),choices = as.list(x$Well))
  # })
  # 
  # observeEvent(input$tvfile, {
  #   x.path <- paste0(uploadDir, input$tvfile)
  #   xdat <- data.table(read.csv(x.path,header = T,sep = ',',quote = '"'))
  #   x<-xdat[rowSums(is.na(xdat[,4])) == 0,]
  #   colnames(x)[2]<-'Well'
  # 
  #   updateSelectInput(session, "blankRowsTV", h4("select blanks"),choices = as.list(x$Well))
  # })
  # 
  # observeEvent(input$sublank, { updateCheckboxInput(session, "sublankTV", value = input$sublank) })
  # observeEvent(input$sublankTV, { updateCheckboxInput(session, "sublank", value = input$sublankTV) })
  
  # observe({
  #   selectedBlanks <- input$blankRows
  #   if (is.null(selectedBlanks)) {selectedBlanks <- character(0)}
  #   x.path <- paste0(uploadDir, input$rawfile)
  #   xdat <- data.table(read.csv(x.path,header = T,sep = ',',quote = '"'))
  #   x<-xdat[rowSums(is.na(xdat[,4])) == 0,]
  #   colnames(x)[2]<-'Well'
  #   updateSelectInput(session, "blankRowsTV",label = "select blanks", choices = as.list(x$Well),selected = selectedBlanks)
  # })
  # 
  #   observe({
  #   selectedBlanksTV <- input$blankRowsTV
  #   if (is.null(selectedBlanksTV)) {selectedBlanksTV <- character(0)}
  #   x.path <- paste0(uploadDir, input$tvfile)
  #   xdat <- data.table(read.csv(x.path,header = T,sep = ',',quote = '"'))
  #   x<-xdat[rowSums(is.na(xdat[,4])) == 0,]
  #   colnames(x)[2]<-'Well'
  #   updateSelectInput(session, "blankRows",label = "select blanks", choices = as.list(x$Well),selected = selectedBlanksTV)
  # })
  # 
  #   
  #   
  #   observe({
  #     if (input$blankType=="ALS") {
  #       shinyjs::hide('manblank')
  #     } else {
  #       shinyjs::show('manblank')
  #     }
  #   })
  #   
  #   
  #   observe({
  #     if (input$blankTypeTV=="ALS") {
  #       shinyjs::hide('manblankTV')
  #     } else {
  #       shinyjs::show('manblankTV')
  #     }
  #   })
  
  observeEvent(input$oldFileName, { 
    if(input$oldFileName=="" | input$newFileName == "" | input$newFileName == input$oldFileName) { return() }
    
    if(file.copy(paste0(uploadDir, input$oldFileName), paste0(uploadDir, input$newFileName))){
      #shinyjs::html('message','copied. ')
      shinyjs::hide(selector = "span.newname")
      if (file.exists(paste0(uploadDir, input$oldFileName))) {
        file.remove(paste0(uploadDir, input$oldFileName))
        #shinyjs::html('fadded','deleted old. ')
        #shinyjs::show('fadded')
      }
      
      shinyjs::html("filelist", as.list(getFiles(uploadDir)))
      updateSelectInput(session, "rawfile", h4("Source data"),choices = list.files(path = uploadDir))
      #updateSelectInput(session, "tvfile", h4("Source data"),choices = list.files(path = uploadDir))
    } else { 
      shinyjs::html('message','err')
    }
  })
  
  
  
  observeEvent(input$stageDelete, {
    if(input$stageDelete!=''){
      #print(input$stageDelete)
      #fn <- paste0(uploadDir,input$stageDelete)
      fn <- trimws(input$stageDelete)
      
      if (!is.null(input$stageDelete) & file.exists(fn)) {
        file.remove(fn)
        print(paste("Removed:",fn))
        shinyjs::html("filelist", as.list(getFiles(uploadDir)))
        shinyjs::html("filelist2", as.list(getPolygonFiles(polygonsDir)))
        shinyjs::html("projectsList", getProjects(projectsDir))
        
        updateSelectInput(session, "rawfile", h4("Source data"),choices = list.files(path = uploadDir))
        updateSelectInput(session, "polygonfile", h4("Polygon - field perimeter [optional]"),choices = list.files(path = polygonsDir,pattern="*.shp"))
        updateSelectInput(session,"destinationProject", p("Destination project"), choices = as.list(list.dirs(projectsDir, recursive=FALSE,full.names = F)))
        updateCheckboxGroupInput(session,"sampleLayers","", choices = list.files(path = paste0(projectsDir,input$destinationProject)),inline = T)
        
      }
    }
  })
  
  
  observeEvent(input$stageDownload, {
    if(input$stageDownload!=''){
      fn=input$stageDownload
      updateTextInput(session, "stageDownload", value = fn)
    }
  })
  
  
  
  # auto-variogram
  observeEvent(input$model, {
    if(input$model=='auto'){
      shinyjs::hide('nugget')
      shinyjs::hide('sill')
      shinyjs::hide('range')
    } else {
      shinyjs::show('nugget')
      shinyjs::show('sill')
      shinyjs::show('range')
    }
  })
  # 
  # 
  # observeEvent(input$stageDownload, {
  #   if(input$stageDownload!=''){
  #     fn <- paste0(uploadDir,input$stageDownload)
  #     if (!is.null(input$stageDelete) & file.exists(fn)) {
  #       file.remove(fn)
  #       shinyjs::html("filelist", as.list(getFiles(uploadDir)))
  #       updateSelectInput(session, "rawfile", h4("Source data"),choices = list.files(path = uploadDir))
  #       updateSelectInput(session, "tvfile", h4("Source data"),choices = list.files(path = uploadDir))
  #     }
  #   }
  # })
  
  # output$stageDownload <- downloadHandler(
  #   filename <- function() {
  #     paste0(uplodDir,input$stageDownload)
  #   },
  #   
  #   content <- function(file) {
  #     file.copy("out.csv", file)
  #   }
  # )
  
  
  # datasetInput <- reactive({
  #   if(input$stageDownload!="") {
  #     fname = paste0(uploadDir,input$stageDownload)
  #     
  #     downloadHandler(
  #       filename = fname,
  #       content = function(file) {
  #         if(input$stageDownload!=''){
  #           df <- data.table(read.csv(fname,header = T))
  #           write.csv(df, file, row.names = FALSE)
  #         }
  #       }
  #     )
  #   } else {
  #     return(FALSE)
  #   }
  # })
  
  # Downloadable csv of selected dataset ----
  
  # 
  # output$downloadRpt <- downloadHandler(
  #   filename = function() {
  #     input$stageDownload
  #   },
  #   
  #   content = function(file) {
  #     
  #     mydf <- report()
  #     dateRange <- input$dates_report
  #     selection <- input$company
  #     export_report(mydf,selection,dateRange)
  #     myfile <- generate_file_name(selection)
  #     file.copy(myfile, file)
  #     
  #   }
  # )
  
  
  
  # 
  #   observeEvent( input$stageDownload , {
  #     if(!is.null(input$stageDownload) & input$stageDownload!=''){
  #       print(paste0(uploadDir,input$stageDownload))      
  #       df <- data.table(read.csv(paste0(uploadDir,input$stageDownload),header = T))
  #       write.csv(df, input$stageDownload, row.names = FALSE)
  #     }
  #   })
  #   
  #   output$downloadData <- downloadHandler(
  #     filename = function() {
  #       paste("data-", Sys.Date(), ".csv", sep="")
  #     },
  #     content = function(file) {
  #       df <- data.table(read.csv(uploadDir,datasetInput(),header = T))
  #       write.csv(df, file)
  #     }
  #   )
  
  #,rawFileType=input$sep
  output$ecahistplot <- renderPlot({ecaHist(rawFile=input$rawfile,compactFactor=input$compactBy,trimLeft=input$trimleft,trimRight=input$trimright,log=input$translog)})
  
  output$ecapointsplot <- renderPlot({ecaPoints(rawFile=input$rawfile,compactFactor=input$compactBy,trimLeft=input$trimleft,trimRight=input$trimright,log=input$translog)})
  
  output$ecavariogramplot <- renderPlot({ecaKrige(rawFile=input$rawfile,compactFactor=input$compactBy,trimLeft=input$trimleft,trimRight=input$trimright,log=input$translog,plotType="variogram")})
  
  # Krige
  observeEvent(input$krigeButton, {
    shinyjs::hide('dlRasterBox')
    shinyjs::show('krigeplot')
    output$ecakrigplot <- renderPlot({ecaKrige(rawFile=input$rawfile,compactFactor=input$compactBy,trimLeft=input$trimleft,trimRight=input$trimright,log=input$translog,plotType="krige",perimeterFile=input$polygonfile)})
    shinyjs::show('dlRasterBox')
  })
  
  
  # Download current kriging rasters
  output$dlRaster <- downloadHandler(
    filename = function() { paste0("ECa_kriged_",input$rawfile,".zip")},
    content = function(file) { file.copy(paste0(stagingDir,"ECa_kriged.zip"), file) }
  )
  
  
  
  observeEvent(input$addRasterButton, {
    shinyjs::show('addToProjectBox')
    shinyjs::hide('addRasterButton')
    html("message4","")
    updateCheckboxGroupInput(session,"sampleLayers","", choices = list.files(path = paste0(projectsDir,input$samplingProject)),inline = T)
    updateSelectInput(session,"vSampleLayers","", choices = list.files(path = paste0(projectsDir,input$vSamplingProject))) #,inline = T)
  })
  
  # Add current kriged rasters to project
  observeEvent(input$addRaster, {
    if(length(input$destinationProject)>0){
      #print(paste0("Copied to: ", projectsDir,input$destinationProject,"/ECa_1m_",format(Sys.time(),"%y%m%d-%H%M%S"),".tif"))
      if(input$translog==T){logt="_log"} else {logt=""}
      file.copy(paste0(stagingDir,"ECa_1m.tif"), paste0(projectsDir,input$destinationProject,"/",gsub("_", " ", substr(input$rawfile, 0, nchar(input$rawfile)-4)),"_1m_c",input$compactBy,"_",as.numeric(input$trimleft)*100,"-",as.numeric(input$trimright)*100,"_",substr(input$polygonfile, 0, nchar(input$polygonfile)-4),logt,".tif"))
      file.copy(paste0(stagingDir,"ECa_05m.tif"), paste0(projectsDir,input$destinationProject,"/",gsub("_", " ", substr(input$rawfile, 0, nchar(input$rawfile)-4)),"_05m_c",input$compactBy,"_",as.numeric(input$trimleft)*100,"-",as.numeric(input$trimright)*100,"_",substr(input$polygonfile, 0, nchar(input$polygonfile)-4),logt,".tif"))
      
      var.dir <- paste0(projectsDir,paste0(input$destinationProject,"variance/"))
      if (!file.exists(var.dir)){ dir.create(file.path(var.dir)) }
      file.copy(paste0(stagingDir,"variance_1m.tif"), paste0(projectsDir,input$destinationProject,"/variance/",gsub("_", " ", substr(input$rawfile, 0, nchar(input$rawfile)-4)),"_1m_c",input$compactBy,"_",as.numeric(input$trimleft)*100,"-",as.numeric(input$trimright)*100,"_",substr(input$polygonfile, 0, nchar(input$polygonfile)-4),logt,"_variance.tif"))
      file.copy(paste0(stagingDir,"variance_05m.tif"), paste0(projectsDir,input$destinationProject,"/variance/",gsub("_", " ", substr(input$rawfile, 0, nchar(input$rawfile)-4)),"_05m_c",input$compactBy,"_",as.numeric(input$trimleft)*100,"-",as.numeric(input$trimright)*100,"_",substr(input$polygonfile, 0, nchar(input$polygonfile)-4),logt,"_variance.tif"))
      
      shinyjs::hide('addToProjectBox')
      shinyjs::show('addRasterButton')
      shinyjs::html("projectsList", getProjects(projectsDir))
      html("message4",paste("Layers added to project <b>",input$destinationProject,"</b>"))
      delay(2000, html("message4",""))
    }
  })
  
  
  
  
  # Clustering
  output$layersCheckbox <- renderUI({
    if(input$mzsProject!=""){
      checkboxGroupInput("clustLayers","", choices = list.files(path = paste0(projectsDir,input$mzsProject)),inline = T)
    }
  }) 
  
  output$cSourcePlot <- renderPlot({clusterSourcePlot(project=input$mzsProject,layers=input$clustLayers)})  
  
  output$cplot <- renderPlot({
    if(input$cSwitch=="clusters"){ 
      clusters(nclust=input$nclust,project=input$mzsProject,layers=input$clustLayers)       
    }
  })  
  
  output$cboxplot <- renderPlot({
    if(input$cSwitch=="clusters"){ 
      clusterBox(nclust=input$nclust,project=input$mzsProject,layers=input$clustLayers)
    }
  })  
  
  output$cviplot <- renderPlot({
    if(input$cSwitch=="indices"){
      #withConsoleRedirect("console", {
        clustersValidity(project=input$mzsProject,layers=input$clustLayers)
      #})
    }
  })
  
  observeEvent(input$cSwitch, {
    if(input$cSwitch=="indices"){
      shinyjs::show(id="plotindex")
      shinyjs::show(id="console")
      shinyjs::hide(id="plotclust")
      shinyjs::hide(id="ncl")
      shinyjs::hide(id="dlButtonCluster")
    } else if(input$cSwitch=="clusters"){
      shinyjs::hide(id="plotindex")
      shinyjs::hide(id="console")
      shinyjs::show(id="plotclust")
      shinyjs::show(id="ncl")
      shinyjs::show(id="dlButtonCluster")
    } else {
      shinyjs::hide(id="plotindex")
      shinyjs::hide(id="plotclust")
      shinyjs::hide(id="console")
      shinyjs::hide(id="ncl")
      shinyjs::hide(id="dlButtonCluster")
      #stop("Interrupted")
    }
  })
  
  # Download current clusters raster
  output$dlRasterCluster <- downloadHandler(
    filename = function() { paste0("clusters_",input$nclust,".tif") },
    content = function(file) { file.copy(paste0(stagingDir,"clusters_",input$nclust,".tif"), file) }
  )
  
  
  
  # Sampling
  output$samplingLayers <- renderUI({
    if(input$samplingProject!=""){
      checkboxGroupInput("sampleLayers","", choices = list.files(path = paste0(projectsDir,input$samplingProject)),inline = T)
    } else {
      html("")
    }
  }) 
  
  
  # QC Sampling
  output$QCsamplingLayers <- renderUI({
    if(input$QCsamplingProject!=""){
      checkboxGroupInput("QCsampleLayers","", choices = list.files(path = paste0(projectsDir,input$QCsamplingProject)),inline = T)
    } else {
      html("")
    }
  }) 
  
  
  # QC2023 Sampling
  output$QC23samplingLayers <- renderUI({
    if(input$QC23samplingProject!=""){
      checkboxGroupInput("QC23sampleLayers","", choices = setdiff(list.files(path = paste0(projectsDir,input$QC23samplingProject)), 
                                                                  list.dirs(paste0(projectsDir,input$QC23samplingProject),recursive = FALSE, full.names = FALSE)),inline = T)
                           #list.files(path = paste0(projectsDir,input$QC23samplingProject)),inline = T)
    } else {
      html("")
    }
  }) 
  
  
  output$sSourcePlot <- renderPlot({clusterSourcePlot(project=input$samplingProject,layers=input$sampleLayers)})  
  
  output$QCSourcePlot <- renderPlot({clusterSourcePlot(project=input$QCsamplingProject,layers=input$QCsampleLayers)})  
  
  
  # Variogram index sampling
  output$vSamplingLayers <- renderUI({
    if(input$vSamplingProject!=""){
      selectInput("vSampleLayers", "", 
                  choices = c("",list.files(path = paste0(projectsDir,input$vSamplingProject)))
      )
    } else {
      html("")
    }
  }) 
  
  output$vSourcePlot <- renderPlot({vSourcePlot(project=input$vSamplingProject,layer=input$vSampleLayers)})  
  
  # Creating two watchers
  #  gargoyle::init("sample_plot_valid","metrics_plot_valid")
  
  observeEvent(input$sampleDesignButton, {
    #gargoyle::trigger("sample_plot_valid")
    shinyjs::show('sampleResults')
    shinyjs::show('sampleMetrics')
    shinyjs::show('sampleDownload')
    shinyjs::show('samplingPlan')
    #shinyjs::hide("sampleMetricsPlot")
  })
  
  observeEvent(input$QCsampleDesignButton, {
    #gargoyle::trigger("sample_plot_valid")
    shinyjs::show('QCsampleResults')
    shinyjs::show('QCsampleMetrics')
    shinyjs::show('QCsampleDownload')
    shinyjs::show('QCsamplingPlan')
  })
  
  output$samplesplot <- renderPlot({
    #gargoyle::watch("sample_plot_valid")
    #print(input$outlier.rm)
    sampleDesign(project=input$samplingProject,layers=input$sampleLayers,n.points=input$npoints,s.type=input$stratification,outlier.rm=input$outlier.rm,seed=input$seed)
  })
  
  
  output$QCsamplesplot <- renderPlot({
    QCsampleDesign(project=input$QCsamplingProject,layers=input$QCsampleLayers,n.points.min=input$QC.min,n.points.max=input$QC.max,s.type=input$QCstratification,outlier.rm=input$QCoutlier.rm,seed=input$QCseed)
  })
  
  
    observeEvent(input$stratification, {
    if(input$stratification=="quantiles"){
      shinyjs::show('oincluded')
    } else {
      shinyjs::hide('oincluded')
    }
  })
  
  observeEvent(input$QCstratification, {
    if(input$stratification=="quantiles"){
      shinyjs::show('QCoincluded')
    } else {
      shinyjs::hide('QCoincluded')
    }
  })
  
    # observeEvent(input$sampleMetricsButton, {
  #   #gargoyle::trigger("metrics_plot_valid")
  #   shinyjs::show("sampleMetricsPlot")
  # })
  
  output$sampleMetricsPlot <- renderPlot({
    #gargoyle::watch("metrics_plot_valid")
    sampleMetrics(project=input$samplingProject,layers=input$sampleLayers,npoints=input$npoints,s.type=input$stratification,outlier.rm=input$outlier.rm,seed=input$seed)
  })
  
  output$dlSamples <- downloadHandler(
    filename = function() { paste0("sampling_plans_results_",min(input$npoints),"-",max(input$npoints),".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"sampling_plans_results.csv"), file) }
  )
  
  output$QCdlSamples <- downloadHandler(
    filename = function() { paste0("QC_sampling_plans_results_",min(input$npoints),"-",max(input$npoints),".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"QC_sampling_plans_results.csv"), file) }
  )
  
  output$QCsampleMetricsPlot <- renderPlot({
    QCsampleMetrics(project=input$QCsamplingProject,layers=input$QCsampleLayers,npoints=input$QCnpoints,s.type=input$QCstratification,outlier.rm=input$QCoutlier.rm,seed=input$QCseed)
  })
  
  output$samplingPlanPlot <- renderPlot({
    #gargoyle::watch("metrics_plot_valid")
    plotSamplingPlan(project=input$samplingProject,layers=input$sampleLayers,sample.size=input$samplingSize,n.zones=input$samplingZones,seed=input$seed,QC=F)
  })
  
  output$QCsamplingPlanPlot <- renderPlot({
    #gargoyle::watch("metrics_plot_valid")
    plotSamplingPlan(project=input$QCsamplingProject,layers=input$QCsampleLayers,sample.size=input$QCsamplingSize,n.zones=input$QCsamplingZones,seed=input$QCseed,QC=T)
  })
  
  output$dlSample <- downloadHandler(
    filename = function() { paste0("sampling_plan_",input$samplingSize,".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"sampling_plan_selected.csv"), file) }
  )
  
  output$QCdlSample <- downloadHandler(
    filename = function() { paste0("QC_sampling_plan_",input$samplingSize,".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"sampling_plan_selected.csv"), file) }
  )
  
  
  
  ### QC2023 ###
  output$QC23SourcePlot <- renderPlot({clusterSourcePlot(project=input$QC23samplingProject,layers=input$QC23sampleLayers)})  
  
  output$QC23samplesplot <- renderPlot({
    v$data <- NULL
    QC23sampleDesign(project=input$QC23samplingProject,layers=input$QC23sampleLayers,n.points.min=input$QC23.min,n.points.max=input$QC23.max,outlier.rm=input$QC23outlier.rm,seed=input$QC23seed,low.var=input$QC23variance,extra.points=input$QC23extra)
  })
  
  
  output$QC23samplingPlanPlot <- renderPlot({
    plotSamplingPlan(project=input$QC23samplingProject,layers=input$QC23sampleLayers,sample.size=input$QC23samplingSize,n.zones=input$QC23samplingZones,seed=input$QC23seed,QC=T)
  })
  
  output$QC23dlSamples <- downloadHandler(
    filename = function() { paste0("QC23_sampling_plans_results_",min(input$QC23.min),"-",max(input$QC23.max),".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"QC23_sampling_plans_results.csv"), file) }
  )
  
  output$QC23dlSample <- downloadHandler(
    filename = function() { paste0("QC23_sampling_plan_",input$QC23samplingSize,".csv")},
    content = function(file) { file.copy(paste0(stagingDir,"sampling_plan_selected.csv"), file) }
  )
  
  observeEvent(input$QC23stratification, {
    if(input$QC23stratification=="quantiles"){
      shinyjs::show('QC23oincluded')
    } else {
      shinyjs::hide('QC23oincluded')
    }
  })
  
  
  v <- reactiveValues(data = NULL)
  
  observeEvent(input$QC23sampleMetricsButton, {
    v$data <- 1
  })  
  
  output$QC23sampleMetricsPlot <- renderPlot({
    if (is.null(v$data)) {
      text(x = 0.5, y = 0.8, paste("Click button to calculate...\n"), cex = 1.2, col = "black")  
    } else {
      QC23sampleMetrics(project=input$QC23samplingProject,layers=input$QC23sampleLayers)
    }
  })
  
  
  
  
  
  
  # observeEvent(input$QC23sampleDesignButton, {
  #   #gargoyle::trigger("sample_plot_valid")
  #   shinyjs::show('QC23sampleResults')
  #   shinyjs::show('QC23sampleMetrics')
  #   shinyjs::show('QC23sampleDownload')
  #   shinyjs::show('QC23samplingPlan')
  # })
  
  
  
  
  
  
  # Variogram index 
  output$variogramsPlot <- renderPlot({
    variogramSample(project=input$vSamplingProject,layer=input$vSampleLayers,n.points=input$vNpoints,v.model=input$model, nugget=input$nugget, sill=input$sill,range=input$range, samp.method=input$vSMethod, data.type=input$vData,to.plot="sample")
  })
  
  
  # Variogram vs Sample-size
  output$XssPlot <- renderPlot({
    variogramSample(project=input$vSamplingProject,layer=input$vSampleLayers,n.points=input$vNpoints,v.model=input$model, nugget=input$nugget, sill=input$sill,range=input$range, samp.method=input$vSMethod, data.type=input$vData,to.plot="xss")
  })
  
  
  # Variogram vs Sample-size
  output$XssMultiPlot <- renderPlot({
    variogramSamples(project=input$vSamplingProject,layer=input$vSampleLayers,n.points=input$vNpoints, data.type=input$vData,to.plot="xss",iterations=input$vIterations)
  })
  
  
  
  
  
}

shinyApp(ui, server)
