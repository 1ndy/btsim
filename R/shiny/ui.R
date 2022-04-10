library(shiny)


helptext <- function() {
  div(
    h1("Bt Simulation"),
    h2("About"),
    p("Bt simulation models the effect of bacillus thuringenesis-derived
      toxins in transgenic corn against the western corn rootworm. It relies on
      principles from population genetics to compute population level changes
      in census number and allele frequency."),
    br(),
    p("This simulation was created for ANSC 446 in SP22 at the University of
       Illinois. Send inquiries to (demeyer3 (at) illinois (dot) edu)"),
    h2("Parameters"),
    HTML(
      '
      <style>
        table, th, td {
          border: 1px solid black;
          border-collapse: collapse;
        }
      </style>
      <table>
        <tr>
          <th>Parameter</th>
          <th>Description</th>
          <th>Value Range</th>
        </tr>
        <tr>
          <td>Generations</td>
          <td>The number of generations to simulate.</td>
          <td>At least 2</td>
        </tr>
        <tr>
          <td>Population Size</td>
          <td>The number of insects to start the simulation with.</td>
          <td>At least 1</td>
        </tr>
        <tr>
          <td>Resistant Allele Frequency</td>
          <td>The starting frequency of the resistant allele, which will always be treated as recessive (i.e. q).</td>
          <td>0..1</td>
        </tr>
        <tr>
          <td>Selection Coefficient</td>
          <td>The selection coefficient to use when computing changes in allele frequency.</td>
          <td>0..1</td>
        </tr>
        <tr>
          <td>Dominance Coefficient</td>
          <td>The dominance level (i.e. h) to be used in certain forms of selection.</td>
          <td>0..1</td>
        </tr>
        <tr>
          <td>Selection Type</td>
          <td>The type of selection to model. Darwinian can make either the dominant allele or the recessive allele advantageous.</td>
          <td>(Listed in dropdown)</td>
        </tr>
        <tr>
          <td>Refuge Crop Percentage</td>
          <td>The percentage of the plants that do not produce Bt toxin.</td>
          <td>0..1</td>
        </tr>
        <tr>
          <td>Bushels per Acre</td>
          <td>This value describes the yield from one acre if no plant is damaged by pests.</td>
          <td>At least 1</td>
        </tr>
        <tr>
          <td>Acreage</td>
          <td>The size of the field being simulated in acres.</td>
          <td>At least 1</td>
        </tr>
        <tr>
          <td>Plants per acre</td>
          <td>The number of plants on each acre of land.</td>
          <td>At least 1</td>
        </tr>
      </table>'
    ),
    h2("References"),
    p('1. Chiang, H. C. "Bionomics of the northern and western corn rootworms." Annual review of entomology 18.1 (1973): 47-72.',a('https://doi.org/10.1146/annurev.en.18.010173.000403'),br(),br(),
      '2. Tabashnik, Bruce E., Thierry Brevault, and Yves Carriere. "Insect resistance to Bt crops: lessons from the first billion acres." Nature biotechnology 31.6 (2013): 510-521.')
  )
}

# Define UI ----
ui <- fluidPage(
  titlePanel("Bt Simulation"),
  sidebarLayout(
    sidebarPanel("Simulation Parameters",
                 numericInput("n_generations", "Generations", 10, min=2, max=200, step=5),
                 numericInput("n_insects", "Initial Pest Population Size", 600000),
                 numericInput("resistant_allele_freq", "Resistant Allele Frequency (recessive)", 0, min=0, max=1, step=0.01),
                 fluidRow(column(6, numericInput("s_val", "selection coefficient", 0.5, min=0, step=0.1)),
                          column(6, numericInput("h_val", "Dominance Coefficient", 0.5, min=0, step=0.1))
                 ),
                 selectInput("selection_mode", "Selection Type", c("Darwinian (Dominant)"="darD",
                                                                   "Darwinian (Recessive)"="darR",
                                                                   "Purifying" = "puri",
                                                                   "Recessive Lethal"="recl",
                                                                   "Detrimental Recessive"="detr",
                                                                   "Detrimental Additive"="deta",
                                                                   "Detrimental Dominant"="detd"), selected="darR"),
                 numericInput("refuge_crop_percentage", "Refuge Crop Percentage", 0.01, min=0,max=1, step=0.01),
                 numericInput("bushels_per_acre", "Bushels per Acre (yield)", 150,min=1),
                 numericInput("acreage", "Acreage", 10,min=1),
                 numericInput("plant_density", "Plants per Acre", 25000, min=1),
                 actionButton("simulate", label="Simulate")
    ),
    mainPanel("Result",
              tabsetPanel(
                tabPanel("Plots",
                         fluidRow(
                           column(6,plotOutput("census_v_generations")),
                           column(6,plotOutput("raf_v_generations"))
                         ),
                         fluidRow(
                           column(6,plotOutput("bpa_v_generations")),
                           column(6,plotOutput("pest_density_v_generations"))
                         )
                ),
                tabPanel("Table",
                         dataTableOutput("sim_data"),
                         downloadButton("download_data", "Download Data")
                ),
                tabPanel("Help", helptext())
              )
    )
  )
)
