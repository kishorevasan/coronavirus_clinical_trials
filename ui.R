# ui page
navbarPage("Coronavirus Clinical Trials",
           tabPanel("Map",
                    div(class="outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("style.css"),
                          includeScript("gomap.js")
                        ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),
                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Clinical Trials Explorer"),
                                      p("Total number of studies till date :",strong(num_records)),
                                      p("Number of studies since nCov outbreak (2019+)", strong(num_new_studies)),
                                      h4("Add Filters:"),
                                      checkboxInput('outbreak','Show trials since outbreak (2019+)'),
                                      checkboxInput('recruiting','Show recruiting trials')
                        )
                    )
           ),
           tabPanel("Home",fluidPage(theme = shinytheme('flatly'),
                                     tags$style(HTML("#plots {height:100px;overflow-y:scroll}")),
                                     pageWithSidebar(
                                       h1("ClinicalTrials.Gov Data"),
                                       sidebarPanel(
                                         p("Total number of studies till date :",strong(num_records)),
                                         p("Number of studies since nCov outbreak (2019+)", strong(num_new_studies)),
                                         p("Number of studies started before 2019 (possibly not nCov)", strong(num_years_before_2019)),
                                         p("Number of studies with date unknown:", strong(num_year_unknown)),
                                         p("Number of studies currently Recruiting:",strong(num_recruiting)),
                                         p("Number of studies completed:", strong(num_completed)),
                                         p("The data was collected using the clinicaltrials.gov search query 'Novel Coronavirus OR Covid 19 OR Sars cov 2'.")
                                       ),
                                       mainPanel(
                                         p("The goal of this application is to provide up to date information on the ongoing clinical trials regarding the",em('Coronavirus.')),
                                         p("The data was collected from", HTML('<a href=\"https:\\clinicaltrials.gov">ClinicalTrials.Gov</a>')," using their API. Please note that the integrity and accuracy of the data presented is not assured by the creators."),
                                         
                                         h2("Latest Study:"),
                                         p(strong("NCT ID:"), HTML(latest_clinical_study$NCTId)),
                                         p(strong("Title:"), latest_clinical_study$BriefTitle),
                                         p(strong("Start Date:"), latest_clinical_study$StartDate),
                                         p(strong("Estimated Completion:"), latest_clinical_study$CompletionDate),
                                         p(strong("Description of the Study:"),latest_clinical_study$BriefSummary),
                                         p(strong("Conditions Covered:"),latest_clinical_study$Condition),
                                         p(strong("Phase:"), latest_clinical_study$Phase),
                                         p(strong("Keywords:"),latest_clinical_study$Keyword),
                                         p(strong("Lead Sponsor:"), latest_clinical_study$LeadSponsorName),
                                         p(strong("Location:"),latest_clinical_study$LocationCity, latest_clinical_study$LocationCountry),
                                         br(),
                                         p(strong("In the sections below we can look at some of the meta-data about these trials.")),
                                         h2("Funders"),
                                         h3("Top Organizations"),
                                         p("Who are the top organizations funding this study?"),
                                         div(style='max-height:500px; overflow-y: scroll; overflow-x :scroll; position: relative',plotlyOutput('funderplot',height = 500 + 15*nrow(funder_data))),
                                         #dataTableOutput("funderData"),
                                         h3("Type of Funders"),
                                         p("What type of organizations are funding?"),
                                         plotlyOutput('funderTypePlot'),
                                         h2("Location"),
                                         h3("Num Clinical Trials by City"),
                                         p("Which cities are conducting clinical trials?"),
                                         p("Note: the number could include joint studies between multiple cities as well."),
                                         div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput('cityplot',height = 500 + 15*nrow(city_data))),
                                         h3("Num Clinical Trials by Country"),
                                         div(style='max-height:500px; overflow-y: scroll; position: relative',plotlyOutput('countryplot',height = 500 + 15*nrow(country_data)))
                                       )
                                     )
                  )
           ),
           tabPanel("Condition Network",fluidPage(theme = shinytheme('flatly')),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel('Co-Condition Network'),
                      sidebarPanel(
                        width = 4,
                        p("Total number of conditions being tested:",strong(nrow(nodes_data))),
                        p("Here a node is a condition being tested in the clinical study and two conditions have an edge together if they are being tested together."),
                        p("A solo node without connections implies that there are studies that test this conditions alone."),
                        p("Node size represents the number of studies involving that condition."),
                        p("Top condition of focus:",strong(top_condition_name), ", Num:",strong(top_condition_count)),
                        checkboxInput('egonet','Show Ego Network'),
                        selectInput('egonode','Select Condition',nodes_data$name)
                      ),
                      mainPanel(
                        forceNetworkOutput("condition_network")
                      )
                    )
           ),
           tabPanel("Data",fluidPage(theme = shinytheme('flatly')),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel('Apply filters'),
                      sidebarPanel(
                        radioButtons("status", "Status of the Study",
                                     choices = study_choices
                        )),
                      mainPanel(
                        dataTableOutput("coronavirusData")
                      )
                    )
             )
)

