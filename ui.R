#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Iterative Preference Averaging"),
  
  fluidPage(
            fluidRow(
             #column(10),
             #column(2,actionButton('debug','Debug')),
             column(12,

                       column(12,
                       column(12,tags$h5('Expecting a file with columns of site, species, amount_eaten and density. All are separated by commas. Optional: first row may contain the headings: site, species, eaten, density')),    
                       column(6,fileInput('file_input', 'File Input', multiple = FALSE, accept = NULL, width = NULL)),
                       column(6,uiOutput('save_text_ui'))
                       ),
                       column(12,
                       column(4,
                              numericInput('maxDiffPref','Max Difference in Preference',0.0001)),
                       column(4,numericInput('maxK','Max # of iterations',2000)),
                              column(4,numericInput('pref_round','Rounding',6))
                       ),

                      column(12,tags$h4(htmlOutput('upload_warning', style="color:red; margin-bottom:20px;"))),
            
                   
           
                       column(12),
                       tabsetPanel(selected = 'Overview',id = 'preferences',
                                   
                        tabPanel('Overview',
                                 column(12,
                                        column(6,
                                               uiOutput('drop_site_ui'),
                                               column(9,numericInput('species_num','Drop sites with <= # of species eaten',1,step = 1,min = 0)),
                                               column(3,radioButtons('species_num_rb','',c(T,F))),
                                               column(9,numericInput('site_eaten_num','Drop site where <= # total amount eaten           ',0,step = 1,min = 0)),
                                               column(3,radioButtons('site_eaten_num_rb','',c(T,F)))
                                               ),
                                        column(6,
                                               uiOutput('drop_species_ui'),
                                               column(9,numericInput('site_num','Drop species present at <= # of sites',1,step = 1,min = 0)),
                                               column(3,radioButtons('site_num_rb','',c(T,F))),
                                               column(9,numericInput('species_eaten_num','Drop species where total amount eaten <= #            ',0,step = 1,min = 0)),
                                               column(3,radioButtons('species_eaten_num_rb','',c(T,F)))
                                        )
                                        
                                 ),
                                 uiOutput('warning_ui'),

                                 uiOutput('error_ui'),

                                 column(6,
                                        column(10,tags$h3(htmlOutput('number_of_sites'))),
                                        column(1,downloadButton("Species_download", "csv")),
                                        column(12,tableOutput('species_count'))
                                        ),
                                 column(6,
                                        column(10,tags$h3(htmlOutput('number_of_species'))),
                                        column(1,downloadButton("Site_download", "csv")),
                                        column(12,tableOutput('site_count'))
                                        ),
                                 textOutput('error_check_text')
                                 ),
                     
                        tabPanel('Data',
                                 tabsetPanel(
                                 tabPanel('Input Data', 
                                          column(11),
                                          column(1,downloadButton("InputData_download", "csv")),
                                          column(12,DT::dataTableOutput('input_data_table'))
                                 ),
                                 tabPanel('Corrected Data',
                                          column(11),
                                          column(1,downloadButton("CorrectedData_download", "csv")),
                                          column(12,DT::dataTableOutput('final_data_table'))
                                 ),
                        tabPanel('Preferences',tabsetPanel(selected = 'Table',
                           tabPanel('Single',
                            column(6,uiOutput('select_site_ui')),
                            column(6,uiOutput('select_species_ui')),
                            column(12,tags$h4(htmlOutput('input_values_text_1'))),
                            column(12,tags$h5(htmlOutput('input_values_text_2')))
                           ),
                           tabPanel('Table',
                              column(11),
                              column(1,downloadButton("InitialPrefs_download", "csv")),
                              column(12,DT::dataTableOutput('alpha_table'))
                           )
                          )))),
                        tabPanel('Preference Estimates',
                          tabsetPanel(
                            tabPanel('Initial',
                          
                              column(12,
                               column(3,radioButtons('prey_input','Prey',c('average','upload'),inline = T)),
                               column(5,uiOutput('initial_prey_pref'))

                               ),

                              
                              
                              column(12,uiOutput('pre_pref_out_ui'))
                            ),
                            tabPanel('Final',
                              column(12,
                                     column(4),
                                     column(8,tags$h4(htmlOutput('scale_maxDiff'))),
                               column(12,uiOutput('scale_n_ui'))
                               ),

                              uiOutput('final_pref_ui_1'),
                              
                              uiOutput('final_pref_ui_2'),
                              
                              uiOutput('final_pref_ui_3')

                        )
                       )
                       
              )))
              
             )
             )
             ))
