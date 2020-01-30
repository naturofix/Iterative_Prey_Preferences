#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)


shinyServer(function(input, output, session) {
 
 #observeEvent(input$debug,{
#  browser()
# })
 
 values = reactiveValues(df = NULL)
 
 output$input_tables_ui = renderUI({
  
   for(entry in table_list){
    local({
    print(entry)

      df_name = entry
      print(df_name)
      output_name =(paste0(entry,'_table'))
      print(output_name)
      output[[output_name]] = DT::renderDataTable({
       
       cmd = paste('df = ',df_name)
       print(cmd)
       eval(parse(text = cmd))
       df
     })
      
   })
   }
   
  #})
   lst = lapply(table_list,function(entry) column(12,
                                                  tabPanel(paste(entry),
                                                  tags$h3(entry),
                                          DT::dataTableOutput(paste0(entry,'_table')))))
   
   lst
   do.call(tagList,lst)
  
 })
   
 output$save_text_ui = renderUI({
  (file_name = initial_file)
   
  if(!is.null(input$file_input)){
   (file_name = gsub('.csv','',input$file_input$name))
  }
  hideTab(inputId = 'preferences', target = "Data")
  hideTab(inputId = 'preferences', target = "Initial Preferences")
  hideTab(inputId = 'preferences', target = "Preference Estimatess")
  textInput('save_text','Save filename text',file_name)
 })
 
 upload_data = reactive({ 
  input_warning = ''
  inFile <- input$file_input
  
  if(is.null(inFile)){
   cmd = paste0('data = ',initial_file)
   eval(parse(text = cmd))
   data
  }else{
   updateTabItems(session, inputId = 'preferences', selected = "Overview")
   data = read.csv(inFile$datapath, header = FALSE,stringsAsFactors = F)
   if(length(colnames(data)) != 4){
     data = NULL
     input_warning = 'Incorrect number of columns'
   }
   if(!is.null(data)){
   if(!is.numeric(data[1,3])){
     #colnames(data) = data[1,]
     data = data[-1,]
     colnames(data) = c('site', 'species', 'amount_eaten', 'density')
     
     #input_warning = 'first row removed and replaced by the standard column header' 
   }else{
    colnames(data) = c('site', 'species', 'amount_eaten', 'density')
   }
  
 
  #cmd = paste0('data = ',input$datafile)
  #eval(parse(text = cmd))
  #data
  remove_duplicates = F
  if(remove_duplicates ==T){
   dup = 1
   data$species = as.character(data$species)
   while(TRUE %in% duplicated(data[,c('site','species')])){
    data$species[duplicated(data[,c('site','species')])] = as.character(paste(data$species[duplicated(data[,c('site','species')])],dup,sep='_'))
   }
  }
  data$site = as.factor(data$site)
  data$species = as.factor(data$species)
  data$amount_eaten = as.numeric(data$amount_eaten)
  data$density = as.numeric(data$density)
  
  as.tbl(data)
  showTab(inputId = 'preferences', target = "Overview")
  showTab(inputId = 'preferences', target = "Data")
  showTab(inputId = 'preferences', target = "Initial Preferences")
  showTab(inputId = 'preferences', target = "Preference Estimates")

     }else{
       hideTab(inputId = 'preferences', target = "Overview")
       hideTab(inputId = 'preferences', target = "Data")
       hideTab(inputId = 'preferences', target = "Initial Preferences")
       hideTab(inputId = 'preferences', target = "Preference Estimates")
     }

  }
  #data$site
  values$df = data
  list(data = data, input_warning = input_warning)
 })
  
 input_data = reactive(upload_data()$data)
 
 output$upload_warning = renderText(upload_data()$input_warning)
 
 output$input_data_table = DT::renderDataTable({
  data = input_data() 
  data
 })
 
 output$InputData_download <- downloadHandler(
  filename = function() {
   paste0(input$save_text,"_InputData.csv")
  },
  content = function(file) {
   write.csv(input_data(), file,row.names = F)
  }
 )
 output$final_data_table = DT::renderDataTable({
  data = final_data()
 })
 
 output$CorrectedData_download <- downloadHandler(
  filename = function() {
   paste0(input$save_text,"_CorrectedData.csv")
  },
  content = function(file) {
   write.csv(final_data(), file, row.names = F)
  }
 )
 
 data_integrity = reactive({
  data = input_data()
  data = values$df
  corr_data = data
  dim(data)
  
  (species = unique(data$species))

  
  (sites = unique(data$site))

  
  site_count = data %>% 
   group_by(species) %>% 
    summarise('Number of sites' = n(),
              'Total amount eaten' = sum(amount_eaten)) %>% 
   ungroup()
  site_count
  
  species_count = data %>% 
   group_by(site) %>% 
   summarise('Number of species' = n(),
             'Total amount eaten' = sum(amount_eaten)) %>% 
   ungroup()
  species_count 
  site_warning = site_count %>%
   filter(`Number of sites` < input$drop_species)
  species_warning = species_count %>%
   filter(`Total amount eaten` == 0)
  species_num_warning = species_count %>%
   filter(`Number of species` < input$drop_site)

  list(data = data,site_count = site_count,species_count = species_count,
       site_warning = site_warning, species_warning = species_warning, species_num_warning = species_num_warning)

 })
 
 output$warning_ui = renderUI({  

    
  (species_list = correct_data()$species_list) 
  (species_eaten_list = correct_data()$species_eaten_list)
  
  (site_list = correct_data()$site_list)
  (site_eaten_list = correct_data()$site_eaten_list)
  #### warnings #####

  output$site_warning_text = renderText({
   if(length(site_list) > 0){
    paste("WARNING : ",paste(unique(site_list),sep = ', ')," has <= ",input$species_num," species, Site dropped<br>")
   }
  })

  
  output$species_warning_text = renderText({
   
   if(length(species_list) > 0){
    paste("WARNING : ",paste(unique(species_list),sep = ', ')," eaten at <= ",input$site_num," sites, Species dropped<br>")
   }

  })

  output$site_eaten_warning_text = renderText({
   if(length(site_eaten_list) > 0){
    paste0("WARNING : Total amount eaten at ",paste(unique(site_eaten_list),sep = ', ')," was <= ",input$site_eaten_num,", Site dropped<br>")
   }

  })
  
  output$species_eaten_warning_text = renderText({
   if(length(species_eaten_list) > 0){
    paste0("WARNING : Total amount of ",paste(unique(species_eaten_list),sep = ', ')," eaten was <= ",input$species_eaten_num,", Species dropped<br>")
   }
   
  })
  output$species_num_warning_rb_ui = renderUI({
   if(dim(species_num_warning)[1] > 0){
    radioButtons('single_species_site_rb',tags$h4(htmlOutput('species_num_warning_text', style="color:orange; margin-bottom:20px;")),c(T,F),T,inline = T)
   }
   #else{
   # radioButtons('single_species_site_rb',tags$h4(htmlOutput('species_num_warning_text')),c(T,F),T,inline = T)
    
   #}
  })
  
  lst = list(column(12,
                    column(12,tags$h4(htmlOutput('site_warning_text', style="color:orange; margin-bottom:20px;"))),
                    #column(12,uiOutput('site_warning_rb_ui')),
                    column(12,tags$h4(htmlOutput('species_warning_text', style="color:orange; margin-bottom:20px;"))),
                    #olumn(12,uiOutput('species_warning_rb_ui')),
                    column(12,tags$h4(htmlOutput('site_eaten_warning_text', style="color:orange; margin-bottom:20px;"))),
                    column(12,tags$h4(htmlOutput('species_eaten_warning_text', style="color:orange; margin-bottom:20px;")))
                    
                    #column(12,uiOutput('species_num_warning_rb_ui'))
                    ))
  do.call(tagList,lst)

  })
  
 
 site_count = function(data){
  df = data %>% 
   group_by(species) %>% 
   summarise('Number of sites' = n(),
             'Total amount eaten' = sum(amount_eaten)) %>% 
   ungroup()
  df = df[!is.na(df$species),]
  return(df)
 }
 species_count = function(data){
  df = data %>% 
   group_by(site) %>% 
   summarise('Number of species' = n(),
             'Total amount eaten' = sum(amount_eaten)) %>% 
   ungroup()
  df = df[!is.na(df$site),]
  
   return(df)
 }
 site_num_warning = function(data,num,run = T){

  if(dim(data)[1] > 0 & run == T){
   df = data %>%
    filter(`Number of sites` <= num)
   len = dim(df)[1]
  }else{
   df = NULL
   len = 0
  }
  return(list(df = df,len = len))
 }
 site_eaten_warning = function(data,num,run = T){
  if(dim(data)[1] > 0 & run == T){
   df = data %>%
    filter(`Total amount eaten` <= num)
   len = dim(df)[1]
  }else{
   df = NULL
   len = 0
  }
  return(list(df = df,len = len))
 }
 
 species_num_warning = function(data,num,run = T){
  if(dim(data)[1] > 0 & run == T){
   df = data %>%
    filter(`Number of species` <= num)
   len = dim(df)[1]
  }else{
   df = NULL
   len = 0
  }
  return(list(df = df,len = len))
  
  
  return(species_num_warning)
 }
 species_eaten_warning = function(data,num,run = T){
  if(dim(data)[1] > 0 & run == T){
  df = data %>%
   filter(`Total amount eaten` <= num)
  len = dim(df)[1]
  }else{
   df = NULL
   len = 0
  }
  return(list(df = df,len = len))
 }
 
 output$drop_site_ui = renderUI({
  (sites = unique(input_data()$site))
  sites = sites[order(sites)]
  
  selectInput('drop_site','Drop Site',sites,NULL,multiple = T)
  
 }) 
 output$drop_species_ui = renderUI({
  (sites = unique(input_data()$species))
  sites = sites[order(sites)]
  selectInput('drop_species','Drop Species',sites,NULL,multiple = T)
 })
 correct_data = reactive({ 
  input_data = input_data() 
  
  data = input_data %>% 
   filter(!species %in% input$drop_species,
          !site %in% input$drop_site)
  dim(data)
  (site_num = input$site_num)
  (site_eaten_num = input$site_eaten_num)
  
  (species_num = input$species_num)
  (species_eaten_num = input$species_eaten_num) 
  

  site_eaten_list = c()
  (site_eaten_list = c(site_eaten_list,paste(species_eaten_warning(species_count(data),site_eaten_num,input$site_eaten_num_rb)$df$site)))
  site_list = c()
  (site_list = c(site_list,paste(species_num_warning(species_count(data),species_num,input$species_num_rb)$df$site)))
  species_list = c()
  (species_list = c(species_list,paste(site_num_warning(site_count(data),site_num,input$site_num_rb)$df$species)))
  species_eaten_list = c()
  (species_eaten_list = c(species_list,paste(site_eaten_warning(site_count(data),species_eaten_num,input$species_eaten_num_rb)$df$species)))
 
   
  corr_data = data %>%
   filter(!species %in% site_num_warning(site_count(data),site_num,input$site_num_rb)$df$species &
           !species %in% site_eaten_warning(site_count(data),species_eaten_num,input$species_eaten_num_rb)$df$species &
           !site %in% species_eaten_warning(species_count(data),site_eaten_num,input$site_eaten_num_rb)$df$site & 
           !site %in% species_num_warning(species_count(data),species_num,input$species_num_rb)$df$site)
  corr_sum = sum(site_num_warning(site_count(corr_data),site_num,input$site_num_rb)$len,
                 site_eaten_warning(site_count(corr_data),species_eaten_num,input$species_eaten_num_rb)$len,
      species_eaten_warning(species_count(corr_data),site_eaten_num,input$site_eaten_num_rb)$len,
      species_num_warning(species_count(corr_data),species_num,input$species_num_rb)$len)
  print(corr_sum)
  corr_data
  while(corr_sum > 0){
   species_list = c(species_list,paste(site_num_warning(site_count(corr_data),site_num,input$site_num_rb)$df$species))
   species_eaten_list = c(species_eaten_list,paste(site_eaten_warning(site_count(corr_data),species_eaten_num,input$species_eaten_num_rb)$df$species))
   site_eaten_list = c(site_eaten_list,paste(species_eaten_warning(species_count(corr_data),site_eaten_num,input$site_eaten_num_rb)$df$site))
   site_list = c(site_list,paste(species_num_warning(species_count(corr_data),species_num,input$species_num_rb)$df$site))
   
   corr_data = corr_data %>% 
    filter(!species %in% site_num_warning(site_count(corr_data),site_num,input$site_num_rb)$df$species &
            !species %in% site_eaten_warning(site_count(corr_data),species_eaten_num,input$species_eaten_num_rb)$df$species &
            !site %in% species_eaten_warning(species_count(corr_data),site_eaten_num,input$site_eaten_num_rb)$df$site & 
            !site %in% species_num_warning(species_count(corr_data),species_num,input$species_num_rb)$df$site)
   corr_data
 
   corr_sum = sum(site_num_warning(site_count(corr_data),site_num,input$site_num_rb)$len,
                  site_eaten_warning(site_count(corr_data),species_eaten_num,input$species_eaten_num_rb)$len,
                  species_eaten_warning(species_count(corr_data),site_eaten_num,input$site_eaten_num_rb)$len,
                  species_num_warning(species_count(corr_data),species_num,input$species_num_rb)$len)
   
   corr_sum
  }
  list(df = corr_data,species_list = species_list,site_eaten_list = site_eaten_list, site_list = site_list, species_eaten_list = species_eaten_list)
       
 })
 
 corrected_data = reactive({  
  (data = data_integrity()$data)
  if(!is.null(data)){
   (site_warning = data_integrity()$site_warning)
   (species_warning = data_integrity()$species_warning)
   (species_num_warning = data_integrity()$species_num_warning)

     data = data %>% 
      filter(!species %in% site_warning$species)

   if(input$drop_site_eaten == T){
     data = data %>% 
      filter(!site %in% species_warning$site)
   }

     data = data %>% 
      filter(!site %in% species_num_warning$site)

   
   data
  }
 })
 
 output$error_check_text = renderText({
  data = final_data()
  if(length(errors_list_r()) > 0){
   data = NULL
   hideTab(inputId = 'preferences', target = "Data")
   hideTab(inputId = 'preferences', target = "Initial Preferences")
   hideTab(inputId = 'preferences', target = "Preference Estimates")
   
  }else{
   showTab(inputId = 'preferences', target = "Data")
   showTab(inputId = 'preferences', target = "Initial Preferences")
   showTab(inputId = 'preferences', target = "Preference Estimates")
   
  }
  paste('')
 })
 
 final_data = reactive({
  data = correct_data()$df

  if(length(errors_list_r()) > 0){
   hideTab(inputId = 'preferences', target = "Data")
   hideTab(inputId = 'preferences', target = "Alpha")
   hideTab(inputId = 'preferences', target = "Averaging")
   
  }else{
   showTab(inputId = 'preferences', target = "Data")
   showTab(inputId = 'preferences', target = "Alpha")
   showTab(inputId = 'preferences', target = "Averaging")
   
  }
  data
 })
 
 site_count_df = reactive({ 
  if(!is.null(final_data())){
   data = final_data()
   
   (sites = unique(data$site))
   output$number_of_sites = renderText({
    paste(length(sites),'Sites')
   })
   output$sites = renderText({
    paste(sites,collapse = ', ')
   })
   site_count(data)
  }
 })
 
 output$site_count = renderTable({
  site_count_df()
 })
 output$Site_download <- downloadHandler(
  filename = function() {
   paste0(input$save_text,"_Site.csv")
  },
  content = function(file) {
   write.csv(site_count_df(), file, row.names = F)
  }
 )
 
 species_count_df = reactive({
  
  if(!is.null(final_data())){
   data = final_data()
   (sites = unique(data$site))
   (species = unique(data$species))
   output$number_of_species = renderText({
    paste(length(species),'Species')
   })
   output$species = renderText({
    paste(species,collapse = ', ')
   })
   species_count(data)

  }
 })
 
 output$species_count = renderTable({
  species_count_df()
 })
 
 output$Species_download <- downloadHandler(
  filename = function() {
   paste0(input$save_text,"_Species.csv")
  },
  content = function(file) {
   write.csv(species_count_df(), file,row.names = F)
  }
 )
 
 errors_list_r = reactive({
  #### Errors ####
  data = correct_data()$df
  errors_list = c() 
  dup = duplicated(data[,c('site','species')])
  dup
  if(TRUE %in% dup){
   (dup_line = paste(paste(data[,c('site','species')][dup,])))
   (dup_site = paste(data$site[dup]))
   (dup_species = paste(data$species[dup]))
   (dup_error = data[dup,])
   dup_error_full = data %>% 
    filter(site %in% dup_error$site & species %in% dup_error$species)
   dup_error_full
   
   (dup_line = paste('ERROR : there is more than one species per site'))
   errors_list = c(errors_list,dup_line)
  }
   output$dup_error_text = renderText({
    if(TRUE %in% dup){
     dup_line
    }
   })
   output$dup_error_table = renderTable({
    if(TRUE %in% dup){
     dup_error_full
     }
    })
   

  errors_list
  amount_eaten_error = data %>% 
   filter(amount_eaten < 0)
  amount_eaten_error
  if(dim(amount_eaten_error)[1] > 0){
   eaten_line = 'ERROR : There are negative values in amount eaten'
   errors_list = c(errors_list,eaten_line)
  }
   output$eaten_error_text = renderText({
    if(dim(amount_eaten_error)[1] > 0){
     print(eaten_line)
    }
   })
   output$eaten_error_table = renderTable({
    if(dim(amount_eaten_error)[1] > 0){
     amount_eaten_error
    }
   })
  
  density_error = data %>% 
   filter(density <= 0)
  density_error
  if(dim(density_error)[1] > 0){
   density_line = 'ERROR : There are non positive values in density'
   errors_list = c(errors_list,density_line)
  }
   output$density_error_text = renderText({
    if(dim(density_error)[1] > 0){
     density_line
    }
   })
   output$density_error_table = renderTable({
    if(dim(density_error)[1] > 0){
     density_error
    }
   })
  
  errors_list
 })
 
 output$error_ui = renderUI({
  errors_list_r()
  lst = list(column(12,
                    tags$h4(htmlOutput('dup_error_text', style="color:red; margin-bottom:20px;")),
                    tableOutput('dup_error_table'),
                    tags$h4(htmlOutput('eaten_error_text', style="color:red; margin-bottom:20px;")),
                    tableOutput('eaten_error_table'),
                    tags$h4(htmlOutput('density_error_text', style="color:red; margin-bottom:20px;")),
                    tableOutput('density_error_table')))
  do.call(tagList,lst)
 })
 
 alpha_function = function(d,e,di,ei){
  alpha = (e/d)/sum(ei/di,na.rm = T)
 }
 
 alpha_list_function = function(values){
  alpha = as.numeric((values$e/values$d)/sum(values$ei/values$di,na.rm = T))
  if(length(alpha) == 0){
   alpha = NA
  }
  return(alpha)
 }
 
 values_function = function(data,i,j){
  (data_line = data %>% 
    filter(site == i, species == j))
  (d = data_line$density)
  (e = data_line$amount_eaten)
   (data_i = data %>% 
     filter(site == i))
   (ei = data_i$amount_eaten)
   (di = data_i$density)
   (variable_list = list('d' = d,'e' = e,'di' = di,'ei' = ei))
  return(variable_list)
 }
 
 input_values = reactive({   
  data = final_data()
  dim(data)
  colnames(data)
  (i = input$select_site)
  (j = input$select_species)
  (m = length(unique(data$site)))
  (n = length(unique(data$species)))
  (data_line = data %>% 
   filter(site == i, species == j))
  (d = data_line$density)
  (e = data_line$amount_eaten)
  (data_i = data %>% 
   filter(site == i))
  (ei = data_i$amount_eaten)
  (di = data_i$density)
  #(alpha = (e/d)/sum(ei/di))
  (input_values = values_function(data,i,j))
  (alpha = alpha_list_function(input_values))
  #alpha = alpha_function(d,e,di,ei)
  list(i = i, j = j,
       m = m, n = n, 
       d = d, e = e,
       ei = ei, di = di,
       alpha = alpha
       )
 })
 
 alpha_table = reactive({  
  data = final_data()

 
  dim(data)
  colnames(data)
  df = NULL
  i  = unique(data$site)[1]
  j = unique(data$species)[1]
  #df = data.frame('species' = unique(data$species))
  df
  for(i in unique(data$site)){
   #print(i)
   j_list = c()
   for(j in unique(data$species)){
    #print(j)
    (input_values = values_function(data,i,j))
    #print(input_values)
    (alpha = alpha_list_function(input_values))
    #print(alpha)
    (j_list = c(j_list,alpha))
   }
   j_list
    if(is.null(df)){
     (cmd = paste0('df = data.frame(`',i,'` = j_list)'))
     eval(parse(text = cmd))
     #rownames(df) = unique(data$species)
    }else{
     (cmd = paste0('df_n = data.frame(`',i,'` = j_list)'))
     eval(parse(text = cmd))
     df = cbind(df,df_n) 
    }
  }
  df
  df_m = as.matrix(df)
  df_m
  rownames(df_m) = unique(data$species)
  df_m = t(df_m)
  df_m
  df = as.data.frame(df_m)

  rownames(df)

  rownames(df) = rownames(df_m)

  df = df[order(rownames(df)),order(colnames(df))]
  m = as.matrix(df)
  rownames(m)

  list(df = df,m = m)
  })
 
 prey_pref_insert = function(d1,p){
  rm_d1 = d1
  for(i in colnames(d1)){
   #print(i)
   rm_d1[,i] = rm_d1[,i] * p[,i] 
  }
  #rm_d1 = d1 * p
  rm_d1
  return(rm_d1)
 }
 
 iteration_row_algorithm = function(m,p){
  m_0 = m
  m_0[is.na(m_0)] = 0
  m_0
  
  d = m_0
  d[d > 0] =1
  d
  d1 = 1 - d
  d1
  
  rm_0 = rowMeans(m_0,na.rm = T)
  rm_0
  print(rm_0)
  #rm_d1 = d1 * rm_0
  rm_d1 = prey_pref_insert(d1,p)
  rs = rowSums(m,na.rm = T)
  (rs_rm_d1 = rowSums(rm_d1))
  #(ad = rowSums(m,na.rm = T) - rs_rm_d1)
  #print(ad)
  (ad_1 = 1-rs_rm_d1)
  
  (m2 = m * (ad_1/rs))
  m2_0 = m2
  m2_0[is.na(m2_0)] = 0
  (m2_rm_d1 = m2_0 + rm_d1)
  print(m2_rm_d1)
  print(rowSums(m2_rm_d1))
  result_list = list('m' = m2,'P' = m2_rm_d1)
  return(result_list)
 }
 
 iteration_column_algorithm = function(mc,Pc,d1){
  #m_0 = mc 
  #m_0[is.na(m_0)] = 0
  #m_0
  (cm_0 = colMeans(Pc,na.rm = T))
  #geometry::dot(t(m_0),(cm_0))
  
  cm = t(matrix(as.numeric(rep(cm_0,dim(mc)[1])),ncol = dim(mc)[1]))
  cm
  cm_d1 = cm * d1
  cm_d1
  #geometry::dot(m_0,cm)
  #geometry::dot(mc,d1)
  (rs = rowSums(mc,na.rm = T))
  (cs = colSums(mc,na.rm = T))
  (ms = colMeans(mc,na.rm = T))
  
  ### need to check these calculation again ####
  (rs_cm_d1 = rowSums(cm_d1))

  (ad_1 = 1-rs_cm_d1)
  
  (m2 = mc * (ad_1/rs))
  m2_0 = m2
  m2_0[is.na(m2_0)] = 0
  (P = m2_0 + cm_d1)
  #result_list = list('m' = m2,'P' = P,colSum = cs, rowSum = rs, colMean = ms)
  
  result_list = list('m' = m2,'P' = P,colSum = colSums(P), rowSum = rowSums(P), colMean = colMeans(m2,na.rm = T),P_colMeans = colMeans(P,na.rm = T))
  
  return(result_list)
 }
 
 output$initial_prey_pref = renderUI({
  if(input$prey_input == 'average'){
   m = alpha_table()$m    
   dim(m)
   #numericInput('initial_prey_pref','Initial Prey Pref',round(1/(dim(m)[2]),input$pref_round))
   radioButtons('zero_pref','Set initial prey preference for prey not eaten to zero',c(T,F),inline = T)
   #NULL
  }else{
    column(12,
           tags$h6("Expecting a file with the first line a list of species names and the second line a list of preferences. All are separated by commas. "),
    
            fileInput('file_input_pref', 'File Input', multiple = FALSE, accept = NULL, width = NULL))
  } 
 })
 
 initial_prey_pref = reactive({ 
  pref = NULL
  pref_warning = ''
  output$pref_warning = renderText(pref_warning)
  type = 'table'
   m = alpha_table()$m
   site_count = site_count_df()

  if(input$prey_input == 'average'){
   if(!is.null(input$initial_prey_pref)){
     average_pref = input$initial_prey_pref
   }else{
     m = alpha_table()$m    
     dim(m)
     
     if(is.null(input$zero_pref)){
       zero_pref = T
     }else{
       zero_pref = input$zero_pref
     }
     zero_pref
     if(zero_pref == T){
       (zero_species = site_count$species[site_count$`Total amount eaten` == 0])
       (zero_num = length(zero_species))
     }else{
       zero_num = 0
     }
     #if(zero_num == 0){ 
      #(average_pref = 1/(dim(m)[2]))
     #}else{
      (average_pref = 1/(dim(m)[2]-zero_num))
     #}
   }
   
    

    output$pref_title = renderText('Initial Prey Preference Averge')
 
    (pref = (matrix(rep(average_pref,dim(m)[2]))))
    rownames(pref) = colnames(m)
    colnames(pref) = 'Initial Prey Preference'
    pref
    if(zero_pref == T){
      pref[zero_species,] = 0
    }
    pref
    output$initial_pref_table = DT::renderDataTable({
     pref
    })
    type = 'average'
   
  }else{

   #pref = try(read.csv(input$file_input_pref$datapath))     
   if(!is.null(input$file_input_pref)){
    pref = read.csv(input$file_input_pref$datapath,header = F,stringsAsFactors = F)
    
    
    dim(pref)
    if(dim(pref)[2] != 2){
      pref_warning = 'Incorrect number of columns'
      pref = NULL
    }
    if(TRUE %in% duplicated(pref[,1])){
      pref_warning = 'Duplicated Species in column 1'
      pref = NULL
    }
    if(!is.null(pref)){
      if(!is.numeric(pref[1,2])){
        colnames(pref) = pref[1,]
        pref = pref[-1,]
      }else{
        colnames(pref) = c('Species','Initial.Prey.Preferences')
      }
      rownames(pref) = pref[,1]
      pref_i = as.data.frame(pref[,2])
      colnames(pref_i) = ' Initial.Prey.Preference'
      rownames(pref_i) = rownames(pref)
      pref_i
      pref = pref_i
      pref_warning = ''
      (missing_columns = colnames(m)[!colnames(m) %in% rownames(pref)])
      if(length(missing_columns) == length(colnames(m))){
        pref_warning = 'None of the species match the input data'
        pref = NULL
      }else{
        if(length(missing_columns) > 0){
         (pref_warning = paste(paste(missing_columns,sep =', '), 'missing from uploaded preferences, so average of',(round(1/dim(m)[2],input$pref_round)),'used'))
         for(entry in missing_columns){
          pref[entry,] = 1 /dim(m)[2]
         }
        }
      }
    }
     
    
    #data
    #pref = data.frame(0.1,0.1,0.1,0.1,0.1,0.1)
    #colnames(pref) = c('Spp1', 'Spp2','Spp3','Spp4','Spp5','Spp6')
    #rownames(pref) = 'Initial Prey Preference'
    output$pref_title = renderText(paste('Initial Prey Preference from File',input$file_input_pref$name))
    output$pref_warning = renderText(pref_warning)
   
   
    output$initial_pref_table = DT::renderDataTable({
     pref
    })
   #dim(alpha_table()$m)[2]
   #if(dim(pref)[2] != dim(alpha_table()$m)[2]){
   # pref = input$initial_prey_pref
   #}
   }
  }
  list(type = type,pref = pref, pref_warning = pref_warning)
 })
 

 
 
 output$pre_pref_out_ui = renderUI({
  if(!is.null(initial_prey_pref()$pref)){
  if(initial_prey_pref()$type == 'average'){
   lst = list(column(12,
               tags$h4(htmlOutput('pref_title')),
               column(11),
               column(1,downloadButton("initial_pref_download", "csv")),
               column(12,DT::dataTableOutput('initial_pref_table'))
               ))
  }else{
   lst = list(column(12,
               tags$h4(htmlOutput('pref_title')),
               htmlOutput('pref_warning',style="color:orange; margin-bottom:20px;"),
               column(11),
               column(1,downloadButton("initial_pref_download", "csv")),
               column(12,DT::dataTableOutput('initial_pref_table'))
               ))
  
  }
  #do.call(tagList,lst)
  }else{
    lst = list(htmlOutput('pref_warning',style="color:red; margin-bottom:20px;"))
  }
  do.call(tagList,lst)
   
 })
 
 output$initial_pref_download <- downloadHandler(
  
  filename = function() {
   paste0(input$save_text,"_Initial_Prey_Preferences.csv")
  },
  content = function(file) {
   write.csv(initial_prey_pref()$pref, file,row.names = T)
  }
 )
 output$pref_text = renderText({
  paste(initial_prey_pref(),collapse = ', ')
  })
 
 
 
 iterative_operations = reactive({   
  if(!is.null(initial_prey_pref()$pref)){
  if(!is.null(alpha_table()$m)){
   if(dim(alpha_table()$m)[1] > 0){
   withProgress(message = paste('Iterative Preference Averaging'),{
  (m = round(alpha_table()$m,input$pref_round))
  m_0 = m
  (m_0[is.na(m_0)] = 0)
  (d = m_0)
  #View(d)
  d[!is.na(m)] = 1
  (d1 = 1 - d)
  maxDiffPref = input$maxDiffPref
  maxK = input$maxK
  #maxDiff = 1
  n = 1
  (ipp = initial_prey_pref())
  (initial_prey_pref = ipp$type)
  (p = t(ipp$pref))
  #(p = round(input$initial_prey_pref,input$pref_round))
  test = 1
  matrix_list = list()
  
  m_i = list()
  m_i$m = m
  #m_i$P = m_0 + (d1 * p) 
  m_i$P = m_0 + prey_pref_insert(d1,p)
  matrix_list[[n]] = m_i
  n = n + 1
  if(test == 1){
   m2 = iteration_row_algorithm(m,p)
   m2$m = round(m2$m,input$pref_round)
   m2$P = round(m2$P,input$pref_round)
   
   m = m2
   
   (m_d1 = m$P*d1)
   dim(m_d1)

   matrix_list[[n]] = m
   n = n + 1
   mc = m2$m
   mcJ = jacobs_function(mc)
   Pc = m2$P
   #(mcJ = jacobs_function(mc))
   #(PcJ = jacobs_function(Pc))
   m2 = iteration_column_algorithm(m2$m,m2$P,d1)
   m2$m = round(m2$m,input$pref_round)
   m2$P = round(m2$P,input$pref_round)
  }
  if(test == 2){
   p_d1 = p * d1
   m
   p_d1
   m_p = m_0 + p_d1
   m_p
   (m_d1 = m_p*d1)
   #m2 
   m1 = list(m = m,P = m_p)
   matrix_list[[n]] = m1
   n = n + 1
   m2 = iteration_column_algorithm(m,m_p,d1)
   m2$m = round(m2$m,input$pref_round)
   m2$P = round(m2$P,input$pref_round)
  }
  

  (m2_d1 = m2$P*d1)
  (maxDiff = max(abs(m_d1 - m2_d1)))
  m = m2
  (m_d1 = m$P*d1)
  m$m_d1 = m_d1

  m$maxDiff = maxDiff
  matrix_list[[n]] = m
  n = n + 1
  while(maxDiff > maxDiffPref & n < maxK){

   m2 = iteration_column_algorithm(m2$m,m2$P,d1)
   m2$m = round(m2$m,input$pref_round)
   m2$P = round(m2$P,input$pref_round)
   (m2_d1 = m2$P*d1)
   (maxDiff = max(abs(m_d1 - m2_d1)))
   m = m2
   (m_d1 = m$P*d1)
   m$m_d1 = m_d1
   #(n = n + 1)
   m$maxDiff = maxDiff
   matrix_list[[n]] = m
   n = n + 1
  }
  n = n - 1
  print(n)
  print(maxDiff)
  m$n = n
  m$maxDiff = maxDiff
  m$matrix_list = matrix_list
  m
   })
  }}}
 })
 
 jacobs_function = function(mc){
  (n = dim(mc)[2])
  (jn = mc * n)
  (jn1 = jn - 1)
  
  (k = (n-2)*mc)
  (k1 = 1 + k)
  (J = jn1/k1)
  J
 }
 
 output$scale_m = DT::renderDataTable({
  if(!is.null(iterative_operations())){
   df = iterative_operations()$matrix_list[[input$select_n +1]]$m
   df = as.data.frame(signif(df,6))
   colnames(df)
   df = df %>% rownames_to_column('site')
   if(annotate == T){
    (x = dim(df)[1])
    (y = dim(df)[2])
    df = rbind(df,c('colSums',signif(colSums(df[,-1],na.rm = T),6)))
    
    df[,-1] = apply(df[,-1],2, function(x) as.numeric(x))
    df = rbind(df,c('colMeans',signif(colMeans(df[1:x,-1],na.rm = T),6)))
    df[,-1] = apply(df[,-1],2, function(x) as.numeric(x))
    
    as.tbl(df)
    df$rowSums = signif(rowSums(df[,-1],na.rm = T),6)
    df$rowMeans = signif(rowMeans(df[,2:y],na.rm = T),6)
    rownames(df) = df$site
    df['colSums','rowSums'] = NA
    df['colSums','rowMeans'] = NA
    df['colMeans','rowSums'] = NA
    df['colMeans','rowMeans'] = NA
   }
   df
  }
 })
 
 pref_output = reactive({
  if(!is.null(input$select_n)){
  
    select_n = input$select_n + 1
    if(input$show_missing_rb == T){
     df = iterative_operations()$matrix_list[[select_n]]$P
    }else{
     df = iterative_operations()$matrix_list[[select_n]]$m
    }
   df
  }
 })
 
 jocobs_output = reactive({
  if(!is.null(input$select_n)){
   select_n = input$select_n + 1
   if(input$show_missing_rb == T){
    df = iterative_operations()$matrix_list[[select_n]]$P
   }else{
    df = iterative_operations()$matrix_list[[select_n]]$m
   }
   df
   J = jacobs_function(df)
   J
  }
  
 })
 
 output$jacobs_table = DT::renderDataTable({
  df = as.data.frame(jocobs_output())
  df = df %>% rownames_to_column('site')
  df
 })
 
 pref_colMean = reactive({
   if(!is.null(input$select_n)){
    if(!is.null(iterative_operations())){
    if(input$select_n + 1 > 1){
      df = t(as.data.frame(iterative_operations()$matrix_list[[input$select_n + 1]]$P_colMean))
      rownames(df) = 'Final Preference'
      df
    }
    }
   }
 })
 
 output$pref_colMean_table = renderTable({
  if(input$select_n >= 2){
   round(pref_colMean(),3)
  }
 },digits = 3)
 output$scale_P = DT::renderDataTable({
  if(!is.null(pref_output())){
   df = pref_output()
  df = as.data.frame(signif(df,6))
  df = df %>% rownames_to_column('site')
  if(input$annotate_rb == T){
   (x = dim(df)[1])
   (y = dim(df)[2])
   df = rbind(df,c('colSums',signif(colSums(df[,-1],na.rm = T),6)))
   
   df[,-1] = apply(df[,-1],2, function(x) as.numeric(x))
   df = rbind(df,c('colMeans',signif(colMeans(df[1:x,-1],na.rm = T),6)))
   df[,-1] = apply(df[,-1],2, function(x) as.numeric(x))
   
   as.tbl(df)
   df$rowSums = signif(rowSums(df[,-1],na.rm = T),6)
   df$rowMeans = signif(rowMeans(df[,2:y],na.rm = T),6)
   rownames(df) = df$site
   df['colSums','rowSums'] = NA
   df['colSums','rowMeans'] = NA
   df['colMeans','rowSums'] = NA
   df['colMeans','rowMeans'] = NA
  }
  df
  #}
  }
 })
 
 
 pref_colMean_save = reactive({
   (data = as.data.frame(t(pref_colMean())))
   data
 })
 
 
 output$EstPrefs_download <- downloadHandler(
  
  filename = function() {
 
   n = iterative_operations()$n
   num = input$select_n + 1
   if(input$select_n+1 == n){
    num = 'Final'
   }
   paste0(input$save_text,"_EstPrefs_",num,".csv")
  },
  content = function(file) {
   write.csv(pref_colMean_save(), file)
  }
 ) 
 
 output$JacobsT_table = renderTable({
  if(input$select_n >= 2){
   round(jacobs_function(pref_colMean()),3)
  }
 },digits = 3)
 

 
 output$JacobsT_download <- downloadHandler(
  
  filename = function() {
   n = iterative_operations()$n
   num = input$select_n + 1
   if(input$select_n+1 == n){
    num = 'Final'
   }
   paste0(input$save_text,"_Jacobs_",num,".csv")
  },
  content = function(file) {
   write.csv(t(jacobs_function(pref_colMean())), file)
  }
 )
 
 output$final_pref_ui_1 = renderUI({
  if(!is.null(initial_prey_pref()$pref)){
  if(!is.null(input$select_n)){
   if(input$select_n >= 2){
    lst = list(column(12,
                      column(12,tags$h4('Preference per species')),
                      column(11,tags$h5("Manly's index")),
                      column(1,downloadButton("EstPrefs_download", "csv")),
                      column(12,tableOutput('pref_colMean_table')),
                      column(11,tags$h5("JacobsT index")),
                      column(1,downloadButton("JacobsT_download", "csv")),
                      column(12,tableOutput('JacobsT_table')),
                      column(12,tags$hr()))
    )
    do.call(tagList,lst)
   }
  }
  }
 })
 output$final_pref_ui_2 = renderUI({

         lst = list(column(12,
                      column(4,tags$h4('Preference matrix')),
                      column(3,radioButtons('show_missing_rb','Show Missing Values',c(F,T),inline = T)),
                      column(3,radioButtons('annotate_rb','Annotate',c(T,F),F,inline = T)),
                      column(1,downloadButton("EstPrefsMaxtrix_download", "csv"))
         ))
         do.call(tagList,lst)

 })
                      
                      
output$final_pref_ui_3 = renderUI({
  if(!is.null(initial_prey_pref()$pref)){
    if(!is.null(input$select_n)){
      if(input$select_n >= 0){
        lst = list(column(12,
                      column(12,DT::dataTableOutput('scale_P'))
               ))
    do.call(tagList,lst)
   }
  }
  }
 })
 
pref_output_save = reactive({
  data = t(pref_output())
  data
})

 output$EstPrefsMaxtrix_download <- downloadHandler(

  filename = function() {
   miss = 'Miss'
   if(input$show_missing_rb == T){
    miss = 'Full'
   }
   n = iterative_operations()$n
   num = input$select_n + 1
   if(input$select_n + 1 == n){
    num = 'Final'
   }
   paste0(input$save_text,"_EstPrefsMaxtrix_",miss,'_',num,".csv")
  },
  content = function(file) {
   write.csv(pref_output_save(), file)
  }
 ) 
 
 
 output$scale_n = renderText({
  iterative_operations()$n
 })
 output$scale_maxDiff = renderText({
   text = ''
  if(!is.null(initial_prey_pref())){
   if(!is.null(input$select_n)){
    
    if(!is.null(iterative_operations()$maxDiff)){
     if(input$select_n > 2){
      text = paste('<br>Final Maximum Difference in Preference =',signif(iterative_operations()$matrix_list[[input$select_n+1]]$maxDiff,3))
     }
    }
   }
  }
  text
 })
 output$scale_n_ui = renderUI({ 
  if(!is.null(iterative_operations()$n)){
   (n = iterative_operations()$n)
   names(iterative_operations()$matrix_list)
   #n = 10
   sliderInput('select_n','Select Iteration',0,n-1,n-1,width = 1200)
  }
 }) 

 
 scale_m = reactive({ 
  m_0 = m
  m_0[is.na(m_0)] = 0
  m_0
  d = m_0
  d[d > 0] =1
  d
  d1 = 1 - d
  d1
  m2 = iteration_algorithm(m)
  (m2_d1 = m2$P*d1)
  m2
  m3 = iteration_algorithm(m2$m)
  (m3_d1 = m3$P*d1)
  (diff = max(abs(m3_d1 - m2_d1)))
  m3
  m4 = iteration_algorithm(m3$m)
  m4
  m5 = iteration_algorithm(m4$m)
  m5
  m6 = iteration_algorithm(m5$m)
  m6
  m7 = iteration_algorithm(m6$m)
  m7
  m = alpha_table()$m
  m  
  rowSums(m,na.rm = T)
  colSums(m,na.rm = T)
  mean(m,na.rm = T)
  m_0 = m
  m_0[is.na(m_0)] = 0
  m_0
  mean(m_0,na.rm = T)
  0.18/0.27
  0.242/0.29
  m_0 * 0.67
  d = m_0
  d[d > 0] =1
  d
  d1 = 1 -d
 d1
 
 d1_m = d1 * rowMeans(m_0)
 d1_m
 (r_d1_m = rowSums(d1_m))
 (ad = rowSums(m,na.rm = T) - r_d1_m)
 m[1,] * ad[1]
 (m2 = m * ad)
 rowSums(m2,na.rm = T)
 (m2_0 = m2)
 m2_0[is.na(m2_0)] = 0
 m2_0
 (m2_d1 = m2_0 + d1_m)
 m2_d1
 rowSums(m2_d1)
 rowMeans(m2_d1)
 rowMeans(m2_0)
  
 colSums(m2_0)
 colSums(m2_d1)
 geometry::dot(m2_0,d1)
 
 p = matrix(rep(0.167,24),ncol = 6)
 p
 (dp = geometry::dot(m_a,p,1))
 1/6
 
 (dp = geometry::dot(m_0[1,],p,1))
 dim(m_0)
 (m_0 * p) + d1_a
 (m_dp = m_0 * dp)
 a = mean(dp)
 d1_a = d1 * a
 m_a = d1_a + m_0
 m_a
 rowSums(m_a)
 mean(m_0)
 m_a / rowSums(m_a)
 m_0 + mean(m_0)
 
 m_0
 scale(m_0)
 scale(m_dp)
 e1 = t(matrix(c(0.18, 0.186, 0.24, 0.06, 0.167, 0.167,
                      0.242, 0.10,0.166,0.05,0.242,0.20,
                      0.20,0.10,0.20,0.07,0.23,0.20,
                      0.167,0.166,0.258,0.05,0.10,0.258),ncol = 4))
 e1
 rowSums(e1)
 colSums(e1)
 rowMeans(e1)
 colMeans(e1)
 geometry::dot(e1,d1)
 e2 = t(matrix(c(0.164, 0.17, 0.219, 0.055, 0.186, 0.206,
               0.227, 0.095, 0.216, 0.047, 0.227, 0.188,
               0.20, 0.10, 0.20, 0.07, 0.23, 0.20,
               0.197, 0.161, 0.249, 0.048,0.096,0.249),ncol = 4))
 e2
 rowSums(e2)
 colSums(e2)
 colMeans(e2)
 geometry::dot(e2,d1)
 e3 = matrix(c(0.163, 0.169, 0.218, 0.055, 0.184, 0.211,
               0.226, 0.093, 0.221, 0.047, 0.226, 0.187,
               0.20, 0.10, 0.20, 0.07, 0.23, 0.20,
               0.197, 0.161, 0.249, 0.048, 0.096, 0.249),ncol = 6)
 rowSums(e3)
 colSums(e3)
 
 m_0
 geometry::dot(m_0,e1)
 }) 
 
 
 output$alpha_table = DT::renderDataTable({
  signif(alpha_table()$df,6) %>% rownames_to_column('site')
 })
 
 output$InitialPrefs_download <- downloadHandler(
  filename = function() {
   paste0(input$save_text,"_InitialPrefs.csv")
  },
  content = function(file) {
   write.csv(alpha_table()$df, file)
  }
 ) 
 

 
 output$input_values_text_1 = renderText({
  input_values = input_values()
  (v1 = paste0(#'Site = ',input_values$i,'</br>',
              #'Species = ',input_values$j,"</br>",
              'Number of sites (m) = ',input_values$m,"</br>",
              'Number of species (n) = ',input_values$n,'</br>',
              tags$hr()))
  v1
 })
 
 output$input_values_text_2 = renderText({
   input_values = input_values()
   (v2 = paste0('Density of ',input_values$j,' at ',input_values$i,' = ',input_values$d,"</br>",
              'Number of ',input_values$j,' eaten at ',input_values$i,' = ',input_values$e,"</br>",
              #'Total density at ',input_values$i,' = ',input_values$di,"</br>",
              #'Total number eaten at ',input_values$i,' = ',input_values$ei,"</br>",
              'Alpha of ',input_values$j,' at ',input_values$i,' = ',input_values$alpha,"</br>",
    tags$hr()))
              
              
  print(v2)
 })
 
 output$select_site_ui = renderUI({
  entries = unique(final_data()$site)
  selectInput('select_site','Select Site',entries)
 })
 
 output$select_species_ui = renderUI({
  entries = unique(final_data()$species)
  selectInput('select_species','Select Species',entries)
 })
   

  
})
