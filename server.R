### Super join MCQ 

library(shiny)
library(tidyverse)
library(ggridges)
library(lubridate)
library(glue)

# Functions ---------------------------------------------------------------

read_multi <- function(file_name,file_path){
    
    df <- tibble(ext = tools::file_ext(file_name), path = file_path)
    
    read <- function(ext,path){  
        
        switch(ext,
               csv = vroom::vroom(path, delim = ","),
               xls = readxl::read_excel(path),
               xlsx = readxl::read_excel(path),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
               
        )
    }
    
    data_ls <- df %>% pmap(read)
    
    names(data_ls) <- str_remove(file_name,"\\.[^\\.]+$") #remove .xxx (content after last dot)
    
    data_ls
    
}

is.valid_list <- function(list, valid_regex){
    
    is.valid_df <- function(df, valid_regex){
        
        map_lgl(valid_regex , ~ str_detect(names(df), .x) %>% any() ) %>% all()
        
    }
    
    list %>% map( ~is.valid_df(.x , valid_regex)  )
}

is.grade.in_list <- function(list,chr){
    
    get_distinct_at <- function(df, ...){
        
        df %>% 
            select(...) %>% 
            distinct() %>% pull()
        
    }
    
    unique_string <- list %>% 
        map(~get_distinct_at(.x, starts_with("Grade"))) %>% 
        flatten_chr() %>% unique()
    
    chr %in% unique_string
    
}

clean_list <- function(list.raw){
    
    list.raw %>%
        map(~select(.x,-Institution,-Department,-starts_with("Q"))) %>%
        map(~rename(.x,Score = starts_with("G"),Email="Email address" ,Started="Started on")) %>%
        map(~unite(.x ,"First name","Surname",sep = " ",col = "Name")) %>%
        map(~mutate(.x,Score = as.numeric(Score), Started = dmy_hm(Started) ,
                    Completed = dmy_hm(Completed))) %>%
        map(~filter(.x,!is.na(Email))) %>%
        map(~mutate(.x, ID = str_extract(Email,"[:digit:]+") %>% as.character(), .keep = "unused")) %>%
        map(~select(.x,Name,ID,Score)) %>%
        map(~filter(.x, !is.na(Score)))
    
}

# Function : Compute score by max score ---------------------------------------------------------------

score_list <- function(list.cleaned){
    
    list.cleaned %>%
        map(~group_by(.x,Name,ID)) %>%
        map(~summarise(.x,Score = max(Score)))
    
}

# Function : Get maximum score from Grade/xxx ---------------------------------------------------------------

get_max_score <- function(list.raw){
    
    list.raw %>%
        map(~names(.)) %>%
        map(~str_extract(.x,"Grade/[:digit:]+")) %>% # extract Grade/digit
        flatten_chr() %>%
        na.omit() %>% as.character() %>%
        str_replace("Grade/","") %>% as.numeric()
    
}

# Function : Readjust score -----------------------------------------------


readjust_score <- function(list.processed, old_max, new_max = old_max){
    
    multiply_factor <- new_max/old_max
    
    list.processed %>% 
        map2(.y = multiply_factor, ~mutate(.x, Score = .y*Score))
}


# Function : Rename Column name  ---------------------------------------------------------------

## with suffix

rename_list <- function(list,var,suffix){
    
    col_names <- glue("{names(list)}","_Max={suffix}") # Max score = ...
    
    rename_2 <- function(list,no,var){
        
        list %>%
            pluck(no) %>%
            rename_with( .cols = {{var}}, ~ col_names[no]  )
        
    }
    
    seq_along(list) %>%
        map(~rename_2(list, no = .x, var = {{var}})) %>%
        set_names(col_names)
}

## No suffix

rename_list_sim <- function(list,var){
    
    col_names <- glue("{names(list)}")
    
    rename_2 <- function(list,no,var){
        
        list %>%
            pluck(no) %>% 
            rename_with( .cols = {{var}}, ~ col_names[no]  )
        
    }
    
    
    seq_along(list) %>% 
        map(~rename_2(list, no = .x, var = {{var}})) %>% 
        set_names(col_names)
}

# Function : Join MCQs  ---------------------------------------------------------------

join_list <- function(list){
    
    list %>% 
        reduce(full_join, by = c("ID","Name")) %>% 
        mutate(Total = rowSums(across(where(is.numeric))))
}

# Function : Join IDs  ---------------------------------------------------------------
join_id <- function(ids, df, ... ){
    
    ids %>% 
        map_df(as.character) %>% 
        mutate(ID = str_extract(ID,"[:digit:]+")) %>% 
        select(ID,Name, ... ) %>% 
        full_join(df, by = "ID") %>% 
        rename(Name_from_ID = "Name.x", 
               Name_from_SELECx = "Name.y") %>% 
        relocate(... , .after = Name_from_SELECx) %>% 
        arrange(ID)
    
}


# Function : Plot ---------------------------------------------------------

plot_dist <- function(df.tidy,num_var, cat_var, type = "density", ...){
    
    core <- switch (type,
                    "density" = df.tidy %>% 
                        ggplot(aes({{num_var}}, {{cat_var}})) + 
                        geom_density_ridges(aes(fill = {{cat_var}}), ...),
                    "hist" = df.tidy %>% 
                        ggplot(aes({{num_var}}, {{cat_var}}, height = stat(density))) +
                        geom_density_ridges(aes(fill = {{cat_var}}),stat = "binline",
                                            draw_baseline = F, ...) 
    )
    
    core + 
        theme_minimal() +
        theme(legend.position = "none") +
        labs(title = "Distribution of Score") + ylab(NULL) 
    
}
# Server ------------------------------------------------------------------


server <- function(input, output, session) {
    
    # Upload files MCQ ---------------------------------------------------------------
    
    list.pre <- reactive({
        
        req(input$file)
        read_multi(file_name = input$file$name, file_path = input$file$datapath)
        
    })
    
    proper_list <- reactive({
        
        valid_regex <- c("First name","Surname","Email address","Grade")
        
        list.pre() %>% is.valid_list(valid_regex) %>% all()
        
    })
    
    proper_list_no_essay <- reactive({
        
        if(proper_list() == TRUE)
            
            !( list.pre() %>% is.grade.in_list(chr = c("Not yet graded")) )
        
        else FALSE 
        
    })
    
    list.raw <- reactive({
        
        shinyFeedback::feedbackWarning("file", !proper_list_no_essay(), "Incorrect file specification")
        
        req(proper_list_no_essay())
        
        list.pre()
        
    })
    
    
    # Upload files IDs ---------------------------------------------------------------
    
    ids <- reactive({
        
        req(input$file_id) # Require - code wait until file uploaded
        
        ext <- tools::file_ext(input$file_id$name)
        switch(ext,
               csv = vroom::vroom(input$file_id$datapath, delim = ","),
               xls = readxl::read_excel(input$file_id$datapath),
               xlsx = readxl::read_excel(input$file_id$datapath),
               validate("Invalid file; Please upload a .csv, .xls or .xlsx file")
        )
    })
    
    proper_id <- reactive({  
        
        all(c("ID","Name") %in% colnames( ids() ) && all(str_detect(ids() %>% pull(ID),"[:digit:]+"))) 
        
    })
    
    
    observeEvent(input$file_id,
                 shinyFeedback::feedbackWarning(
                     "file_id", 
                     !proper_id(),
                     "Incorrect ID file specification"
                 )  
    )
    
    
    
    # Clean ---------------------------------------------------------------
    
    list.cleaned <- reactive({
        
        list.raw() %>% clean_list()
    })
    
    # Process : compute max score by each student ---------------------------------------------------------------
    
    list.processed <- reactive({
        
        list.cleaned() %>% score_list()
    })
    
    
    # Re-adjust score ----------------------------------------------------------
    
    
    # Checkbox : Re-adjust ----------------------------------------------------
    
    
    check_string <- reactive({  
        
        if(input$check_readjust == T){"show"}else{"not_show"}
        
    })
    
    observeEvent(input$check_readjust,{
        updateTabsetPanel(session, "tab_readjust", selected = check_string())
        
    })
    
    
    
    list.label <- reactive({ paste("Max score of",names(list.raw() )) })
    
    output$readjust <- renderUI({
        map2(.x = list.label(), .y = old_max() ,~ numericInput(.x, label = .x, value = .y , min = 0))
    })
    
    
    
    old_max <- reactive({
        
        list.raw() %>% get_max_score()
    })
    
    new_max <- reactive({  map_dbl(list.label(), ~input[[.x]]) })
    
    
    list.processed_2 <- reactive({
        
        if(isTruthy(input$check_readjust)){
            
            list.processed() %>% readjust_score(old_max = old_max(), new_max = new_max())
            
        }else{ 
            
            list.processed() %>% readjust_score(old_max = old_max(), new_max = old_max())
            
        }
        
    })  
    
    # Rename with max_score ---------------------------------------------------------------
    
    list.renamed <- reactive({
        
        if(isTruthy(input$check_readjust)){
            
            list.processed_2() %>% rename_list(var = Score, suffix = new_max() )
            
        }else{   
            
            list.processed_2() %>% rename_list(var = Score, suffix = old_max() )
            
        }
        
    })
    
    # Join list to DF ---------------------------------------------------------------
    
    df.joined <- reactive({
        
        list.renamed() %>% join_list()
        
    })
    
    # Join DF to ids ---------------------------------------------------------------
    
    id_cols <- reactive({ 
        
        req(proper_id())
        ids() %>% select(-ID,-Name) %>% colnames() })
    
    output$select <- renderUI({
        
        if(input$add_cols == T){
            selectInput("cols","Choose column",choices = id_cols(), multiple = TRUE)
        }
        
    })
    
    
    df.joined_ids <- reactive({
        
        if(( !isTruthy(input$file_id)) || ( !proper_id()) ){  
            
            df.joined()
            
        }else if(input$add_cols == T){
            
            join_id(ids = ids() ,df = df.joined(), input$cols)
            
        }else{
            join_id(ids = ids() ,df = df.joined())}
        
        
    })  
    
    
    
    # Missing names ---------------------------------------------------------------
    
    missing_name <- reactive({
        
        df.joined_ids() %>% filter_at(vars(starts_with("Name")), any_vars(is.na(.)))
        
    })
    
    output$missing <- renderTable({ missing_name() })
    
    
    # Plot --------------------------------------------------------------------
    
    df.joined_tidy <- reactive({
        
        list.processed_2() %>% 
            rename_list_sim(Score) %>% 
            join_list() %>% 
            select(-Total) %>% 
            pivot_longer(cols = c(-Name,-ID), names_to = "Quiz", values_to = "Score") 
        
    })
    
    output$plot <- renderPlot({
        
        switch (input$plot_opt,
                "density" = df.joined_tidy() %>% plot_dist(Score,Quiz,type = "density",quantile_lines = TRUE, 
                                                           alpha = 0.7, size = 0.3),
                
                "hist" = df.joined_tidy() %>% plot_dist(Score,Quiz,type = "hist",bins = input$binwidth, 
                                                        alpha = 0.7, size = 0.3) 
        )
        
        
    }, res = 96)
    
    
    # Tab : binwidth ----------------------------------------------------------
    
    
    observeEvent(input$plot_opt,{
        
        updateTabsetPanel(session, "tab_binwidth", 
                          selected = switch (input$plot_opt,
                                             "density" = "not_show",
                                             "hist" = "show")
                          
        )
        
    })
    
    
    
    output$min_max <- renderText({
        
        if(is.null(input$plot_brush)) return("")
        
        else
            
            min <- round(input$plot_brush$xmin, 2)
        max <- round(input$plot_brush$xmax, 2)
        
        HTML("<b>Filter range</b> : <code>", min, "</code>", "<b> - </b>",
             "<code>",max, "</code>")
        
    })
    
    # Display table with optional Filter in plot  ----------------------------------------------------------  
    
    output$table <- renderDataTable({
        
        if(is.null(input$plot_brush) ){
            
            df.joined_ids()
            
        }else{
            
            df.joined_ids() %>% 
                filter(across(c(where(is.numeric),-Total),~between(.x, input$plot_brush$xmin,input$plot_brush$xmax) ))
            
        }  
        
    },options = list(lengthMenu = c(5,10,20,50), pageLength = 5 ))
    
    # Download ---------------------------------------------------------------
    
    output$download <- downloadHandler(
        
        filename = function() {
            paste0("Combined_score",".xlsx") #remove .xxx
        },
        content = function(file) {
            
            openxlsx::write.xlsx(df.joined_ids(), file)
        }
    )
    
}

