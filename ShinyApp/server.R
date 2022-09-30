# server script

server <- function(input, output, session) {
  ###################
  # data import tab #
  ###################
  sheetNames <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if(ext == 'xls' | ext == "xlsx"){
      readxl::excel_sheets(input$upload$datapath)
    } else {
      "No Sheets"
    }
  })
  
  observe({
    updateSelectInput(
      session, "sheet", choices = sheetNames()
    )
  })
  
  
  
  data <- reactive({
    if((!is.null(input$upload)) && (input$sheet != "")){
      ext <- tools::file_ext(input$upload$name)
      switch(
        ext,
        csv = read.csv(input$upload$datapath),
        xls = read_xls(input$upload$datapath, sheet = input$sheet),
        xlsx = read.xlsx(input$upload$datapath, sheet = input$sheet),
        sas7bdat = read_sas(input$upload$datapath),
        sav = read_spss(input$upload$datapath),
        dta = read_dta(input$upload$datapath),
        validate(paste0("Invalid file; Please upload a file of the following",
                        " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta"))
      )
    } else {
      NULL
    }
  })
  
  data_info <- reactive({
    if((!is.null(data()))){
      text <- paste0("The data has ", nrow(data()), " rows and ", ncol(data()),
                     " columns. The variable types are displayed below.", 
                     " Please review each variable and check that its class", 
                     " (numeric or character) is as expected.")
    } else {
      text <- paste0("Import your data file.  
                     Accepted file types are xls, xlsx",
                     ", csv, SAS (sas7bdat), Stata (dta), or SPSS (sav).")
    }
    text
  })
  
  output$dataInfo <- renderText(data_info())
  
  ####################################
  # Categorical Variables Table  tab #
  ####################################
  data_class <- reactive({
    if((!is.null(data()))){
      data.frame(
        "Variable" = colnames(data()),
        "Class" = sapply(1:ncol(data()), function(x){class(data()[[x]])})
      )
    }
  })
  
  
  output$info <- DT::renderDataTable({
    DT::datatable(data_class())
  })
  
  chr_vars <- reactive({
    if((!is.null(data()))){
      data_class()[data_class()$Class == 'character' |
                     data_class()$Class == 'factor', ]$Variable
    }
  })
  
  data_chr <- reactive({
    if((!is.null(data()))){
      data() %>%
        dplyr::select(any_of(chr_vars()))
    }
  })
  
  data_chr_distinct <- reactive({
    if((!is.null(data_chr()))){
      data_chr() %>%
        summarise_all(n_distinct) %>%
        t() %>%
        as.data.frame() %>%
        rownames_to_column(var = "Variable") %>%
        rename("N_Distinct" = V1)
    }
  })
  
  data_chr_summarize <- reactive({
    if((!is.null(data_chr_distinct()))){
      data_chr_distinct() %>%
        filter(N_Distinct <= 20)
    }
  })
  
  chr_vars_summ <- reactive({
    if((!is.null(data_chr_summarize()))){
      unlist(data_chr_summarize()$Variable)
    }
  })
  
  data_chr_summ <- reactive({
    if((!is.null(data_chr_summarize()))){
      data() %>%
        dplyr::select(any_of(chr_vars_summ()))
    }
  })
  
  dat_chr_table <- reactive({
    if((!is.null(data_chr_summ()))){
      tableby(~., data = data_chr_summ(), test = FALSE,
              cat.stats = c('countpct', 'Nmiss'))
    }
  })
  
  overall_label <- reactive({
    if((!is.null(data_chr_summ()))){
      paste0("Overall (N=", nrow(data()), ")")
    }
  })
  
  chr_table <- reactive({
    if((!is.null(data_chr_summ()))){
      tmp <- summary(dat_chr_table())$object$Overall %>%
        filter(Overall != "") %>%
        rowwise() %>%
        mutate(
          Test = case_when(
            label != "N-Miss" ~ paste0(Overall[1], " (", round(Overall[2], 2),
                                       "%)"),
            label == "N-Miss" ~ paste0(Overall[1])
          )
        ) %>%
        ungroup() %>%
        mutate("Variable" = "variable") %>%
        rename("Category" = "label") %>%
        dplyr::select(Variable, variable, Category, Test)
      
      colnames(tmp)[colnames(tmp) == "Test"] <- overall_label()
      tmp
    }
  })
  
  dat_chr_distinct_check <- reactive({
    if((!is.null(data_chr_distinct()))){
      data_chr_distinct() %>%
        filter(N_Distinct > 20)
    }
  })
  
  chr_Message <- reactive({
    if((!is.null(dat_chr_distinct_check()))){
      chr_vars_check <- unlist(dat_chr_distinct_check()$Variable)
      if(length(chr_vars_check) > 0){
        text <- paste0(
          "The following variables are stored as a character/factor with more",
          " than 20 unique responses: ", paste(chr_vars_check, collapse = ", "),
          ". Consider checking if these are numeric values stored as text",
          " or character values with typos/spelling differences between similar",
          " responses (e.g. Male, male, m, M)."
        )
      }
    }
    text
  })
  
  output$chrTable <- DT::renderDataTable({
    if((!is.null(chr_table()))){
      DT::datatable(
        chr_table(), rownames = FALSE,
        extensions = 'RowGroup',
        options=list(columnDefs = list(list(visible=FALSE,
                                            targets=c(0, 1))),
                     rowGroup = list(dataSrc = 1),
                     pageLength = 20)
      ) %>%
        formatStyle(names(chr_table()), textAlign = 'center')
    }
  })
  
  output$chrMessage <- renderText(chr_Message())
  
  #####################################
  # Categorical Variables Figures tab #
  #####################################
  chr_plots <- reactive({
    if((!is.null(data_chr_summ()))){
      tmp_chr_plots <- vector(mode = 'list', length = ncol(data_chr_summ()))
      
      withProgress(message = "Creating Figures", value = 0, {
        n_chr <- length(tmp_chr_plots)
        
        for(i in 1:n_chr){
          tmp_chr_plots[[i]] <- xmlSVG({
            show(ggplot(data_chr_summ(), aes(x = .data[[chr_vars_summ()[i]]])) +
                   geom_bar() +
                   theme_bw())
          }, standalone = TRUE)
          
          incProgress(1/n_chr, detail = paste("Figure", i))
          
          Sys.sleep(0.1)
        }
      })
      
      
      tmp_chr_plots
    }
  })
  
  cP1 <- htmlwidgets::JS("function(slick, index) {
    return '<a>'+(index+1)+'</a>';
    }")
  
  opts_dot_number1 <- settings(
    initialSlide = 0,
    slidesToShow = 1,
    slidesToScroll = 1,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP1
  )
  
  output$ChrSlickR <- renderSlickR({
    if((!is.null(chr_plots()))){
      slickR(chr_plots(), height = 550, width = "95%", slideId = 'chr') +
        settings(slidesToShow = 1, slidesToScroll = 1) + 
        opts_dot_number1
    }
    
  })
  
  ###############################
  # Numeric Variables Table tab #
  ###############################
  num_vars <- reactive({
    if((!is.null(data()))){
      data_class()[data_class()$Class == 'numeric' |
                     data_class()$Class == 'integer', ]$Variable
    }
  })
  
  dat_num <- reactive({
    if((!is.null(data()))){
      data() %>%
        dplyr::select(any_of(num_vars()))
    }
  })
  
  dat_num_table <- reactive({
    if((!is.null(dat_num()))){
      tableby(~., data = dat_num(), test = FALSE, 
              numeric.stats = c('meansd', 'medianq1q3', 'range', 'Nmiss'))
    }
  })
  
  num_table <- reactive({
    if((!is.null(dat_num_table()))){
      tmp2 <- summary(dat_num_table())$object$Overall %>%
        filter(Overall != "") %>%
        rowwise() %>%
        mutate(
          Test = case_when(
            label == "Mean (SD)" ~ paste0(format(round(Overall[1], 2), nsmall = 2), 
                                          " (", format(round(Overall[2], 2), 
                                                       nsmall = 2), ")"),
            label == "Median (Q1, Q3)" ~ paste0(format(round(unname(Overall[1]), 2), 
                                                       nsmall = 2),
                                                " (", 
                                                format(round(unname(Overall[2]), 2), 
                                                       nsmall = 2),
                                                ", ", 
                                                format(round(unname(Overall[3]), 2), 
                                                       nsmall = 2),
                                                ")"),
            label == "Range" ~ paste0(format(round(Overall[1], 2), nsmall = 2), 
                                      " \U2012 ", 
                                      format(round(Overall[2], 2), nsmall = 2)),
            label == "N-Miss" ~ paste0(Overall[1])
          )
        ) %>%
        ungroup() %>%
        mutate("Variable" = variable) %>%
        rename("Summary" = "label") %>%
        dplyr::select(Variable, variable, Summary, Test)
      
      colnames(tmp2)[colnames(tmp2) == 'Test'] <- overall_label()
      tmp2
    }
  })
  
  output$numTable <- DT::renderDataTable({
    if((!is.null(num_table()))){
      DT::datatable(
        num_table(), rownames = FALSE,
        extensions = 'RowGroup',
        options=list(columnDefs = list(list(visible=FALSE,
                                            targets=c(0, 1))),
                     rowGroup = list(dataSrc = 1),
                     pageLength = 12)
      ) %>%
        formatStyle(names(num_table()), textAlign = 'center')
    }
  })
  
  
  #################################
  # Numeric Variables Figures tab #
  #################################
  num_plots <- reactive({
    if((!is.null(dat_num()))){
      tmp_num_plots <- vector(mode = 'list', length = ncol(dat_num()))
      
      withProgress(message = "Creating Figures", value = 0, {
        n_num <- length(tmp_num_plots)
        
        for(i in 1:n_num){
          tmp_num_plots[[i]] <- xmlSVG({
            show(ggplot(dat_num(), aes(x = .data[[num_vars()[i]]])) +
                   geom_histogram() +
                   theme_bw())
          }, standalone = TRUE)
          
          incProgress(1/n_num, detail = paste("Figure", i))
          
          Sys.sleep(0.1)
        }
        
      })
      tmp_num_plots
    }
  })
  
  cP2 <- htmlwidgets::JS("function(slick, index) {
      return '<a>'+(index+1)+'</a>';
                             }")
  
  opts_dot_number2 <- settings(
    initialSlide = 0,
    slidesToShow = 1,
    slidesToScroll = 1,
    focusOnSelect = TRUE,
    dots = TRUE,
    customPaging = cP2
  )
  
  output$NumSlickR <- renderSlickR({
    if((!is.null(num_plots()))){
      slickR(num_plots(), height = 550, width = "95%", slideId = 'num') +
        settings(slidesToShow = 1, slidesToScroll = 1) + 
        opts_dot_number2
    }
  })
  
}