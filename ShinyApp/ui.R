# ui script

ui <- navbarPage(
  "CTSC Data Loofah",
  
  tabPanel(
    "Introduction",
    
    fluidPage(
      fluidRow(
        h3("Purpose"),
        p('The purpose of this tool is to ',
          ' investigate the data and its quality prior to analysis. The ',
          'goal is to catch data issues such as numeric variables stored',
          ' as text (e.g. numbers with a white space at the end, commas',
          ', etc.),categorical variables that have inconsistent values ',
          '(e.g. "male" vs "Male" vs "m" vs "M"), numeric variables ',
          'with extreme or nonsensical values (e.g. BMI of 203.4), or ',
          'highlight the use of certain values that are often used to ',
          'code for missing values (e.g. -9, 888, 999). Note that some ',
          'categorical/factor variables may be stored as numeric ',
          'values. For the purposes of this tool they will be treated ',
          'as numeric values, but this is not a data quality issue as ',
          'they can easily be converted to factors in statistical ',
          'software prior to analysis. This tool is not meant to ',
          'create final summary statistics! Rather, this tool is to ',
          'help the user identify potential data errors that then ',
          'need to be corrected prior to conducting statistical analyses.'),
        h3("Instructions"),
        p("To upload your data go to the `Data Import` tab and click `Browse`",
          " to find your data file. If your data is stored in xls or xlsx you",
          " can select the sheet to upload from the `Sheet` dropdown.")
      )
    )
  ),
  
  tabPanel(
    "Data Import",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload", NULL, accept = c(".csv", ".xlsx", ".xls",
                                     ".sas7bdat", ".sav",
                                     ".dta")
        ),
        selectInput('sheet', "Choose Sheet",  NULL)
      ),
      mainPanel(
        fluidRow(
          column(
            11,
            DT::dataTableOutput("info")
          )
        ),
        
        fluidRow(
          column(
            11,
            p(textOutput("dataInfo"))
          )
        )
      )
    )
  ),
  
  tabPanel(
    "Categorical Variables: Summary Table",
    fluidPage(
      
      fluidRow(
        column(
          12,
          DT::dataTableOutput("chrTable")
        )
        
      ),
      
      fluidRow(
        column(
          11,
          p(textOutput("chrMessage"))
        )
      )
    )
  ),
  
  tabPanel(
    "Categorical Variables: Figures",
    fluidPage(
      slickROutput("ChrSlickR")
    )
  ),
  
  tabPanel(
    "Numeric Variables: Summary Table",
    fluidPage(
      fluidRow(
        
        column(
          12,
          DT::dataTableOutput("numTable")
        )
        
      )
    )
  ),
  
  tabPanel(
    "Numeric Variables: Figures",
    fluidPage(
      slickROutput("NumSlickR")
    )
  )
  
)