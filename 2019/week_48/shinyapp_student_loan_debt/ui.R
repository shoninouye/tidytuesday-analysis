library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  # shinythemes::themeSelector(),
  # theme = shinythemes::shinytheme('simplex'),
  
  # Application title
  titlePanel("Contribution to Student Loan Payments"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       checkboxGroupInput("repaymentType",
                          "Repayment Types:",
                          c("Consolidation" = "pct_consolidation", 
                            "Rehabilitation" = "pct_rehabilitation", 
                            "Voluntary Payments" = "pct_voluntary_payments", 
                            "Wage Garnishments" = "pct_wage_garnishments"),
                          selected = c("Consolidation" = "pct_consolidation", 
                                       "Rehabilitation" = "pct_rehabilitation", 
                                       "Voluntary Payments" = "pct_voluntary_payments", 
                                       "Wage Garnishments" = "pct_wage_garnishments")
       ),
       dateRangeInput("dateRange",
                      label = "Enter Date Range (yyyy-mm-dd):",
                      start = as.Date("2015-12-31"),
                      end = as.Date("2018-12-31"),
                      min = min(loan_payments$date),
                      max = max(loan_payments$date)
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(
         tabPanel("Plot", plotOutput("plot")),
         tabPanel("Table", DTOutput("table"))
    )
  )
)
)
)
