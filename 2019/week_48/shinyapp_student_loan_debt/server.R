library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  filtered_loan_payments <- reactive({
    # Plot percentage of contribution to total loan payments
      loan_payments %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2],
             payment_type %in% input$repaymentType)
    })
    output$plot <- renderPlot({
      filtered_loan_payments() %>% 
        ggplot(aes(x = date, y = pct_paid, color = payment_type)) +
        geom_line(size = 2) +
        scale_y_continuous(label = scales::percent) +
        labs(x = "Time",
             y = "Percentage of total loan payments",
             color = "Payment Type",
             subtitle = "Percentage of student loan repayment type contribution at the end of each fiscal quarter (3 months)") +
        scale_color_brewer(labels = c("Consolidation", "Rehabilitation", "Voluntary Payments", "Wage Garnishments"),
                           palette = "Paired") +
        theme_minimal()
    })
    output$table <- renderDT({
      filtered_loan_payments()
    })

})
