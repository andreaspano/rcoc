server <- function(input, output, session) {
    
    output$pl_member <- renderPlot({pl_member})
    output$tbl_member <- DT::renderDataTable({my_clan_member})
    
    output$pl_donation <- renderPlot({pl_donation})
    output$tbl_donation <- DT::renderDataTable({tbl_donation})
    
    output$tbl_clan_info <- DT::renderDataTable(DT::datatable(tbl_clan_info, 
                                                              options = list(paging = FALSE, searching = FALSE),
                                                              colnames = c(' ' = 1, '  ' = 2),
                                                              rownames = FALSE))
    
    output$clan_logo <- renderText({c('<img src="',clan_logo_url,'">')})
    
    output$rule_list <- renderUI(HTML("<ul><li>Non essere Juventini</li><li>Essere attivi ed educati</li><li>Almeno un attacco in guerra</li><li>Almeno 2,000 punti nei giochi del clan </li></ul>"))
    
    output$pl_compare <- renderPlot({pl_compare})
    
    output$pl_opponent <- renderPlot({pl_opponent})
    
    output$tbl_opponent <- DT::renderDataTable({opponent_member}) 
    
    # output$summary <- renderPrint({
    #     summary(cars)
    # })
    # output$table <- DT::renderDataTable({DT::datatable(cars)})
}


