ui <- fluidPage(
    
    titlePanel("Now We Can"),
    
    navlistPanel(
        "About us",
        tabPanel("Clan", 
                 mainPanel(
                        htmlOutput("clan_logo"),
                        h3('Clan Rules'),
                        uiOutput("rule_list"),
                        h3 ("Clan Info"),
                        DT::dataTableOutput("tbl_clan_info")
                        )
                 ),
        tabPanel("Members",
                 mainPanel(
                     fluidRow(
                         plotOutput("pl_member", height = 600), 
                         DT::dataTableOutput("tbl_member"))
                     )
                 ),
        
        tabPanel("Donations",
                 mainPanel(
                     fluidRow(
                         plotOutput("pl_donation", height = 600), 
                         DT::dataTableOutput("tbl_donation"))
                 )
        ),
        
        "War",
        tabPanel("Opponents",
                 mainPanel(
                     fluidRow(
                         plotOutput("pl_opponent", height = 600), 
                         DT::dataTableOutput("tbl_opponent"))
                 )
        ),
        
        tabPanel("Comparison",
                 mainPanel(
                     fluidRow(
                         plotOutput("pl_compare", height = 1000,  width = 600)
                     )
                 )
                 ),
        
        "-----"
        
    )
)

