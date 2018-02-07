####Load necessary libraries####
library(data.table)# for easy manipulation of data table
library(sqldf)
library(lubridate)
library(ggplot2)
library(scales)
library(fiftystater)
library(cdlTools)
library(LendingClub)
library(shiny)
library(repmis)
library(plotly)
library(shinyjs)
library(shinydashboard)

####Personal Setup####



source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/vintage.Rda?raw=true")
source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/state.Rda?raw=true")

# setwd("U:/Documents/Professional EDU/DataScience/RStudio/LC/Competitor Research/LCInvestmentAdvisor")
# load("./smmry_2015.Rda")
# load("./smmry_2016.Rda")
# load("./smmry_2017.Rda")


# Define UI for application that draws a histogram
ui <- fluidPage(
  useShinyjs(),

  navbarPage(id = "bar",
  
  "Lending Club Investment Advisor",
  ####Historical Performance####
  tabPanel(
    value="hist",
    
    "Historical Performance",
    

    fluidPage(

      titlePanel("Lending Club Historical Performance"),
      sidebarLayout(
        sidebarPanel(
          
          h3("Loan Filter(Blank Means Overall)"),
          splitLayout( submitButton(text = "Refresh Filter"),
                       downloadButton("downloadData", "Download Filtered Data")
          ),
          
          selectInput("vintage", label="Loan Vintage", 
                      choices=levels(vintage$vintage),
                      selected = c("2017Q1","2017Q2","2017Q3"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("term", label="Loan Term", 
                      choices=c(" 36 months"," 60 months"),
                      selected = c(" 36 months"),
                      
                      multiple = T,
                      selectize = TRUE),
          selectInput("purpose", label="Loan Purpose", 
                      choices=c("Car","Credit Card","Debt Consolidation","Home Improvement","Major Purchase",
                                "Medical","Moving","Other","Small Business","Vacation" ),
                      selected = c("Car","Credit Card","Debt Consolidation","Home Improvement"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("addr_state", label="Location State", 
                      choices=levels(state$addr_state),
                      selected = c("CA","TX","NY","PA"),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("monthly_inc","Monthly Income (>=$12,000 are grouped into $12,000)",min=0,max=12000,value=c(0,12000),step=1000),
          selectInput("dlq", label="Delinquencies (Last 2 yrs)", 
                      choices=c( "0", "1-3", "4+" ),
                      multiple = T,
                      selectize = TRUE),
          selectInput("pub_rec", label="Public Record", 
                      choices=c("with public record","without public record"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("grade", label="Risk Grade", 
                      choices=c("A", "B", "C", "D", "E", "F", "G"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("int_rate", label="Interest Rate", 
                      choices=c("a.<7.00%","b.7.00%-10.99%","c.11.00%-14.99%", "d.15.00%-19.99%", "e.>20.00%" ),
                      multiple = T,
                      selectize = TRUE),
          selectInput("homeowner", label="Homer Ownership", 
                      choices=c("MORTGAGE", "NONE","OTHER","OWN","RENT","ANY"),
                      multiple = T,
                      selectize = TRUE),
          sliderInput("inq_last_6mths","Inquiries within 6 Months before Issuance (4+ inquiries are grouped into 4)",min=0,max=4,value=4,step=1),
          selectInput("credit", label="Credit Score", 
                      choices=c("660-670" ,"671-690" ,"691-710" ,"711-800" ,"801+"),
                      multiple = T,
                      selectize = TRUE),
          selectInput("Income_verified", label="Income Verified", 
                      choices=c("Not Verified","Verified"),
                      multiple = T,
                      selectize = TRUE)
          
          
          
        ),

        mainPanel(tabsetPanel(
          id="histbar",
          tabPanel(
          title = "Standard Plot",
          value = "tab1",
          h4(textOutput("test")),
          plotlyOutput("plot1"),
          plotlyOutput("plot2"),
          plotlyOutput("plot3"),
          plotlyOutput("plot4")
          
        ),

        tabPanel(
          title="Customized Plot",
          value="tab2",
          fluidRow(
            column(4,
                   hr(),
                   selectInput('xaxis', 'X-Axis', 
                               choices =
                                 c('Overall','Purpose','State','Issue Year','Issue Month','Vintage','Grade','Term','Loan Status','Home ownership',	
                                   'Public Record','Interest Rate Bucket','Inquiries within 6 Months before Issuance','Credit Score','Income Verified',
                                   'Monthly Income','Delinquencies (Last 2 yrs)','Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                                   'Total Received Interest','Total Received Payment','Interest Rate','Credit Loss Rate','Risk Adjusted Return','Avg Loan Size','Simple Average Age')
                               ,
                               selectize=TRUE)
            ),
            column(4,
                   hr(),
                   selectInput('yaxis', 'Y-Axis', 
                               choices =
                                 c('Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                                   'Total Received Interest','Total Received Payment','Interest Rate','Credit Loss Rate','Risk Adjusted Return','Avg Loan Size','Simple Average Age')
                               ,
                               selectize=TRUE)
            )
            
          ),
          fluidRow(
            column(4,
                   hr(),
                   selectInput('size', 'Bubble Size', 
                               choices =
                                 c('Constant'
                                   ,'Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                                   'Total Received Interest','Total Received Payment','Interest Rate','Credit Loss Rate','Risk Adjusted Return','Avg Loan Size','Simple Average Age')
                               ,
                               selectize=TRUE)
            ),
            column(4,
                   hr(),
                   selectInput('color', 'Bubble Color', 
                               choices =
                                 c('Overall','Purpose','State','Issue Year','Issue Month','Vintage','Grade','Term','Loan Status','Home ownership',	
                                   'Public Record','Interest Rate Bucket','Inquiries within 6 Months before Issuance','Credit Score','Income Verified',
                                   'Monthly Income','Delinquencies (Last 2 yrs)')
                               ,
                               selectize=TRUE)
            )
            
          ),
          submitButton(text = "Refresh Plot"),
          plotlyOutput("bubble")
          
          
          
        ),
        tabPanel(

          title="Backtest Summary",
          value="tab3",
          tableOutput("summary1"),
          tableOutput("summary2"),
          tableOutput("summary3"),
          tableOutput("summary4"),
          tableOutput("summary5")
          )
          
        
        )
        )
        )
      )
      
    ),
  ####Live Investment####
  tabPanel(
    value="invest",
    title="Live Investment",
    
    
    
    fluidPage(
      splitLayout(
      titlePanel("Lending Club Live Investment"),
      textInput(inputId = "LCid",label="Account ID"),
      passwordInput(inputId = "API", label="API"),
      fluidPage(br(),p(" "),
      submitButton(text = "Refresh")
      )),
      
        
        tabsetPanel(
          id="invbar",
          tabPanel(
            title = "Portfolio Summary",
            value = "tab4",
            
                tableOutput("acct_smmry"),
                sidebarLayout(
                  sidebarPanel(
                  selectInput('xaxis2', 'Bubble X-Axis', 
                                          choices =
                                            c('Overall',"Purpose","Term","Loan Status","Grade","Subgrade","Current Payment Status"
                                              ,"Credit Trend","Portfolio","Vintage","Application Type",
                                              "Note Count"
                                              ,"Loan Amount","Note Amount","Weight Avg Rate","Risk Adjusted Return","Payments Received"
                                              ,"Principal Received","Interest Received","Principal Pending","Principal at Risk","Principal Charged Off"
                                              ,"Principal at Risk Rate","Charged Off Rate"
                                              ,"Weight Avg Age(Days)"
                                              )
                                          ,
                                          selectize=TRUE),
                  selectInput('yaxis2', 'Bubble Y-Axis', 
                              choices =
                                c(
                                  "Note Count"
                                  ,"Loan Amount","Note Amount","Weight Avg Rate","Risk Adjusted Return","Payments Received"
                                  ,"Principal Received","Interest Received","Principal Pending","Principal at Risk","Principal Charged Off"
                                  ,"Principal at Risk Rate","Charged Off Rate"
                                  ,"Weight Avg Age(Days)"
                                )
                              ,
                              selectize=TRUE),
                  selectInput('size2', 'Bubble Size', 
                              choices =
                                c('Constant',"Note Count"
                                  ,"Loan Amount","Note Amount","Weight Avg Rate","Risk Adjusted Return","Payments Received"
                                  ,"Principal Received","Interest Received","Principal Pending","Principal at Risk","Principal Charged Off"
                                  ,"Principal at Risk Rate","Charged Off Rate"
                                  ,"Weight Avg Age(Days)")
                              ,
                              selectize=TRUE),
                  selectInput('color2', 'Bubble Color(Summary Class)', 
                              choices =
                                c("Portfolio","Purpose","Term","Loan Status","Grade","Subgrade","Current Payment Status"
                                  ,"Credit Trend","Vintage"
                                  ,"Application Type",'Overall')
                              ,
                              selectize=TRUE),
                  submitButton(text = "Refresh Plot/Summary")
                  ),
            mainPanel(
 

              
              plotlyOutput("bubble2"),
              tableOutput("acct_smmry2")
            )
            )
  
              
              
            )
          ),
          
          tabPanel(
            title="Primary Market Investment",
            value="tab5"
 
            
            
            
          )
          
        
        
      )
    )
  #####
    
  )

)


# Define server logic required to draw a histogram
server <- function(input, output,session) {


  dataInput <- reactive({
    if(length(input$vintage)>0){
      selected_vintage<-data.frame(vintage=input$vintage)
    }else{
      selected_vintage<-data.frame(vintage=levels(vintage$vintage))
    } 
    if(length(input$term)>0){
      selected_term<-data.frame(term=input$term)
    }else{
      selected_term<-data.frame(term=c(" 36 months"," 60 months"))
    } 
    if(length(input$purpose)>0){
      selected_purpose<-data.frame(purpose=input$purpose)
    }else{
      selected_purpose<-data.frame(purpose=c("Car","Credit Card","Debt Consolidation","Home Improvement","Major Purchase",
                                             "Medical","Moving","Other","Small Business","Vacation" ))
    }
    if(length(input$addr_state)>0){
      selected_state<-data.frame(addr_state=input$addr_state)
    }else{
      selected_state<-data.frame(addr_state=levels(state$addr_state))
    }
    
    selected_mth_inc<-data.frame(min=min(input$monthly_inc),max=max(input$monthly_inc))
    if(length(input$dlq)>0){
      selected_dlq<-data.frame(dlq=input$dlq)
    }else{
      selected_dlq<-data.frame(dlq=c( "0", "1-3", "4+" ))
    }
    if(length(input$pub_rec)>0){
      selected_pub_rec<-data.frame(pub_rec=input$pub_rec)
    }else{
      selected_pub_rec<-data.frame(pub_rec=c("with public record","without public record"))
    }
    if(length(input$grade)>0){
      selected_grade<-data.frame(grade=input$grade)
    }else{
      selected_grade<-data.frame(grade=c("A", "B", "C", "D", "E", "F", "G"))
    }
    if(length(input$int_rate)>0){
      selected_int_rate<-data.frame(int_rate=input$int_rate)
    }else{
      selected_int_rate<-data.frame(int_rate=c("a.<7.00%","b.7.00%-10.99%","c.11.00%-14.99%", "d.15.00%-19.99%", "e.>20.00%" ))
    }
    if(length(input$homeowner)>0){
      selected_homeowner<-data.frame(homeowner=input$homeowner)
    }else{
      selected_homeowner<-data.frame(homeowner=c("MORTGAGE", "NONE","OTHER","OWN","RENT","ANY"))
    }
    selected_inq<-data.frame(inq=max(input$inq_last_6mths))
    if(length(input$credit)>0){
      selected_credit<-data.frame(credit=input$credit)
    }else{
      selected_credit<-data.frame(credit=c("660-670" ,"671-690" ,"691-710" ,"711-800" ,"801+"))
    }  
    if(length(input$Income_verified)>0){
      selected_incv<-data.frame(Income_verified=input$Income_verified)
    }else{
      selected_incv<-data.frame(Income_verified=c("Not Verified","Verified"))
    }
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Loading 2015 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2015.Rda?raw=true")
    tmp_2015<-sqldf(
      "select 
      a.*
      from smmry_2015 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2015)

    progress$set(message = "Loading 2016 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2016.Rda?raw=true")
    tmp_2016<-sqldf(
      "select 
      a.*
      from smmry_2016 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2016)

    progress$set(message = "Loading 2017 Summary")
    source_data("https://github.com/GYang14/DataProductProject/blob/gh-pages/smmry_2017.Rda?raw=true")
    tmp_2017<-sqldf(
      "select 
      a.*
      from smmry_2017 a
      inner join selected_vintage b
      on a.issue_year||a.issue_qtr=b.vintage
      inner join selected_term b1
      on a.term=b1.term
      inner join selected_purpose c
      on a.purpose=c.purpose
      inner join selected_state d
      on a.addr_state=d.addr_state
      inner join  selected_mth_inc e
      on a.monthly_inc>=e.min and a.monthly_inc<=e.max
      inner join selected_dlq f
      on a.delinq_2yrs_group=f.dlq
      inner join selected_pub_rec g
      on a.public_record=g.pub_rec
      inner join selected_int_rate h
      on a.interest_rate=h.int_rate
      inner join selected_grade i
      on a.grade=i.grade
      inner join selected_homeowner j
      on a.home_ownership=j.homeowner
      inner join selected_inq k
      on a.inq_last_6mths <= k.inq
      inner join selected_credit l
      on a.credit_score = l.credit
      inner join selected_incv m
      on a.Income_verified = m.Income_verified
      ")
    rm(smmry_2017)
    tmp<-rbind(tmp_2015,tmp_2016,tmp_2017)
    rm(tmp_2015,tmp_2016,tmp_2017)
    tmp
  })
  

  facet_class_bar_plot<-function(data,x,facet,y,class,position="stack",title,xlab="",xsize=8,ylab="",classlab=class,yformat=dollar_format()){
    tmp<-data[,c(x,facet,y,class)]
    names(tmp)<-c("x","facet","y","class")
    theme_set(theme_bw())
    p<-ggplot(tmp, aes(x = x, y=y)) + 
      geom_bar(stat="identity",position=position, width=.5, aes(fill=class)) + 
      facet_grid(.~ facet, space="free_x", scales="free_x", switch="x") +
      labs(title=title,x=xlab,y=ylab,fill = classlab) + 
      theme(axis.text.y = element_text(angle=90),axis.text.x = element_text(angle=0, size =xsize,hjust = 0.5),strip.placement = "outside",
            legend.title = element_text(angle=0, size =xsize*1.5),
            panel.ontop = F,
            panel.spacing=unit(0,"cm"))+
      scale_y_continuous( label=yformat)
    return(p)
  }

  
  
  output$plot1 <- renderPlotly({
    toggle(selector = "#histbar li a[data-value=tab2]")
    toggle(selector = "#histbar li a[data-value=tab3]")
    toggle(selector = "#bar li a[data-value=invest]")
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot1")
      data<-dataInput()
      dollar_by_issue<-sqldf("select issue_year,issue_qtr,      
                             purpose,sum(loan_amnt/1000000) as loan_amount from data group by 1,2,3")
      p<-facet_class_bar_plot(dollar_by_issue,x="issue_qtr",facet="issue_year",y="loan_amount",class="purpose",title="Loan Issuance (in $Million) by Issuance Date and Purpose",xlab = "",ylab="",classlab="Purpose")
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      ggplotly(p)
     }
    else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
  })
  
  output$plot2 <- renderPlotly({
    #toggle(selector = "#histbar li a[data-value=tab2]")
    #toggle(selector = "#histbar li a[data-value=tab3]")
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot2")
      data<-dataInput()
      loan_by_state<-
        sqldf("select issue_year,addr_state,sum(loan_amnt/1000000.0) as loan_amnt_in_M from data group by 1,2")
      loan_by_state$issue_year<-as.factor(loan_by_state$issue_year)
      loan_by_state$facet<-""
      p<-facet_class_bar_plot(loan_by_state,x="addr_state",facet="facet",y="loan_amnt_in_M",class="issue_year",title="Loan Issuance(in $Million) by State",xsize=6.5,classlab="Issuance Year")
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      ggplotly(p)
    }
    else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
    
  })
  output$plot3 <- renderPlotly({
    #toggle(selector = "#histbar li a[data-value=tab2]")
    #toggle(selector = "#histbar li a[data-value=tab3]")
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot3")
      
      data<-dataInput()
      loan_by_issue_grade<-sqldf("select issue_year,
                                       case when grade in ('A','B','C','D','E') then grade
      else 'F+G' end as grade,
                                 loan_status,sum(loan_amnt/1000000) as loan_amnt from data group by 1,2,3")
      loan_by_issue_grade$loan_status<-as.character(loan_by_issue_grade$loan_status)
      loan_by_issue_grade$loan_status<-factor(loan_by_issue_grade$loan_status,levels = c("Current","In Grace Period","Fully Paid","Late (16-30 days)","Late (31-120 days)","Default","Charged Off"))
      p<-facet_class_bar_plot(loan_by_issue_grade,x="grade",facet="issue_year",y="loan_amnt",class="loan_status",title="Loan Origination Amount(in $Million) by Latest Loan Status",classlab="Latest Loan Status")
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      ggplotly(p)
    } else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
  })
  output$plot4 <- renderPlotly({
    #toggle(selector = "#histbar li a[data-value=tab2]")
    #toggle(selector = "#histbar li a[data-value=tab3]")
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Plot4")
      
      data<-dataInput()
      hist_return_by_grade<-
        sqldf("
              select 
              issue_year,
                    case when grade in ('A','B','C','D','E') then grade
      else 'F+G' end as grade,
              sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
              sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return'
              from data
              group by 1,2
              ")
      hist_return_by_grade.m<-melt(hist_return_by_grade,id.vars=c('issue_year','grade'))
      p<-facet_class_bar_plot(hist_return_by_grade.m,x="grade",facet="issue_year",y="value",class="variable",position="dodge",title="Interest Rate and Risk Adjusted Return",xlab = "",ylab="",classlab="Rate",yformat=percent_format())
      toggle(selector = "#histbar li a[data-value=tab2]")
      toggle(selector = "#histbar li a[data-value=tab3]")
      toggle(selector = "#bar li a[data-value=invest]")
      
      ggplotly(p)
    } else{
      toggle(selector = "#histbar li a[data-value=tab2]")
      toggle(selector = "#histbar li a[data-value=tab3]")
      toggle(selector = "#bar li a[data-value=invest]")
      return(NULL)
    }

   
    
  })


  output$bubble <- renderPlotly({
    toggle(selector = "#histbar li a[data-value=tab1]")
    toggle(selector = "#histbar li a[data-value=tab3]")
    toggle(selector = "#bar li a[data-value=invest]")
    if(nrow(dataInput())>0){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating Bubble Chart")
      
      tmp<-dataInput()
      names(tmp)<-c('Purpose','State','Issue Year','Issue Month','issue_qtr','Grade','Term','Loan Status','Home ownership',	
                     'Public Record','Interest Rate Bucket','Inquiries within 6 Months before Issuance','Credit Score','Income Verified',
                     'Monthly Income',	'Delinquencies (Last 2 yrs)','Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                     'Total Received Interest','Total Received Payment',	'investor_funded_amt',	'total_pymnt_inv',	'amnt_inv_x_int_rate',	'amnt_inv_x_rar','amnt_inv_x_acr',	'mob')
      tmp$Vintage<-paste0(tmp$`Issue Year`,tmp$`issue_qtr`)
      tmp$Overall<-"Overall"
      tmp$`Issue Year`<-as.factor(as.character(tmp$`Issue Year`))
      tmp$`Issue Month`<-as.factor(as.character(tmp$`Issue Month`))
      tmp$`Inquiries within 6 Months before Issuance`<-as.factor(as.character(tmp$`Inquiries within 6 Months before Issuance`))
      tmp$`Monthly Income`<-as.factor(as.character(tmp$`Monthly Income`))
      
      if(input$xaxis%in%c(
        'Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
        'Total Received Interest','Total Received Payment','Interest Rate','Credit Loss Rate',
        'Risk Adjusted Return','Avg Loan Size','Simple Average Age')){xaxis<-"Overall"}
      else{
        xaxis<-input$xaxis
      }
        tmp<-tmp[,c(input$color,xaxis,'Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                    'Total Received Interest','Total Received Payment',	'investor_funded_amt',	'total_pymnt_inv',	'amnt_inv_x_int_rate',	'amnt_inv_x_rar','amnt_inv_x_acr',	'mob')]
        names(tmp)<-c("color","xaxis",'Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
                      'Total Received Interest','Total Received Payment',	'investor_funded_amt',	'total_pymnt_inv',	'amnt_inv_x_int_rate',	'amnt_inv_x_rar','amnt_inv_x_acr',	'mob')

        smmry<-sqldf("select color,xaxis
                      ,sum(`Loan Count`) as 'Loan Count'
                      ,sum(`Origination Principal`) as 'Origination Principal'
                      ,sum(`Outstanding Principal`) as 'Outstanding Principal'
                      ,sum(`Total Received Principal`) as 'Total Received Principal'
                      ,sum(`Total Received Interest`) as 'Total Received Interest'
                      ,sum(`Total Received Payment`) as 'Total Received Payment'
                      ,sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate'
                      ,sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate'

                      ,sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return'
                      ,sum(`Outstanding Principal`)/sum(`Loan Count`) as 'Avg Loan Size'
                      ,sum(mob)/sum(`Loan Count`) as 'Simple Average Age'
                      from tmp
                      group by 1,2
                     ")
        rm(tmp)
        smmry$Constant<-1
        if(input$xaxis%in%c(
          'Loan Count','Origination Principal','Outstanding Principal','Total Received Principal',
          'Total Received Interest','Total Received Payment','Interest Rate','Credit Loss Rate',
          'Risk Adjusted Return','Avg Loan Size','Simple Average Age')){
          smmry<-smmry[,c(input$xaxis,input$yaxis,input$size,"color")]
        }else{
          smmry<-smmry[,c("xaxis",input$yaxis,input$size,"color")]
        }
        
        names(smmry)<-c("xaxis","yaxis","size","color")
        
        p <- ggplot(smmry, aes(x=xaxis, y=yaxis))+labs(title="Customized Bubble Plot",x='',y='   ',color=input$color,size=input$size)
        p<-p+ geom_point(aes(col=color, size=size))
        #+theme(axis.text.y = element_text(angle=90))
        if(input$yaxis%in%c(
          'Origination Principal','Outstanding Principal','Total Received Principal',
          'Total Received Interest','Total Received Payment','Avg Loan Size')){
          p<-p+scale_y_continuous(label=dollar_format())
          
        }else if(input$yaxis%in%c(
          'Loan Count','Simple Average Age')){
          p<-p+scale_y_continuous(label=comma)
        }else{
          p<-p+scale_y_continuous(label=percent)
        }  
        if(input$xaxis%in%c(
          'Origination Principal','Outstanding Principal','Total Received Principal',
          'Total Received Interest','Total Received Payment','Avg Loan Size')){
          p<-p+scale_x_continuous(label=dollar_format())
          
        }else if(input$xaxis%in%c(
          'Loan Count','Simple Average Age')){
          p<-p+scale_x_continuous(label=comma)
        }else if(input$xaxis%in%c(
        'Interest Rate','Risk Adjusted Return','Credit Loss Rate')){
          p<-p+scale_x_continuous(label=percent)
        }
        toggle(selector = "#histbar li a[data-value=tab1]")
        toggle(selector = "#histbar li a[data-value=tab3]")
        toggle(selector = "#bar li a[data-value=invest]")
          
        ggplotly(p) %>% 
          layout(
                 legend = list(orientation = "h",y=-0.2))
    } else{
      toggle(selector = "#histbar li a[data-value=tab1]")
      toggle(selector = "#histbar li a[data-value=tab3]")
      toggle(selector = "#bar li a[data-value=invest]")
      return(NULL)
    }
    
    
  })
  
  output$test <- renderText({
    
    if(nrow(dataInput())==0)
      print("Filters Resulting Empty Set, Please Change Filters")
    
  })

  output$summary1 <- renderTable({
    toggle(selector = "#histbar li a[data-value=tab1]")
    toggle(selector = "#histbar li a[data-value=tab2]")
    toggle(selector = "#bar li a[data-value=invest]")
    if(nrow(dataInput())>0){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 1")
    data<-dataInput()
    smmry<-sqldf("select 
                 sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate',
                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data ")
    smmry$`Risk Adjusted Return`<-paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
    smmry$`Interest Rate`<-paste(format(round(smmry$`Interest Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Credit Loss Rate`<-paste(format(round(smmry$`Credit Loss Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry} else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
    
        
  }, caption = "All Matching Loans",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  output$summary2 <- renderTable({
    if(nrow(dataInput())>0){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 4")
    data<-dataInput()
    smmry<-sqldf("select issue_year as 'ISSUE YEAR',
                 sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate',

                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
    smmry$`Interest Rate`<-paste(format(round(smmry$`Interest Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Credit Loss Rate`<-paste(format(round(smmry$`Credit Loss Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry} else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
    
  }, caption = "ISSUE YEAR",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary3 <- renderTable({
    if(nrow(dataInput())>0){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 2")
    data<-dataInput()
    smmry<-sqldf("select grade as 'GRADE',
                 sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate',

                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
    smmry$`Interest Rate`<-paste(format(round(smmry$`Interest Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Credit Loss Rate`<-paste(format(round(smmry$`Credit Loss Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry} else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
    
  }, caption = "GRADE",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary4 <- renderTable({
    if(nrow(dataInput())>0){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 3")
    data<-dataInput()
    smmry<-sqldf("select purpose as 'PURPOSE',
                 sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate',

                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
    smmry$`Interest Rate`<-paste(format(round(smmry$`Interest Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Credit Loss Rate`<-paste(format(round(smmry$`Credit Loss Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    smmry} else{
      #toggle(selector = "#histbar li a[data-value=tab2]")
      #toggle(selector = "#histbar li a[data-value=tab3]")
      return(NULL)
    }
    
  }, caption = "PURPOSE",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))

  output$summary5 <- renderTable({
    if(nrow(dataInput())>0){
    progress <- shiny::Progress$new()
    on.exit(progress$close())
    progress$set(message = "Calculating Summary 5")
    data<-dataInput()
    smmry<-sqldf("select term as 'TERM',
                 sum(amnt_inv_x_rar)/sum(investor_funded_amt) as 'Risk Adjusted Return',
                 sum(amnt_inv_x_int_rate)/sum(investor_funded_amt) as 'Interest Rate',
                 sum(amnt_inv_x_acr)/sum(investor_funded_amt) as 'Credit Loss Rate',

                 sum(loan_amnt) as 'Origination Principal',
                 sum(out_prncp) as 'Outstanding Principal',
                 sum(loan_count) as 'Count',
                 sum(mob)/sum(loan_count) as 'Simple Average Age',
                 sum(total_rec_int) as 'Interest'
                 from data
                 group by 1")
    smmry$`Risk Adjusted Return`<-paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
    smmry$`Interest Rate`<-paste(format(round(smmry$`Interest Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Credit Loss Rate`<-paste(format(round(smmry$`Credit Loss Rate`*100,2),nsmall=2),"%",sep="")
    smmry$`Origination Principal`<-dollar(smmry$`Origination Principal`)
    smmry$`Outstanding Principal`<-dollar(smmry$`Outstanding Principal`)
    smmry$`Count`<-comma(smmry$`Count`)
    smmry$`Simple Average Age`<-round(smmry$`Simple Average Age`,2)
    smmry$`Interest`<-dollar(smmry$`Interest`)
    toggle(selector = "#histbar li a[data-value=tab1]")
    toggle(selector = "#histbar li a[data-value=tab2]")
    toggle(selector = "#bar li a[data-value=invest]")
    smmry} else{
      toggle(selector = "#histbar li a[data-value=tab1]")
      toggle(selector = "#histbar li a[data-value=tab2]")
      toggle(selector = "#bar li a[data-value=invest]")
      return(NULL)
    }
    
  }, caption = "TERM",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  LC_acct_smmry <- reactive({
    if(!(input$LCid==''||input$API=='')){
      id<-input$LCid
      API<-input$API
      CRED<-MakeCredential(id,API)
      acct_smmry<-AccountSummary_(CRED)
      acct_kpi<-acct_smmry$content
      acct_kpi}else{
        return(NULL)
      }
  })
  LC_note_detail <- reactive({
    if(!(input$LCid==''||input$API=='')){
      id<-input$LCid
      API<-input$API
      CRED<-MakeCredential(id,API)
      DetailedNotesOwned<-DetailedNotesOwned(CRED)$content
      DetailedNotesOwned}else{
        return(NULL)
      }
  })
  
  output$acct_smmry <- renderTable({
    if(!is.null(LC_acct_smmry())){
      acct_kpi<-LC_acct_smmry()
    acct_kpi$adjustedAccountValue<-acct_kpi$availableCash+acct_kpi$infundingBalance+acct_kpi$outstandingPrincipal-acct_kpi$adjustments.adjustmentForPastDueNotes
    acct_kpi$TotalPayments<-acct_kpi$receivedInterest+acct_kpi$receivedPrincipal+acct_kpi$receivedLateFees
    acct_kpi<-acct_kpi[,c(
      "netAnnualizedReturn.combinedNAR"
      ,"netAnnualizedReturn.combinedAdjustedNAR"
      ,"availableCash"
      ,"infundingBalance"
      ,"outstandingPrincipal"
      ,"accountTotal"
      ,"adjustments.adjustmentForPastDueNotes"
      ,"adjustedAccountValue"
      ,"receivedInterest"
      ,"TotalPayments"
      ,"totalNotes"
    )]
    acct_kpi$netAnnualizedReturn.combinedNAR<-
      paste(format(round(acct_kpi$netAnnualizedReturn.combinedNAR*100,2),nsmall=2),"%",sep="")
    acct_kpi$netAnnualizedReturn.combinedAdjustedNAR<-
      paste(format(round(acct_kpi$netAnnualizedReturn.combinedAdjustedNAR*100,2),nsmall=2),"%",sep="")
    acct_kpi$availableCash<-dollar(acct_kpi$availableCash)
    acct_kpi$infundingBalance<-dollar(acct_kpi$infundingBalance)
    acct_kpi$outstandingPrincipal<-dollar(acct_kpi$outstandingPrincipal)
    acct_kpi$accountTotal<-dollar(acct_kpi$accountTotal)
    acct_kpi$adjustments.adjustmentForPastDueNotes<-paste0("(",dollar(acct_kpi$adjustments.adjustmentForPastDueNotes),")")
    acct_kpi$adjustedAccountValue<-dollar(acct_kpi$adjustedAccountValue)
    acct_kpi$receivedInterest<-dollar(acct_kpi$receivedInterest)
    acct_kpi$TotalPayments<-dollar(acct_kpi$TotalPayments)
    acct_kpi$totalNotes<-comma(acct_kpi$totalNotes)
    names(acct_kpi)<-c("Net Annualized Return"
                       ,"Adjusted Net Annualized Return"
                       ,"Available Cash"
                       ,"Committed Cash"
                       ,"outstanding Principal"
                       ,"Account Value"
                       ,"Adjustment For Past-Due Notes"
                       ,"Adjusted Account Value"
                       ,"Interest Received"
                       ,"Total Payments"
                       ,"Total Notes"
    )
    acct_kpi
    }else{
      return(NULL)
    }
      
    
  }, caption = "Account Summary",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$bubble2 <- renderPlotly({
    if(!is.null(LC_note_detail())){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating LC Bubble Chart")
      
      tmp<-LC_note_detail()
      tmp$LoanAgeinDays<-as.integer(as.Date.character(substr(tmp$nextPaymentDate,1,10),format = "%Y-%m-%d")
                                    -as.Date.character(substr(tmp$issueDate,1,10),format = "%Y-%m-%d"))
      tmp$subgrade<-tmp$grade
      tmp$grade<-substr(tmp$subgrade,1,1)
      tmp$Vintage<-substr(tmp$issueDate,1,7)
      tmp<-tmp[,c("purpose","interestRate","loanLength","loanStatus","grade","subgrade","currentPaymentStatus"
                  ,"creditTrend","portfolioName","loanAmount","noteAmount","paymentsReceived","accruedInterest"
                  ,"principalPending","interestPending","principalReceived","interestReceived","Vintage"
                  ,"LoanAgeinDays","applicationType")]
       names(tmp)<-c("Purpose","Interest Rate","Term","Loan Status","Grade","Subgrade","Current Payment Status"
                     ,"Credit Trend","Portfolio","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                     ,"Principal Pending","Interest Pending","Principal Received","Interest Received","Vintage"
                     ,"Loan Age(Days)","Application Type")
      tmp$Overall<-"Overall"
      tmp$`Term`<-as.factor(as.character(tmp$`Term`))
      if(input$xaxis2%in%c("Note Count"
        ,"Weight Avg Rate","Loan Amount","Note Amount","Payments Received","Accrued Interest"
        ,"Principal Pending","Interest Pending","Principal Received","Interest Received"
        ,"Weight Avg Age(Days)")){xaxis<-"Overall"}
      else{
        xaxis<-input$xaxis2
      }
      tmp<-tmp[,c(input$color2,xaxis,
                  "Interest Rate","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                  ,"Principal Pending","Interest Pending","Principal Received","Interest Received"
                  ,"Loan Age(Days)")]
      names(tmp)<-c("color","xaxis","Interest Rate","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                    ,"Principal Pending","Interest Pending","Principal Received","Interest Received"
                    ,"Loan Age(Days)")
      tmp$loanStatus<-LC_note_detail()[,c("loanStatus")]
      tmp$out_prncp_multiplier <-
        ifelse(
          tmp$loanStatus == 'In Grace Period',
          0.31,
          ifelse(
            tmp$loanStatus == 'Late (16-30 days)',
            0.62,
            ifelse(
              tmp$loanStatus == 'Late (31-120 days)',
              0.88,
              ifelse(tmp$loanStatus == 'Default',0.72, 
                     ifelse(tmp$loanStatus == 'Charged Off',1, 0))
            )
          )
        )
      tmp$`Payments Received`<-as.numeric(tmp$`Payments Received`)
      tmp$`Principal Received`<-as.numeric(tmp$`Principal Received`)
      tmp$`Principal Pending`<-as.numeric(tmp$`Principal Pending`)
      tmp$`Note Amount`<-as.numeric(tmp$`Note Amount`)
      
      tmp$real_SAR <-
        ((
          tmp$`Payments Received`
          -tmp$`Principal Received`
          
          - tmp$`Principal Pending`*tmp$out_prncp_multiplier
        ) / ((tmp$`Note Amount`+tmp$`Principal Pending`)/2)+1
        ) ^ (1 / (tmp$`Loan Age(Days)` / 365)) - 1
      
      tmp$real_SAR<-ifelse(tmp$`Principal Received`==0&tmp$`Principal Pending`==0,-1,tmp$real_SAR)

      
      smmry<-sqldf("select color,xaxis
                   ,count(*) as 'Note Count'
                   ,sum(`Loan Amount`) as 'Loan Amount'
                   ,sum(`Note Amount`) as 'Note Amount'
                   ,sum(`Interest Rate`*`Note Amount`)/sum(`Note Amount`*100) as 'Weight Avg Rate'
                   ,sum(real_SAR*`Note Amount`)/sum(`Note Amount`) as 'Risk Adjusted Return'
                   ,sum(`Payments Received`) as 'Payments Received'
                   ,sum(`Principal Received`) as 'Principal Received'
                   ,sum(`Interest Received`) as 'Interest Received'
                   ,sum(`Principal Pending`) as 'Principal Pending'
                   ,sum(case when loanStatus in ('In Grace Period') or loanStatus like 'Late%' then `Principal Pending` else 0 end) as 'Principal at Risk'
                   ,sum(case when loanStatus in ('Charged Off') then `Principal Pending` else 0 end) as 'Principal Charged Off'
                   ,sum(case when loanStatus in ('In Grace Period') or loanStatus like 'Late%' then `Principal Pending` else 0 end)/sum(`Note Amount`) as 'Principal at Risk Rate'
                   ,sum(case when loanStatus in  ('Charged Off') then `Principal Pending` else 0 end)/sum(`Note Amount`) as 'Charged Off Rate'
                   ,sum(`Loan Age(Days)`*`Note Amount`)/sum(`Note Amount`) as 'Weight Avg Age(Days)'

                   from tmp
                   group by 1,2
                   ")
      rm(tmp)
      smmry$Constant<-1
      smmry$`Note Count`<-as.numeric(smmry$`Note Count`)
      smmry$`Loan Amount`<-as.numeric(smmry$`Loan Amount`)
      smmry$`Note Amount`<-as.numeric(smmry$`Note Amount`)
      smmry$`Weight Avg Rate`<-as.numeric(smmry$`Weight Avg Rate`)
      smmry$`Risk Adjusted Return`<-as.numeric(smmry$`Risk Adjusted Return`)
      smmry$`Payments Received`<-as.numeric(smmry$`Payments Received`)
      smmry$`Principal Received`<-as.numeric(smmry$`Principal Received`)
      smmry$`Interest Received`<-as.numeric(smmry$`Interest Received`)
      smmry$`Principal Pending`<-as.numeric(smmry$`Principal Pending`)
      smmry$`Principal at Risk`<-as.numeric(smmry$`Principal at Risk`)
      smmry$`Principal Charged Off`<-as.numeric(smmry$`Principal Charged Off`)
      smmry$`Principal at Risk Rate`<-as.numeric(smmry$`Principal at Risk Rate`)
      smmry$`Charged Off Rate`<-as.numeric(smmry$`Charged Off Rate`)

      smmry$`Weight Avg Age(Days)`<-as.numeric(smmry$`Weight Avg Age(Days)`)
      
      if(input$xaxis2%in%c(
        "Note Count"
        ,"Loan Amount","Note Amount","Weight Avg Rate","Risk Adjusted Return","Payments Received"
        ,"Principal Received","Interest Received","Principal Pending","Principal at Risk","Principal Charged Off"
        ,"Principal at Risk Rate","Charged Off Rate"
        ,"Weight Avg Age(Days)")){
        smmry<-smmry[,c(input$xaxis2,input$yaxis2,input$size2,"color")]
      }else{
        smmry<-smmry[,c("xaxis",input$yaxis2,input$size2,"color")]
      }
      
      names(smmry)<-c("xaxis","yaxis","size","color")
      
      p <- ggplot(smmry, aes(x=xaxis, y=yaxis))+labs(title="Customized Bubble Plot",x='',y='   ',color=input$color2,size=input$size2)
      p<-p+ geom_point(aes(col=color, size=size))
      #+theme(axis.text.y = element_text(angle=90))
      if(input$yaxis2%in%c(
        "Loan Amount","Note Amount","Payments Received"
        ,"Principal Pending","Principal Received","Interest Received","Principal at Risk","Principal Charged Off")){
        p<-p+scale_y_continuous(label=dollar_format())
        
      }else if(input$yaxis2%in%c(
        "Note Count","Weight Avg Age(Days)")){
        p<-p+scale_y_continuous(label=comma)
      }else{
        p<-p+scale_y_continuous(label=percent)
      }  
      if(input$xaxis2%in%c(
        "Loan Amount","Note Amount","Payments Received"
        ,"Principal Pending","Principal Received","Interest Received","Principal at Risk","Principal Charged Off")){
        p<-p+scale_x_continuous(label=dollar_format())
        
      }else if(input$xaxis2%in%c(
        "Note Count","Weight Avg Age(Days)")){
        p<-p+scale_x_continuous(label=comma)
      }else if(input$xaxis2%in%c(
        "Weight Avg Rate","Risk Adjusted Return","Principal at Risk Rate","Charged Off Rate")){
        p<-p+scale_x_continuous(label=percent)
      }

      
      ggplotly(p) %>% 
        layout(
          legend = list(orientation = "h",y=-0.2))
    } else{
    
      return(NULL)
    }
    
    
  })
  
  output$acct_smmry2 <- renderTable({
    if(!is.null(LC_note_detail())){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Creating LC Class Summary")
      
      tmp<-LC_note_detail()
      tmp$LoanAgeinDays<-as.integer(as.Date.character(substr(tmp$nextPaymentDate,1,10),format = "%Y-%m-%d")
                                    -as.Date.character(substr(tmp$issueDate,1,10),format = "%Y-%m-%d"))
      tmp$subgrade<-tmp$grade
      tmp$grade<-substr(tmp$subgrade,1,1)
      tmp$Vintage<-substr(tmp$issueDate,1,7)
      tmp<-tmp[,c("purpose","interestRate","loanLength","loanStatus","grade","subgrade","currentPaymentStatus"
                  ,"creditTrend","portfolioName","loanAmount","noteAmount","paymentsReceived","accruedInterest"
                  ,"principalPending","interestPending","principalReceived","interestReceived","Vintage"
                  ,"LoanAgeinDays","applicationType")]
      names(tmp)<-c("Purpose","Interest Rate","Term","Loan Status","Grade","Subgrade","Current Payment Status"
                    ,"Credit Trend","Portfolio","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                    ,"Principal Pending","Interest Pending","Principal Received","Interest Received","Vintage"
                    ,"Loan Age(Days)","Application Type")

      tmp$`Term`<-as.factor(as.character(tmp$`Term`))
      tmp$Overall<-"Overall"
      tmp<-tmp[,c(input$color2,
                  "Interest Rate","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                  ,"Principal Pending","Interest Pending","Principal Received","Interest Received"
                  ,"Loan Age(Days)")]
      names(tmp)<-c("color","Interest Rate","Loan Amount","Note Amount","Payments Received","Accrued Interest"
                    ,"Principal Pending","Interest Pending","Principal Received","Interest Received"
                    ,"Loan Age(Days)")
      tmp$loanStatus<-LC_note_detail()[,c("loanStatus")]
      tmp$out_prncp_multiplier <-
        ifelse(
          tmp$loanStatus == 'In Grace Period',
          0.31,
          ifelse(
            tmp$loanStatus == 'Late (16-30 days)',
            0.62,
            ifelse(
              tmp$loanStatus == 'Late (31-120 days)',
              0.88,
              ifelse(tmp$loanStatus == 'Default',0.72, 
                     ifelse(tmp$loanStatus == 'Charged Off',1, 0))
            )
          )
        )
      tmp$`Payments Received`<-as.numeric(tmp$`Payments Received`)
      tmp$`Principal Received`<-as.numeric(tmp$`Principal Received`)
      tmp$`Principal Pending`<-as.numeric(tmp$`Principal Pending`)
      tmp$`Note Amount`<-as.numeric(tmp$`Note Amount`)
      
      tmp$real_SAR <-
        ((
          tmp$`Payments Received`
          -tmp$`Principal Received`
          
          - tmp$`Principal Pending`*tmp$out_prncp_multiplier
        ) / ((tmp$`Note Amount`+tmp$`Principal Pending`)/2)+1
        ) ^ (1 / (tmp$`Loan Age(Days)` / 365)) - 1
      
      tmp$real_SAR<-ifelse(tmp$`Principal Received`==0&tmp$`Principal Pending`==0,-1,tmp$real_SAR)
      
      smmry<-sqldf("select color
                   ,count(*) as 'Note Count'
                   ,sum(`Loan Amount`) as 'Loan Amount'
                   ,sum(`Note Amount`) as 'Note Amount'
                   ,sum(`Interest Rate`*`Note Amount`)/sum(`Note Amount`*100) as 'Weight Avg Rate'
                   ,sum(real_SAR*`Note Amount`)/sum(`Note Amount`) as 'Risk Adjusted Return'
                   ,sum(`Payments Received`) as 'Payments Received'
                   ,sum(`Principal Received`) as 'Principal Received'
                   ,sum(`Interest Received`) as 'Interest Received'
                   ,sum(`Principal Pending`) as 'Principal Pending'
                   ,sum(case when loanStatus in ('In Grace Period') or loanStatus like 'Late%' then `Principal Pending` else 0 end) as 'Principal at Risk'
                   ,sum(case when loanStatus in ('Charged Off') then `Principal Pending` else 0 end) as 'Principal Charged Off'
                   ,sum(case when loanStatus in ('In Grace Period') or loanStatus like 'Late%' then `Principal Pending` else 0 end)/sum(`Note Amount`) as 'Principal at Risk Rate'
                   ,sum(case when loanStatus in  ('Charged Off') then `Principal Pending` else 0 end)/sum(`Note Amount`) as 'Charged Off Rate'
                   ,sum(`Loan Age(Days)`*`Note Amount`)/sum(`Note Amount`) as 'Weight Avg Age(Days)'
                   
                   from tmp
                   group by 1
                   ")
      rm(tmp)
      smmry$`Note Count`<-as.numeric(smmry$`Note Count`)
      smmry$`Note Amount`<-as.numeric(smmry$`Note Amount`)
      smmry$`Weight Avg Rate`<-as.numeric(smmry$`Weight Avg Rate`)
      smmry$`Payments Received`<-as.numeric(smmry$`Payments Received`)
      smmry$`Principal Pending`<-as.numeric(smmry$`Principal Pending`)
      smmry$`Principal Received`<-as.numeric(smmry$`Principal Received`)
      smmry$`Interest Received`<-as.numeric(smmry$`Interest Received`)
      smmry$`Weight Avg Age(Days)`<-as.numeric(smmry$`Weight Avg Age(Days)`)
      smmry$`Principal Current`<-smmry$`Principal Pending`-smmry$`Principal at Risk`-smmry$`Principal Charged Off`
      smmry$`Risk Adjusted Return`<-as.numeric(smmry$`Risk Adjusted Return`)
     
      smmry$`Principal at Risk`<-as.numeric(smmry$`Principal at Risk`)
      smmry$`Principal Charged Off`<-as.numeric(smmry$`Principal Charged Off`)
      smmry$`Principal at Risk Rate`<-as.numeric(smmry$`Principal at Risk Rate`)
      smmry$`Charged Off Rate`<-as.numeric(smmry$`Charged Off Rate`)
  
      
      
            
      smmry$`Note Count`<-comma(smmry$`Note Count`)
      smmry$`Note Amount`<-dollar(smmry$`Note Amount`)
      smmry$`Weight Avg Rate`<-
        paste(format(round(smmry$`Weight Avg Rate`*100,2),nsmall=2),"%",sep="")
      smmry$`Risk Adjusted Return`<-
        paste(format(round(smmry$`Risk Adjusted Return`*100,2),nsmall=2),"%",sep="")
      smmry$`Payments Received`<-dollar(smmry$`Payments Received`)
      smmry$`Principal Pending`<-dollar(smmry$`Principal Pending`)
      smmry$`Principal Current`<-dollar(smmry$`Principal Current`)
      smmry$`Principal at Risk`<-dollar(smmry$`Principal at Risk`)
      smmry$`Principal Charged Off`<-dollar(smmry$`Principal Charged Off`)
      smmry$`Principal Received`<-dollar(smmry$`Principal Received`)
      smmry$`Interest Received`<-dollar(smmry$`Interest Received`)
      smmry$`Weight Avg Age(Days)`<-comma(smmry$`Weight Avg Age(Days)`)
      smmry<-smmry[,c('color','Note Count','Note Amount','Weight Avg Rate','Risk Adjusted Return','Payments Received','Principal Received'
                      ,'Interest Received','Principal Pending','Principal Current','Principal at Risk','Principal Charged Off','Weight Avg Age(Days)')]
      names(smmry)<-c(input$color2,'Note Count','Note Amount','Weight Avg Rate','Risk Adjusted Return','Payments Received','Principal Received'
                      ,'Interest Received','Principal Pending','Principal Current','Principal at Risk','Principal Charged Off','Weight Avg Age(Days)')
      smmry

    } else{
      
      return(NULL)
    }
    
    
  }, caption = "Summary by Class",
  caption.placement = getOption("xtable.caption.placement", "top"), 
  caption.width = getOption("xtable.caption.width", NULL))
  
  output$downloadData <- downloadHandler(
    
    filename = function() {
      "summary.csv"
    },
    content = function(file) {
      write.csv(dataInput(), file, row.names = FALSE)
    }
  )

  
  session$onSessionEnded(stopApp)
}

# Run the application 
shinyApp(ui = ui, server = server)