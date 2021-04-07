library('shiny')
library('DT')
library('shinythemes')
library('lubridate')
library('dplyr')
library('ggplot2')
library('highcharter')
library('d3heatmap')
library('tableHTML')
library('lmtest')
library('car')
library('nortest')
library('Matching')
library('pROC')
library('plotROC')
library('plotly')
library('SDMTools')
library('readr')
library('data.table')
options(shiny.maxRequestSize = 100000*1024^2)
shinyApp(
  ui = 
    tagList(
    shinythemes::themeSelector(),
    navbarPage(
      
       theme = shinytheme("united"),  # <--- To use a theme, uncomment this
      "Model Validation", fluid = TRUE,
      tabPanel("WORKING WITH THE DATA",
               
               sidebarPanel(
                 fileInput("file1", "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")), width = 3
                 
                 
                 
               ),
               mainPanel(
                 tabsetPanel(
                   tabPanel("DATA HANDLING",
                            mainPanel(
                              actionButton('action200','See Subsets'),
                            h5(strong("This datatable is editable. Just double click on a cell to edit it. Also, one could filter each column based on range of values one would like to see and download those filtered datatables by clicking on CSV, one could also alter column visibility, the plus sign can be used to expand the rows so that the values in the columns that not visible can be viewed. One could select rows that would paste the row numbers selected and create a new datatable below with the selected rows. This datatable can also be downloaded by clicking on csv above it. You could use this subset for OOS by reading it as the main file. This subset is not visible till you select rows")),
                            #actionButton("add_dummy_row","Add new dummy row"),
                            DT::dataTableOutput("training_dataset"),
                            uiOutput("selected_rows_UI"),
                            h4('Training Data'),
                            DT::dataTableOutput('subt'),
                            h4('Testing Data'),
                            DT::dataTableOutput('subte'))
                            
                   )
                   
                 )
               )
      ),
      navbarMenu("MODEL FIT & TESTS",
               
                 tabPanel('Linear Regression',
                          navlistPanel('LINEAR REGRESSION TESTS',
                                       navbarMenu('Model Fit',
                                                  tabPanel('Main Model Fit',
                                                sidebarPanel(
                                                    
                                                  uiOutput("Dependent"),
                                                  
                                                  uiOutput('columns'),
                                                  uiOutput('ui.action'),
                                                checkboxInput('Intercept','Intercept',TRUE)
                                               
                                                
                                                ),
                                                mainPanel(
                                                  h4(strong('Estimates')),
                                                  verbatimTextOutput('txtout'),
                                                  h4(strong('Actual vs Predicted')),
                                                  highchartOutput('avsp')
                                                )),
                                                tabPanel('OOS version 1',
                                                         fluidRow(column(8,
                                                                         verbatimTextOutput('train_out_main')),
                                                                  column(4,
                                                                         h5('actual and predicted for test data'),
                                                                         dataTableOutput('compare_main')   )),
                                                         
                                                         
                                                         fluidRow(
                                                           column(width=4,
                                                                  #        tableOutput('compare'),
                                                                  tableOutput('RMSE_main'))
                                                           
                                                         )
                                                )),
                                       navbarMenu('Variable Plots and OOS',
                                                  tabPanel('Time series plots if required',
                                                mainPanel(
                                                  checkboxInput('need','Are time series plots required?',FALSE),
                                                  uiOutput('varlist'),
                                                  
                                                  highchartOutput('vtsplots')
                                                )),
                                                tabPanel('Scatter plots plus subsetting',
                                                mainPanel(
                                                  fluidRow(column(4,
                                                  uiOutput('pred1')),
                                                  column(4,
                                                         uiOutput('pred2'))),
                                                  actionButton('action4','See Subsets'),
                                                  plotOutput("plot1", brush = "plot_brush"),
                                                  
                                                  h4('Training Data'),
                                                  dataTableOutput('Trainingdata'),
                                                  h4('Testing Data'),
                                                  dataTableOutput('Testingdata')
                                                  
                                                  
                                                  
                                                )),
                                                tabPanel('OOS',
                                                         #actionButton('action1','Press'),
                                                         fluidRow(column(8,
                                                                  verbatimTextOutput('train_out')),
                                                                  column(4,
                                                                         h5('actual and predicted for test data'),
                                                                         dataTableOutput('compare')   )),
                                                                  
                                                                  
                                                           fluidRow(
                                                              column(width=4,
                                                           #        tableOutput('compare'),
                                                                  tableOutput('RMSE'))
                                                  
                                                           )
                                                )
                                                           
                                                        ),
                                                           
                                              
                                                           
                                                         
                                       tabPanel("Linearity/Additivity Check",
                                                fluidRow(column(4,offset=4,h3("Ramsey's Reset Test"))),
                                                actionButton('action60','See Results'),
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(3,offset=1,
                                                                textInput('mp','Max Power')),
                                                         column(3,
                                                                selectInput('type','type',choices = c('','fitted','regressor','princomp'),selected = '')),
                                                         column(3,textInput('los1','Level of Significance'))),
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(4,offset=4,
                                                tableHTML_output('reset'))),
                                                fluidRow(column(12,h1(''))),
                                                h5(strong('For graphical assessment of linearity, see the actual vs predicted plot in the Model Fit tab. Also, one could check linearity from the residuals vs predicted and the scale location plots in the heteroskedasticity tab. Another point to note is that this test is carried out as a two tailed test. See the note in the autocorrelation tab'))
                                                ),
                                       navbarMenu('Autocorrelation Check',
                                              tabPanel('autocorrelation tests',
                                              
                                                          fluidRow(
                                                            column(3,
                                                                   actionButton('action10','See Results'))),
                                                            # column(3,
                                                            #        actionButton('dl','Download Table'))),
                                                       fluidRow(column(12,h1(''))),
                                                       fluidRow(column(3, offset=2,
                                                                       tableHTML_output('tre'))),
                                                          # fluidRow(
                                                          #   column(6,
                                                          #          plotOutput('acf')),
                                                          #   column(6,
                                                          #          plotOutput('pacf'))
                                                          # ),
                                                       fluidRow(column(12,h1(''))),
                                                          fluidRow(
                                                            column(4,
                                                                   checkboxInput('met1','Ljung-Box',FALSE),
                                                                   checkboxInput('met2','Durbin-Watson',FALSE),
                                                                   checkboxInput('met3','Breusch-Godfrey',FALSE)),
                                                            column(4,
                                                                   textInput('t1','Lag Order'),
                                                                   textInput('t2','Level of Significance')),
                                                            column(4,
                                                                   checkboxInput('te1','One-tailed',FALSE),
                                                                   checkboxInput('te2','Two-tailed',FALSE))
                                                          ),
                                                      fluidRow(column(12,h1(''))),
                                                      h5(strong('Note: From what I understand, all the hypothesis tests perform a general two-tailed test. There are a few like Durbin Watson and Bootstap KS that include an argument to provide the alternative as two sided or greater or less. The p-value is nothing but the area under the probability distribution from one extreme of the curve upto the test statistic obtained with respect to the sample. For tests that have an optional argument for alternative hypothesis, if we put the alternative as greater, it measures area from the right and for less, it measures it from left. Therefore, suppose alternate is greater and we set the significance level to 0.05, if we get a p-value of greater than 0.95, though this is greater than 0.05, it actually indicates that the null is rejected when the alternative is less. The greater and less are complimentary. Therefore, I have only implemented for greater, but assigned a red flag even when the p-value is greater than 1-los. This would be clearer when you see results of one-sided Bootstrap KS. When the alternative is chosen to be two-sided, the p-value returned is the lower of the p-values obtained from the greater alternative and the less alternative. Therefore, it only indicates that the null hypothesis is rejected, but not from which side. Usually, our interest is only two-sided. For example, I am just interested in significant autocorrelation rather than positive or negative autocorrelation.'))
                                                          ),
                                              tabPanel('ACF and PACF plots',
                                                       fluidRow(column(3,
                                                        actionButton('action20','See Plots'))),
                                                       fluidRow(column(4,
                                                                       uiOutput('var2')),
                                                         column(4,              
                                                       textInput('lag','Max Lag')),
                                                       column(4,textInput('ci','CI'))),
                                                       fluidRow(column(12,h1(''))),
                                                       h4(strong('ACF plot')),
                                                       plotOutput('acf'),
                                                       fluidRow(column(12,h1(''))),
                                                       h4(strong('PACF plot')),
                                                       plotOutput('pacf'))),

                                                
                                       tabPanel('Heteroskedasticity Check',
                                                fluidRow(
                                                  column(3,
                                                         actionButton('action30','See Results'))),
                                                
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(3, offset=3,
                                                                tableHTML_output('tre1'))),
                                                
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(
                                                  column(4,offset=2,
                                                         checkboxInput('met11','Breusch-Pagan',FALSE),
                                                         checkboxInput('met21','Non Constant Variance Score',FALSE)),
                                                  
                                                  column(4,
                                                         #textInput('t1','Lag Order'),
                                                         textInput('t21','Level of Significance'),
                                                         
                                                         checkboxInput('te11','One-tailed',FALSE),
                                                         checkboxInput('te21','Two-tailed',FALSE))
                                                ),
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(6,
                                                             h4(strong('Residuals(Y) vs Predicted(X)')),   
                                                         highchartOutput('rvsf')),
                                                         column(6,
                                                                h4(strong('Scale Location')),      
                                                         highchartOutput('srvsf')))),
                                                
                                       tabPanel('Multicollinearity Check',
                                                #tabPanel('Correlations',
                                                fluidRow(column(3,
                                                  actionButton('action5','See Correlations and VIFs')),
                                                  column(3,textInput('th','VIF threshold'))),
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(8,
                                                                
                                                d3heatmapOutput('heatmap')),
                                                column(4,tableHTML_output('vif'))),
                                                fluidRow(column(12,h1(''))),
                                                dataTableOutput('corr')),
                                       tabPanel('Normality Check',
                                                uiOutput('var10'),
                                                h4(strong('QQ plot')),
                                                highchartOutput('qqplot'),
                                                fluidRow(column(12,h1(''))),
                                                actionButton('action50','See Results'),
                                                fluidRow(column(12,h1(''))),
                                                fluidRow(column(4,offset=4,tableHTML_output('norm'))),
                                                fluidRow(
                                                  column(3,offset=2,
                                                         checkboxInput('nor1','Anderson Darling',FALSE),
                                                         checkboxInput('nor2','Shapiro Wilk',FALSE),
                                                         checkboxInput('nor3','Bootstrap KS',FALSE)),

                                                  column(3,
                                                         #textInput('t1','Lag Order'),
                                                         textInput('los','Level of Significance'),

                                                         checkboxInput('ot','One-tailed',FALSE),
                                                         checkboxInput('tt','Two-tailed',FALSE)),
                                                  column(3,
                                                         textInput('nb','# bootstraps for KS'))
                                                ))
                                                ,
                                                widths = c(3,9)
                                       )),
                 tabPanel('Logistic Regression',
                          navlistPanel('LOGISTIC REGRESSION TESTS',
                                       navbarMenu('Model Fit',
                                                tabPanel('Estimates and ROC',
                                                         
                                                         sidebarPanel(
                                                           
                                                           uiOutput("loDependent"),
                                                           
                                                           uiOutput('locolumns'),
                                                           uiOutput('loui.action'),
                                                           checkboxInput('loIntercept','Intercept',TRUE)
                                                           
                                                           
                                                         ),
                                                         mainPanel(
                                                           h4(strong('Estimates')),
                                                           verbatimTextOutput('lotxtout'),
                                                           h4(strong('Receiver Operating Characteristic')),
                                                           plotlyOutput('roc')
                                                         ))
                                                 ,
                                                  tabPanel('Performance',
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(6,offset=3,
                                                                          tableHTML_output('com'))),
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(12,h1(''))),
                                                          fluidRow(column(6,offset=3,
                                                                          tableHTML_output('per'))),
                                                          fluidRow(column(12,h1(''))),
                                                          h5(strong('This tab is exclusively for confusion matrix and metrics calculated using that. For now, I have not added other dignostic tests for logistic regression. For logistic, many of the assumptions of OLS do not hold true. For example, normality and heteroskedasticity are not assumptions of logistic. Autocorrelation and Multicollinearity checks are important though. I would be creating similar tabs to the ones in linear regression for the assumptions that hold true for logistic and also for variable plots and OOS. Since that would just require some tweaking of the code that I have written for linear regression, it should not take much effort. '))
                                                # )
                                                )),
                                       # tabPanel('Variable Plots'),
                                       # tabPanel("AUC/ROC"),
                                       # tabPanel('Residual autocorrelation check'),
                                       # tabPanel('Multicollinearity Check'),
                                       widths=c(3,9)))
      ),
               
               
      tabPanel("SOME INTERESTING STUFF", 
               h5(strong('This tab is for adding links or images of anything in risk management that is interesting ')),
               fluidRow(column(12,h1(''))),
               uiOutput('tab1'),
               
               uiOutput('tab2')),
      tabPanel('INSTRUCTIONS/TIPS',
               h5("I created this tab to actually provide some kind of a tutorial on building shiny apps and creating some interesting visualizations using htmlwidgets. I still have to figure out how I can do that. Briefly, creating an app involves two major elements, the UI and the server. Through UI, you alter the appearance of the app in terms of where what should be placed. Server is where all the computations occur. 
                         Creating a Shiny app was a great experience and I learnt a lot in the process. Python's dash which is known to be Python's
shiny is also worth trying. I will definitely try doing something using dash as well
                        "),
br(),

h5('Prerequisites to build a Shiny app:'),

h5("It is all about practice. One needs to start somewhere. I had never built an app before this. Coding for building an app is somewhat different from conventional programming. You need to understand reactivity and render functions. The more you do, the better you understand. 
You could have a look at the shiny apps that are present on the shiny gallery of the R studio website. I really got inspired seeing some of them.I will add the link in the tab 'Some interesting stuff'. From conventional programming, I would say that it would be helpful if you know to write functions and have some knowledge of the common data structures.
 "),
br(),

h5('Why Shiny?'),

h5('Typically a web application requires you to write HTML codes that incorporate CSS for styling and JavaScript for making it interactive. Shiny or Dash (Python) save
you from too much of that effort. However, you may need to use CSS at times to style your app the way you want, but honestly, that is not a must, plus HTML and CSS
are not that difficult to pick up. The interactivity, as I have already mentioned comes through what is called reactivity and render functions. You will see that most
of my plots are also interactive. Now that is facilitated by what we call as htmlwidgets.'),
br(),
h5('What are htmlwidgets?'),

h5('htmlwidgets are'),

h5('1. a framework that binds JavaScript with a programming language like R or Python'),

h5('2. a collection of packages actually written using JavaScript, supported on R, which help you build interactive plots, charts or maps.
Apart from the javascript libraries I have used, there are some other wonderful javacript libraries, each best in their own way. Eg: leaflet in R,
folium in python, vis.js in R, Bokeh in python, many of which I am yet to work with.'),
br(),

h5('Things to keep in mind while using this app'),
h5('1.The tabs are linked to one another, this is obvious, but I still wanted to highlight. Of course, you cannot run the tests unless you have residuals which are obtained only if you specify the independent and dependent variables. In case at any point, you 
                         do not want to check the estimates, but just want a list of variables and residuals to test on, you need not press the action button for estimates to save time and memory, but you would need to enter the independent and dependent variables anyway.
This is done by something called isolate, which stops the reactivity based on changing input till the user presses an action button. This is very common, you can relate to this with your experiences when you fill forms and it updates only when you click on an update button.
Imagine, if it kept rendering as and when you typed something. That would make your app incredibly slow.'),
h5("2.The time series plots are optional. If you want to create time series plots, firstly you need to have a Date column in the file you upload (this is obvious) and that needs to have the name 'Date' only (not Dates or dates). The format of the dates need to be dd-mm-yyyy. Reason: when you read a file, it detects this date column as a character variable or a factor variable and for
                         a plot to understand that this denotes dates, you need to convert it into a Date object. I still need to make this work for other date formats like Q12017 or Q417 or 31st Jan 2017. For now, one could just change the format through excel and since the interval for subsequent observations is fixed in our models, it should not be a great effort."),
h5("It is possible that sometimes when you upload the data, you see an error in the subsequent tabs like 'argument length 0' or 'missing value TRUE/FALSE needed'. I have indeed added conditional statements for error handling, so it is much better now than what it used to be when I had just created the app without the conditions. I think I need to make the error handling process even better. However, this does not interfere with the working of the app. Once the user provides his/her inputs, these small error messages do not show up and you get your results "),
h5('3.The subsets in variable plots are created by visually choosing points on the gg scatter plot. I found this very interesting.'),
br(),
h5('I will keep adding points to this page whenever I feel something might be an issue while running this app. You can always reach out to me :)')

    
  ))),
  server = function(input, output, session) {
     filedata <- reactive({
       infile <- input$file1
       if(is.null(infile))
         return(NULL)
       #read.csv(infile$datapath, header = TRUE)
       #readr::read_delim(infile$datapath,delim = ',')
       fread(infile$datapath,data.table = FALSE)
     })

    output$training_dataset = DT::renderDataTable(filedata(),rownames = FALSE,
                                                  #server = FALSE, ## Required for addRow to work
                                                  extensions = c("Responsive", "Buttons",'KeyTable'),
                                                  options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
                                                  filter = list(position = 'top'), editable = TRUE, selection = list(target ='row+column'))



    output$selected_rows_UI = renderUI({
      if (is.null(input$training_dataset_rows_selected)) {
        "No rows selected yet"
      } else {
        print(paste(
          "Selected rows",
          paste(input$training_dataset_rows_selected, collapse = ", ")
        ))

        paste("Selected rows",
              paste(input$training_dataset_rows_selected, collapse = ", "))

      }
})

    subt1 <- reactive({
      md<-filedata()
      if(is.null(md))return(NULL)
      if(is.null(input$training_dataset_rows_selected))return(NULL)
      md[-input$training_dataset_rows_selected,]
    })
    output$subt = DT::renderDataTable({
      input$action200
      isolate({
      md<-filedata()
      if(is.null(md))return(NULL)
      if(is.null(input$training_dataset_rows_selected))return(NULL)
      DT::datatable(
        subt1(),rownames=FALSE,
        extensions = c('Responsive','Buttons','KeyTable'),
        options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
        filter = list(position = 'top'), editable = TRUE)
      
    })},server = FALSE)
    
    subte1 <- reactive({
      md<-filedata()
      if(is.null(md))return(NULL)
      if(is.null(input$training_dataset_rows_selected))return(NULL)
      md[input$training_dataset_rows_selected,]
    })
    
    output$subte = DT::renderDataTable({
      input$action200
      isolate({
        md<-filedata()
        if(is.null(md))return(NULL)
        if(is.null(input$training_dataset_rows_selected))return(NULL)
        DT::datatable(
          subte1(),rownames=FALSE,
          extensions = c('Responsive','Buttons','KeyTable'),
          options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
          filter = list(position = 'top'), editable = TRUE)
        
      })},server = FALSE)
    
    output$Trainingdata = DT::renderDataTable(
      {
        input$action4
        isolate({
          md<-filedata()
          if(is.null(md))return(NULL)
          if(is.null(td()))return(NULL)
          DT::datatable(
            td(),rownames=FALSE,
            extensions = c('Responsive','Buttons','KeyTable'),
            options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
            filter = list(position = 'top'), editable = TRUE)
          
        })},server = FALSE)
    output$Dependent <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("Dependent","Select ONE variable as dependent variable from:",c('',items))
    })

    output$loDependent <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("loDependent","Select ONE response variable from:",c('',items))
    })
    output$varlist <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("varlist","variable to plot",c('',items))
    })

    output$pred1 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("pred1","choose a variable X",c('',items))
    })

    output$pred2 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("pred2","choose a variable Y",c('',items))
    })

    output$var2 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("var2","Choose a variable",c('','Residuals',items))
    })
    
    output$var10 <- renderUI({
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectInput("var10","Choose a variable",c('','Residuals',items))
    })
    
    cr <- reactive({

      df <- filedata()
      if (is.null(df)) return(NULL)
      df<- df[sapply(df,is.numeric)]
      round(cor(df),5)
    })


    output$heatmap <- renderD3heatmap({
      input$action5
      isolate({
      if(is.null(cr()))return(NULL)
      d3heatmap(cr(),cellnote = cr())

    })
    })
    output$corr <- DT::renderDataTable({
      input$action5
      isolate({
        if(is.null(cr()))return(NULL)
      DT::datatable(cr(),


      extensions = c("Responsive", "Buttons",'KeyTable'),
      options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, pageLength = 5, scrollX=TRUE, scrollY=TRUE))

      })},server=FALSE)




    hc <- reactive({
      df<- filedata()
      if (is.null(df)) return(NULL)
      df <- df %>%
        mutate(Date = dmy(Date))
      if(input$varlist=='')return(NULL)

       highchart(type='stock')%>%
         #iv = as.name(input$varlist)
         hc_add_series_times_values(
           dates = df[,'Date'],
           values = df[,input$varlist],name = input$varlist,
           color = 'green'
         )%>%

         hc_legend(enabled=TRUE)
    })

    output$vtsplots <- renderHighchart({
      if(input$need==FALSE)return(NULL)
    
      hc()
    })

    output$plot1 <- renderPlot({
      df<- filedata()
      if (is.null(df)) return(NULL)
      # df <- df %>%
      #   mutate(Date = dmy(Date))

      if(input$pred1==''||input$pred2=='')return(NULL)
      #s = ggplot(df, aes_string(x=df[,input$pred1], y=df[,input$pred2])) + geom_point(color='darkblue')
      s=ggplot(data = df) +
        geom_point(aes_string(x = input$pred1, y = input$pred2),color = 'blue')
      s1 = s + labs(x=input$pred1)
      s1+labs(y=input$pred2)
    })


    output$acf <- renderPlot({
      input$action20
      isolate({
      df <- filedata()
      if(is.null(df))return(NULL)
      if(input$var2=='')return(NULL)
      if(input$lag=='')return(NULL)
      if(input$ci=='')return(NULL)
      ciline <- qnorm((1 - as.numeric(input$ci))/2)/sqrt(nrow(df))
      if(input$var2!='Residuals'){
      bacf = acf(df[,input$var2],plot=FALSE,lag.max=as.numeric(input$lag))
      bacfdf = with(bacf,data.frame(lag,acf))
      
      }
      if(input$var2=='Residuals'){
        if(input$Dependent=='')return(NULL)
        if(length(input$columns)==0)return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          fit = lm(fmla,data=df)
        }
        else{
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
          fit = lm(fmla,data=df)
        }
        bacf = acf(fit$residuals,plot=FALSE,lag.max=as.numeric(input$lag))
        bacfdf = with(bacf,data.frame(lag,acf))
      }
        q = ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
          geom_hline(aes(yintercept = ciline,color='darkblue')) +geom_hline(aes(yintercept = -ciline, color = 'darkblue'))+
          geom_segment(mapping = aes(xend = lag, yend = 0,color='darkred'))
        return(q)
      
    })
})
    
    output$pacf <- renderPlot({
      input$action20
      isolate({
        df <- filedata()
        if(is.null(df))return(NULL)
        if(input$var2=='')return(NULL)
        if(input$lag=='')return(NULL)
        if(input$ci=='')return(NULL)
        ciline <- qnorm((1 - as.numeric(input$ci))/2)/sqrt(nrow(df))
        if(input$var2!='Residuals'){
          bacf = pacf(df[,input$var2],plot=FALSE,lag.max=as.numeric(input$lag))
          bacfdf = with(bacf,data.frame(lag,acf))
          
        }
        if(input$var2=='Residuals'){
          if(input$Dependent=='')return(NULL)
          if(length(input$columns)==0)return(NULL)
          if(input$Intercept==FALSE){
            fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
            fit = lm(fmla,data=df)
          }
          else{
            fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
            fit = lm(fmla,data=df)
          }
          bacf = pacf(fit$residuals,plot=FALSE,lag.max=as.numeric(input$lag))
          bacfdf = with(bacf,data.frame(lag,acf))
        }
        q = ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
          geom_hline(aes(yintercept = ciline,color='darkblue')) +geom_hline(aes(yintercept = -ciline, color = 'darkblue'))+
          geom_segment(mapping = aes(xend = lag, yend = 0,color='darkred'))
        return(q)
      })
    })
    
    output$roc <- renderPlotly({
      input$action100
      isolate({
        df <- filedata()
        if(is.null(df))return(NULL)
        if(input$loDependent=='')return(NULL)
        if(input$locolumns=='')return(NULL)
        if(input$loIntercept==FALSE){
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(c(0,input$locolumns),collapse="+")))
          fit = glm(fmla,data=df,family='binomial')
        }
        else{
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(input$locolumns,collapse="+")))
          fit = glm(fmla,data=df,family = 'binomial')
        }
        preds = predict(fit)
        roc=roc(df[,input$loDependent]~preds,data=df)
        broc = with(roc,data.frame(specificities,sensitivities))
        broc[,'specificities']=1-broc[,'specificities']
        q = ggplot(data=broc,mapping=aes(x=specificities,y=sensitivities))+
          geom_line(color='darkred')+
          style_roc(xlab='False Positive Rates (1-specificity)',
                    ylab='True Positive Rates (sensitivity)',theme=theme_grey)
        q1 = ggplotly(q)
        q1
      })
    })
    
    output$com <- render_tableHTML({
      input$action100
      isolate({
        df <- filedata()
        if(is.null(df))return(NULL)
        if(input$loDependent=='')return(NULL)
        if(input$locolumns=='')return(NULL)
        if(input$loIntercept==FALSE){
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(c(0,input$locolumns),collapse="+")))
          fit = glm(fmla,data=df,family='binomial')
        }
        else{
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(input$locolumns,collapse="+")))
          fit = glm(fmla,data=df,family = 'binomial')
        }
        cm= confusion.matrix(obs=df[,input$loDependent],pred= fit$fitted.values,threshold=0.5)
        conm = matrix(ncol=2,nrow=2)
        colnames(conm)=c('Positive','Negative')
        rownames(conm)=c('Positive','Negative')
        conm[1,1]=cm[1,1]
        conm[1,2] = cm[1,2]
        conm[2,1] = cm[2,1]
        conm[2,2]=cm[2,2]
        out=tableHTML(conm
                  
                  ,row_groups = list(c(2), c( 'Model')),second_headers = list(c(0,0,2),c('','','target')),widths=rep(100,4),caption='Confusion Matrix')%>%add_theme('colorize')%>%add_css_caption(css = list(c('color', 'font-size','text-align'), c('blue', '20px','centre')))
        out
  
      })
    })
    
    output$per <- render_tableHTML({
      input$action100
      isolate({
        df <- filedata()
        if(is.null(df))return(NULL)
        if(input$loDependent=='')return(NULL)
        if(input$locolumns=='')return(NULL)
        if(input$loIntercept==FALSE){
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(c(0,input$locolumns),collapse="+")))
          fit = glm(fmla,data=df,family='binomial')
        }
        else{
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(input$locolumns,collapse="+")))
          fit = glm(fmla,data=df,family = 'binomial')
        }
        cm= confusion.matrix(obs=df[,input$loDependent],pred= fit$fitted.values,threshold=0.5)
        pm = matrix(nrow=5,ncol=2)
        colnames(pm)<-c('Metric Name','Values')
        pm_names= c('Sensitivity','Specificity','Positive Predictive Value','Negative Predictive Value','Accuracy')
        pm[,1]=pm_names
        pm[1,2]=round(cm[1,1]/(cm[1,1]+cm[2,1]),5)
        pm[2,2]=round(cm[2,2]/(cm[1,2]+cm[2,2]),5)
        pm[3,2]=round(cm[1,1]/(cm[1,1]+cm[1,2]),5)
        pm[4,2]=round(cm[2,2]/(cm[2,1]+cm[2,2]),5)
        pm[5,2]=round((cm[2,2]+cm[1,1])/(cm[2,1]+cm[2,2]+cm[1,1]+cm[1,2]),5)
        
        out=tableHTML(pm,rownames=FALSE,widths=rep(200,2),caption='Performance Metrics')%>%add_theme('rshiny-blue')%>%
        add_css_caption(css = list(c('color', 'font-size','text-align'), c('blue', '20px','centre')))
        
        out
      })
    })
    
    brushed <- reactive({
      #input$action1
      #isolate({
      df<- filedata()

      if (is.null(df))
        return(NULL)


      # df <- df %>%
      #   mutate(Date = dmy(Date))

      if(input$pred1==''||input$pred2=='')return(NULL)


      brushedPoints(df, input$plot_brush, allRows = TRUE)

    #})
    })

    td = reactive({

      if(is.null(brushed)){return(NULL)}

      brushed()[brushed()$selected,]})

    ted = reactive({

      if(is.null(brushed)){return(NULL)}

      brushed()[!brushed()$selected,]
    })

    output$Trainingdata = DT::renderDataTable(
      {
        input$action4
        isolate({
      md<-filedata()
      if(is.null(md))return(NULL)
      if(is.null(td()))return(NULL)
        DT::datatable(
      td(),rownames=FALSE,
                                  extensions = c('Responsive','Buttons','KeyTable'),
                                  options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
                                  filter = list(position = 'top'), editable = TRUE)

      })},server = FALSE)


  output$Testingdata = DT::renderDataTable(
    {
      input$action4
      isolate({
    md<-filedata()
    if(is.null(md))return(NULL)
    if(is.null(ted()))return(NULL)

    DT::datatable(
      ted(),rownames=FALSE,
                                              extensions = c('Responsive','Buttons','KeyTable'),
                                              options = list(dom = 'Bfrtip', buttons = c('colvis', "csv"),keys = TRUE, scrollX=TRUE),
                                              filter = list(position = 'top'), editable = TRUE)

    })},server=FALSE)



    output$columns = renderUI({

      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectizeInput("columns","Select Independent Variables:",items,multiple=TRUE)
    })

    output$locolumns = renderUI({
      
      df <- filedata()
      if (is.null(df)) return(NULL)
      items=names(df)
      names(items)=items
      selectizeInput("locolumns","Select Independent Variables:",items,multiple=TRUE)
    })


    output$txtout <- renderPrint({
      #if(input$action==0)return(NULL)
      input$action
      isolate({
        df <- filedata()
        if (is.null(df)) return(NULL)
        if(input$Dependent=='') return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          summary(lm(fmla,data=df))
        }
        else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        summary(lm(fmla,data=df))
        }
      })
    })
    
    output$lotxtout <- renderPrint({
      #if(input$action==0)return(NULL)
      input$action100
      isolate({
        df <- filedata()
        if (is.null(df)) return(NULL)
        if(input$loDependent=='') return(NULL)
        if(input$loIntercept==FALSE){
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(c(0,input$locolumns),collapse="+")))
          summary(glm(fmla,data=df,family='binomial'))
        }
        else{
          fmla <- as.formula(paste(input$loDependent," ~ ",paste(input$locolumns,collapse="+")))
          summary(glm(fmla,data=df,family = 'binomial'))
        }
      })
    })

    output$train_out <- renderPrint({

        md <- filedata()
        if(is.null(md))return(NULL)
        df <- td()
        if (is.null(df)) return(NULL)

        if(input$Dependent=='') return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          summary(lm(fmla,data=df))
        }
        else{
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
          summary(lm(fmla,data=df))
        }
      #})
    })

    output$train_out_main <- renderPrint({

      md <- filedata()
      if(is.null(md))return(NULL)

      if (is.null(subt1())) return(NULL)

      if(input$Dependent=='') return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        summary(lm(fmla,data=subt1()))
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        summary(lm(fmla,data=subt1()))
      }
      #})
    })
    tr = reactive({
      df = filedata()
      if(is.null(df))return(NULL)
      if(length(input$columns)==0)return(NULL)
      if(input$Dependent=='')return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      if(input$met1==TRUE){
        if(input$te1==TRUE){return(h6(strong('you need to select two-tailed. See the note below')))}
      testout1 = matrix(nrow=length(input$columns)+1,ncol=2)
      rownames(testout1)<-c(input$columns,'Residuals')
      colnames(testout1)<-c('statistic','p_value')
      for(i in input$columns){
        testout1[i,'p_value'] = Box.test(df[,i],type='Ljung-Box',lag=as.numeric(input$t1))$p.value
        testout1[i,'statistic']=Box.test(df[,i],type='Ljung-Box',lag=as.numeric(input$t1))$statistic
        }
      # if(input$Intercept==FALSE){
      #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
      #   fit = lm(fmla,data=df)
      # }
      # else{
      #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
      #   fit = lm(fmla,data=df)
      # }
      testout1['Residuals','p_value']=Box.test(fit$residuals,type='Ljung-Box',lag = as.numeric(input$t1))$p.value
      testout1['Residuals','statistic']=Box.test(fit$residuals,type='Ljung-Box',lag = as.numeric(input$t1))$statistic
      if(input$te2==TRUE){
        testout1 = tableHTML(testout1,round=5)%>%
          add_css_conditional_column(conditional='<',value=as.numeric(input$t2), css=list('background-color',
                                                                                          'red'),columns=
                                       c('p_value'))
        # %>%
        #   tableHTML_to_image()
      }
      # if(input$te2==TRUE){
      #   testout1 = tableHTML(testout1,round=5)%>%
      #     add_css_conditional_column(conditional='<',value=(as.numeric(input$t2))/2, css=list('background-color',
      #                                                                                         'red'),columns=
      #                                  c('p_value'))%>%
      #     add_css_conditional_column(conditional='>',value=1-((as.numeric(input$t2))/2), css=list('background-color',
      #                                                                                             'red'),columns=
      #                                  c('p_value'))
        # %>%
        #   tableHTML_to_image()
      return(testout1)
      }
      
      
      
      if(input$met2==TRUE){
        testout1 = matrix(nrow=1,ncol=2)
        rownames(testout1)<-c('Residuals')
        colnames(testout1)<-c('statistic','p_value') 
        
      if(input$te1==TRUE){
        testout1['Residuals','p_value']=dwtest(fmla,alternative='greater',data=df)$p.value
        testout1['Residuals','statistic']=dwtest(fmla,alternative='greater',data=df)$statistic
        testout1 = tableHTML(testout1,round=5)%>%
          add_css_conditional_column(conditional='<',value=as.numeric(input$t2), css=list('background-color',
                                                                              'red'),columns=
                                       c('p_value'))%>%
          add_css_conditional_column(conditional='>',value=1-(as.numeric(input$t2)), css=list('background-color',
                                                                                                  'red'),columns=
                                       c('p_value'))
        # %>%
        #   tableHTML_to_image()
      }
      if(input$te2==TRUE){
        testout1['Residuals','p_value']=dwtest(fmla,alternative='two.sided',data=df)$p.value
        testout1['Residuals','statistic']=dwtest(fmla,alternative='two.sided',data=df)$statistic
        testout1 = tableHTML(testout1,round=5)%>%
          add_css_conditional_column(conditional='<',value=as.numeric(input$t2), css=list('background-color',
                                                                                  'red'),columns=
                                       c('p_value'))
        # %>%
        #   add_css_conditional_column(conditional='>',value=1-((as.numeric(input$t2))/2), css=list('background-color',
        #                                                                               'red'),columns=
        #                                c('p_value'))
        # %>%
        #   tableHTML_to_image()
      }
      return(testout1)
      }
      if(input$met3==TRUE){
        if(input$te1==TRUE){return(h6(strong('Please select two-tailed. See the note below')))}
        testout1 = matrix(nrow=1,ncol=2)
        rownames(testout1)<-c('Residuals')
        colnames(testout1)<-c('statistic','p_value') 
        testout1['Residuals','p_value']=bgtest(fmla,type='Chisq',order = as.numeric(input$t1),data=df)$p.value
        testout1['Residuals','statistic']=bgtest(fmla,type='Chisq',order = as.numeric(input$t1),data=df)$statistic
        if(input$te2==TRUE){
          
          testout1 = tableHTML(testout1,round=5)%>%
            add_css_conditional_column(conditional='<',value=as.numeric(input$t2), css=list('background-color',
                                                                                            'red'),columns=
                                         c('p_value'))
          # %>%
          #   tableHTML_to_image()
        }
        # if(input$te1==TRUE){
        #   
        #   testout1 = tableHTML(testout1,round=5)%>%
        #     add_css_conditional_column(conditional='<',value=as.numeric(input$t2), css=list('background-color',
        #                                                                                         'red'),columns=
        #                                  c('p_value'))%>%
        #     add_css_conditional_column(conditional='>',value=1-((as.numeric(input$t2))/2), css=list('background-color',
        #                                                                                             'red'),columns=
        #                                  c('p_value'))
          # %>%
          #   tableHTML_to_image()
        
        return(testout1)
      }
      })

    output$tre = render_tableHTML({
      input$action10
      isolate({
      tr()


    })
    })
    
    tr1 = reactive({
      df = filedata()
      if(is.null(df))return(NULL)
      if(length(input$columns)==0)return(NULL)
      if(input$Dependent=='')return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      if(input$met11==TRUE){
        if(input$te11==TRUE){return(h6('Please select two-tailed. See the note in the autocorrelation tab'))}
        testout1 = matrix(nrow=1,ncol=2)
        colnames(testout1)<-c('statistic','p_value')
        bp = bptest(fmla,data=df)
        for(i in input$columns){
          testout1[1,'p_value'] = bp$p.value
          testout1[1,'statistic']=bp$statistic
        }
        if(input$te21==TRUE){
          testout1 = tableHTML(testout1,round=5)%>%
            add_css_conditional_column(conditional='<',value=as.numeric(input$t21), css=list('background-color',
                                                                                            'red'),columns=
                                         c('p_value'))
        }
        # if(input$te21==TRUE){
        #   testout1 = tableHTML(testout1,round=5)%>%
        #     add_css_conditional_column(conditional='<',value=as.numeric(input$t21), css=list('background-color',
        #                                                                                         'red'),columns=
        #                                  c('p_value'))%>%
        #     add_css_conditional_column(conditional='>',value=1-((as.numeric(input$t21))/2), css=list('background-color',
        #                                                                                             'red'),columns=
        #                                  c('p_value'))
        # }
        return(testout1)
      }
      
      if(input$met21==TRUE){
        if(input$te11==TRUE){return(h6('Please select two-tailed. See the note in the autocorrelation tab '))}
        testout1 = matrix(nrow=1,ncol=2)
        colnames(testout1)<-c('statistic','p_value')
        ncv = ncvTest(fit)
        for(i in input$columns){
          testout1[,'p_value'] = ncv$p
          testout1[,'statistic']=ncv$ChiSquare
        }
        if(input$te21==TRUE){
          testout1 = tableHTML(testout1,round=5)%>%
            add_css_conditional_column(conditional='<',value=as.numeric(input$t21), css=list('background-color',
                                                                                            'red'),columns=
                                         c('p_value'))
        }
        # if(input$te11==TRUE){
        #   testout1 = tableHTML(testout1,round=5)%>%
        #     add_css_conditional_column(conditional='<',value=as.numeric(input$t21), css=list('background-color',
        #                                                                                         'red'),columns=
        #                                  c('p_value'))%>%
        #     add_css_conditional_column(conditional='>',value=1-((as.numeric(input$t21))/2), css=list('background-color',
        #                                                                                             'red'),columns=
        #                                  c('p_value'))
        # }
        return(testout1)
      }
      
    })
    
    output$tre1 = render_tableHTML({
      input$action30
      isolate({
        tr1()
        
        
      })
    })
    # output$downloadData <- downloadHandler({
    #   testre = as.data.frame(input$tre)}
    #   filename = function() {
    #     paste("test_re", Sys.Date(), ".csv", sep="")
    #   },
    #   content = function(file) {
    #     write_tableHTML(testre, file)
    #   }
    # 
    # )
    
    tr2 = reactive({
      df = filedata()
      if(is.null(df))return(NULL)
      if(length(input$columns)==0)return(NULL)
      if(input$Dependent=='')return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      if(input$nor1==TRUE){
        if(input$ot==TRUE){return(h6(strong('Please select two tailed. See the note in the autocorrelation tab')))}
        testout1 = matrix(nrow=length(input$columns)+1,ncol=2)
        rownames(testout1)<-c(input$columns,'Residuals')
        colnames(testout1)<-c('statistic','p_value')
        for(i in input$columns){
          testout1[i,'p_value'] = ad.test(df[,i])$p.value
          testout1[i,'statistic']=ad.test(df[,i])$statistic
        }
        # if(input$Intercept==FALSE){
        #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        #   fit = lm(fmla,data=df)
        # }
        # else{
        #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        #   fit = lm(fmla,data=df)
        # }
        testout1['Residuals','p_value']=ad.test(fit$residuals)$p.value
        testout1['Residuals','statistic']=ad.test(fit$residuals)$statistic
        
        if(input$tt==TRUE){
          testout1 = tableHTML(testout1,round=5)%>%
            add_css_conditional_column(conditional='<',value=as.numeric(input$los), css=list('background-color',
                                                                                                'red'),columns=
                                         c('p_value'))
        }
        return(testout1)
      }
      
      if(input$nor2==TRUE){
        if(input$ot==TRUE){return(h6(strong('Please select two tailed. See the note in the autocorrelation tab')))}
        testout1 = matrix(nrow=length(input$columns)+1,ncol=2)
        rownames(testout1)<-c(input$columns,'Residuals')
        colnames(testout1)<-c('statistic','p_value')
        for(i in input$columns){
          testout1[i,'p_value'] = shapiro.test(df[,i])$p.value
          testout1[i,'statistic']=shapiro.test(df[,i])$statistic
        }
        # if(input$Intercept==FALSE){
        #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        #   fit = lm(fmla,data=df)
        # }
        # else{
        #   fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        #   fit = lm(fmla,data=df)
        # }
        testout1['Residuals','p_value']=shapiro.test(fit$residuals)$p.value
        testout1['Residuals','statistic']=shapiro.test(fit$residuals)$statistic
        
        if(input$tt==TRUE){
          testout1 = tableHTML(testout1,round=5)%>%
            add_css_conditional_column(conditional='<',value=as.numeric(input$los), css=list('background-color',
                                                                                                'red'),columns=
                                         c('p_value'))
        }
        return(testout1)
      }
      if(input$nor3==TRUE){
        testout1 = matrix(nrow=length(input$columns)+1,ncol=2)
        rownames(testout1)<-c(input$columns,'Residuals')
        colnames(testout1)<-c('statistic','p_value')
        if(input$tt==TRUE){
          for(i in input$columns){
            testout1[i,'p_value'] = ks.boot(df[,i],qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$p.value
            testout1[i,'statistic']=ks.boot(df[,i],qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$statistic
          }
        testout1['Residuals','p_value']=ks.boot(fit$residuals,qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$p.value
        testout1['Residuals','statistic']=ks.boot(fit$residuals,qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$statistic
        testout1 = tableHTML(testout1,round=5)%>%
          add_css_conditional_column(conditional='<',value=as.numeric(input$los), css=list('background-color',
                                                                                          'red'),columns=
                                       c('p_value'))
        
      }
      if(input$ot==TRUE){
        for(i in input$columns){
          testout1[i,'p_value'] = ks.boot(df[,i],qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='greater')$ks$p.value
          testout1[i,'statistic']=ks.boot(df[,i],qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='greater')$ks$statistic
        }
        testout1['Residuals','p_value']=ks.boot(fit$residuals,qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$p.value
        testout1['Residuals','statistic']=ks.boot(fit$residuals,qnorm(ppoints(nrow(df))),nboots=as.numeric(input$nb),alternative='two.sided')$ks$statistic
        testout1 = tableHTML(testout1,round=5)%>%
          add_css_conditional_column(conditional='<',value=as.numeric(input$los), css=list('background-color',
                                                                                          'red'),columns=
                                       c('p_value'))%>%
          add_css_conditional_column(conditional='>',value=1-as.numeric(input$los), css=list('background-color',
                                                                                            'red'),columns=
                                       c('p_value'))
        
      }
      return(testout1)
      }
    })
    
    output$norm = render_tableHTML({
      input$action50
      isolate({
        tr2()
        
        
      })
    })
    
    tr5 = reactive({
      df = filedata()
      if(is.null(df))return(NULL)
      if(length(input$columns)==0)return(NULL)
      if(input$Dependent=='')return(NULL)
      if(input$type=='')return(NULL)
      
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      testout1 = matrix(nrow=1,ncol=2)
      colnames(testout1)=c('statistic','p_value')
      res= resettest(formula=fmla, power = c(2:as.numeric(input$mp)), type = input$type, data = df)
      testout1[1,'p_value'] = res$p.value
      testout1[1,'statistic']=res$statistic      
      
      testout1 = tableHTML(testout1,round=5)%>%
        add_css_conditional_column(conditional='<',value=as.numeric(input$los1), css=list('background-color',
                                                                                         'red'),columns=
                                     c('p_value'))
      
      return(testout1)
      
    })
    
    output$reset = render_tableHTML({
      input$action60
      isolate({
        tr5()
        
        
      })
    })
    
    tr6 = reactive({
      df = filedata()
      if(is.null(df))return(NULL)
      if(length(input$columns)==0)return(NULL)
      if(input$Dependent=='')return(NULL)
      if(input$th=='')return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      testout1 = matrix(nrow=length(input$columns),ncol=1)
      colnames(testout1)=c('VIF')
      rownames(testout1)=input$columns 
      testout1[,'VIF']=car::vif(fit)   
      
      testout1 = tableHTML(testout1,round=5)%>%
        add_css_conditional_column(conditional='>',value=as.numeric(input$th), css=list('background-color',
                                                                                        'red'),columns=
                                     c('VIF'))
      
      return(testout1)
      
    })
    
    output$vif = render_tableHTML({
      input$action5
      isolate({
        tr6()
        
        
      })
    })
    compar = reactive({

        md = filedata()
        if(is.null(md))return(NULL)
        #if(is.null(input$plot1))return(NULL)
      df = td()
      tf = ted()
      if (is.null(df)) return(NULL)
      if(input$Dependent=='') return(NULL)
      #if(is.null(input$train_out)) return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=df)

      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=df)
      }
      pr = predict(fit,tf)

      ap = cbind(tf[,input$Dependent],pr)
      colnames(ap)<-c('actual','predicted')

      ap

    })

    output$compare <- DT::renderDataTable(compar(), rownames = FALSE, extensions = c('Responsive','Buttons','KeyTable'),
                                          options = list(dom = 'Bfrtip', buttons = c("csv"),keys = TRUE, scrollX=TRUE),
                                          filter = list(position = 'top'), editable = FALSE, server=FALSE)


    compar_main = reactive({

      md = filedata()
      if(is.null(md))return(NULL)
      #if(is.null(input$plot1))return(NULL)

      if (is.null(subt1())) return(NULL)
      if(input$Dependent=='') return(NULL)
      #if(is.null(input$train_out)) return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        fit = lm(fmla,data=subt1())

      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        fit = lm(fmla,data=subt1())
      }
      pr = predict(fit,subte1())

      ap = cbind(subte1()[,input$Dependent],pr)
      colnames(ap)<-c('actual','predicted')

      ap

    })

    output$compare_main <- DT::renderDataTable(compar_main(), rownames = FALSE, extensions = c('Responsive','Buttons','KeyTable'),
                                          options = list(dom = 'Bfrtip', buttons = c("csv"),keys = TRUE, scrollX=TRUE),
                                          filter = list(position = 'top'), editable = FALSE, server=FALSE)


    output$RMSE<- renderTable({

      md = filedata()
      if(is.null(md))return(NULL)
      if(input$Dependent=='') return(NULL)
      df = td()

      if (is.null(df)) return(NULL)
       df1 = ted()
       if(is.null(df1))return(NULL)
       avp = compar()
       if(is.null(avp))return(NULL)

       rmse = sqrt(sum((avp[,'actual']-avp[,'predicted'])^2)/nrow(avp))
       rmse_tb = data.frame("RMSE(test data)"=rmse)

       rmse_tb

    })

    output$RMSE_main<- renderTable({

      md = filedata()
      if(is.null(md))return(NULL)

      if (is.null(subt1())) return(NULL)

      if(is.null(subte1()))return(NULL)
      if(input$Dependent=='') return(NULL)
      avp = compar_main()
      if(is.null(avp))return(NULL)

      rmse = sqrt(sum((avp[,'actual']-avp[,'predicted'])^2)/nrow(avp))
      rmse_tb = data.frame("RMSE(test data)"=rmse)

      rmse_tb

    })

    output$avsp <- renderHighchart({

      input$action
      isolate({
      df<-filedata()
      if (is.null(df)) return(NULL)
      if(input$Dependent=='') return(NULL)
      if(input$Intercept==FALSE){
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
        sum_fm = lm(fmla,data=df)
      }
      else{
        fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
        sum_fm= lm(fmla,data=df)
      }
      hchart(df,'scatter',hcaes_string(df[,input$Dependent],y=sum_fm$fitted.values),color='red',name = 'actual vs predicted')%>%
        hc_xAxis(title='')%>%
        hc_yAxis(title='')%>%
        hc_add_series(data.frame(x=c(range(df[,input$Dependent])[1]:range(df[,input$Dependent])[2]),
                                 y=c(range(df[,input$Dependent])[1]:range(df[,input$Dependent])[2])),
                      'line',hcaes(x=x,y=y))
    })

    })
    
    output$qqplot <- renderHighchart({
    df<-filedata()
        if (is.null(df)) return(NULL)
        if(input$Dependent=='') return(NULL)
        if(length(input$columns)==0)return(NULL)
        if(input$var10=='')return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          sum_fm = lm(fmla,data=df)
        }
        else{
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
          sum_fm= lm(fmla,data=df)
        }
    pts = qnorm(ppoints(nrow(df)))
    if(input$var10!='Residuals'){
      
       ht= 
         hchart(df,'scatter',hcaes_string(df[,input$var10],y=pts),color='red',name = 'qqplot')%>%
          hc_xAxis(title='')%>%
          hc_yAxis(title='')
       # %>%
       #    hc_add_series(data.frame(x=seq(0,max(range(df[,input$var10])[2],range(pts)[2]),by=0.0001),
       #                             y=seq(0,max(range(df[,input$var10])[2],range(pts)[2]),by=0.0001)),
       #                  'line',hcaes(x=x,y=y))
       return(ht)}
    if(input$var10=='Residuals'){
      
      ht= hchart(df,'scatter',hcaes_string(x=sum_fm$residuals,y=qnorm(ppoints(nrow(df)))),color='red',name = 'qqplot')%>%
        hc_xAxis(title='')%>%
        hc_yAxis(title='')
      # %>%
      #   hc_add_series(data.frame(x=seq(0,max(range(df[,input$var10])[2],range(pts)[2]),by=0.0001),
      #                            y=seq(0,max(range(df[,input$var10])[2],range(pts)[2]),by=0.0001)),
      #                 'line',hcaes(x=x,y=y))
      return(ht)
    }
      })
      
    
    
    output$rvsf <- renderHighchart({

      
        df<-filedata()
        if (is.null(df)) return(NULL)
        if(input$Dependent=='') return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          sum_fm = lm(fmla,data=df)
        }
        else{
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
          sum_fm= lm(fmla,data=df)
        }
        hchart(df,'scatter',hcaes_string(x=sum_fm$fitted.values,y=sum_fm$residuals),color='red',name = 'residuals vs predicted')%>%
          hc_xAxis(title='',label='predicted')%>%
          hc_yAxis(title='',label='residuals')
      

    })
    
    output$srvsf <- renderHighchart({
      
      
        df<-filedata()
        if (is.null(df)) return(NULL)
        if(input$Dependent=='') return(NULL)
        if(input$Intercept==FALSE){
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(c(0,input$columns),collapse="+")))
          sum_fm = lm(fmla,data=df)
        }
        else{
          fmla <- as.formula(paste(input$Dependent," ~ ",paste(input$columns,collapse="+")))
          sum_fm= lm(fmla,data=df)
        }
        sr = sqrt(abs(rstandard(sum_fm)))
        hchart(df,'scatter',hcaes_string(x=sum_fm$fitted.values,y=sr),color='red',name = 'std. residuals vs predicted(Scale Location)')%>%
          hc_xAxis(title='',label='predicted')%>%
          hc_yAxis(title='',label='residuals')
      
      
    })

    output$ui.action <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("action", "Press after reading file and selecting variables")
    })
    
    output$loui.action <- renderUI({
      if (is.null(input$file1)) return()
      actionButton("action100", "Press after reading file and selecting variables")
    })
    
    url1 = a("Python's Dash vs R's Shiny: Interesting Read",href='https://www.r-bloggers.com/shiny-vs-dash-a-side-by-side-comparison/')
    output$tab1 = renderUI({tagList('Link:',url1)})
    url2 = a('Interesting variations that are faster than Gradient Descent',href='http://ruder.io/optimizing-gradient-descent/')
    output$tab2 = renderUI({tagList('Link:',url2)})
  }
)
