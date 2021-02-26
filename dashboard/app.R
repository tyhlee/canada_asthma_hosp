#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source('global.R')
options(shiny.deprecation.messages=FALSE)

####### UI #########
ui = dashboardPagePlus(
    ####### dashboard settings ########
    # skin = "midnight",
    
    ####### header ########
    dashboardHeaderPlus(
        title = tagList(
            span(class = "logo-lg", "Canada Asthma Hosp. Rates",
                 style='font-weight: bold; font-size: 14px'), 
            # img(src = title.image,width='100%',height='100%',align='centre')),
            img(src = title.image,style="width: 30px",align='centre')),
        enable_rightsidebar = TRUE,
        rightSidebarIcon='gears'
    ),
    
    ####### left sidebar ########
    dashboardSidebar(
        id="",
        tags$head(
            tags$script(
                HTML(
                    "$(document).ready(function(){
                          // Bind classes to menu items, easiet to fill in manually
                          var ids = ['intro','rates','about'];
                          for(i=0; i<ids.length; i++){
                            $('a[data-value='+ids[i]+']').addClass('my_item_class');
                          }

                          // Register click handeler
                          $('.my_item_class').on('click',function(){
                            // Unactive menuItems
                            $('.my_item_class').parent().removeClass('active');
                          })
                        })
                        "
                )
            )
        ),
        sidebarMenu(
            menuItem(
                text = "Background", 
                tabName = "intro",
                # badgeColor = "green",
                icon = icon("hospital-user")
            )
        ),
        
        sidebarMenu(
            
            menuItem(
                text = "Rates", 
                tabName = "rates",
                # badgeColor = "green",
                icon = icon("chart-line")
                # menuSubItem(icon=NULL,
                # radioButtons('input_tu',
                #             'Time Unit',
                #             tu_list,
                #             'Yearly')),
                # menuSubItem(icon=NULL,
                #             checkboxGroupInput('input_prov',
                #                                'Provinces',
                #                                province_list,
                #                                "CA"))
            )
            ),
        # sidebarMenu(
        #     menuItem(
        #         text='Length of stay',
        #         tabName='los',
        #         icon=icon('calendar-alt')
        #     )
        # ),
        sidebarMenu(
            menuItem(
                text='Change-point analysis',
                tabName='cp',
                icon=icon('exchange-alt')
            )
        ),
        sidebarMenu(
            menuItem(
                text = "About us", 
                tabName = "about",
                # badgeColor = "green",
                icon = icon("address-card")
            )
        )
    ),
    
    ####### body #######
    dashboardBody(
        # box(plotOutput("hello"),collapsible = T)
        tabItems(

# Intro tab ---------------------------------------------------------------

            tabItem(tabName = 'intro',
                    h2("TODO")
                    ),


# Rates tab ---------------------------------------------------------------


            tabItem(tabName = 'rates',
                    # fluidRow(
                    #     column(width=col_width,
                    #            align='center',
                    #            radioButtons('input_tu',
                    #                         'Time Unit',
                    #                         tu_list,
                    #                         'Yearly',inline = T))
                    #     ),
                    fluidRow(
                        column(width=col_width,
                               align='center',
                               radioButtons('input_ag',
                                                  'Age group category',
                                            names(ag_list),
                                                  selected="binary_18",
                                                  inline = T))
                        ),
                    fluidRow(
                        column(width=col_width,
                               align='center',
                               uiOutput("age_box"))
                        ),

                    
                    fluidRow(
                        column(width=col_width,
                               align='center',
                               box(plotOutput("age_sex")%>% 
                                       withSpinner(color="#0dc5c1"),
                                   width=rates_box_width,
                                   title=''))
                        ),
                    
                    # fluidRow(
                    #   column(width=col_width,
                    #          align='center',
                    #          radioButtons('input_age_los',
                    #                       'Length of stay',
                    #                       names(ag_list),
                    #                       "binary_18",
                    #                       inline = T))
                    # ),
                    # 
                    fluidRow(
                      column(width=col_width,
                             align='center',
                             uiOutput("los_age_box"))
                    ),
                    
                    fluidRow(
                      column(width=col_width,align='center',
                             box(plotOutput("los")%>% 
                                   withSpinner(color="#0dc5c1"),
                                 width=rates_box_width,
                                 title=''))
                    )
                    ),

# cp analysis -------------------------------------------------------------

            tabItem(tabName='cp',
                    fluidRow(
                      column(width=col_width,
                             align='center',
                             radioButtons('input_cp_ag',
                                          'Age group category',
                                          names(ag_list),
                                          selected="binary_18",
                                          inline = T))
                    ),
                   
                    fluidRow(
                      column(width=col_width,
                             align='center',
                             uiOutput("cp_age_box"))
                    ),
                    
                    fluidRow(
                        column(width=col_width,
                               align=col_width,
                               dataTableOutput("tab.cp",width = '100%') %>% 
                                 withSpinner(color="#0dc5c1"))
                    ),
                    fluidRow(
                        column(width=12,
                               align='center',
                               plotOutput("gg.cp") %>% 
                                   withSpinner(color="#0dc5c1"))
                    )
            ),
# About tab ---------------------------------------------------------------

            tabItem(tabName = 'about',
                    h1("TODO")
                    )
            )
        
        
    ),
    
    ####### right sidebar ########
    rightSidebar(
        
        
        
    ),
    
    ####### footer ########
    title = "Canada Asthma Hospitalization Dashboard",
    footer = dashboardFooter(
        left = "By Tae Yoon Lee",
        right = "UBC RESP 2020"
    )
)

######### Server ###########
server <- function(input, output) {
    
  
    output$age_box <- renderUI({
      checkboxGroupInput('input_age',
                         'Age group',
                         ag_list[[match(input$input_ag,names(df_global))]][-1],
                         ag_list[[match(input$input_ag,names(df_global))]][2],
                         inline=T)
    })
    
    output$los_age_box <- renderUI({
      radioButtons('input_age_los',
                         'Age group',
                         ag_list[[match(input$input_ag,names(df_global))]][-1],
                         ag_list[[match(input$input_ag,names(df_global))]][2],
                   inline=T)
    })
    
    ####### df #######
    df_plot_ag <- reactive({
        df_global[[match(input$input_ag,names(df_global))]] %>% 
            filter(age_group %in% input$input_age)
    })
    # browser()
    df_plot_los <- reactive({
      df_global[[match(input$input_ag,names(df_global))]] %>% 
        filter(age_group %in% input$input_age_los)
    })
    
    ######## rates by sex-age ##########
    output$age_sex <- renderPlot({
        ggplot(data = df_plot_ag(),
                      aes(y=rate,x=Year,colour=age_group)) +
            geom_line()+
            geom_point() +
            geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper,x=Year), width=.2) +
            ylim(0,ceiling(max(df_plot_ag()$CI_upper))) +
            ylab(ylab_rate) +
            xlab("") + 
            theme_text_size +
            scale_x_continuous(breaks=c(seq(min_year,max_year,2))) +
            theme_classic()+
            facet_grid(.~sex) +
            theme(legend.position="top", legend.box = "horizontal",
                  legend.title=element_blank())
    })
    
    ######### rates + los by sex-age ##########
    output$los <- renderPlot({
      tmp <- df_plot_los()
      ratio <- (max(tmp$CI_upper)-min(tmp$CI_lower))/ (max(tmp$los)+min(tmp$los))
      ggplot(data = tmp,#df_plot_los(),
             aes(y=rate,x=Year)) +
        geom_line()+  
        geom_point() +
        geom_errorbar(aes(ymin=CI_lower, ymax=CI_upper,x=Year), width=.2) +
        # ylim(0,ceiling(df_plot_los()$CI_upper)) +
        ylab(ylab_rate) +
        xlab("") + 
        theme_text_size +
        # geom_col(aes(y=los*8,x=Year),fill="#bdbdbd") +
        geom_line(aes(y=los*ratio,x=Year),col="#7d93c8") +
        geom_point(aes(y=los*ratio,x=Year),alpha=0.8,col="#7d93c8") +
        scale_y_continuous(sec.axis=sec_axis(~(.)/ratio,name="Average Length of Stay (days)"),
                           limits=c(0,ceiling(max(tmp$CI_upper)))) +
        scale_x_continuous(breaks=c(seq(min_year,max_year,2))) +
        theme_classic()+
        facet_grid(.~sex) +
        theme(legend.position="top", legend.box = "horizontal",
              legend.title=element_blank())
    })
    
    ######### cp tab ##########
    
    output$cp_age_box <- renderUI({
      radioButtons('input_cp_age',
                   'Age group',
                   ag_list[[match(input$input_cp_ag,names(df_cp_global))]][-1],
                   ag_list[[match(input$input_cp_ag,names(df_cp_global))]][2],
                   inline=T)
    })
    
    df_cp <- reactive({
      df_cp_global[[match(input$input_cp_ag,names(df_cp_global))]] %>% 
        filter(`Age group` %in% input$input_cp_age)
    })
    
    output$tab.cp <- DT::renderDataTable(
      df_cp() %>% 
        select(-`Relative change (%)`)
    )
    
    output$gg.cp <- reactivePlot(function() {
      tmp <- gg_cp_global[[match(input$input_cp_ag,names(gg_cp_global))]]
      tmp <- tmp[grep(input$input_cp_age,names(tmp))]
      
      df_cp() %>% 
        distinct(`Sex`,`Age group`,`Relative change (%)`) %>% 
        select(`Relative change (%)`) %>% 
        unlist() -> tmp_rc
      
      gridExtra::grid.arrange(tmp[[1]]+theme_text_size2 + 
                                ggtitle(paste0("Female: ", tmp_rc[1], " relative change in the rate of slope"))+
                                theme(
                                  plot.title = element_text(hjust = 0.5)),
                              tmp[[2]]+theme_text_size2 + 
                                ggtitle(paste0("Male: ", tmp_rc[2], " relative change in the rate of slope"))+
                                theme(
                                  plot.title = element_text(hjust = 0.5)),
                              nrow=1)
        # # browser()
        # if(is.null(input$tab.cp_rows_selected)){
        #     return(NULL)
        # }else{
        # gg_cp_list[[input$tab.cp_rows_selected]]
        # }
        })
        
    
}

######### Run the application ##########
shinyApp(ui = ui, server = server)
