library(dplyr)
library(ggplot2)
library(ggrepel)
library(magrittr)
library(tidyr)
library(shiny)
library(leaflet)
library(leafpop)
library(sf)
library(scales)
library(stringr)
library(rsconnect)
library(leaflet.extras)
library(shinyBS)
library(shinyjs)
library(shinycssloaders)



#loading the shapefile 
world_polygon_app <- st_read("world_polygon_app.shp") %>% 
  filter(!nam_lng %in% c("Swaziland", "Palestine")) %>% 
  mutate(nam_lng=recode(nam_lng, `Democratic Republic of the Congo`="Congo [DRC]"))%>% 
  select(-c("VCstHPB" , "VCstPRB" , "VC75PBC"))

#getting world_polygon without the geomtry columns i.e get just the dataframe
world_polygon_nogeom <- st_drop_geometry(world_polygon_app) 

addUnits <- function(n) {
  labels <- ifelse(n < 1000, n,  # less than thousands
                   ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                          ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                 ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                        ifelse(n < 1e15, paste0(round(n/1e12), 'T'), # in trillions
                                               'too big!'
                                        )))))
  return(labels)
}

mill <-  scales::unit_format(unit = "M", scale = 1e-6, accuracy = .01)

#Creating function for bar chart 
plot_test <- function(country_name, vc_cvx){
  
  if(vc_cvx==FALSE){
    
    total_df <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name) %>%
      mutate(TtCstDB=(DlCstDs+VCstDs_b),
             VCstHPB=(NmHthPr*TtCstDB*num_dos),
             VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
             VC75PBC=(tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) %>% 
      select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      gather(Total_type, `Total_val`, -Year) %>%
      mutate(Vcc1= case_when(Total_type=="VCstHPB"~ "ValH",
                             Total_type=="VCstPRB" ~"ValR",
                             Total_type=="VC75PBC"~"Val7")) %>% 
      mutate(`type_val`="dot")
    
    g1 <-  ggplot(total_df ,
                  aes(Vcc1, `Total_val`))+
      geom_point(aes(color=`type_val`), size=4)+
      geom_text_repel(aes(label=mill(`Total_val`)),min.segment.length = Inf, box.padding = 0.5, size=3.5, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("dot"="#FB262A"))+
      xlim("Val7", "ValR", "ValH")+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 8.5),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold"),
            legend.position = "bottom",
            plot.title = element_text(size=11, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste(country_name,"Chart","(M: Million, k: Thousand)"))
    
    g1
    
  }else if (vc_cvx!=FALSE){
    
    total_df <- world_polygon_nogeom  %>%
      filter(nam_lng==country_name) %>%
      mutate(DelCstHthPrf=(NmHthPr*DlCstDs*num_dos),
             DelCstPpRsk=(tot_pop*HghRskP*DlCstDs*num_dos),
             DelCst75Pp=(tot_pop*DlCstDs*num_dos*pr_hrd_c)+(tot_pop*DlCstDs*num_dos*pr_hrd_b),
             PrcCstHthPrf=ifelse(Cvx_lgb=="Yes",(NmHthPr*VCstDs_c*num_dos),(NmHthPr*VCstDs_b*num_dos)),
             PrcCstPpRsk=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*VCstDs_c*num_dos), (tot_pop*HghRskP*VCstDs_b*num_dos)),
             PrcCst75Pp=ifelse(Cvx_lgb=="Yes", ((tot_pop*VCstDs_c*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b)),
                               ((tot_pop*VCstDs_b*num_dos*pr_hrd_c)+(tot_pop*VCstDs_b*num_dos*pr_hrd_b))),
             VCstHPB=(DelCstHthPrf+ PrcCstHthPrf),
             VCstPRB=(DelCstPpRsk+PrcCstPpRsk),
             VC75PBC=(DelCst75Pp+PrcCst75Pp)) %>% 
      select(Year, VCstHPB, VCstPRB, VC75PBC) %>% 
      gather(Total_type, `Total_val`, -Year) %>%
      mutate(Vcc1= case_when(Total_type=="VCstHPB"~ "ValH",
                             Total_type=="VCstPRB" ~"ValR",
                             Total_type=="VC75PBC"~"Val7")) %>% 
      mutate(`type_val`="dot")
    
    g1 <-  ggplot(total_df ,
                  aes(Vcc1, `Total_val`))+
      geom_point(aes(color=`type_val`), size=4)+
      geom_text_repel(aes(label=mill(`Total_val`)),min.segment.length = Inf, box.padding = 0.5, size=3.5, fontface="bold")+
      scale_y_continuous(labels = addUnits, n.breaks = 5)+
      scale_color_manual(values = c("dot"="#FB262A"))+
      xlim("Val7", "ValR", "ValH")+
      theme_minimal()+
      theme(axis.title.y=element_blank(),
            axis.title.x = element_blank(),
            axis.text.x = element_text(face = "bold", size = 8.5),
            legend.title =  element_blank(),
            legend.text = element_text(face="bold"),
            legend.position = "bottom",
            plot.title = element_text(size=11, face="bold", hjust = 0),
            plot.title.position = "plot")+
      labs(title = paste(country_name, "Chart","(M: Million, k: Thousand)"))
    
    g1
    
    
  }
}

ui <- bootstrapPage(
  useShinyjs(),
  navbarPage(title = "", inverse = TRUE, 
             
             tabPanel("Test Map", 
                      
                      div(class = "outer",
                          tags$head(
                            # Include our custom CSS
                            tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
                          ),
                          
                          tags$style("#controls {
                            overflow: auto;
                            max-height: 90vh
                            }"),
                          
                          # If not using custom CSS, set height of leafletOutput to a number instead of percent
                          leafletOutput("map", width="100%", height="100%"),
                          
                          tags$head(
                            # this changes the size of the popovers
                            tags$style(".popover{width:270px;height:325px;}")
                          ),
                          
                          #Floating panel
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                        width = 330, style='background-color: #eaf2f8',
                                        
                                        
                                        tags$h6(tags$i(tags$b("You can click any country on the map to view changes in chart."))),
                                        
                                        tags$h6(tags$i(tags$b("Choose any of the radio buttons below to see changes in choropleth."))),
                                        
                                        div(style = "margin-top:-5px"),
                                        
                                        radioButtons("maptype", list(tags$span(style = "font-weight: bold;font-size: 12px; color:#262626", "")),
                                                     choiceNames =  list(tags$span(style = "font-size: 12px; color:#262626", "A"),
                                                                         tags$span(style = "font-size: 12px; color:#262626", "B"), 
                                                                         tags$span(style = "font-size: 12px; color:#262626", "C")),
                                                     choiceValues = c("A", "B", "C")),
                                        
                                        checkboxInput("cvx_check", h5(tags$span(style="font-size: 12px"," check to include some data in calculations"),
                                                                      tags$style(type="text/css", "#q01_1 {vertical-align: top;}"),
                                                                      bsButton("q01_1", label="", icon=icon("question"), style="info", size="extra-small")),
                                                      value = FALSE),
                                        bsPopover(id="q01_1", title="",
                                                  content=paste("Some text"),
                                                  placement = "bottom",
                                                  trigger = "focus",
                                                  options = list(container = "body")
                                        ),
                                        
                                        div(style = "margin-top:-30px"),
                                        
                                        selectInput("country", "",choices = NULL, width = "150px"),
                                        
                                        div(style = "margin-top:-17px"),
                                        
                                        plotOutput("plot_test", height = "200px", width = "100%")%>% withSpinner(),
                                        
                                        
                                        
                                        
                          ),
                          
                      )
             )
             
  )
)

server <- function(input, output, session) {
  
  updateSelectInput(session,
                    "country", choices = world_polygon_app$nam_lng)
  
  #leaflet basemap
  output$map <- renderLeaflet({
    leaflet(world_polygon_app) %>% addProviderTiles(providers$CartoDB) %>% 
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>% 
      addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE)) %>% 
      addResetMapButton()  
  })
  
  #plot function
  output$plot_test <- renderPlot({
    plot_test(input$country,  input$cvx_check)
  })
  
  filtered_world_polygon <- reactive({
    
    order_of_75per <- c("Below 65 Million","65 - 145 Million", "145 - 275 Million",
                        "275 - 650 Million", "Above 650 Million")
    order_of_risk <- c("Below 4 Million", "4 - 8 Million", "8 - 20 Million", 
                       "20 - 45 Million", "Above 45 Million")
    order_of_health <- c("Below 200 Thousand", "200 - 700 Thousand", "700 Thousand - 2 Million",
                         "2 - 6 Million",
                         "Above 6 Million")
    
    if(input$cvx_check==FALSE){
      
      country_polygon <- world_polygon_app %>%
        filter(nam_lng==input$country) %>%
        mutate(TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=(NmHthPr*TtCstDB*num_dos),
               VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
               VC75PBC= (tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) 
      
      remaining_polygon <- world_polygon_app %>%
        filter(nam_lng!=input$country) %>% 
        mutate(TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=(NmHthPr*TtCstDB*num_dos),
               VCstPRB=(tot_pop*HghRskP*TtCstDB*num_dos),
               VC75PBC= (tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)) 
      
      new_updated_polygon <- rbind(remaining_polygon, country_polygon) %>%
        mutate(cst_rng_75= case_when(VC75PBC<=65000000~ "Below 65 Million",
                                     VC75PBC>65000000 & VC75PBC<=145000000~"65 - 145 Million",
                                     VC75PBC>145000000 & VC75PBC<=275000000~"145 - 275 Million",
                                     VC75PBC>275000000 & VC75PBC<=650000000~"275 - 650 Million",
                                     VC75PBC>650000000~"Above 650 Million")) %>%
        mutate(cst_rng_75=factor(cst_rng_75, levels =  order_of_75per)) %>%
        mutate(cst_rng_rsk=case_when(VCstPRB<=4000000~ "Below 4 Million",
                                     VCstPRB>4000000 & VCstPRB<=8000000~"4 - 8 Million",
                                     VCstPRB>8000000 & VCstPRB<=20000000~"8 - 20 Million",
                                     VCstPRB>20000000 & VCstPRB<=45000000~"20 - 45 Million",
                                     VCstPRB>45000000~"Above 45 Million")) %>%
        mutate(cst_rng_rsk=factor(cst_rng_rsk, levels =  order_of_risk)) %>%
        mutate(cst_rng_hlth=case_when(VCstHPB<=200000~ "Below 200 Thousand",
                                      VCstHPB>200000 & VCstHPB<=700000~"200 - 700 Thousand",
                                      VCstHPB>700000 & VCstHPB<=2000000~"700 Thousand - 2 Million",
                                      VCstHPB>2000001 & VCstHPB<=6000000~"2 - 6 Million",
                                      VCstHPB>6000000~"Above 6 Million")) %>%
        mutate(cst_rng_hlth=factor(cst_rng_hlth, levels =  order_of_health))
      
      return (new_updated_polygon)
      
    }else{
      
      order_of_75per <- c("Below 65 Million","65 - 145 Million", "145 - 275 Million",
                          "275 - 650 Million", "Above 650 Million")
      order_of_risk <- c("Below 4 Million", "4 - 8 Million", "8 - 20 Million", 
                         "20 - 45 Million", "Above 45 Million")
      order_of_health <- c("Below 200 Thousand", "200 - 700 Thousand", "700 Thousand - 2 Million",
                           "2 - 6 Million",
                           "Above 6 Million")
      
      country_polygon <- world_polygon_app %>%
        filter(nam_lng==input$country) %>% 
        mutate(TtCstDC=(DlCstDs+VCstDs_c),
               TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=ifelse(Cvx_lgb=="Yes",(NmHthPr*TtCstDC*num_dos),(NmHthPr*TtCstDB*num_dos)),
               VCstPRB=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*TtCstDC*num_dos), (tot_pop*HghRskP*TtCstDB*num_dos)),
               VC75PBC=ifelse(Cvx_lgb=="Yes", ((tot_pop*TtCstDC*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)),
                              ((tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b))))
      
      remaining_polygon <- world_polygon_app %>% 
        filter(nam_lng!=input$country)%>% 
        mutate(TtCstDC=(DlCstDs+VCstDs_c),
               TtCstDB=(DlCstDs+VCstDs_b),
               VCstHPB=ifelse(Cvx_lgb=="Yes",(NmHthPr*TtCstDC*num_dos),(NmHthPr*TtCstDB*num_dos)),
               VCstPRB=ifelse(Cvx_lgb=="Yes", (tot_pop*HghRskP*TtCstDC*num_dos), (tot_pop*HghRskP*TtCstDB*num_dos)),
               VC75PBC=ifelse(Cvx_lgb=="Yes", ((tot_pop*TtCstDC*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b)),
                              ((tot_pop*TtCstDB*num_dos*pr_hrd_c)+(tot_pop*TtCstDB*num_dos*pr_hrd_b))))
      
      new_updated_polygon <- rbind(remaining_polygon, country_polygon) %>% 
        mutate(cst_rng_75= case_when(VC75PBC<=65000000~ "Below 65 Million",
                                     VC75PBC>65000000 & VC75PBC<=145000000~"65 - 145 Million",
                                     VC75PBC>145000000 & VC75PBC<=275000000~"145 - 275 Million",
                                     VC75PBC>275000000 & VC75PBC<=650000000~"275 - 650 Million",
                                     VC75PBC>650000000~"Above 650 Million")) %>% 
        mutate(cst_rng_75=factor(cst_rng_75, levels =  order_of_75per)) %>% 
        mutate(cst_rng_rsk=case_when(VCstPRB<=4000000~ "Below 4 Million",
                                     VCstPRB>4000000 & VCstPRB<=8000000~"4 - 8 Million",
                                     VCstPRB>8000000 & VCstPRB<=20000000~"8 - 20 Million",
                                     VCstPRB>20000000 & VCstPRB<=45000000~"20 - 45 Million",
                                     VCstPRB>45000000~"Above 45 Million")) %>% 
        mutate(cst_rng_rsk=factor(cst_rng_rsk, levels =  order_of_risk)) %>% 
        mutate(cst_rng_hlth=case_when(VCstHPB<=200000~ "Below 200 Thousand",
                                      VCstHPB>200000 & VCstHPB<=700000~"200 - 700 Thousand",
                                      VCstHPB>700000 & VCstHPB<=2000000~"700 Thousand - 2 Million",
                                      VCstHPB>2000001 & VCstHPB<=6000000~"2 - 6 Million",
                                      VCstHPB>6000000~"Above 6 Million")) %>% 
        mutate(cst_rng_hlth=factor(cst_rng_hlth, levels =  order_of_health))
      
      return (new_updated_polygon)
    }
  })
  
  factpal <- reactive({
    colorFactor("Blues", filtered_world_polygon()$cst_rng_75)
  })
  
  
  factpal_pop <- reactive({
    colorFactor("Oranges", filtered_world_polygon()$cst_rng_rsk)
    
  })
  
  factpal_health <- reactive({
    colorFactor("Greens", filtered_world_polygon()$st_rng_hlth)
  })
  
  my_poup_70p <- reactive({
    
    if(input$cvx_check==FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos,VCstDs_b,
                      DlCstDs, VC75PBC) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VC75PBC= dollar_format()(VC75PBC)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1=Cvx_lgb, 
                      Row2= num_dos,
                      Row3= VCstDs_b,
                      Row4= DlCstDs,
                      Row5 = (VC75PBC))
      
    }else if (input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng, Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VC75PBC) %>% 
        dplyr::mutate(cvx_price70p=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VC75PBC= dollar_format()(VC75PBC)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1= num_dos,
                      Row2=Cvx_lgb, 
                      Row3= cvx_price70p,
                      Row4= VCstDs_b,
                      Row5= DlCstDs,
                      Row6 = (VC75PBC)) %>% 
        dplyr::select(-VCstDs_c)
      
    }
    
  })
  
  my_poup_risky <- reactive({
    
    if(input$cvx_check==FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos,  VCstDs_b,
                      DlCstDs, VCstPRB) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstPRB= dollar_format()(VCstPRB)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1=Cvx_lgb, 
                      Row2= num_dos,
                      Row3= VCstDs_b,
                      Row4= DlCstDs,
                      Row5 = (VCstPRB))
      
    }else if (input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng, Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VCstPRB) %>% 
        dplyr::mutate(cvx_pricersk=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstPRB= dollar_format()(VCstPRB)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1=Cvx_lgb, 
                      Row2= num_dos,
                      Row3= cvx_pricersk,
                      Row4= VCstDs_b,
                      Row5= DlCstDs,
                      Row6 = (VCstPRB)) %>% 
        dplyr::select(-VCstDs_c)
      
    }
  })
  
  my_poup_health <- reactive({
    
    if(input$cvx_check==FALSE){
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos, VCstDs_b,
                      DlCstDs, VCstHPB) %>% 
        dplyr::mutate(VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstHPB= dollar_format()(VCstHPB)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1=Cvx_lgb, 
                      Row2= num_dos,
                      Row3= VCstDs_b,
                      Row4= DlCstDs,
                      Row5 = (VCstHPB)) 
      
    }else if(input$cvx_check!=FALSE){
      
      st_drop_geometry(filtered_world_polygon()) %>% 
        dplyr::select(nam_lng,Cvx_lgb, num_dos, VCstDs_c, VCstDs_b,
                      DlCstDs, VCstHPB) %>% 
        dplyr::mutate(cvx_pricehlth=ifelse(Cvx_lgb=="Yes", VCstDs_c, "Not eligible")) %>% 
        dplyr::mutate(VCstDs_c= dollar_format()(VCstDs_c),
                      VCstDs_b= dollar_format(accuracy = .01)(VCstDs_b),
                      DlCstDs=dollar_format(accuracy = .01)(DlCstDs),
                      VCstHPB= dollar_format()(VCstHPB)) %>% 
        dplyr::rename(Country= nam_lng,
                      Row1=Cvx_lgb, 
                      Row2= num_dos,
                      Row3= cvx_pricehlth,
                      Row4= VCstDs_b,
                      Row5= DlCstDs,
                      Row6 = (VCstHPB)) %>% 
        dplyr::select(-VCstDs_c)
    }
  })
  
  observe({
    if(input$maptype=="A" & input$cvx_check==FALSE){
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>%
          clearShapes() %>%
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal()(cst_rng_75),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_70p(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>%
          clearControls() %>%
          addLegend("bottomleft", pal = factpal(), values =~na.omit(cst_rng_75), opacity = 1,
                    title = "AA")
      })
    }
    
    if (input$maptype=="A" & input$cvx_check!=FALSE){
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>%
          clearShapes() %>%
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal()(cst_rng_75),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_70p(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>%
          clearControls() %>%
          addLegend("bottomleft", pal = factpal(), values =~na.omit(cst_rng_75), opacity = 1,
                    title = "AA")
      })
      
    }
    
    if (input$maptype=="B"& input$cvx_check==FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_pop()(cst_rng_rsk),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_risky(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal =  factpal_pop(), values =~na.omit(cst_rng_rsk), opacity = 1,
                    title = "BB")
      })
    }
    
    if (input$maptype=="B"& input$cvx_check!=FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_pop()(cst_rng_rsk),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_risky(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal =  factpal_pop(), values =~na.omit(cst_rng_rsk), opacity = 1,
                    title = "BB")
      })
    }
    
    if(input$maptype=="C"& input$cvx_check==FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_health()(cst_rng_hlth),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_health(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal = factpal_health(), values =~na.omit(cst_rng_hlth), opacity = 1,
                    title = "CC")
        
      })
    }
    if(input$maptype=="C"& input$cvx_check!=FALSE){
      
      isolate({
        leafletProxy("map", data = filtered_world_polygon()) %>% 
          clearShapes() %>% 
          addPolygons(smoothFactor = 0.2 ,fillOpacity = 0.6, weight = 0.3, opacity = 1,
                      color = "pink", fillColor = ~factpal_health()(cst_rng_hlth),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      popup = popupTable(my_poup_health(),  feature.id = FALSE, row.numbers = F),
                      layerId = ~nam_lng) %>% 
          clearControls() %>% 
          addLegend("bottomleft", pal = factpal_health(), values =~na.omit(cst_rng_hlth), opacity = 1,
                    title = "CC")
        
      })
      
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$maptype=="A"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr",
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
        
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  observeEvent(input$map_shape_click,{
    if(input$maptype=="B"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr", 
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  observeEvent(input$map_shape_click, {
    if(input$maptype=="C"){
      
      event <- input$map_shape_click
      
      sub <-   filtered_world_polygon()[filtered_world_polygon()$nam_lng==event$id, c("nam_lng", "HghRskP", "NmHthPr", 
                                                                                      "num_dos", "VCstDs_b", "VCstDs_b")]
      if (is.null(event)){
        updateSelectInput(session,
                          "country", choices = filtered_world_polygon()$nam_lng)
      }else{
        updateSelectInput(session, inputId = "country", selected = sub$nam_lng)
      }
      
    }
  })
  
  long_lat <- reactive({
    subset(filtered_world_polygon(), nam_lng==input$country,
           select=c(long, lat))
  })
  
  observe({
    leafletProxy("map", data = filtered_world_polygon()) %>% 
      setView(lng = long_lat()$long, lat =long_lat()$lat ,zoom = 3.3)
  })
  
}

shinyApp(ui, server)