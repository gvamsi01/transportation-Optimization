#installing the packages for shiny dashboard
if(!require(shiny)){install.packages("shiny")}
if(!require(ggmap)){install.packages("ggmap")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(devtools)){install.packages("devtools")}
if(!require(shinythemes)){install.packages("shinythemes")}
#if(!require(leaflet)){devtools::install_github('rstudio/leaflet')}
if(!require(plotly)){install.packages("plotly")}
if(!require(fields)){install.packages("fields")}

#Generating the shiny dashboard
shinyServer(
  function(input,output,session){
    
    #reading only paid transactions
    transaction=read.csv("Paid Transactions.csv")
    first_record=read.csv("first_record.csv")
    trip=read.csv("Trip Sheet final_1.csv")
    
    #Generating map for All source and destinations
    output$map = renderLeaflet({
      
      geo_loc1=geocode(location = input$loc1,output = "latlon",source="google")
      geo_loc2=geocode(location = input$loc2,output = "latlon",source="google")
      
      colnames(geo_loc1)=c("loc1_long","loc1_lat")
      colnames(geo_loc2)=c("loc2_long","loc2_lat")
      
      Transaction=subset(transaction,Hour==input$hour)
      Transactions=cbind(Transaction,geo_loc1,geo_loc2)
      
      #to calculate distance between points on the earth
      r=6378137
      x=mutate(Transactions,diff_lat1=abs(lat-geo_loc1$loc1_lat)*pi/180,diff_long1=abs(long-geo_loc1$loc1_long)*pi/180)
      x=mutate(x,diff_lat2=abs(lat-geo_loc2$loc2_lat)*pi/180,diff_long2=abs(long-geo_loc2$loc2_long)*pi/180)
      x=mutate(x,lat1=lat*pi/180,loc1_lat_rad=geo_loc1$loc1_lat*pi/180,loc2_lat_rad=geo_loc2$loc2_lat*pi/180)
      x=mutate(x,a=sin(diff_lat1/2)^2+cos(lat1)*cos(loc1_lat_rad)*sin(diff_long1/2)^2)
      x=mutate(x,b=sin(diff_lat2/2)^2+cos(lat1)*cos(loc2_lat_rad)*sin(diff_long2/2)^2)
      x=mutate(x,distance_1=2*r*atan2(sqrt(a),sqrt(1-a)),distance_2=2*r*atan2(sqrt(b),sqrt(1-b)))
      distance = x[,-c(15:27)]
      
      #Distance=subset(distance,Hour==input$hour)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     
      Loc1=subset(distance,distance_1 <= input$rad_1)
      Loc2=subset(distance,distance_2 <= input$rad_2)
      
      #giving separate images to status for both locations
      CarIcon_1=awesomeIcons(library = "ion",markerColor = "cadetblue",iconColor = "yellow",
                             icon = ifelse(Loc1$status=="POB","flag",
                                           ifelse(Loc1$status=="PAYMENT","home","android-car")))
      CarIcon_2=awesomeIcons(library = "ion",markerColor = "cadetblue",iconColor = "yellow",
                             icon = ifelse(Loc2$status=="POB","flag",
                                           ifelse(Loc2$status=="PAYMENT","home","android-car")))
      
      #adding pop-up to the markers
      carInfo_1 = paste (Loc1$taxi_no, "<br>",Loc1$Region,"<br>",Loc1$Date,Loc1$Time, Loc1$status )
      carInfo_2 = paste (Loc2$taxi_no, "<br>",Loc2$Region,"<br>",Loc2$Date,Loc2$Time, Loc2$status )
      
      #drawing the map for visualization
      leaflet() %>%
        addTiles(group="Open Street Maps") %>%
        addProviderTiles("CartoDB.Positron",group="CartoDB.Positron") %>%
        #addProviderTiles("Stamen.TonerLite",group="Stamen.TonerLite") %>%
        setView(103.800,1.3300,zoom = 12) %>%
        addAwesomeMarkers(lng = Loc1$long,lat = Loc1$lat,clusterOptions = markerClusterOptions(),popup= carInfo_1,group="Zone_1",icon=CarIcon_1) %>%
        addAwesomeMarkers(lng = Loc2$long,lat = Loc2$lat,clusterOptions = markerClusterOptions(),popup= carInfo_2,group="Zone_2",icon=CarIcon_2) %>%
        addCircles(lng = geo_loc1$loc1_long,geo_loc1$loc1_lat,radius = input$rad_1,popup = paste("Zone_1","<br>","radius:",input$rad_1),fillOpacity = 0,group="Boundary") %>%
        addCircles(lng = geo_loc2$loc2_long,geo_loc2$loc2_lat,radius = input$rad_2,popup = paste("Zone_2","<br>","radius:",input$rad_2),fillOpacity = 0,group="Boundary") %>%
        #addPolylines(lng = d$long,lat = d$lat,color = "red",fillOpacity = 0.3) %>%
        addMeasure() %>%
        #Layers Control
        addLayersControl(
          baseGroups=c("Positron","Open Street Maps"),
          options=layersControlOptions(collapsed = F),
          overlayGroups=c("Zone_1", "Zone_2","Boundary")
        )   
      
    }) #ending of first leaflet map
    
    #Generating map for commuters starting from source and going to destination
    output$map_1 = renderLeaflet({
      
      geo_loc1=geocode(location = input$loc1,output = "latlon",source="google")
      geo_loc2=geocode(location = input$loc2,output = "latlon",source="google")
      
      colnames(geo_loc1)=c("loc1_long","loc1_lat")
      colnames(geo_loc2)=c("loc2_long","loc2_lat")
      
      First_record=subset(first_record,Hour==input$hour)
      first_records=cbind(First_record,geo_loc1,geo_loc2)
      
      #to calculate distance between points on the earth
      r=6378137
      y=mutate(first_records,diff_lat1=abs(lat-geo_loc1$loc1_lat)*pi/180,diff_long1=abs(long-geo_loc1$loc1_long)*pi/180)
      y=mutate(y,diff_lat2=abs(lat-geo_loc2$loc2_lat)*pi/180,diff_long2=abs(long-geo_loc2$loc2_long)*pi/180)
      y=mutate(y,lat1=lat*pi/180,loc1_lat_rad=geo_loc1$loc1_lat*pi/180,loc2_lat_rad=geo_loc2$loc2_lat*pi/180)
      y=mutate(y,a=sin(diff_lat1/2)^2+cos(lat1)*cos(loc1_lat_rad)*sin(diff_long1/2)^2)
      y=mutate(y,b=sin(diff_lat2/2)^2+cos(lat1)*cos(loc2_lat_rad)*sin(diff_long2/2)^2)
      y=mutate(y,distance_1=2*r*atan2(sqrt(a),sqrt(1-a)),distance_2=2*r*atan2(sqrt(b),sqrt(1-b)))
      first_record_distance = y[,-c(15:27)]
      
      #executing a loop to extract only source and destinations
      exp=NULL
      status_pob="NONE"
      status_payment="NONE"
      count=0
      selected=0
      for (i in 1:nrow(first_record_distance)){
        if (first_record_distance$status[i]=="POB"){
          if (first_record_distance$status[i]!= status_pob){
            if (first_record_distance$distance_1[i]<= input$rad_1){
              status_pob= first_record_distance$status[i]
              status_payment="NONE"
              selected=1
              exp=rbind(exp,first_record_distance[i,])
            }
          }
        }
        else {
          if (first_record_distance$status[i]=="PAYMENT"){
            if (first_record_distance$status[i] != status_payment & selected==1){
              status_payment= first_record_distance$status[i]
              status_pob = "NONE"
              selected = 0
              exp=rbind(exp,first_record_distance[i,])
            }
          }
        }
      }
      
      if (nrow(as.data.frame(exp)) != 0){
        
        #subsetting the data related to source and destination
        Source=subset(exp,status=="POB")
        dest=subset(exp,status=="PAYMENT")
        
        #adding markers to the places
        CarIcon_Source=awesomeIcons(library = "ion",markerColor = "cadetblue",iconColor = "yellow", icon = "flag")
        CarIcon_dest=awesomeIcons(icon = "home",library = "ion", markerColor = "red",iconColor = "white")
        
        #adding pop-up to the markers
        carInfo_Source = paste (Source$taxi_no, "<br>",Source$Region,"<br>",Source$status,"<br>",Source$Date,Source$Time)
        carInfo_dest = paste (dest$taxi_no, "<br>",dest$Region,"<br>",dest$status, "<br>",dest$Date,dest$Time)
        
        
        #plotting the points on the map
        leaflet() %>%
          #adding base layers to the map
          addTiles(group="Open Street Maps") %>%
          addProviderTiles("CartoDB.Positron",group="CartoDB.Positron") %>%
          setView(103.800,1.3300,zoom = 12) %>%
          #adding markers to the map
          addAwesomeMarkers(lng = Source$long,lat = Source$lat,icon = CarIcon_Source,clusterOptions = markerClusterOptions(),popup = carInfo_Source, group = "Source") %>%
          addAwesomeMarkers(lng = dest$long,lat = dest$lat,icon = CarIcon_dest,clusterOptions = markerClusterOptions(),popup = carInfo_dest, group="Destination") %>%
          #adding the circular radius to the maps 
          addCircles(lng = geo_loc1$loc1_long,geo_loc1$loc1_lat,radius = input$rad_1,popup = paste("Source :",input$loc1,"<br>","radius:",input$rad_1),fillOpacity = 0,group="Boundary") %>%
          addCircles(lng = geo_loc2$loc2_long,geo_loc2$loc2_lat,radius = input$rad_2,popup = paste("Destination:",input$loc2,"<br>","radius:",input$rad_2),fillOpacity = 0,group="Boundary") %>%
          addMeasure() %>%
          #adding Layers to the map
          addLayersControl(
            baseGroups=c("Positron","Open Street Maps"),
            options=layersControlOptions(collapsed = F),
            overlayGroups=c("Source","Destination","Boundary")
          )
      }else {
        content = paste(sep= "<br/>","<b>","<i>","There are no commuters travelling from the designated place","<b>","<i>")
        leaflet() %>%
          addTiles(group="Open Street Maps") %>%
          addProviderTiles("CartoDB.Positron",group="CartoDB.Positron") %>%
          setView(103.800,1.3300,zoom=12) %>%
          addMeasure() %>% 
          addPopups(geo_loc1$loc1_long,geo_loc1$loc1_lat,content,options=popupOptions(closeButton=F,maxWidth=500,maxHeight = 65,zoomAnimation = T)) %>%
          #addLegend(position = "bottomright",title="There are no records in the designated area choosen by you",pal = qpal) %>%
          addLayersControl(
            baseGroups=c("Positron","Open Street Maps"),
            options = layersControlOptions(collapsed=F)
          )
      }
    })  #ending of second leaflet map
    
    #printing the table 
    output$mytable = renderDataTable(trip,options = list(pageLength = 10))
    
    #Generating plot for hour
    output$trend = renderPlotly({
      First_record=subset(first_record,Hour==input$hour_)
      
      counts=table(First_record$status,First_record$Region)
      
      f1 =  list(family = "Arial, sans-serif",size = 18,color = "lightgrey")
      f2 = list(family = "Old Standard TT, serif",size = 14,color = "black")
      
      a = list(
        title = "No of Commuters at that particular hour",
        titlefont = f1,showticklabels = TRUE,
        tickangle = 45,tickfont = f2,
        #exponentformat = "e",
        showexponent = "All"
      )
      
      Region=c("Central","East","North","North-East","West")
      No.of_travellers = c(counts[1,1],counts[1,2],counts[1,3],counts[1,4],counts[1,5])
      p= plot_ly(x = Region,y = No.of_travellers,name = "PAYMENT",type = "bar") %>%
        add_trace(x = c("Central","East","North","North-East","West"),
                  y = c(counts[2,1],counts[2,2],counts[2,3],counts[2,4],counts[2,5]),name = "POB",type="bar") %>%
        layout(title = "No of travellers in a particular hour")
      
    }) #ending of plotyly for trend
    
    output$North = renderPlotly({
      
      First_record=subset(first_record,Hour==input$hour_)
      exp_north=NULL
      status_pob="NONE"
      status_payment="NONE"
      count=0
      selected=0
      for (i in 1:nrow(First_record)){
        if (First_record$status[i]=="POB" & First_record$Region[i]==input$sour){
          if (First_record$status[i]!= status_pob){
            status_pob= First_record$status[i]
            status_payment="NONE"
            selected=1
            exp_north=rbind(exp_north,First_record[i,])
          }
        }
        else {
          if (First_record$status[i]=="PAYMENT"){
            if (First_record$status[i] != status_payment & selected==1){
              status_payment= First_record$status[i]
              status_pob = "NONE"
              selected = 0
              exp_north=rbind(exp_north,First_record[i,])
            }
          }
        }
      }  
      x=table(exp_north$Region,exp_north$status)
      x=as.data.frame(x[,1])
      Region=c("Central","East","North","North-East","West")
      No.of_cabs_Central = c(x[1,1],x[2,1],x[3,1],x[4,1],x[5,1])
      plot_ly(labels=Region,values=No.of_cabs_Central,type="pie",showlegend=T) %>%
        layout(title = "Travellers from source Region")
      
    }) #ending of plotyly for north region
    
    output$North_East = renderPlotly({
      
      First_record=subset(first_record,Hour==input$hour_)
      exp_northeast=NULL
      status_pob="NONE"
      status_payment="NONE"
      count=0
      selected=0
      for (i in nrow(First_record):1){
        if (First_record$status[i]=="PAYMENT" & First_record$Region[i]==input$dest){
          if (First_record$status[i]!= status_pob){
            status_payment= First_record$status[i]
            status_pob="NONE"
            selected=1
            exp_northeast=rbind(exp_northeast,First_record[i,])
          }
        }
        else {
          if (First_record$status[i]=="POB"){
            if (First_record$status[i] != status_payment & selected==1){
              status_pob= First_record$status[i]
              status_payment = "NONE"
              selected = 0
              exp_northeast=rbind(exp_northeast,First_record[i,])
            }
          }
        }
      }
      b=table(exp_northeast$Region,exp_northeast$status)
      b=as.data.frame(b[,2])
      Region=c("Central","East","North","North-East","West")
      No.of_cabs_northeast = c(b[1,1],b[2,1],b[3,1],b[4,1],b[5,1])
      plot_ly(labels=Region,values=No.of_cabs_northeast,type="pie",showlegend=T) %>%
        layout(title = "Travellers to Destination Region")
      
    }) #ending of plotly for north-east region
    

  } #ending of function braces
) #ending of shiny server brackets