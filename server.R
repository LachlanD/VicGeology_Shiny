#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(lwgeom)
library(leaflet)
library(sf)
library(grid)
library(gridExtra)
library(viridis)


#Too big to preload load dynamically instead
load("layer/geology_layer.RData")

#Set system variable
sf_use_s2(FALSE)


shinyServer(function(input, output, session) {
    
    #Main map initialise on Vic
    map <- leaflet() %>%
        addTiles() %>%
        fitBounds(lng1 = 140.1, lng2 = 150.2, lat1 = -34.8, lat2 = -38.5)
    
    #Map for pop up windows
    pop_map <- leaflet() %>% 
        addTiles()
    
    
    
    #global click value maybe
    x<-0
    
    # The selected file, if any
    userFile <- reactive({
        validate(need(input$gpx, message = FALSE))
        
        input$gpx
    })
    
    #Load gpx file and calculate the cumulative distance
    trail <- reactive({
        validate(need(userFile(), message = FALSE))
        
        trail <- data.frame()
        trail <- sf::st_read(userFile()$datapath, layer = "track_points", quiet = TRUE)
        
        #remove bad points
        #trail <- trail[!is.na(trail$time),]
        
        d<-sf::st_distance(trail)
        trail$Dis <- as.numeric(cumsum(d[1,]))/1000
        
        # Update map
        bb <- as.numeric(st_bbox(trail))
        
        t <- trail %>% 
            st_combine() %>%
            st_cast(to = "LINESTRING") %>%
            st_sf()
        
        leafletProxy("main_map", session) %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolylines(data = t)
        
        trail
    }) %>% debounce(500)
    
    cl <- reactive({
        validate(need(trail_grp(), message = FALSE))
        t <- trail_grp()
        
        v <- viridis(length(unique(t$NAME)), option = "turbo")
        names(v) <- unique(t$NAME)
        
        v
    })
    
    #Find the polygons that the gpx file pass through and group them into blocks
    trail_grp <- reactive({
        validate(need(trail <- trail(), message = FALSE))
        
        suppressMessages(trail<-sf::st_join(trail, gsf, join = st_nearest_feature))
        
        trail$GEO_GRP<-1
        
        d<-trail[1,]$ID
        grp<-1
        for(i in 1:nrow(trail)-1){
            if(!(d %in% trail[i+1,]$ID)) {
                grp<-grp+1
                d<-trail[i+1,]$ID
            } 
            trail[i+1,]$GEO_GRP<-grp
        }
        
        trail
    })
    
    #trail_grp<-debounce(trail_g, 1000)
    
    
    ## Moved inside the trail reactive
    # When the gpx file is loaded update the map
    #observe({
    #    t <- trail()
    #    bb <- as.numeric(st_bbox(t))
    #    
    #    t <- t %>% 
    #        st_combine() %>%
    #        st_cast(to = "LINESTRING") %>%
    #        st_sf()
    #    
    #     leafletProxy("main_map", session) %>%
    #         fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
    #         addPolylines(data = t)
    # }, priority = 1)
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$plot_click, {
        validate(need(input$plot_click, message = FALSE))
        
        x <<- input$plot_click$x
        showModal(dataModal())
    })
    
    # Update the the slider to match the input file and x-axis selection
    observe({
        t <- trail()
        if(input$x_axis == 'd') {
            d <- t$Dis
            updateSliderInput(session, "range",  value = c(0, ceiling(max(d))), max = ceiling(max(d)))
        } else {
            d <- range(t$time, na.rm = TRUE)
            updateSliderInput(session, "range", value = d,min = d[1], max = d[2])
        }
    })
    
    #Zoom in when plot is brushed 
    observeEvent(input$plot_brush,{
        val<-input$plot_brush
        
        xmin<-val$xmin
        xmax<-val$xmax
        
        updateSliderInput(session, "range",  value = c(xmin,xmax))
    })
    
    #Update the plot when the selected range changes
    observeEvent(input$range, {
        validate(need(trail <- trail(), message = FALSE))
        
        t <- trail()
        if( input$x_axis == 'd'){
            A <- max(input$range[1], 0)
            B <- min(input$range[2], max(t$Dis))
        
            start <- st_coordinates(t[t$Dis >= A,][1,])
            end <- st_coordinates(t[t$Dis >= B,][1,])
        } else {
            r <- range(t$time, na.rm=TRUE)
            A <- max(input$range[1], r[1])
            B <- min(input$range[2], r[2])
            
            start <- st_coordinates(t[t$time >= A,][1,])
            end <- st_coordinates(t[t$time >= B,][1,])
        }
        
        
        # Also show the start and end points on map
        leafletProxy("main_map", session) %>%
            clearMarkers() %>% 
            addCircleMarkers(lng = start[1], lat = start[2], color = "green", fillOpacity = 0.5) %>%
            addCircleMarkers(lng = end[1], lat = end[2], color = "red", fillOpacity = 0.5)
        
    })
    
    # Render the geo plot
    output$elePlot <- renderPlot({
        validate(need(trail_grp(), message = FALSE))
        validate(need(cl(), message = FALSE))
        session$resetBrush("plot_brush")
        
        A <- input$range[1]
        B <- input$range[2]
        t <- trail_grp()
        
        
        if(input$x_axis == 'd') {
            ggplot(data = t, aes(fill=NAME, group = GEO_GRP)) + 
                geom_ribbon(aes(x=Dis, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(A,B) + scale_fill_manual(values = cl())
        } else {
            geo <- t[!is.na(t$time),]
            ggplot(data = t, aes(fill=NAME, group = GEO_GRP)) + 
                geom_ribbon(aes(x=time, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                labs(x ="Time", y = "Elevation (m)") +
                xlim(A,B)
        }
    })
    
    # Pop window
    dataModal <- function() {

        t <- trail_grp()
        c <- input$plot_click
        
        
        if(input$x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        
        lo <- leafletOutput("pop_map")
        
        if(!is.null(lo)){
            modalDialog(
                tags$div(tags$b("Name: "), dat$NAME, tags$br(), tags$b("Lithology: "), dat$LITHOLOGY, tags$br(), tags$b("Description: "), dat$DESC, tags$br(), tags$b("History: "), dat$GEOLHIST),
                
                lo,
                
                tags$a(href=dat$SPECIF_URI, "Geology Vic URI", target="_blank")
        )}
    }
    
    output$pop_map <- renderLeaflet({
        c <- input$plot_click
        
        t <- trail_grp()
        
        if(input$x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        dat <- t[t$GEO_GRP==dat$GEO_GRP,]
        bb <- as.numeric(st_bbox(dat))
        
        line <- dat %>% 
            sf::st_combine() %>%
            sf::st_cast(to = "LINESTRING", warn = FALsE) %>%
            sf::st_sf()
        
        
        
        poly <- gsf[gsf$ID %in% dat$ID,]$geometry
        
        poly <- poly %>%
            st_cast(to = "POLYGON") %>%
            st_sf()
        
        pop_map %>%
            clearShapes() %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolygons(data = poly, fillColor = as.character(cl()[dat$NAME[1]]), fillOpacity =  0.3, stroke = FALSE) %>%
            addPolylines(data = line)  
    })
    
    output$main_map <- renderLeaflet(map)
    
})


    
