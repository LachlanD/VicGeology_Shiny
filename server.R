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
library(zoo)


#Too big to preload load dynamically instead
load("layer/geology_layer.RData")

#Set system variable
sf_use_s2(FALSE)
origin <- "1970-01-01" #R Epoch

shinyServer(function(input, output, session) {
    
    #Main map initialise on Vic
    map <- leaflet() %>%
        addTiles() %>%
        fitBounds(lng1 = 140.1, lng2 = 150.2, lat1 = -34.9, lat2 = -38.5)
    
    #Map for pop up windows
    pop_map <- leaflet() %>% 
        addTiles()
    
    
    
    #Inputs work better as globals 
    x <- 0
    xmin <- 0
    xmax <- 100
    x_axis <- 'd'
    
    # The selected file, if any
    userFile <- reactive({
        validate(need(input$gpx, message = FALSE))
        
        input$gpx
    })
    
    start <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()$time
        if(all(is.na(t)))
        {
            start <- "time missing from gpx file"
        } else {
            start <- as.character(as.POSIXct(min(t, na.rm = TRUE), origin = origin))
        }
        start
    })
    
    end <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()$time
        if(all(is.na(t)))
        {
            end <- " "
        } else {
            end <- as.character(as.POSIXct(max(t, na.rm = TRUE), origin = origin))
        }
        end
    })
    
    total_distance <- reactive({
        validate(need(trail(), message = FALSE))
        t<-trail()
        
        t$Dis[nrow(t)]
    })
    
    ele_colour <- reactive({
        validate(need(trail <- trail_d(), message = FALSE))
        
        viridis(max(trail$ele, na.rm = TRUE)-min(trail$ele, na.rm = TRUE), direction = -1, option = "plasma")  
    })
        
    
    #Load gpx file and calculate the cumulative distance
    trail <- reactive({
        validate(need(userFile(), message = FALSE))
        
        trail <- data.frame()
        trail <- sf::st_read(userFile()$datapath, layer = "track_points", quiet = TRUE)
        
        d<-sf::st_distance(trail)
        trail$Dis <- as.numeric(cumsum(d[1,]))/1000
        if(!all(is.na(trail$time))) {
            trail$time <- na.approx(as.numeric(trail$time))
            updateRadioButtons(
                session, 
                "x_axis", 
                selected = x_axis, 
                choiceNames = c("distance (m)", "time"), 
                choiceValues = c('d', 't'), 
                inline = TRUE
            )
        } else {
            x_axis <<- 'd'
            updateRadioButtons(
                session, 
                "x_axis", 
                selected = 'd', 
                choiceNames = c("distance (m)"), 
                choiceValues = c('d'), 
                inline = TRUE
            )
        }
        
        
        trail
    }) 
    
    trail_d <- debounce(trail, 500)
    
    cl <- reactive({
        validate(need(trail_grp(), message = FALSE))
        t <- trail_grp()
        
        v <- viridis(length(unique(t$NAME)), option = "turbo")
        names(v) <- unique(t$NAME)
        
        v
    })
    
    #Find the polygons that the gpx file pass through and group them into blocks
    trail_grp <- reactive({
        validate(need(trail <- trail_d(), message = FALSE))
        
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
    
    observeEvent(input$x_axis,{
        x_axis <<- input$x_axis
        
        t <- trail()
        
        if(x_axis == 'd') {
            d <- t$Dis
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
        } else {
            xmin <<- min(t$time, na.rm = TRUE)
            xmax <<- max(t$time, na.rm = TRUE)
            updateSliderInput(session, 
                              "range",  
                              value = as.POSIXct(c(xmin, xmax), origin = origin), 
                              min = as.POSIXct(xmin, origin = origin),
                              max = as.POSIXct(xmax, origin = origin))
        }
        
    })
    

    #When the gpx file is loaded update the map
    observe({
       t <- trail()
       bb <- as.numeric(st_bbox(t))

       t <- t %>%
           st_combine() %>%
           st_cast(to = "LINESTRING") %>%
           st_sf()

        leafletProxy("main_map", session) %>%
            fitBounds(lng1 = bb[1], lng2 = bb[3], lat1 = bb[2], lat2 = bb[4]) %>%
            addPolylines(data = t)
    }, priority = 1)
    
    # Pop up info window when the plot is double clicked
    observeEvent(input$plot_click, {
        validate(need(input$plot_click, message = FALSE))
        
        x <<- input$plot_click$x
        showModal(dataModal())
    })
    
    # Update the the slider to match the input file and x-axis selection
    observe({
        t <- trail()
        
        if(x_axis == 'd') {
            d <- t$Dis
            xmin <<- 0
            xmax <<- ceiling(max(d))
            updateSliderInput(session, "range", value = c(xmin, xmax), min = 0, max = xmax)
        } else {
            xmin <<- min(t$time, na.rm = TRUE)
            xmax <<- max(t$time, na.rm = TRUE)
            updateSliderInput(session, 
                              "range",  
                              value = as.POSIXct(c(xmin, xmax), origin = origin), 
                              min = as.POSIXct(xmin, origin = origin),
                              max = as.POSIXct(xmax, origin = origin))
        }
    })
    
    
    #Zoom in when plot is brushed 
    observeEvent(input$plot_brush,{
        val<-input$plot_brush
        t<-trail_d()
        
        if(x_axis == 'd') {
            xmin <<- max(val$xmin, 0)
            xmax <<- min(val$xmax, max(t$Dis))
            updateSliderInput(session, "range",  value = c(xmin,xmax))
        } else {
            r <- range(t$time, na.rm = TRUE)
            xmin <<- max(as.numeric(val$xmin), min(t$time, na.rm = TRUE))
            xmax <<- min(as.numeric(val$xmax), max(t$time, na.rm = TRUE))
            updateSliderInput(session, "range", value = as.POSIXct(c(xmin, xmax), origin = origin))
        }
    })
    

    #Update the plot when the selected range changes
    observeEvent(input$range, {
        validate(need(trail <- trail(), message = FALSE),need(input$range, message = FALSE))
        t <- trail()
        
        if(x_axis == 'd'){
            xmin <<- max(input$range[1], 0)
            xmax <<- min(input$range[2], max(t$Dis))

            start <- st_coordinates(t[t$Dis >= xmin,][1,])
            end <- st_coordinates(t[t$Dis >= xmax,][1,])
        } else {
            
            xmin <<- max(as.numeric(input$range[1]), min(t$time, na.rm = TRUE))
            xmax <<- min(as.numeric(input$range[2]), max(t$time, na.rm = TRUE))
            
            start <- st_coordinates(t[t$time >= xmin,][1,])
            end <- st_coordinates(t[t$time >= xmax,][1,])
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
        validate(need(input$range, message = FALSE))
        
        session$resetBrush("plot_brush")
        
        t <- trail_grp()
        
        if(x_axis == 'd') {
            gg <- ggplot(data = t, aes(fill=NAME, group = GEO_GRP)) + 
                geom_ribbon(aes(x=Dis, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                labs(x ="Distance (m)", y = "Elevation (m)") +
                xlim(xmin, xmax) + 
                scale_fill_manual(values = cl())
        } else {
            geo <- t[!is.na(t$time),]
            gg <- ggplot(data = t, aes(fill=NAME, group = GEO_GRP)) + 
                geom_ribbon(aes(x = as.POSIXct(time, origin = origin, tz = "Australia/Victoria"), ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                labs(x ="Time", y = "Elevation (m)") +
                xlim(as.POSIXct(xmin, origin = origin, tz = "Australia/Victoria"), as.POSIXct(xmax, origin = origin, tz = "Australia/Victoria"))  + 
                scale_fill_manual(values = cl())
        }
        gg
    })
    
    # Pop window
    dataModal <- function() {

        t <- trail_grp()
        c <- input$plot_click
        
        
        if(x_axis == 'd'){
            dat <- t[t$Dis>=x,][1,]
        } else {
            dat <- t[t$time>=x,][1,]
        }
        
        
        leaf_out <- leafletOutput("pop_map")
        
        if(!is.null(leaf_out)){
            modalDialog(
                tags$div(tags$b("Name: "), dat$NAME, tags$br(), 
                         tags$b("Type: "), dat$GEOLUT, tags$br(),
                         tags$b("Rank: "), dat$RANK, tags$br(),
                         tags$b("Lithology: "), dat$LITHOLOGY, tags$br(), 
                         tags$b("Description: "), dat$DESC, tags$br(), 
                         tags$b("History: "), dat$GEOLHIST),
                leaf_out,
                
                tags$a(href=dat$REPLIT_URI, "Lithology", target="_blank"),
                tags$a(href=dat$GEOLUT_URI, "Formation", target="_blank"),
                tags$a(href=dat$REPAGE_URI, "Age", target="_blank"),
                easyClose = TRUE
        )}
    }
    
    output$pop_map <- renderLeaflet({
        c <- input$plot_click
        
        t <- trail_grp()
        
        if(x_axis == 'd'){
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
    
    output$info <- renderUI({ 
        validate(need(start(), message = FALSE), need(end(), message = FALSE), need(total_distance(), message = FALSE))
        
        tags$div(tags$b("Start: "), start(), tags$br(), 
                 tags$b("End: "), end(), tags$br(), 
                 tags$b("Total distance: "), round(total_distance()), "m")
    })
    
    output$main_map <- renderLeaflet(map)
    
})


    
