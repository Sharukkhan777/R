library(shiny)
library(shinyjs)
library(shinythemes)
library(leaflet)
library(dplyr)



source("others/amenities calculation.R")
source("others/ml_data_create.R")
library(reticulate)
reticulate::source_python("others/predict_result.py")

#========================================
#========================================
#------------HELPERS FUNCTION------------
#========================================
#========================================

#========================================
#----------------Take coordinate from google link-------------------
take_coordinates <- function(link){
  # link = "https://www.google.com/maps/@10.7560541,73.4225996,891970m/data=!3m1!1e3"
  link <- toString(link)
  link <- strsplit(link,split='@', fixed=TRUE)
  link <- link[[1]][2]
  link <- strsplit(link,split=',', fixed=TRUE)
  options(digits = 9)
  lati <- as.numeric(link[[1]][1])
  longi <- as.numeric(link[[1]][2])
  latilongi <- c(lati,longi)
  return(latilongi)
}

#====================================
# Other Operations


ui <- fluidPage(theme = shinytheme("darkly"),
                 # shinyjs::inlineCSS(css),
                 shinyjs::useShinyjs(),
                 includeCSS("www/style.css"),
                navbarPage(id = "inTabset",
                 title = "Make My Hotel",
                 #######################
                 #-------Page 1---------
                 #######################
                 tabPanel("Lati-Longi", 
                          fluidPage(
                            HTML('<img src="PAGE 1 MAP.gif" loop="infinite" style="width:800px;height:70px;">'),
                            br(),
                          sidebarLayout(
                            sidebarPanel(
                              
                              h4("Go to Google Maps"),
                              HTML('
                                <a href="https://www.google.com/maps/@9.3339589,75.7656699,348867m/data=!3m1!1e3" target="_blank">
                                  <img src="gmap_icon.svg" alt="Gmaps" width="100" height="150">
                                </a>'
                              ),
                              h4("Pin your desired location and paste the link below"),
                              textInput(inputId = "latilongilink",label = "Paste the Link Here"),
                            actionButton(inputId = "linksubmit",label = "Submit"),
                            br()
                            ),
                            mainPanel(
                        
                              leafletOutput("my_leaf"),
                              br(),
                            ),)),
                          fluidPage(
                            actionButton(inputId = "gotopage2", class = "next-css",icon = icon("check-square"),label = "NEXT")
                          ),
                          br(),
                          br(),
                          br()
                          ),
                   
                 ######################
                 # -------Page 2---------
                 ######################
                 tabPanel("Competitors",
                            div(id="opacity_50_dark",
                                HTML('<img src="PAGE COMPE.gif" loop="infinite" class="center" style="width:830px;height:70px;">'),
                                br(),
                                br(),
                                h3("Competitors within 0.25 km"),
                                dataTableOutput(outputId = "comp_table_025"),
                                hr(),
                                h3("Average Cost For 2"),
                                sliderInput(inputId = "acft", label = "You can refer you competitors above", min = 10, max = 10000, value = 500, step = NULL),
                                actionButton(inputId = "gotopage3", class = "next-css",icon = icon("check-square"),label = "NEXT"),
                                br(),
                                br(),
                                br()
                            )
                 #
                 ),
                 #######################
                 #-------Page 2---------
                 #######################        
                 tabPanel("Amenities", 
                          fluidPage(
                            div(id="opacity_50_dark",
                              HTML('<img src="PAGE 2 AMENITY.gif" loop="infinite" style="width:800px;height:70px;">'),
                              br(),
                              br(),
                              h3("Amenities within 0.25 km"),
                              dataTableOutput(outputId = "ame_table_025"),
                              hr(),
                              h3("Amenities within 0.5 km"),
                              dataTableOutput(outputId = "ame_table_05"),
                              hr(),
                              h3("Amenities within 1 km"),
                              dataTableOutput(outputId = "ame_table_1"),
                              hr(),
                              h3("Amenities within 2 km"),
                              dataTableOutput(outputId = "ame_table_2"),
                              hr(),
                              h3("Amenities within 5 km"),
                              dataTableOutput(outputId = "ame_table_5"),
                              hr(),
                              br(),
                              br(),
                              actionButton(inputId = "gotopage4", class = "next-css",icon = icon("check-square"),label = "NEXT"),
                              br(),
                              br(),
                              br()
                            )
                          )
                 ),

                 #######################
                 #-------Page 3---------
                 #######################        
                 tabPanel("Establishment", div(id="opacity_50_dark",
                                               HTML('<img src="PAGE EST.gif" loop="infinite" style="width:830px;height:70px;">'),
                                               br(),
                                               br(),
                                               radioButtons(inputId = "est",
                                                            label = "Select One Establishment: ",
                                                            choices = c("_- Bakery" = "Bakery_est",
                                                                        "_- Bar" = "Bar_est",
                                                                        "_- Beverage Shop" = "Beverage Shop_est",
                                                                        "_- Bhojanalya" = "Bhojanalya_est",
                                                                        "_- Butcher Shop" = "Butcher Shop_est",
                                                                        "_- Cafe" = "Cafe_est",
                                                                        "_- Casual Dining" = "Casual Dining_est",
                                                                        "_- Club" = "Club_est",
                                                                        "_- Confectionery" = "Confectionery_est",
                                                                        "_- Dessert Parlour" = "Dessert Parlour_est",
                                                                        "_- Dhaba" = "Dhaba_est",
                                                                        "_- Fine Dining" = "Fine Dining_est",
                                                                        "_- Food Court" = "Food Court_est",
                                                                        "_- Food Truck" = "Food Truck_est",
                                                                        "_- Kiosk" = "Kiosk_est",
                                                                        "_- Lounge" = "Lounge_est",
                                                                        "_- Mess" = "Mess_est",
                                                                        "_- Paan Shop" = "Paan Shop_est",
                                                                        "_- Pub" = "Pub_est",
                                                                        "_- Quick Bites" = "Quick Bites_est",
                                                                        "_- Shack" = "Shack_est",
                                                                        "_- Sweet Shop" = "Sweet Shop_est"    
                                                            )),
                                               
                                               #next button
                                               actionButton(inputId = "gotopage5", class = "next-css",icon = icon("check-square"),label = "NEXT"),
                                               br(),
                                               br(),
                                               br()
                                               )
                          ),
                 #######################
                 #-------Page 4---------
                 #######################        
                 tabPanel("Cuisines", div(id="opacity_50_dark",
                                          HTML('<img src="PAGE CUISINES.gif" loop="infinite" style="width:830px;height:70px;">'),
                                          br(),
                                          br(),
                                          checkboxGroupInput(inputId = "cusi",
                                                             label = "You can select multiple cusines:",
                                                             width =  "400px",
                                                             choices = c("_` Afghan" = "Afghan_cusi",
                                                                         "_` African" = "African_cusi",
                                                                         "_` American" = "American_cusi",
                                                                         "_` Andhra" = "Andhra_cusi",
                                                                         "_` Arabian" = "Arabian_cusi",
                                                                         "_` Armenian" = "Armenian_cusi",
                                                                         "_` Asian" = "Asian_cusi",
                                                                         "_` Awadhi" = "Awadhi_cusi",
                                                                         "_` Bakery" = "Bakery_cusi",
                                                                         "_` Bar Food" = "Bar Food_cusi",
                                                                         "_` BBQ" = "BBQ_cusi",
                                                                         "_` Belgian" = "Belgian_cusi",
                                                                         "_` Bengali" = "Bengali_cusi",
                                                                         "_` Beverages" = "Beverages_cusi",
                                                                         "_` Bihari" = "Bihari_cusi",
                                                                         "_` Biryani" = "Biryani_cusi",
                                                                         "_` British" = "British_cusi",
                                                                         "_` Bubble Tea" = "Bubble Tea_cusi",
                                                                         "_` Burger" = "Burger_cusi",
                                                                         "_` Burmese" = "Burmese_cusi",
                                                                         "_` Cafe" = "Cafe_cusi",
                                                                         "_` Charcoal Chicken" = "Charcoal Chicken_cusi",
                                                                         "_` Chettinad" = "Chettinad_cusi",
                                                                         "_` Chinese" = "Chinese_cusi",
                                                                         "_` Coffee and Tea" = "Coffee and Tea_cusi",
                                                                         "_` Coffee" = "Coffee_cusi",
                                                                         "_` Continental" = "Continental_cusi",
                                                                         "_` Crepes" = "Crepes_cusi",
                                                                         "_` Cuisine Varies" = "Cuisine Varies_cusi",
                                                                         "_` Desserts" = "Desserts_cusi",
                                                                         "_` Drinks Only" = "Drinks Only_cusi",
                                                                         "_` European" = "European_cusi",
                                                                         "_` Fast Food" = "Fast Food_cusi",
                                                                         "_` Finger Food" = "Finger Food_cusi",
                                                                         "_` French" = "French_cusi",
                                                                         "_` Fried Chicken" = "Fried Chicken_cusi",
                                                                         "_` Fusion" = "Fusion_cusi",
                                                                         "_` Goan" = "Goan_cusi",
                                                                         "_` Greek" = "Greek_cusi",
                                                                         "_` Grill" = "Grill_cusi",
                                                                         "_` Gujarati" = "Gujarati_cusi",
                                                                         "_` Healthy Food" = "Healthy Food_cusi",
                                                                         "_` Hot dogs" = "Hot dogs_cusi",
                                                                         "_` Hyderabadi" = "Hyderabadi_cusi",
                                                                         "_` Ice Cream" = "Ice Cream_cusi",
                                                                         "_` Indian" = "Indian_cusi",
                                                                         "_` Indonesian" = "Indonesian_cusi",
                                                                         "_` Iranian" = "Iranian_cusi",
                                                                         "_` Irish" = "Irish_cusi",
                                                                         "_` Italian" = "Italian_cusi",
                                                                         "_` Japanese" = "Japanese_cusi",
                                                                         "_` Juices" = "Juices_cusi",
                                                                         "_` Kebab" = "Kebab_cusi",
                                                                         "_` Kerala" = "Kerala_cusi",
                                                                         "_` Konkan" = "Konkan_cusi",
                                                                         "_` Korean" = "Korean_cusi",
                                                                         "_` Lebanese" = "Lebanese_cusi",
                                                                         "_` Maharashtrian" = "Maharashtrian_cusi",
                                                                         "_` Malaysian" = "Malaysian_cusi",
                                                                         "_` Malwani" = "Malwani_cusi",
                                                                         "_` Mangalorean" = "Mangalorean_cusi",
                                                                         "_` Mediterranean" = "Mediterranean_cusi",
                                                                         "_` Mexican" = "Mexican_cusi",
                                                                         "_` Middle Eastern" = "Middle Eastern_cusi",
                                                                         "_` Mithai" = "Mithai_cusi",
                                                                         "_` Modern Indian" = "Modern Indian_cusi",
                                                                         "_` Momos" = "Momos_cusi",
                                                                         "_` Mongolian" = "Mongolian_cusi",
                                                                         "_` Moroccan" = "Moroccan_cusi",
                                                                         "_` Mughlai" = "Mughlai_cusi",
                                                                         "_` North Eastern" = "North Eastern_cusi",
                                                                         "_` North Indian" = "North Indian_cusi",
                                                                         "_` Paan" = "Paan_cusi",
                                                                         "_` Panini" = "Panini_cusi",
                                                                         "_` Parsi" = "Parsi_cusi",
                                                                         "_` Peruvian" = "Peruvian_cusi",
                                                                         "_` Pizza" = "Pizza_cusi",
                                                                         "_` Portuguese" = "Portuguese_cusi",
                                                                         "_` Rajasthani" = "Rajasthani_cusi",
                                                                         "_` Raw Meats" = "Raw Meats_cusi",
                                                                         "_` Roast Chicken" = "Roast Chicken_cusi",
                                                                         "_` Rolls" = "Rolls_cusi",
                                                                         "_` Russian" = "Russian_cusi",
                                                                         "_` Salad" = "Salad_cusi",
                                                                         "_` Sandwich" = "Sandwich_cusi",
                                                                         "_` Seafood" = "Seafood_cusi",
                                                                         "_` Singaporean" = "Singaporean_cusi",
                                                                         "_` South American" = "South American_cusi",
                                                                         "_` South Indian" = "South Indian_cusi",
                                                                         "_` Spanish" = "Spanish_cusi",
                                                                         "_` Sri Lankan" = "Sri Lankan_cusi",
                                                                         "_` Steak" = "Steak_cusi",
                                                                         "_` Street Food" = "Street Food_cusi",
                                                                         "_` Sushi" = "Sushi_cusi",
                                                                         "_` Tamil" = "Tamil_cusi",
                                                                         "_` Tea" = "Tea_cusi",
                                                                         "_` Tex-Mex" = "Tex-Mex_cusi",
                                                                         "_` Thai" = "Thai_cusi",
                                                                         "_` Tibetan" = "Tibetan_cusi",
                                                                         "_` Turkish" = "Turkish_cusi",
                                                                         "_` Vietnamese" = "Vietnamese_cusi",
                                                                         "_` Wraps" = "Wraps_cusi"
                                                                         
                                                             )),
                                          #next button
                                          actionButton(inputId = "gotopage6", class = "next-css",icon = icon("check-square"),label = "NEXT"),
                                          br(),
                                          br(),
                                          br()
                 )
                 ),
                 #######################
                 #-------Page 5---------
                 #######################        
                 tabPanel("Highlights", div(id="opacity_50_dark",
                                            HTML('<img src="PAGE HIGHLIGHTS.gif" loop="infinite" style="width:830px;height:70px;">'),
                                            br(),
                                            br(),
                                            checkboxGroupInput(inputId = "highlights",
                                                               label = "You can select multiple highlights:",
                                                               width =  "400px",
                                                               choices = c(
                                                                 "_` 4/5 Star" = "4/5 Star_hl",
                                                                 "_` Above 18 Only" = "Above 18 Only_hl",
                                                                 "_` Air Conditioned" = "Air Conditioned_hl",
                                                                 "_` All Day Breakfast" = "All Day Breakfast_hl",
                                                                 "_` Available for Functions" = "Available for Functions_hl",
                                                                 "_` Axis Bank - Dining Delights" = "Axis Bank - Dining Delights_hl",
                                                                 "_` Beer" = "Beer_hl",
                                                                 "_` Board Games" = "Board Games_hl",
                                                                 "_` Breakfast" = "Breakfast_hl",
                                                                 "_` Brunch" = "Brunch_hl",
                                                                 "_` Buffet" = "Buffet_hl",
                                                                 "_` Bulk Orders Accepted" = "Bulk Orders Accepted_hl",
                                                                 "_` BYOB" = "BYOB_hl",
                                                                 "_` Card Upon Delivery" = "Card Upon Delivery_hl",
                                                                 "_` Cash" = "Cash_hl",
                                                                 "_` Catering Available" = "Catering Available_hl",
                                                                 "_` City View" = "City View_hl",
                                                                 "_` Couple Entry Only" = "Couple Entry Only_hl",
                                                                 "_` Craft Beer" = "Craft Beer_hl",
                                                                 "_` Credit Card" = "Credit Card_hl",
                                                                 "_` Dance Floor" = "Dance Floor_hl",
                                                                 "_` Dark Kitchen" = "Dark Kitchen_hl",
                                                                 "_` Debit Card" = "Debit Card_hl",
                                                                 "_` Delivery" = "Delivery_hl",
                                                                 "_` Desserts and Bakes" = "Desserts and Bakes_hl",
                                                                 "_` Digital Payments Accepted" = "Digital Payments Accepted_hl",
                                                                 "_` Dinner" = "Dinner_hl",
                                                                 "_` Disabled Friendly" = "Disabled Friendly_hl",
                                                                 "_` DJ" = "DJ_hl",
                                                                 "_` Drive in" = "Drive in_hl",
                                                                 "_` Drive Thru" = "Drive Thru_hl",
                                                                 "_` Entry Fee" = "Entry Fee_hl",
                                                                 "_` Free Parking" = "Free Parking_hl",
                                                                 "_` Free Wifi" = "Free Wifi_hl",
                                                                 "_` Fullbar" = "Fullbar_hl",
                                                                 "_` Gaming Area" = "Gaming Area_hl",
                                                                 "_` Gastro Pub" = "Gastro Pub_hl",
                                                                 "_` Gin Bar" = "Gin Bar_hl",
                                                                 "_` Gluten Free Options" = "Gluten Free Options_hl",
                                                                 "_` Group Meal" = "Group Meal_hl",
                                                                 "_` Halal" = "Halal_hl",
                                                                 "_` Home Baker" = "Home Baker_hl",
                                                                 "_` Hookah" = "Hookah_hl",
                                                                 "_` Indoor Seating" = "Indoor Seating_hl",
                                                                 "_` Karaoke" = "Karaoke_hl",
                                                                 "_` Keto Options" = "Keto Options_hl",
                                                                 "_` Kid Friendly" = "Kid Friendly_hl",
                                                                 "_` LGBTQIA Friendly" = "LGBTQIA Friendly_hl",
                                                                 "_` Live Entertainment" = "Live Entertainment_hl",
                                                                 "_` Live Music" = "Live Music_hl",
                                                                 "_` Live Sports Screening" = "Live Sports Screening_hl",
                                                                 "_` Lunch Menu" = "Lunch Menu_hl",
                                                                 "_` Lunch" = "Lunch_hl",
                                                                 "_` Luxury Dining" = "Luxury Dining_hl",
                                                                 "_` Mall Parking" = "Mall Parking_hl",
                                                                 "_` Members Only" = "Members Only_hl",
                                                                 "_` Nightlife" = "Nightlife_hl",
                                                                 "_` No Alcohol Available" = "No Alcohol Available_hl",
                                                                 "_` No Seating Available" = "No Seating Available_hl",
                                                                 "_` Outdoor Seating" = "Outdoor Seating_hl",
                                                                 "_` Paid Wifi" = "Paid Wifi_hl",
                                                                 "_` Pet Friendly" = "Pet Friendly_hl",
                                                                 "_` Pool Table" = "Pool Table_hl",
                                                                 "_` Poolside" = "Poolside_hl",
                                                                 "_` Pre-Ordering Required" = "Pre-Ordering Required_hl",
                                                                 "_` Private Dining Area Available" = "Private Dining Area Available_hl",
                                                                 "_` Pure Veg" = "Pure Veg_hl",
                                                                 "_` Resto Bar" = "Resto Bar_hl",
                                                                 "_` Restricted Entry" = "Restricted Entry_hl",
                                                                 "_` Romantic Dining" = "Romantic Dining_hl",
                                                                 "_` Rooftop" = "Rooftop_hl",
                                                                 "_` Seaside" = "Seaside_hl",
                                                                 "_` Seaview" = "Seaview_hl",
                                                                 "_` Self Service" = "Self Service_hl",
                                                                 "_` Serves Alcohol" = "Serves Alcohol_hl",
                                                                 "_` Serves Cocktails" = "Serves Cocktails_hl",
                                                                 "_` Serves Jain Food" = "Serves Jain Food_hl",
                                                                 "_` Smoking Area" = "Smoking Area_hl",
                                                                 "_` Sneakpeek" = "Sneakpeek_hl",
                                                                 "_` Sodexo" = "Sodexo_hl",
                                                                 "_` Sports TV" = "Sports TV_hl",
                                                                 "_` Standing Tables" = "Standing Tables_hl",
                                                                 "_` Table booking for Groups" = "Table booking for Groups_hl",
                                                                 "_` Table booking not available" = "Table booking not available_hl",
                                                                 "_` Table booking recommended" = "Table booking recommended_hl",
                                                                 "_` Table Reservation Not Required" = "Table Reservation Not Required_hl",
                                                                 "_` Table reservation required" = "Table reservation required_hl",
                                                                 "_` Takeaway Available" = "Takeaway Available_hl",
                                                                 "_` Ticket Restaurant" = "Ticket Restaurant_hl",
                                                                 "_` Unlimited Pizza" = "Unlimited Pizza_hl",
                                                                 "_` Valet Parking Available" = "Valet Parking Available_hl",
                                                                 "_` Variable Menu" = "Variable Menu_hl",
                                                                 "_` Vegan Options" = "Vegan Options_hl",
                                                                 "_` Wheelchair Accessible" = "Wheelchair Accessible_hl",
                                                                 "_` Wifi" = "Wifi_hl",
                                                                 "_` Wine Tasting" = "Wine Tasting_hl",
                                                                 "_` Wine" = "Wine_hl"
                                                               )),
                                            #next button
                                            actionButton(inputId = "gotopage7", class = "next-css",icon = icon("check-square"),label = "NEXT"),
                                            br(),
                                            br(),
                                            br()
                 )
                 ),
                 #######################
                 #-------Page 6---------
                 #######################        
                 tabPanel("Rating", 
                          h3("Your Rating is"),
                          textOutput(outputId = "Rating_out")
                          )
                 )
            )
# Import R packages needed for the app here:
library(shiny)
library(DT)
library(RColorBrewer)

# Define any Python packages needed for the app here:
PYTHON_DEPENDENCIES = c('pip', 'numpy','pandas','joblib')
# py_install(PYTHON_DEPENDENCIES)

server <- function(input, output, session) {
  
  library(reticulate)
  reticulate::source_python("predict_result.py")
  
  
  # ------------------ App virtualenv setup (Do not edit) ------------------- #

  virtualenv_dir = Sys.getenv('VIRTUALENV_NAME')
  python_path = Sys.getenv('PYTHON_PATH')
  
  # Create virtual env and install dependencies
  reticulate::virtualenv_create(envname = virtualenv_dir, python = python_path)
  reticulate::virtualenv_install(virtualenv_dir, packages = PYTHON_DEPENDENCIES, ignore_installed=TRUE)
  reticulate::use_virtualenv(virtualenv_dir, required = T)
  
  # 
  # 
  # create a new environment 
  # virtualenv_create("r-reticulate")
  # 
  # # install SciPy
  # virtualenv_install("r-reticulate", "pip")
  # virtualenv_install("r-reticulate", "numpy")
  # virtualenv_install("r-reticulate", "sklearn")
  # virtualenv_install("r-reticulate", "pandas")
  # virtualenv_install("r-reticulate", "joblib")
  # 
  # use_virtualenv("r-reticulate")
  
  
  
  # default value for the map
  output$my_leaf <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      setView(lat = 11.2519441, lng = 77.9059267, zoom = 7)
      
  })
  
  # Page 2 by clicking the next button on Page 1
  observeEvent(input$gotopage2,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Competitors")
    
  })
  observeEvent(input$gotopage3,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Amenities")
    ml_data["average_cost_for_two"][[1]] <- as.integer(input$acft)
  })
  observeEvent(input$gotopage3,{
    ml_data["average_cost_for_two"][[1]] <- as.integer(input$acft)
  })
  observeEvent(input$gotopage4,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Establishment")
    
  })
  observeEvent(input$gotopage5,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Cuisines")
    
  })
  observeEvent(input$gotopage6,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Highlights")
    
  })
  observeEvent(input$gotopage7,{
    updateNavbarPage(session = session, inputId = "inTabset", selected = "Rating")
    write.csv(ml_data, "others/data_input.csv")
    
    output$Rating_out <- renderText({
      return(show_rating())})
  })
  # find the area
  observeEvent(input$linksubmit,{
    
    latilongivalues <- take_coordinates(input$latilongilink)
    latiVal <- latilongivalues[1]
    longiVal <- latilongivalues[2]
    
    # write to the ml data++++++++
    ml_data["latitude"][[1]] <- latiVal
    ml_data["longitude"][[1]] <- longiVal
    
    
    # page 1 function
    output$my_leaf <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)
        ) %>%
        setView(lat = latiVal, lng = longiVal, zoom = 12)%>%
        addMarkers(data = cbind(longiVal,latiVal))
    })
    #===========================
    # competitors
    comp_table <- get_table_comp(latiVal, longiVal)
    output$comp_table_025 <- renderDataTable({comp_table[[1]]},
                                            options = list(scrollY='250px', paging=FALSE)
    )
    ml_data["min_ct"][[1]] <- comp_table[[2]]
    ml_data["avg_ct"][[1]] <- comp_table[[3]]
    ml_data["max_ct"][[1]] <- comp_table[[4]]
    ml_data["min_ar"][[1]] <- comp_table[[5]]
    ml_data["avg_ar"][[1]] <- comp_table[[6]]
    ml_data["max_ar"][[1]] <- comp_table[[7]]
    #===========================
    # amenity function
    amenity_tables <- get_table_amenity(latiVal, longiVal)
    output$ame_table_025 <- renderDataTable({amenity_tables[[1]]},
                                            options = list(scrollY='150px', paging=FALSE)
                                            )
    output$ame_table_05 <- renderDataTable({amenity_tables[[2]]},
                                            options = list(scrollY='150px', paging=FALSE)
    )
    output$ame_table_1 <- renderDataTable({amenity_tables[[3]]},
                                            options = list(scrollY='150px', paging=FALSE)
    )
    output$ame_table_2 <- renderDataTable({amenity_tables[[4]]},
                                            options = list(scrollY='150px', paging=FALSE)
    )
    output$ame_table_5 <- renderDataTable({amenity_tables[[5]]},
                                            options = list(scrollY='150px', paging=FALSE)
    )
    # write to the ml data++++++++
    ml_data$amenity.within.0.25km[[1]] <- amenity_tables[[6]]
    ml_data$amenity.within.0.5km[[1]] <- amenity_tables[[7]]
    ml_data$amenity.within.1km[[1]] <- amenity_tables[[8]]
    ml_data$amenity.within.2km[[1]] <- amenity_tables[[9]]
    ml_data$amenity.within.5km[[1]] <- amenity_tables[[10]]
    View(ml_data)
  })
}

shinyApp(ui, server)

