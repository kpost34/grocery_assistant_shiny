#Created by Keith Post on 3/19/23
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#load packages
pacman::p_load(shiny,shinyjs)

#source in functions and objects



#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-fluidPage("Grocery Assistant App",
  #spaceholder for theme
  useShinyjs(),
  
  ##### Create structure as tabPanels
  tabsetPanel(id="main_tabset",type="hidden",
              
    ##### Main Menu
    tabPanel(title="Main",
      strong(h3("What would you like to do today?'))")),
      radioButtons(inputId="rad_main_main",
      
  
  
  
  
)


















#---------------------------------------
# NEXT



# DONE


# LAST COMMIT




