#Created by Keith Post on 3/19/23
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#load packages
pacman::p_load(shiny,here,shinyMobile,shinyjs)

#source in functions and objects
source(here("obj_fns","grocery_assistant_obj_01.R"))
source(here("obj_fns","grocery_assistant_fn_01.R"))


#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-f7Page(
# ui<-f7TabLayout(title=NULL,navbar=NULL,
#   #spaceholder for theme
#   useShinyjs(),
#   
#   ##### Create structure as f7Tabs (similar to tabsetPanel)
  f7Tabs(id="main_tabset",
#               
#     ##### Main Menu=================================================================================
    f7Tab(title="Main",
          tabName="main_tab",
          hidden=TRUE,
      strong(h2("Welcome to the Grocery Assistant App!")),
      h4("This application will help you develop a database of recipes, choose weekly menus,
         and generate shopping lists. New features will be added periodically and listed here
         when available."),
      br(),
      strong(h3("What would you like to do today?")),
        f7Radio(inputId="rad_op_main",
                label=NULL,
                choices=op_choices_main,
                selected=NULL
        ),

      
      ### Manual Data Input=========================================================================
      # f7Button(inputId="go",label="Go"),
        f7Sheet(
          id="man_input_sheet1",
          label="Please add a recipe",
          orientation="top",
          swipeToClose=TRUE,
          swipeToStep=TRUE,
          hiddenItems=tagList(
            f7Text(inputId="txt_recipe_sheet1",
                   label="Recipe:"),
            splitLayout(
              f7CheckboxGroup(inputId="chkGrp_app_sheet1",
                  label="Appliance(s) used:",
                  choices=app_choices_sheet1),
              f7CheckboxGroup(inputId="chkGrp_protein_sheet1",
                              label="Protein source(s):",
                              choices=protein_choices_sheet1)
              ),
            
            f7Block("Enter first ingredient",
              f7Text(inputId="txt_ingred1_nm_sheet1",
                   label="Name"),
              splitLayout(
                f7Text(inputId="txt_ingred1_size_sheet1",
                   label="Size (e.g., 12 oz can)"),
                div(
                  f7Stepper(inputId="txt_ingred1_n_sheet1",
                      label="Count",
                      min=0,
                      max=20,
                      value=1,
                      step=0.5,
                      size="large",
                      raised=TRUE),
                  style="display: inline-block; vertical-align: middle;"
                  # style="margin-top: 20px; font-size:15pt; height=25px"
                ),
                tags$style(type='text/css', "#txt_ingred1_n_sheet1 { width:100%; margin-top: 30px;}")
              )
                
              )
            )
          )
        )
      )
    )


















#---------------------------------------
# NEXT



# DONE


# LAST COMMIT




