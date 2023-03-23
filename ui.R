#Created by Keith Post on 3/19/23
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#

#load packages
pacman::p_load(shiny,here,shinyMobile,english,tidyverse)

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
      ## Recipe entry sheet
      f7Sheet(
        id="man_input_recipe_sheet1",
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
          f7Button(inputId="btn_ingred_entry_sheet1",
                   label="Click to enter ingredients")
        )
      ),
      
      ## Ingredient entry sheet 1
      f7Sheet(
        id="man_input_ingred_sheet2",
        label="Please enter ingredients",
        orientation="top",
        swipeToClose=TRUE,
        swipeToStep=TRUE,
        hiddenItems=tagList( 
          textOutput("txt_out_recipe_sheet2"),
          
          # f7Block("Enter first ingredient",
          #   f7Text(inputId="txt_ingred1_nm_sheet1",
          #        label="Name"),
          #   splitLayout(cellWidths=c("75%","25%"),
          #     f7Text(inputId="txt_ingred1_size_sheet1",
          #        label="Size (e.g., 12 oz can)"),
          #     div(class="label-left",
          #       f7Stepper(inputId="txt_ingred1_n_sheet1",
          #           label="Count",
          #           min=0,
          #           max=20,
          #           value=1,
          #           step=0.5,
          #           size="large",
          #           raised=TRUE),
          #       #provides margins around text (which helps center stepper label with input)
          #       style="padding: 40px 0px 0px 0px; font-size: 20px;"
          #     ),
          #     #aligns stepper input with text input
          #     tags$style(type='text/css', "#txt_ingred1_n_sheet1 { vertical-align: middle;
          #                width: 80%}")
          #     
          #   ), 
            add_ingredients(n=4,sheet_num=1),
            
            f7Button(inputId="btn_ingred_entry_sheet2",
                     label="Add more ingredients"),
          ),
        br()
        ),
      
        ## Ingredient entry sheet 2
        f7Sheet(
          id="man_input_ingred_sheet3",
          label="Please enter ingredeints",
          orientation="top",
          swipeToClose=TRUE,
          hiddenItems=tagList(
            textOutput("tax_out_recipe_sheet3"),
            add_ingredients(n_prev=4,n=4,sheet_num=2)
          )
        )
      ) 
    ) 
  )



















#---------------------------------------
# LATER
#while typing ingredients, there will be a suggestion for one in database
#shopping list will round counts up (e.g., ceiling)


# NEXT



# DONE




# LAST COMMIT
#finally got ingredient size and counts to align
#split recipe and ingredients into two sheets
#functionalized ingredient list




