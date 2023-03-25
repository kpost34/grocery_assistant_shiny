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
      
      tableOutput("recipe_tab"),
      tableOutput("ingred_tab"),

      
      ### Manual Data Input=========================================================================
      ## Recipe entry sheet
      f7Sheet(
        id="man_input_recipeSheet",
        label="Please add a recipe",
        orientation="top",
        swipeToClose=TRUE,
        swipeToStep=TRUE,
        swipeHandler=FALSE,
        hiddenItems=tagList(
          f7Text(inputId="txt_recipe_recipeSheet",
                 label="Recipe:"),
          splitLayout(
            f7CheckboxGroup(inputId="chkGrp_app_recipeSheet",
                label="Appliance(s) used:",
                choices=app_choices_sheet1),
            f7CheckboxGroup(inputId="chkGrp_protein_recipeSheet",
                            label="Protein source(s):",
                            choices=protein_choices_sheet1)
          ),
          f7Button(inputId="btn_ingred_entry_recipeSheet",
                   label="Enter ingredients")
          # splitLayout(
          #   f7Button(inputId="btn_submit_recipe_sheet1",
          #            label="Submit recipe info"),
          #   f7Button(inputId="btn_ingred_entry_sheet1",
          #            label="Enter ingredients")
        )
      ),
      
      ## Ingredient entry sheet 1
      f7Sheet(
        id="man_input_ingredSheet1",
        label="Please enter ingredients",
        orientation="top",
        swipeToClose=TRUE,
        swipeToStep=TRUE,
        swipeHandler=FALSE,
        hiddenItems=tagList( 
          textOutput("txt_out_recipe_ingredSheet1"),
          add_ingredients(n=4),
          br(),
          splitLayout(
            f7Button(inputId="btn_return_recipe_ingredSheet1",
                     label="Return to recipe info"),
            f7Button(inputId="btn_ingred_entry_ingredSheet1",
                     label="Add more ingredients"),
          ),
          linebreaks(2),
          f7Button(inputId="btn_submit_recipe_ingred_ingredSheet1",
                   label="Submit recipe info & ingredients")
        )
      ),
      
      ## Ingredient entry sheet 2
      f7Sheet(
        id="man_input_ingredSheet2",
        label="Please enter ingredeints",
        orientation="top",
        swipeToClose=TRUE,
        swipeToStep=TRUE,
        swipeHandler=FALSE,
        hiddenItems=tagList(
          textOutput("txt_out_recipe_ingredSheet2"),
          add_ingredients(n_prev=4,n=4),
          # f7Button(inputId="btn_submit_ingred_sheet3",
          #          label="Submit ingredient info"),
          splitLayout( 
            f7Button(inputId="btn_previous_ingred_ingredSheet2",
                     label="Previous ingredients"),
            br()
          ),
          br(),
          splitLayout(
            f7Button(inputId="btn_return_recipe_ingredSheet2",
                     label="Return to recipe info"),
            br()
          )
        )
      )
    ) 
  ) 
)



















#---------------------------------------
# LATER
#while typing ingredients, there will be a suggestion for one in database
#shopping list will round counts up (e.g., ceiling)
#figure out a way to always display sheet 3 in the background so that submit button is always
  #visible
#figure out how to get clicking a button on same item 2x in a row or going back and forth b/t
  #two sheets will actually manifest
#add condition--if no ingredients selected then can't submit (same with recipe)


# NEXT



# DONE




# LAST COMMIT
#added various buttons for ingredient info & made them functional
#added eventReactive()s and made submit button functional
#updated sheet names
#got eventReactive() to grab inputs and populate DFs



