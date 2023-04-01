#Created by Keith Post on 3/19/23


#load packages
pacman::p_load(shiny,here,shinyMobile,english,tidyverse,shinyjs,DT)

#source in functions and objects
source(here("obj_fns","grocery_assistant_obj_01.R"))
source(here("obj_fns","grocery_assistant_fn_01.R"))


#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-f7Page(
  useShinyjs(),
# ui<-f7TabLayout(title=NULL,navbar=NULL,
#   #spaceholder for theme
#   ##### Create structure as f7Tabs (similar to tabsetPanel)
  f7Tabs(id="main_tabset",
                 
    ##### Main Menu=================================================================================
    f7Tab(title="Main",
          tabName="main_tab",
          hidden=TRUE,
      div(
        strong(h2("Welcome to the Grocery Assistant App!")),
        h4("This application will help you develop a database of recipes, choose weekly menus,
           and generate shopping lists. New features will be added periodically and listed here
           when available."),
        br(),
        strong(h3("What would you like to do today?")),
        #add block of buttons
        f7Block(
          f7Button(inputId="btn_add1_recipe_main",
                   label="Manually add recipe"),
          br(),
          f7Button(inputId="btn_manage_recipe_main",
                   label="View/edit/delete recipes"),
          br(),
          f7Button(inputId="btn_gen_list_main",
                   label="Generate shopping list"),
          br(),
          f7Button(inputId="btn_addmult_recipe_main",
                    label="Add recipes using file (computer recommended)"),
          br(),
          f7Button(inputId="btn_test_app_main",
                   label="Test App with Pre-Loaded Data")
        ),
        br(),
        h4("recent recipe"),
        tableOutput("recipe_tab"),
        br(),
        h4("recipe database"),
        tableOutput("recipe_database"),
        br(),
        h4("recent ingredient"),
        tableOutput("ingred_tab"),
        br(),
        h4("ingred database"),
        tableOutput("ingred_database"),
        style="margin-left:100px; margin-right: 100px"
      ),

      
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
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          #inputs for recipe info
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
        )
      ),
      
      ## Ingredient entry sheet 1
      f7Sheet(
        id="man_input_ingredSheet1",
        label="Please enter ingredients",
        orientation="top",
        swipeToClose=FALSE,
        swipeToStep=TRUE,
        swipeHandler=FALSE, 
        hiddenItems=tagList( 
          splitLayout(
            f7Button(inputId="btn_return_recipe_ingredSheet1",
                     label="Return to recipe info"),
            f7Button(inputId="btn_ingred_entry_ingredSheet1",
                     label="Add more ingredients"),
          ),
          br(),
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          h2(strong(textOutput("txt_out_recipe_ingredSheet1"))),
          add_ingredients(n=4),
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
          ),
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          h2(strong(textOutput("txt_out_recipe_ingredSheet2"))),
          add_ingredients(n_prev=4,n=4),
          f7Button(inputId="btn_submit_recipe_ingred_ingredSheet2",
                   label="Submit recipe info & ingredients")
        )
      )
    ),
    
    ##### Search/Browse Recipes=====================================================================
    f7Tab(title="Recipes",
          tabName="recipe_tab",
          hidden=TRUE,
      div(
        strong(h2("Feel free to browse, search, edit, and delete recipes")),
        DTOutput("recipe_db_recipe"),
        f7Button(inputId="btn_return_main_recipe",
                 label="Return to main menu"),
        style="margin-left:100px; margin-right: 100px"
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
#password protection
#auto-complete
#change styling of buttons
#fix delay/poor responsiveness with previous ingredient actionButton
#use segment to bunch buttons
#slideshow of images (recipe)
#display recipe 'cards' (which have pictures of dishes--manual upload or auto-internet search)



# NEXT
# 1) recipe & ingredient databases to update (delete/edit/add) appropriately
    # remove recipe button functionality
# 2) view/edit button generates cards (which also has delete option)
# 3) demonstration functionality
# 4) batch add capability
# 5) generate shopping list
# 6) ability to save status with login




# DONE




# LAST COMMIT
# created button for demo
# created recipe and ingredient info for demo
# re-thought approach to building recipe and ingredient databases using reactiveValues


