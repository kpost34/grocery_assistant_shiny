#Created by Keith Post on 3/19/23


#load packages
pacman::p_load(shiny,here,shinyMobile,english,tidyverse,shinyjs,DT,tools,rmarkdown,shinyscreenshot,
               mailR,htmlTable,readxl,googledrive,gargle,googlesheets4)



#source in functions and objects
source(here("obj_fns","grocery_assistant_obj_01.R"))
source(here("obj_fns","grocery_assistant_fn_01.R"))
source(here("config.R"))


#run options
#run at start to connect to gmail account
options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)



#--------------------------------------------------------------------------------------------------#
###### Define UI====================================================================================
#--------------------------------------------------------------------------------------------------#
ui<-f7Page(
  useShinyjs(),
  shinyFeedback::useShinyFeedback(),
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
                   label=div(f7Icon("plus"),
                             "Manually add recipe")),
          br(),
          f7Button(inputId="btn_manage_recipe_main",
                   label=div(f7Icon("eye"),
                             "View ",
                             f7Icon("pencil"),
                             "Edit ",
                             f7Icon("trash"),
                             "Delete ",
                             f7Icon("floppy_disk"),
                             "Save Recipes")
                   ),
          br(),
          f7Button(inputId="btn_meal_plan_main",
                   label=div(f7Icon("square_split_2x1"),
                             "Meal planner")),
          br(),
          f7Button(inputId="btn_upload_recipe_main",
                    label=div(f7Icon("plus"),
                              f7Icon("plus"),
                              "Add recipes from file (computer recommended)")),
          br(),
          f7Button(inputId="btn_load_db_main",
                   label=div(f7Icon("arrow_up_square"),
                             "Load saved database (from file or app)")),
          br(),
          f7Button(inputId="btn_preload_data_main",
                   label="Test App with Pre-Loaded Data"),
          br(),
          f7Button(inputId="btn_reset_db_main",
                   label=div(f7Icon("arrow_counterclockwise_circle"),
                             "Reset app"),
                   color="red")
        ),
        br(),
        #temporary tables to see server function
        h4("recent recipe (manual upload)"),
        tableOutput("recipe_tab"),
        br(),
        h4("recipe database"),
        tableOutput("recipe_database"),
        br(),
        h4("recent ingredient (manual upload)"),
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
                     label="Return to recipe info",
                     color="purple"),
            f7Button(inputId="btn_ingred_entry_ingredSheet1",
                     label="Add more ingredients")
          ),
          br(),
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          h2(strong(textOutput("txt_out_recipe_ingredSheet1"))),
          add_ingredients(n=4),
          linebreaks(2),
          f7Button(inputId="btn_submit_recipe_ingred_ingredSheet1",
                   label="Submit recipe info & ingredients",
                   color="green")
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
                     label="Return to recipe info",
                     color="purple"),
            br()
          ),
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          h2(strong(textOutput("txt_out_recipe_ingredSheet2"))),
          add_ingredients(n_prev=4,n=4),
          f7Button(inputId="btn_submit_recipe_ingred_ingredSheet2",
                   label="Submit recipe info & ingredients",
                   color="green")
        )
      )
    ),
    
    ##### View/Edit/Delete/Save Recipes=============================================================
    f7Tab(title="Recipes",
          tabName="recipe_tab",
          hidden=TRUE,
      div(
        strong(h2("Feel free to browse, search, edit, and delete recipes")),
        DTOutput("recipe_db_recipe"),
        br(),
        splitLayout(cellWidths=c("75%","25%"),
          br(),
          f7DownloadButton(outputId="btn_download_db_recipe",
                           label="Download a Copy")
        ),
        splitLayout(cellWidths=c("60%","40%"),
          br(),
          f7Block(
            f7BlockTitle(title="Save a copy to app",size="medium"),
            f7Text(inputId="txt_sheet_nm_recipe",
                   #html used to get multiline label
                   label=HTML("Enter your name
                              <br />
                              (use <em>firstinitial_lastname</em>, e.g., k_post)"),
            ),
            f7Button(inputId="btn_save_db_recipe",
                     label=div(f7Icon("floppy_disk"),"Save database to app"),
                     color="green")
          )
        ),
        br(),
        f7Button(inputId="btn_return_main_recipe",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left:100px; margin-right: 100px"
      ),
      br(),
      #view recipe cards
      # f7Card(id="test_card",
      #        title=demo_recipeDF[1,1])
    ),
    
    ##### Generate Shopping List====================================================================
    f7Tab(title="Meal Planner",
          tabName="planner_tab",
          hidden=TRUE,
      div(
        splitLayout(cellWidths=c("75%","25%"),
          strong(h2("Meal planner")),
          f7Button(inputId="btn_view_plan_planner",
                   label=div(f7Icon("eye"),
                             "View meal plan/shopping list"))
        ),
        h3("Select the recipes to add to your meal plan"),
        DTOutput("recipe_db_planner"),
        br(),
        splitLayout(cellWidths=c("75%","25%"),
          br(),
          f7Button(inputId="btn_reset_planList_planner",
                   label=div(f7Icon("arrow_counterclockwise_circle"),"Reset plan/list"),
                   color="red")
        ),
        linebreaks(2),
        f7Button(inputId="btn_return_main_planner",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left:100px; margin-right: 100px"
      )
    ),
    
    
    
    ##### Shopping List=============================================================================
    f7Tab(title="Shopping List",
          tabName="list_tab",
          hidden=TRUE,
      div(
        splitLayout(cellWidths=c("75%","25%"),
          strong(h2("Meal plan & shopping list")),
          f7Button(inputId="btn_return_planner_list",
                 label="Return to meal planner",
                 color="purple")
        ),
        #create div() for screenshot() in server function
        div(id="plan-list-screenshot",
          DTOutput("recipe_list_list"),
          linebreaks(2),
          DTOutput("shopping_list_list")
        ),
        br(),
        #email it
        splitLayout(cellWidths=c("75%","25%"),
          br(),
          f7Block(
            f7BlockTitle(title="Email plan & list",size="medium"),
            f7Text(inputId="txt_email_address_list",
                   label="Recipient email address"),
            shinyFeedback::useShinyFeedback(),
            # textOutput("warn_no_email_address_list"),
            f7Button(inputId="btn_planList_email_list",
                     label=div(f7Icon("envelope"),"Send email"))
          )
        ),
        br(),
        #take a screenshot
        splitLayout(cellWidths=c("75%","25%"),
          br(),
          f7Button(inputId="btn_planList_screenshot_list",
                   label=div(f7Icon("camera"),"Screenshot plan & list"))
        ),
        br(),
        #export to pdf
        splitLayout(cellWidths=c("75%","25%"),            
          br(),
          f7DownloadButton(outputId="btn_planList_export_list",
                           label="Export plan & list to pdf")
        ),
        br(),
        #reset
        splitLayout(cellWidths=c("75%","25%"),
          br(),
          f7Button(inputId="btn_reset_planList_list",
                   label=div(f7Icon("arrow_counterclockwise_circle"),"Reset plan/list"),
                   color="red")
        ),
        linebreaks(2),
        #return to main menu
        f7Button(inputId="btn_return_main_list",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left:100px; margin-right: 100px"
      )
    ),
    
    
    
    
    ##### Upload Files to Database==================================================================
    f7Tab(title="Upload Recipes",
          tabName="upload_recipes",
          hidden=TRUE,
      div(
        strong(h2("Batch add recipes and their ingredients to database by file")),
        br(),
        #step 1: download copy of tempalte
        h3("1) Download a copy of the template"),
        splitLayout(cellWidths=c("25%","75%"),
          f7DownloadButton(outputId="btn_template_download_upload",
                           label="Download Template"),
          br()
        ),
        br(),
        "File should contain six columns:",
          tags$li(strong("id:"),"identifer: assign recipes numerically starting with 1"),
          tags$li(strong("recipe:"), "name of recipe"),
          tags$li(strong("appliance:"), "one or more of 'stove', 'oven', 'grill', 'broiler', 'slow cooker'; multiple
             appliances separated by a ', '"),
          tags$li(strong("protein:"), "one or more of 'chicken', 'beef', 'steak', 'pork', 'fish', or 'vegetarian'"),
          tags$li(strong("name:"), "name of ingredient"),
          tags$li(strong("size:"), "size of ingredient"),
          tags$li(strong("n:"), "number of that particular ingredient needed in recipe"),
        br(),
        #2: input your info
        h3("2) Replace example in file with your recipes and ingredients"),
        br(),
        #3; save file
        h3("3) Save file"),
        br(),
        #4: upload file to db
        h3("4) Upload file to database"),
        f7File(
          inputId="file_upload_recipe_upload",
          label="",
          accept=c(".csv",".xls",".xlsx"),
          width="25%",
          buttonLabel=div(f7Icon("cloud_upload"), "Upload file")
        ),
        br(),
        #5: preview uploaded data
        htmlOutput("ui_txt_preview_upload_upload"),
        # h3("5) Preview upload"),
        DTOutput("file_upload_table"),
        textOutput("txt_warning_upload"),
        #6: confirm upload
        htmlOutput("ui_txt_confirm_upload_upload"),
        uiOutput("ui_btn_confirm_upload_upload"),
        br(),
        #7: another upload
        htmlOutput("ui_txt_another_upload_upload"),
        br(),
        #button to return to main menu
        f7Button(inputId="btn_return_main_upload",
                 label=div(f7Icon("return"),
                           "Return to main menu"),
                 color="purple"),
        style="margin-left:100px; margin-right: 100px"
      )
    ),
    
    
    
    ##### Load Database=============================================================================
    f7Tab(title="Load Database",
          tabName="load_database",
          hidden=TRUE,
      div(
        strong(h2("Load database from file or app")),
        br(),
        f7Row(
          f7Col(
            f7File(inputId="file_load_file_load",
                   label="Load database from file",
                   buttonLabel= div(f7Icon("folder"),
                                    "Browse for file"))
          ),
          f7Col(
            f7Text(inputId="txt_sheet_nm_load",
                   label=HTML("Enter your name
                              <br />
                              (use <em>firstinitial_lastname</em>, e.g., k_post)")),
            f7Button(inputId="btn_load_sheet_load",
                     label=div(f7Icon("arrow_up_square"),
                               "Load database from app"))
          )
        ),
        br(),
        f7Button(inputId="btn_return_main_load",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left:100px; margin-right: 100px"
      )
    )
  )
)
    






#---------------------------------------
# LATER-------------------------
# UI
# turn any repeated functions into custom functions (e.g., use of splitLayout?)
# potentially reduce delay() durations


# BACK-END
# develop custom functions to limit server code
# see if any observeEvents can be combined


# STYLING
# slideshow of images (recipe) on main app page




# NEXT-------------------------
# view/edit button generates cards (which also has delete option)



# DONE--------------------------




# LAST COMMIT-------------------
# resolved delays and lack of responsiveness to buttons when manually entering dishes

