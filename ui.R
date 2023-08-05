#Created by Keith Post on 3/19/23


#load packages
pacman::p_load(shiny,here,shinyMobile,english,tidyverse,shinyjs,DT,tools,rmarkdown,kableExtra,
               shinyscreenshot,mailR,htmlTable,readxl,googledrive,gargle,googlesheets4,
               magick,waiter)



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
  ##### Create structure as f7Tabs (similar to tabsetPanel)
  f7Tabs(id="main_tabset",
                 
    ##### Main Menu=================================================================================
    f7Tab(title="Main",
          tabName="main_tab",
          hidden=TRUE,
  
      div( #to create margins
        #developer info & instructions buttons
        splitLayout(cellWidths=c("10%","75%","15%"),
          f7Button(inputId="btn_dev_info_main",
                   label=f7Icon("info_circle"),
                   color="deeppurple"),
          br(),
          f7Button(inputId="btn_instructions_main",
                   label=f7Icon("book_fill"),
                   color="deeppurple")
          ),
          br(),
        #opening text
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
          f7File(inputId="file_load_file_main",
                 label=NULL,
                 buttonLabel= div(f7Icon("folder"),
                                  "Load database from file"),
                 width="100%"),
          br(),
          #set up user id for new/recurring users
          h3("Enter your user id to save/load your database from/to app"),
          f7Row(
            f7Col(
              f7Text(inputId="txt_user_id_main",
                     label=HTML("use <em>firstinitial_lastname</em><br> e.g., j_doe"))
            ),
            f7Col(
              linebreaks(2),
              f7Button(inputId="btn_user_id_main",
                       label="Submit user id",
                       color="green"),
              strong("Current user:"),
              textOutput("txt_out_user_id_main")
            )
          ),
          br(),
          #database load btn appears only after user_id() entered successfully
          uiOutput("ui_btn_load_sheet_main"),
          br(),
          f7Button(inputId="btn_preload_data_main",
                   label="Test App with Pre-Loaded Data"),
          br(),
          #resets db and user_id
          f7Button(inputId="btn_reset_db_main",
                   label=div(f7Icon("arrow_counterclockwise_circle"),
                             "Reset app"),
                   color="red")
        ),
        
        #NOTE: tables to see server function--uncomment when want to track data
        # br(),
        # h4("recent recipe (manual upload)"),
        # tableOutput("recipe_tab"),
        # br(),
        # h4("recipe database"),
        # tableOutput("recipe_database"),
        # br(),
        # h4("recent ingredient (manual upload)"),
        # tableOutput("ingred_tab"),
        # br(),
        # h4("ingred database"),
        # tableOutput("ingred_database"),
        
        #percentages help with different display sizes
        style="margin-left: 3%; margin-right: 3%"
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
          # splitLayout(
          #segment provides container for buttons
          f7Segment(
            f7Button(inputId="btn_return_recipe_ingredSheet1",
                     label="Return to recipe info",
                     color="purple"),
            f7Button(inputId="btn_ingred_entry_ingredSheet1",
                     label="Add more ingredients"),
            container="segment"
          ),
          #right-aligns text
          p("Click outside sheet to return to main menu",style="text-align: right"),
          h2(strong(textOutput("txt_out_recipe_ingredSheet1"))),
          add_ingredients(n=4),
          br(),
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
    
    
    ##### Application Instructions==================================================================
    f7Tab(title="App instructions",
          tabName="instruct_tab",
          hidden=TRUE,
      #div is to set up margins
      div(
        h2("How to use the Grocery Assistant App"),
        #build out UI using accordions
        f7Accordion(
          multiCollapse=TRUE,
          inputId="overview_accord",
          f7AccordionItem(
            title=em("Overview"),
            div(
              objective_txt,
              linebreaks(2),
              style="margin-left: 2%; margin-right: 2%"
            )
          )
        ),
        
        h3("How to..."),
        f7Accordion(
          multiCollapse=TRUE,
          inputId="howto_accord",
          f7AccordionItem(
            title=em("Add recipes"),
            div(
              recipe_add_intro_txt,
              linebreaks(2),
              f7Block(
                f7BlockHeader(strong("Manually")),
                recipe_add_manual_txt
              ),
              br(),
              f7Block(
                f7BlockHeader(strong("By file")),
                recipe_add_file_txt
              ),
            linebreaks(2),
            style="margin-left: 2%; margin-right: 2%"
            )
          ),
          #programmatically add list of accordiions
          add_accordions(list_instruct)
        ),
        linebreaks(2),
        f7Button(inputId="btn_return_main_instruct",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left: 10%; margin-right: 10%"
      )
    ),
    
    ##### View/Edit/Delete/Save Recipes=============================================================
    ### Page associated with recipe browser
    f7Tab(title="Recipes",
          tabName="recipe_tab",
          hidden=TRUE,
      div(
        #display user_id on all pages 
        f7Align(
          htmlOutput("txt_out_user_id_recipe"),
          side="right"),
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
            h3(strong(textOutput("txt_out_save_app_recipe"))),
            uiOutput("ui_btn_save_db_recipe")
          )
        ),
        br(),
        f7Button(inputId="btn_return_main_recipe",
                 label=div(f7Icon("return"),"Return to main menu"),
                 color="purple"),
        style="margin-left: 3%; margin-right: 3%"
      )
    ),
    
    ##### Meal Planner==============================================================================
    f7Tab(title="Meal Planner",
          tabName="planner_tab",
          hidden=TRUE,
      div(
        #display user_id on all pages
        f7Align(
          htmlOutput("txt_out_user_id_planner"),
          side="right"
        ),
        br(),
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
        #display user_id on all pages
        f7Align(
          htmlOutput("txt_out_user_id_list"),
          side="right"
        ),
        br(),
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
        splitLayout(cellWidths=c("60%","40%"),
          br(),
          f7Block(
            f7BlockTitle(title="Email plan & list",size="medium"),
            f7Text(inputId="txt_email_address_list",
                   label="Recipient email address"),
            f7Button(inputId="btn_planList_email_list",
                     label=div(f7Icon("envelope"),"Send email"))
          )
        ),
        br(),
        #take a screenshot
        splitLayout(cellWidths=c("60%","40%"),
          br(),
          f7Button(inputId="btn_planList_screenshot_list",
                   label=div(f7Icon("camera"),"Screenshot plan & list"))
        ),
        br(),
        #export to pdf
        splitLayout(cellWidths=c("60%","40%"),            
          br(),
          f7DownloadButton(outputId="btn_planList_export_list",
                           label="Export plan & list to pdf")
        ),
        br(),
        #reset
        splitLayout(cellWidths=c("60%","40%"),
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
        style="margin-left: 3%; margin-right: 3%"
      )
    ),
    
    
    
    
    ##### Upload Files to Database==================================================================
    f7Tab(title="Upload Recipes",
          tabName="upload_recipes",
          hidden=TRUE,
      div(
        #display user_id on all pages
        f7Align(
          htmlOutput("txt_out_user_id_upload"),
          side="right"
        ),
        strong(h2("Batch add recipes and their ingredients to database by file")),
        br(),
        #step 1: download copy of template
        h3("1) Download a copy of the template"),
        splitLayout(cellWidths=c("25%","75%"),
          f7DownloadButton(outputId="btn_template_download_upload",
                           label="Download Template"),
          br()
        ),
        br(),
        "File should contain six columns:",
          tags$li(strong("recipe:"), "name of recipe"),
          tags$li(strong("appliance:"), "one or more of 'stove', 'oven', 'grill', 'broiler', 'slow cooker'; multiple
             appliances separated by a ', '"),
          tags$li(strong("protein:"), "one or more of 'chicken', 'beef', 'steak', 'pork', 'fish', or 'vegetarian'"),
          tags$li(strong("name:"), "name of ingredient"),
          tags$li(strong("size:"), "size of ingredient"),
          tags$li(strong("n:"), "number of that particular ingredient needed in recipe"),
        br(),
        #2: input info
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
        style="margin-left: 3%; margin-right: 3%"
      )
    )
  )
)
    







# LATER+++++++++++++++++++++++++++++++++++++++++++++++++++
# UI
# after launching on shinyapps--see how DT actionbuttons are working then decide to use in separate
  #cols if needed--use button labels as colnames and icons on buttons; consider to make
  #'recipe' a hyperlink for viewing a pic of it


# BACK-END
# see if any observeEvents can be combined--e.g., those that generate alert/confirm dialogs


# Database additions/updates
# for both manual entry and editing, should temp tibble be stored/created at submit button then
  #added to db after confirm? (this would speed up processing as it would separate steps)


# MUCH LATER
# embed dish image in DT as thumbnail which can enlarge when user clicks on it
# slide show of dish images--hint: use image_animate() from {magick}
# formatting of instructions--e.g., bullets
# add waiters (spinners) during image changes



# NEXT+++++++++++++++++++++++++++++++++++++++++++++++++++
#1) general work on UI/UX--styling--perhaps some banner when test_mode is running




# DONE++++++++++++++++++++++++++++++++++++++++++++++++++++++







# LAST COMMIT+++++++++++++++++++++++++++++++++++++++++++++++
# if no dishes in db_df() then tibble is empty but with colheaders so that it can still be
  #saved to googledrive
# app deletes any extraneous images of dishes once data saved to app
