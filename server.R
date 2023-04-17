#Created by Keith Post on 3/19/23



#--------------------------------------------------------------------------------------------------#
###### Define Server Function=======================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  
  ##### Main Tab####################################################################################
  #### UI===========================================================================================
  ### Main menu buttons
  ## Display recipe sheet
  observeEvent(input$btn_add1_recipe_main, {
    updateF7Sheet("man_input_recipeSheet")
  })

  ## Move to recipe tab
  observeEvent(input$btn_manage_recipe_main,{
    updateF7Tabs(id="main_tabset",selected="recipe_tab")
  })
  
  
  ## Move to meal planner tab
  observeEvent(input$btn_meal_plan_main,{
    updateF7Tabs(id="main_tabset",selected="planner_tab")
  })
  
  
  ## Display dialog for pre-loaded data
  observeEvent(input$btn_preload_data_main,{
    f7Dialog(
      id="dialog_confirm_preload_data",
      title="Confirm pre-loaded data",
      type="confirm",
      text="Click OK to use pre-loaded data. Note that this will remove all unsaved data."
    )
  })
  
  
  ## Display dialog for resetting data
  observeEvent(input$btn_reset_db_main,{
    f7Dialog(
      id="dialog_confirm_reset_db_data",
      title="Confirm app reset",
      type="confirm",
      text="Click OK to reset app to initial conditions. Note that this will remove all unsaved data."
    )
  })
  
  
  ## Move to upload tab
  observeEvent(input$btn_upload_recipe_main,{
    updateF7Tabs(id="main_tabset", selected="upload_recipes")
  })
  
  
  
  #### Server=======================================================================================
  ### Initialize tmp reactiveValues
  ## Set recipe$tmp to an empty tibble
  recipe<-reactiveValues(tmp=tibble())
  
  ## Set ingred$tmp to an empty tibble
  ingred<-reactiveValues(tmp=tibble())
  
  
  ## Set recipe$list to an empty tibble
  recipe<-reactiveValues(list=tibble())
  
  ## Set ingred$list to an empty tibble
  ingred<-reactiveValues(list=tibble())
  
  
  ### Initialize database reactiveValues
  ## Set recipe$db to an empty tibble
  recipe<-reactiveValues(db=tibble())
  
  ## Set ingred$db to an empty tibble
  ingred<-reactiveValues(db=tibble())
  
  
  ### Pre-loaded data
  ## Populate recipe$db with pre-loaded recipes
  observeEvent(input$dialog_confirm_preload_data,{
    req(input$dialog_confirm_preload_data)
    recipe$db<-demo_recipeDF
    ingred$db<-demo_ingredDF
    recipe$list<-tibble()
    ingred$list<-tibble()
  })
  
  
  ### Reset
  ## Reset app to initial conditions (clear dbs)
  observeEvent(input$dialog_confirm_reset_db_data,{
    req(input$dialog_confirm_reset_db_data)
    recipe$db<-tibble()
    ingred$db<-tibble()
    recipe$list<-tibble()
    ingred$list<-tibble()
  })
  
  
  
  
  #-------------------------------------------------------
  #### NOTE: TEMP OUTPUT ####
  #temporary--see what's being stored
  output$recipe_tab<-renderTable({
    recipe$tmp
  })
  
  #temporary--see what's being stored (multiple recipes)
  output$recipe_database<-renderTable({
    recipe$db
  })
  #--------------------------------------------------------
  
  
  #-----------------------------------------------
  #### NOTE: TEMP OUTPUT ####
  #temporary--see what's being stored
  output$ingred_tab<-renderTable({
    ingred$tmp
  })
  
  
  #temporary--see what's being stored (multiple recipes)
  output$ingred_database<-renderTable({
    ingred$db
  })
#-------------------------------------------------
  
  
  
  
  ##### Manual Data Sheets##########################################################################
  #### UI===========================================================================================
  
  ### Recipe Sheet----------------------------------------------------------------------------------
  ## Display ingredient sheet 1 (sheet 2 overall)
  observeEvent(input$btn_ingred_entry_recipeSheet, {
    updateF7Sheet("man_input_ingredSheet1")
  })
  
  
  
  ### Ingredients (for recipe) Sheet 1--------------------------------------------------------------
  ## Display recipe name
  output$txt_out_recipe_ingredSheet1<-renderText({
    input$txt_recipe_recipeSheet
  })
  
  
  ## Return to recipe sheet
  observeEvent(input$btn_return_recipe_ingredSheet1, {
    updateF7Sheet("man_input_recipeSheet")
  })
  
  
  ## Display second ingredient sheet
  observeEvent(input$btn_ingred_entry_ingredSheet1, {
    updateF7Sheet("man_input_ingredSheet2")
  })
  
  ## Confirm submission of manual addition of recipe/ingredient info
  # Display modal/dialog
  #both submit buttons in {} for observeEvent to listen to them
  observeEvent(eventExpr={
    input$btn_submit_recipe_ingred_ingredSheet1|
    input$btn_submit_recipe_ingred_ingredSheet2
    }, {
    f7Dialog(
      id="dialog_confirm_manual_add",
      title="Confirm submission",
      type="confirm",
      text="Click OK to submit recipe and ingredient information."
    )
  },
  ignoreInit=TRUE)
  
  # Display toast notification & reset values after confirming submission
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    f7Toast(
      text=paste(input$txt_recipe_recipeSheet,"added to database"),
      position="center",
      closeButton=FALSE,
      closeTimeout=3500
    )
    # Recipe sheet
    #delay used because dialog confirm triggers eventReactive (moves data to db) and observeEvent
      #(clears values)
    delay(1000,
          updateF7Text(inputId="txt_recipe_recipeSheet",
                       value=character(0))
    )
    
    c("chkGrp_app_recipeSheet","chkGrp_protein_recipeSheet") %>%
      map(function(x){
        delay(1000,
              updateF7Checkbox(x,value=character(0)))
      })
    
    # Ingred sheets
    c(
      paste0("txt_ingred",1:8,"_nm_ingredSheets"),
      paste0("txt_ingred",1:8,"_size_ingredSheets"),
      paste0("stp_ingred",1:8,"_n_ingredSheets")
    ) %>%
      map(function(x){
        if(str_detect(x,"^txt")){
          delay(1000,
                updateF7Text(x,value=character(0)))
        }
        else if(str_detect(x,"^stp")){
          delay(1000,
                updateF7Stepper(inputId=x,value=1))
        }
      })
  })
  
  
  
  ## Cancel submission and return to page if "Cancel" is hit
  observeEvent(input$dialog_confirm_manual_add,{
    req(!input$dialog_confirm_manual_add)
    updateF7Sheet("man_input_ingredSheet1")
  })

  
  
  ### Ingredients (for recipe) Sheet 2--------------------------------------------------------------
  ## Display recipe name
  output$txt_out_recipe_ingredSheet2<-renderText({
    input$txt_recipe_recipeSheet
  })
  
  
  ## Return to first ingredient sheet
  observeEvent(input$btn_previous_ingred_ingredSheet2, {
    updateF7Sheet("man_input_ingredSheet1")
  })
  
  
  ## Return to recipe sheet
  observeEvent(input$btn_return_recipe_ingredSheet2, {
    updateF7Sheet("man_input_recipeSheet")
  })
  
  
  #### Back-end=====================================================================================
  ### Temp recipe info
  ## Submit recipe info (after confirming in dialog)
  observeEvent(input$dialog_confirm_manual_add, {
    req(input$dialog_confirm_manual_add)
    recipe$tmp<-tibble(
      recipe=input$txt_recipe_recipeSheet,
      appliance=toString(sort(input$chkGrp_app_recipeSheet)),
      protein=toString(sort(input$chkGrp_protein_recipeSheet))
    )
  })
  
  
  ### Recipe database info
  ## Add new recipe to database
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    # newrows<-recipe_tmpDF()
    recipe$db<-bind_rows(recipe$db,recipe$tmp)
    }
  )
  

  
  ### Temp ingredient info
  ## Submit ingredient info
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    1:8 %>%
      map_df(function(x){
        #populates tibble rows if ingredient name & size have 1+ chr
        if(nchar(input[[paste0("txt_ingred",x,"_nm_ingredSheets")]])>0 &
           nchar(input[[paste0("txt_ingred",x,"_size_ingredSheets")]])>0) {
        tibble(
          recipe = input$txt_recipe_recipeSheet,
          name = input[[paste0("txt_ingred",x,"_nm_ingredSheets")]],
          size = input[[paste0("txt_ingred",x,"_size_ingredSheets")]],
          n = input[[paste0("stp_ingred",x,"_n_ingredSheets")]]
          )
        } else {NULL}
        }) -> ingred$tmp
    }
  )
  
  
  
  ### Ingredient database info
  ## Add ingredients from new recipe to database
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    # newrows<-ingred_tmpDF()
    ingred$db<-bind_rows(ingred$db,ingred$tmp)
    }
  )
  
  



  ##### Search/Browse Recipes Tab###################################################################
  #### UI===========================================================================================
  ### Return to main menu
  observeEvent(input$btn_return_main_recipe,{
    updateF7Tabs(id="main_tabset",selected="main_tab")
  })
  
  
  ### Display card after hitting View/edit button
  # observeEvent(input$view_button,{
  #   updateF7Card(id="test_card")
  # })
  
  ### Display modal/dialog after hitting Delete button
  #both submit buttons in {} for observeEvent to listen to them
  observeEvent(eventExpr={
    input$delete_button #|
    #PLACEHOLDER FOR DELETE BUTTON ON CARDS
    }, {
    f7Dialog(
      id="dialog_confirm_delete",
      title="Confirm delete",
      type="confirm",
      text="Click OK to remove recipe from database."
    )
  },
  ignoreInit=TRUE)
  
  
  
  
  

  #### Back-end=====================================================================================
  ### Joined DF of recipes and ingredients
  ## Create reactive of recipe df joined with ingredient df
  dt_df<-reactive({
    #if reactiveValues for the recipe & ingred dbs are empty, then no dt_df is an empty tibble
    if(nrow(recipe$db)==0 & nrow(ingred$db)==0){
      tibble()
    }
    #but if there are data in each, then a combined table is made
    else if(nrow(recipe$db)>0 & nrow(ingred$db)>0){
      recipe$db %>%
        left_join(ingred$db) %>%
        unite(col="ingredient",n,size,name,sep=" ") %>%
        mutate(ingredient=str_replace(ingredient,"(?<=[0-9]) (?=[0-9])","-")) %>%
        group_by(recipe,appliance,protein) %>%
        summarize(ingredients=toString(ingredient)) %>%
        ungroup() #%>%
        #add actionButtons to Actions column
        # mutate(Actions=paste(
        #   shinyInput(actionButton,
        #              nrow(.),
        #              id="edit_",
        #              label="View/edit",
        #              class="btn-danger",
        #              onclick="Shiny.setInputValue(\"view_button\",  this.id.concat(\"_\", Math.random()))"),
        #   shinyInput(actionButton,
        #              nrow(.),
        #              id="delete_",
        #              label="Delete",
        #              icon=shiny::icon("trash"),
        #              class="btn-danger",
        #              onclick="Shiny.setInputValue(\"delete_button\",  this.id.concat(\"_\", Math.random()))")))
    }
  })
  
  
  
  ## Display database as table
  output$recipe_db_recipe<-renderDT(
    dt_df() %>%
      mutate(Actions=paste(
        shinyInput(actionButton,
                   nrow(.),
                   id="edit_",
                   label="View/edit",
                   class="btn-danger",
                   onclick="Shiny.setInputValue(\"view_button\",  this.id.concat(\"_\", Math.random()))"),
        shinyInput(actionButton,
                   nrow(.),
                   id="delete_",
                   label="Delete",
                   icon=shiny::icon("trash"),
                   class="btn-danger",
                   onclick="Shiny.setInputValue(\"delete_button\",  this.id.concat(\"_\", Math.random()))"))),
    server=FALSE,
    selection="none",
    rownames=FALSE,
    escape=FALSE,
    extensions="Buttons",
    options=list(
      dom="Bfrtip",
      pageLength=10,
      buttons=list(
        list(extend="excel",title="Grocery Assistant Database",filename="grocery_assistant"),
        list(extend="print",title="Grocery Assistant Database")
      )
    ),
    caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color:black;  
                                      font-size:200% ;","Recipe Database")
  )

  ## If no data then message is returned
  # output$blank_dt_df_recipe<-renderText({
  #   req(nrow(recipe$db)==0 & nrow(ingred$db)==0)
  #   "No data in database."
  # })
  
  
  
  ### Display toast notification and remove values after confirming deletion
  observeEvent(input$dialog_confirm_delete,{
    req(input$dialog_confirm_delete)
    deleted_row<-as.numeric(strsplit(input$delete_button,"_")[[1]][2])
    nm<-dt_df()[[deleted_row,"recipe"]]
    recipe$db<-recipe$db %>% filter(recipe!=nm)
    ingred$db<-ingred$db %>% filter(recipe!=nm)
    
    f7Toast(
      text=paste(nm,"removed from database"),
      position="center",
      closeButton=FALSE,
      closeTimeout=3500
    )
    
  })

  
  
  ##### Generate Ingredient List/Meal Planner Tab ##################################################
  #### UI===========================================================================================  
  ### See meal plan
  observeEvent(input$btn_view_plan_planner,{
    updateF7Tabs(id="main_tabset",selected="list_tab")
  })
  
  
  ### Return to main menu
  observeEvent(input$btn_return_main_planner,{
    updateF7Tabs(id="main_tabset",selected="main_tab")
  })
  
  
  ### Display dialog for resetting plan/list
  observeEvent(input$btn_reset_planList_planner,{
    f7Dialog(
      id="dialog_confirm_reset_planList_planner",
      title="Confirm plan reset",
      type="confirm",
      text="Click OK to erase meal plan and shopping list."
    )
  })
  
  
  #### Back-end=====================================================================================
  ## Display database as table
  output$recipe_db_planner<-renderDT(
    dt_df() %>%
      mutate(Actions=paste(
        shinyInput(actionButton,
                   nrow(.),
                   id="add_",
                   label="Add to plan",
                   onclick="Shiny.setInputValue(\"add_button\",  this.id.concat(\"_\", Math.random()))"))),
    server=FALSE,
    selection="none",
    rownames=FALSE,
    escape=FALSE,
    options=list(
      dom="frtip",
      pageLength=10
    ),
    caption = htmltools::tags$caption(style = "caption-side: top; text-align: center; color:black;  
                                      font-size:200% ;","Recipe Database")
  )
  
  
  ## Add item to meal plan--updates recipes and ingredients--and display toast notification
  observeEvent(input$add_button,{
    #recipe & ingreds added to meal plan
    added_row<-as.numeric(strsplit(input$add_button,"_")[[1]][2])
    #new recipe name
    recipe_nm<-dt_df()[[added_row,"recipe"]]
    #new ingredients that will be added to list
    ingreds<-ingred$db %>% 
      filter(recipe==recipe_nm) %>%
      select(-recipe) 
    #add ingredients to list
    ingred$list<-bind_rows(ingred$list,ingreds) %>%
      group_by(name,size) %>%
      summarize(n=sum(n) %>% ceiling)
    #new recipe added to list
    recipe_new<-as_tibble_col(recipe_nm,column_name="recipe")
    recipe$list<-bind_rows(recipe$list,recipe_new)
    
    #toast notification
    f7Toast(
      text=paste(recipe_nm,"added to meal plan"),
      position="center",
      closeButton=FALSE,
      closeTimeout=2000
    )
  })
  
  
  ## Reset plan and list
  observeEvent(input$dialog_confirm_reset_planList_planner,{
    req(input$dialog_confirm_reset_planList_planner)
    recipe$list<-tibble()
    ingred$list<-tibble()
  })
  
  
  ##### Meal Plan/Shopping List Tab ################################################################
  #### UI===========================================================================================
  ### Return to meal planner tab
  observeEvent(input$btn_return_planner_list,{
    updateF7Tabs(id="main_tabset",
                 selected="planner_tab")
  })
  
  ### Return to main menu
  observeEvent(input$btn_return_main_list,{
    updateF7Tabs(id="main_tabset",
                 selected="main_tab")
  })
  
  
  ## Display dialog for resetting plan/list
  observeEvent(input$btn_reset_planList_list,{
    f7Dialog(
      id="dialog_confirm_reset_planList_list",
      title="Confirm plan reset",
      type="confirm",
      text="Click OK to erase meal plan and shopping list."
    )
  })


  
  #### Back-end=====================================================================================
  ### Display meal plan and shopping list
  ## Meal plan
  # datatable
  output$recipe_list_list<-renderDT(
    recipe$list,
    rownames=FALSE,
    options=list(
      dom="ft"
    ),
    caption = htmltools::tags$caption(style = "caption-side: top; text-align: left; color:black;  
                                      font-size:150% ;","Meal Plan")
  )
  
  #htmlTable
  meal_email_table<-reactive({
    htmlTable(recipe$list,rnames=FALSE)
  })
                                    
  ## Shopping list
  #datatable
  output$shopping_list_list<-renderDT(
    ingred$list,
    rownames=FALSE,
    options=list(
      dom="ft"
    ),
    caption = htmltools::tags$caption(style = "caption-side: top; text-align: left; color:black;  
                                      font-size:150% ;","Shopping List")
  )
  
  #htmlTable
  ingredient_email_table<-reactive({
    htmlTable(ingred$list,rnames=FALSE)
  })
  
  
  ### Get a copy of plan and list
  ## By email
  # Reactive object for issuing warning if no email address provided
  
  
  observeEvent(input$btn_planList_email_list,{
    
    #logical object for minimal req for an email address entered
    address_entered<-nchar(input$text_email_address_list)>0 & 
      str_detect(input$text_email_address_list,"@")
    
    address_entered<-str_detect(input$text_email_address_list,email_str_detect)
    
    #warning returned if not entered
    shinyFeedback::feedbackWarning("text_email_address_list",
                                   !address_entered,
                                   "Please enter a valid email address")

    #prevents app from failing if blank email entered
    req(address_entered,cancelOutput=TRUE)
    
    #create body of email
    email_body<-paste0("<p> Meal plan </p>",
                       meal_email_table(),
                       "<br>",
                       "<br>",
                       "<p> Shopping list </p>",
                       ingredient_email_table()
                       )
    
    #send email
    send.mail(from="grocery.asst.app@gmail.com",
              to=paste0("<",input$text_email_address_list,">"),
              subject="meal plan & grocery list",
              body=email_body,
              smtp = list(host.name = "smtp.gmail.com", port = 587, 
                          user.name = x1, passwd = x2, ssl = TRUE),
              authenticate = TRUE,
              html=TRUE,
              send = TRUE)
    
    #create toast notification
    f7Toast(
        text=paste("Meal plan and shopping list sent to",input$text_email_address_list),
        position="center",
        closeButton=FALSE,
        closeTimeout=3000
      )
  })
  
  
  ## By screenshot
  observeEvent(input$btn_planList_screenshot_list,{
    screenshot(
      id="plan-list-screenshot",
      scale=0.8,
      filename="plan-and-list-screenshot.png"
    )
  })
  
  
  ## Download as pdf
  output$btn_planList_export_list<-downloadHandler(
    filename="meal-plan-and-shopping-list.pdf",
    content=function(file){
      tempReport<-file.path(tempdir(),"plan-and-list-template.Rmd")
      file.copy("plan-and-list-template.Rmd",tempReport,overwrite=TRUE)

      params<-list(table1=recipe$list,
                   table2=ingred$list
      )

      rmarkdown::render(input=tempReport,
                        output_file=file,
                        params=params,
                        envir=new.env(parent=globalenv()))
    }
  )

  
  
  ### Reset plan and list
  observeEvent(input$dialog_confirm_reset_planList_list,{
    req(input$dialog_confirm_reset_planList_list)
    recipe$list<-tibble()
    ingred$list<-tibble()
  })
  
  

  
  
  
  ##### Upload Recipes Tab #########################################################################
  #### UI===========================================================================================
  ### Return to main menu
  observeEvent(input$btn_return_main_upload,{
    updateF7Tabs(id="main_tabset",selected="main_tab")
  })
  
  
  
  #### Back-end=====================================================================================
  ### File download template
  ## Create template by code
  templateDF<-tibble(
    demo_recipeDF %>% 
      left_join(demo_ingredDF) %>% 
      filter(row_number() %in% 1:3)
  )
  
  
  ## Download template
  output$btn_template_download_upload <- downloadHandler(
    filename="grocery-assistant-template.csv",
    content=function(file){
      write_csv(templateDF,file)
    }
  )
  
  
  ### File upload
  ## Display toast notification that file uploaded
  observeEvent(input$file_upload_recipe_upload,{
    if(tools::file_ext(input$file_upload_recipe_upload$name) %in% c("csv","xls","xlsx")){
      f7Toast(
        text=paste(input$file_upload_recipe_upload$name,"uploaded to database"),
        position="center",
        closeButton=FALSE,
        closeTimeout=3500
      )
    }
  })
  
  
  ## Read in uploaded file 
  uploaded_file<-reactive({
    req(input$file_upload_recipe_upload)
    
    ext<-tools::file_ext(input$file_upload_recipe_upload$name)
    switch(ext,
      csv = read_csv(input$file_upload_recipe_upload$datapath,show_col_types=FALSE),
      xls = read_xls(input$file_upload_recipe_upload$datapath),
      xlsx = read_xlsx(input$file_upload_recipe_upload$datapath)
    ) %>%
      mutate(across(!n,str_to_lower))
  })

  # Split uploaded file into recipe and ingredient DFs
  recipe_upload<-reactive({
    uploaded_file() %>%
      select(recipe:protein) %>%
      distinct()
  })
  
  ingred_upload<-reactive({
    uploaded_file() %>%
      select(recipe,name:n)
  })
  
  
  ## Add uploaded files to database
  observeEvent(input$file_upload_recipe_upload,{
    #delay is to allow time for  uploaded_file() to split into recipe_upload() and ingred_upload()
    delay(1000,{
      recipe$db<-bind_rows(recipe$db,recipe_upload())
      ingred$db<-bind_rows(ingred$db,ingred_upload())
    })
  })

  
  
  # TEMP CODE: show tibble of uploaded file
  output$file_upload_table<-renderTable({
    uploaded_file()
  })
  


  
  

  
  

}





