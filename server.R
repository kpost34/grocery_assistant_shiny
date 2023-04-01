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
      text="Click to confirm that you would like to submit recipe and ingredient information."
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
  ### Submit recipe info (after confirming in dialog)
  recipe<-reactiveValues(tmp=tibble())
  
  observeEvent(input$dialog_confirm_manual_add, {
    req(input$dialog_confirm_manual_add)
    recipe$tmp<-tibble(
      recipe=input$txt_recipe_recipeSheet,
      appliance=toString(sort(input$chkGrp_app_recipeSheet)),
      protein=toString(sort(input$chkGrp_protein_recipeSheet))
    )
  })

  
  
  #NOTE: this will change if pre-loaded
  ### Develop recipe database
  recipe<-reactiveValues(db=tibble())
  
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    # newrows<-recipe_tmpDF()
    recipe$db<-bind_rows(recipe$db,recipe$tmp)
    }
  )
  
  
  #temporary--see what's being stored
  output$recipe_tab<-renderTable({
    recipe$tmp
  })
  
  #temporary--see what's being stored (multiple recipes)
  output$recipe_database<-renderTable({
    recipe$db
  })
  
  
  ### Submit first ingredient info
  ingred<-reactiveValues(tmp=tibble())
  
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
  
  #NOTE: this will change if pre-loaded
  ingred<-reactiveValues(db=tibble())
  
  
  ### Develop ingredient database
  observeEvent(input$dialog_confirm_manual_add,{
    req(input$dialog_confirm_manual_add)
    # newrows<-ingred_tmpDF()
    ingred$db<-bind_rows(ingred$db,ingred$tmp)
    }
  )
  
  
  
  #temporary--see what's being stored
  output$ingred_tab<-renderTable({
    ingred$tmp
  })
  
  
  #temporary--see what's being stored (multiple recipes)
  output$ingred_database<-renderTable({
    ingred$db
  })
  
  
  
  # OLD APPROACH==================
  ### Submit recipe info (after confirming in dialog)
  # recipe_tmpDF<-eventReactive(input$dialog_confirm_manual_add, {
  #   req(input$dialog_confirm_manual_add)
  #   tibble(
  #     recipe=input$txt_recipe_recipeSheet,
  #     appliance=toString(sort(input$chkGrp_app_recipeSheet)),
  #     protein=toString(sort(input$chkGrp_protein_recipeSheet))
  #   )
  # })
  # 
  # 
  # 
  # #NOTE: this will change if pre-loaded
  # recipe_data<-data.frame()
  # 
  # 
  # ### Develop recipe database
  # recipe_db<-eventReactive(
  #   eventExpr=input$dialog_confirm_manual_add,
  #   valueExpr={
  #     req(input$dialog_confirm_manual_add)
  #     newrows<-recipe_tmpDF()
  #     recipe_data<<-bind_rows(recipe_data,newrows)
  #   }
  # )
  # 
  # 
  # #temporary--see what's being stored
  # output$recipe_tab<-renderTable({
  #   recipe_tmpDF()
  # })
  # 
  # #temporary--see what's being stored (multiple recipes)
  # output$recipe_database<-renderTable({
  #   recipe_db()
  # })
  # 
  # 
  # ### Submit first ingredient info
  # ingred_tmpDF<-eventReactive(
  #   eventExpr=input$dialog_confirm_manual_add,
  #   valueExpr={
  #     req(input$dialog_confirm_manual_add)
  #     1:8 %>%
  #     map_df(function(x){
  #       #populates tibble rows if ingredient name & size have 1+ chr
  #       if(nchar(input[[paste0("txt_ingred",x,"_nm_ingredSheets")]])>0 &
  #          nchar(input[[paste0("txt_ingred",x,"_size_ingredSheets")]])>0) {
  #       tibble(
  #         recipe = input$txt_recipe_recipeSheet,
  #         name = input[[paste0("txt_ingred",x,"_nm_ingredSheets")]],
  #         size = input[[paste0("txt_ingred",x,"_size_ingredSheets")]],
  #         n = input[[paste0("stp_ingred",x,"_n_ingredSheets")]]
  #         )
  #       } else {NULL}
  #     }) 
  #   }
  # )
  # 
  # #NOTE: this will change if pre-loaded
  # ingred_data<-data.frame()
  # 
  # 
  # ### Develop ingredient database
  # ingred_db<-eventReactive(
  #   eventExpr=input$dialog_confirm_manual_add,
  #   valueExpr={
  #     req(input$dialog_confirm_manual_add)
  #     newrows<-ingred_tmpDF()
  #     ingred_data<<-bind_rows(ingred_data,newrows)
  #   }
  # )
  # 
  # 
  # 
  # #temporary--see what's being stored
  # output$ingred_tab<-renderTable({
  #   ingred_tmpDF()
  # })
  # 
  # 
  # #temporary--see what's being stored (multiple recipes)
  # output$ingred_database<-renderTable({
  #   ingred_db()
  # })



  ##### Search/Browse Recipes Tab###################################################################
  #### UI===========================================================================================
  ### Return to main menu
  observeEvent(input$btn_return_main_recipe,{
    updateF7Tabs(id="main_tabset",selected="main_tab")
  })
  

  #### Back-end=====================================================================================
  ### Create reactive of recipe df joined with ingredient df
  # dt_df<-reactive({
  #   recipe_db() %>%
  #     left_join(ingred_db()) %>%
  #     unite(col="ingredient",n,size,name,sep=" ") %>%
  #     mutate(ingredient=str_replace(ingredient,"(?<=[0-9]) (?=[0-9])","-")) %>%
  #     group_by(recipe,appliance,protein) %>%
  #     summarize(ingredients=toString(ingredient)) %>%
  #     ungroup() %>%
  #     #add actionButtons to Actions column
  #     mutate(Actions=paste(
  #       shinyInput(actionButton, 
  #                  nrow(recipe_db()),
  #                  id="edit_",
  #                  label="View/edit",
  #                  onclick=paste0("Shiny.onInputChange( \"edit_button\" , this.id)")),
  #       shinyInput(actionButton, 
  #                  nrow(recipe_db()),
  #                  id="delete_",
  #                  label="Delete",
  #                  onclick=paste0("Shiny.onInputChange( \"delete_button\" , this.id)"))),
  #       row=row_number())
  # })
  
  
  
  ### Display database as table
  # output$recipe_db_recipe<-renderDT(
  #   dt_df() %>%
  #     select(-row),
  #   server=FALSE,
  #   selection="none",
  #   rownames=FALSE,
  #   escape=FALSE,
  #   extensions="Buttons",
  #   options=list(
  #     dom="Bfrtip",
  #     pageLength=10,
  #     buttons=list(
  #       list(extend="excel",title="Grocery Assistant Database",filename="grocery_assistant"),
  #       list(extend="print",title="Grocery Assistant Database")
  #     )
  #   )
  # )
  
  
  ### Show dialog after clicking delete button
  # recipe_db<-eventReactive(input$delete_button,{
  #   deleted_row<-as.numeric(strsplit(input$delete_button,"_")[[1]][2])
  #   nm<-dt_df()[[deleted_row,"recipe"]]
  #   recipe_db<<-recipe_db() %>% filter(recipe!=nm)
  #   # ingred_db<<-ingred_db() %>% filter(recipe!=nm)
  # },
  # ignoreInit=TRUE)
  


  

  
  
}





