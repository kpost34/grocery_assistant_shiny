#Created by Keith Post on 3/19/23



#--------------------------------------------------------------------------------------------------#
###### Define Server Function=======================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  
  ##### Manual Data Sheets##########################################################################
  #### UI===========================================================================================
  ### Display recipe sheet
  observeEvent(input$rad_op_main, {
    if(input$rad_op_main=="Manually add recipe"){
      updateF7Sheet("man_input_recipeSheet")
    }
  })
  
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
  
  
  ### Submit ingredient info
  # eventReactive(input$btn_submit_ingred_sheet2, {
  #   
  # })
  
  
  ## Return to recipe sheet
  observeEvent(input$btn_return_recipe_ingredSheet1, {
    updateF7Sheet("man_input_recipeSheet")
  })
  
  ## Display second ingredient sheet
  observeEvent(input$btn_ingred_entry_ingredSheet1, {
    updateF7Sheet("man_input_ingredSheet2")
  })
  
  
  
  ### Ingredients (for recipe) Sheet 2--------------------------------------------------------------
  ## Display recipe name
  output$txt_out_recipe_ingredSheet2<-renderText({
    input$txt_recipe_recipeSheet
  })
  
  
  ### Submit ingredient info
  # eventReactive(input$btn_submit_ingred_sheet3, {
  #   
  # })
  
  
  ## Return to first ingredient sheet
  observeEvent(input$btn_previous_ingred_ingredSheet2, {
    updateF7Sheet("man_input_ingredSheet1")
  })
  
  
  ## Return to recipe sheet
  observeEvent(input$btn_return_recipe_ingredSheet2, {
    updateF7Sheet("man_input_recipeSheet")
  })
  
  
  #### Back-end=====================================================================================
  ## Submit recipe info
  recipe_tmpDF<-eventReactive(input$btn_submit_recipe_ingred_ingredSheet1, {
    tibble(
      recipe=input$txt_recipe_recipeSheet,
      appliance=toString(sort(input$chkGrp_app_recipeSheet)),
      protein=toString(sort(input$chkGrp_protein_recipeSheet))
    )
  })
  
  #temporary--see what's being stored
  output$recipe_tab<-renderTable({
    recipe_tmpDF()
  })
  
  
  ### Submit first ingredient sheet
  ingred_list<-list("name","size","n")
  
  
  # ingred_tmpDF<-eventReactive(
  #   eventExpr=input$btn_submit_recipe_ingred_ingredSheet1,
  #   valueExpr={
  #     1:8 %>%
  #     map_df(function(x){
  #       if(length(input[[paste0("txt_ingred ",x,"_nm_ingredSheets")]])==0){
  #         NULL
  #       }
  #       if(length(input[[paste0("txt_ingred",x,"_nm_ingredSheets")]])>0){
  #       tibble(
  #         recipe = input$txt_recipe_recipeSheet,
  #         name = input[[paste0("txt_ingred",x,"_nm_ingredSheets")]],
  #         size = input[[paste0("txt_ingred",x,"_size_ingredSheets")]],
  #         n = input[[paste0("stp_ingred",x,"_n_ingredSheets")]]
  #         )
  #       }
  #     }) %>%
  #       filter(str_detect(name,"[:alpha:]"))
  #   }
  # )
  
  ingred_tmpDF<-eventReactive(
    eventExpr=input$btn_submit_recipe_ingred_ingredSheet1,
    valueExpr={
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
      }) 
    }
  )
  
  
  
  
  
  
  #temporary--see what's being stored
  output$ingred_tab<-renderTable({
    ingred_tmpDF()
  })

  
  
  ### Submit second ingredient sheet
  
  
  
}





