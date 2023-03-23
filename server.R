#Created by Keith Post on 3/19/23



#--------------------------------------------------------------------------------------------------#
##### Define Server Function========================================================================
#--------------------------------------------------------------------------------------------------#
server<-function(input,output,session){
  
  ##### Manual Data Sheets==========================================================================
  #### Recipe Sheet
  ### Display sheet
  observeEvent(input$rad_op_main, {
    if(input$rad_op_main=="Manually add recipe"){
      updateF7Sheet("man_input_recipe_sheet1")
    }
  })
  
  #### Ingredients (for recipe) Sheet
  ### Display sheet
  observeEvent(input$btn_ingred_entry_sheet1, {
    updateF7Sheet("man_input_ingred_sheet2")
  })
  
  
  ### Display recipe name
  output$txt_out_recipe_sheet2<-renderText({
    input$txt_recipe_sheet1
  })
  
  
  #### Ingredients (for recipe) Sheet
  ### Display second sheet
  observeEvent(input$btn_ingred_entry_sheet2, {
    updateF7Sheet("man_input_ingred_sheet3")
  })
  
  
  ### Display recipe name
  output$txt_out_recipe_sheet3<-renderText({
    input$txt_recipe_sheet1
  })
  
}
