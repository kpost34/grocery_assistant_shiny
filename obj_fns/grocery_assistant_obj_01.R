#Created by Keith Post on 3/19/23


#### Recipe Sheet Choices===========================================================================
### Appliance Choices
app_choices_sheet1<-c(
  "Stove",
  "Oven",
  "Slow cooker",
  "Grill",
  "Broiler"
  )


### Protein Source Choices
protein_choices_sheet1<-c(
  "Chicken",
  "Beef",
  "Steak",
  "Pork",
  "Fish",
  "Vegetarian"
)



##### View Recipes==================================================================================
#### Add/Update Image by File--image file extensions
img_ext<-c(
  "jpg",
  "png",
  "gif",
  "webp",
  "tiff",
  "psd",
  "raw",
  "bmp",
  "heif",
  "indd",
  "jpeg 2000",
  "svg",
  "ai",
  "eps",
  "pdf"
)




#### App Test=======================================================================================
### Generate recipe & ingredient data
demo_recipeDF <- tibble(
  # id=1:3,
  recipe=c("Chicken alfredo","Chicken tacos","Pot roast"),
  appliance=c("Stove, Slow cooker","Slow cooker","Slow cooker"),
  protein=c("Chicken","Chicken","Beef")
)


demo_ingredDF <- tibble(
  recipe=c(
    rep("Chicken alfredo",3),
    rep("Chicken tacos",4),
    rep("Pot roast",4)
  ),
  name=c(
    "chicken breast","alfredo sauce","pasta",
    "chicken breast","salsa","taco seasoning", "corn tortillas",
    "chuck roast","golden potatoes","baby carrots","beef broth"
  ),
  size=c(
    "large","12 oz jar","16 oz package",
    "large","12 oz jar","packet","30-pack",
    "3 lb","whole","16 oz bag","15 oz can"
  ),
  n=c(
    2,3,1,
    2,1,1,1,
    1,6,1,1
  )
)




#### Meal Plan & Shopping List======================================================================
### Send email copy
## String for detecting legitimate emails
email_str_detect<-"[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*@(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?"





##### Instructions==================================================================================
### Overview
objective_txt <- "The Grocery Assistant App helps users build a saveable database of recipes,
plan meals, and create grocery shopping lists. All tasks can be achieved directly through the
app: adding recipes, viewing images of dishes, saving/loading databases of recipes, and developing
ingredient lists for purchase. However, alternative file-based tasks are available for many of
the tasks (e.g., batch-adding recipes, saving the database). To test out the app, simply hit
'Test App with Pre-Loaded Data'"



### How to
## Add recipes
recipe_add_intro_txt <- "New recipes can be added in two ways: 1) manually by entering information
into web forms or 2) by file."


# Manual recipe add
recipe_add_manual_txt <- "To manually add a recipe, select 'Manually Add Recipe', which displays
an information sheet. Complete this form, select 'Enter Ingredients', and fill out info for at 
least one ingredient. If more than four ingredients are necessary, select 'Add More Ingredients'
at the top-right, complete the necessary info, and hit 'Submit Recipe Info & Ingredients' at
the bottom of the sheet. Confirm the selection."


# Batch recipe add (by file)
recipe_add_file_txt <- "Recipes can be added in batches by uploading a formatted file. To start
this process, select 'Add Recipes from File`. Next, simply follow the steps provided. 
1) Download a template (in csv format) by clicking that button. A key is provided to help understand
the columns. 

2) Using the template and key, populate the file using the information associated with one or
more recipes.

3) Save the file using a logical file name.

4) Click `Upload File`, navigate to the file, and click `Open`. If the file format is incorrect,
a message will be returned. Please correct the error(s) and re-save the file.

5) A preview of the uploaded file will appear hear. Please look it over.

6) If the preview (from step 5), looks fine, click `Confirm`.

7) As the message states, if you'd like to load more recipes in this manner. Please create another
file, save, and hit `Upload File`."


## Recipe browser
recipe_browser_txt <- "To view, edit, delete, and/or save recipes, hit the second blue button on 
the main menu. If no data are present in the database (whether by adding recipes through sheets
or files, loading a database from file or the app, or being in test mode (see below)), then
the table will be empty. Conversely, with data present in the database, the table will be populated.

If there are many recipes in the database, searching by keyword may be helpful to find a dish,
protein, or ingredient. This can be achieved using the Search field in the upper-right. A copy of
the database can be printed using the button in the upper-left and downloaded with the blue button
in the lower-right. 

For specific, dish-based tasks, use the Action column in the table. For instance, if a user wants 
to edit recipe information or ingredients, click one of those buttons in the Actions column. Either 
button will bring up a popup. Edit the information, then click the green
update button in the upper-left corner. To delete a recipe from the database, click the Delete
button. Lastly, to see an image of the recipe, select View. If an image of the dish is present
in the database, it will be displayed in a popup. If not, a placeholder text image will be
shown instead. To update the image, click the blue button, choose the file, and select Open. The
new image will be displayed and saved to the app."


## Plan meals
planner_list_txt <- "Planning meals is simple. Hit `Meal Planner` from the main menu, and select
the desired dishes using the `Add to plan` buttons in the Actions column. If you want to add a
meal more than once to a plan, simply click the button multiple times. Every dish added to the
plan will be used to build a grocery list. If you make a mistake, simply reset the list with
the red button. 

To view the plan and shopping list, hit the blue button. The meal plan is 
displayed in the upper table, and the grocery list in the lower table. If you already have some
of the ingredients, or if you need additional quantities of a particular ingredient, simply
click the +, -, or delete (trash icon) buttons in the shopping list table. Search fields are 
provided for each table. If, after viewing the plan and/or list, you prefer to start over, click
the 'Reset Plan/List' button, and navigate back to the planner using the 'Return to Meal Planner'
button. 

If you like the plan and list, it can be copied in multiple ways. First, it can be emailed by 
providing an email address and hitting 'Send Email'. If you prefer, a simple screenshot, then
hit the second blue button. Or if you rather have this information as a pdf, click the 'Export
Plan & List to PDF'."


## Load database from file
file_load_db_txt <- "Loading a database from file is very simple. If you saved a copy of your database
as a csv file using `View/Edit/Delete/Save Recipes` -> 'Download a Copy', then simply click on
'Load Database From File', navigate to the saved file, click Open, and confirm. If you go to the
database browser, your dishes and associated ingredients should be in the table."


## Engage in user id mode
user_id_txt <- "The Grocery Assistant App is designed to with and without a user id. If operating 
anonymously, a user can add recipes (via info sheets or file), edit recipe and ingredient info,
delete dishes, save a copy of the database (via file download), build meal plans and grocery
lists, and communicate them in multiple ways (e.g., screenshot, email, pdf). If the user wants
to maintain a database is this manner, they would need to remember to save a copy of their
database each time before exiting and load it at the start of session.

However, with the user id mode, all the above features are available PLUS the user can save/load
the database directly to/from the app (see below). A user id is simply '[firstinitial]_[lastname]'.
Enter this in the field on the main page, and click `Submit User Id`. It will now appear as 
'Current User' on the main page as well as many pages of the app."

## Save/load database from app
app_save_load_txt <- "As explained above, one convenient feature of this app is the ability to save 
and load a database directly to/from the app without the need of csv or excel files. Please note that 
this is only possible when operating the app as a user (see above).

Once a valid user id is provided, these features become available. To save the currently loaded
database to the app, simply navigate to the database browser page, click the 'Save Database to
App' button, and confirm. That's it!

To load the database from the app, make sure a valid user id has been submitted. Once completed,
the blue 'Load Database from App' button will appear. Simply click it and confirm."


## Test app with pre-loaded data
preload_txt <- "To test the app without having to manually enter information, select 'Test App with
Pre-Loaded Data' from the main menu and confirm the entry. Now the current user becomes 'test mode',
which is displayed as 't_mode' in the app. Three recipes have been loaded, which can be found in
the 'View/Edit/Delete/Save Recipes` page. Specifically, an image of each dish can be viewed (and
replaced), recipe and ingredient information can be modified, and the dish can be deleted. A copy
of the recipes "


## Reset app
reset_txt <- "To reset the recipes loaded into the app, the meal plan, and the user_id, click on
'Reset App' and confirm. Note that this does not delete information saved to the app."


### Put together objects
## Develop vector of names
nm_instruct <- c("View, edit, save, and delete recipes",
                 "Plan meals & build a grocery list",
                 "Load database from file",
                 "Engage in user id mode",
                 "Save and load database from app",
                 "Test app with pre-loaded data",
                 "Reset app")

### Develop list of character vectors with names

list_instruct <- list(recipe_browser_txt,planner_list_txt,file_load_db_txt,user_id_txt,
                      app_save_load_txt,preload_txt,reset_txt) %>%
  set_names(nm_instruct)









