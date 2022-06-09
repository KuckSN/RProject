# MathX Recognizer
### **What is MathX Recognizer**
Mathematical Expression Recognizer (MathX Recognizer) is essentially a Machine Learning model trained to recognize mathematical expression into text form that can be computed by computers.  
<br />

### **How to use MathX Recognizer**
In the "Main" tab, users only have to upload an image of mathematical expression. Then, our machine learning model will predict read the mathematical expression from the image and compute the result of the mathematical expression. Result of the mathematical expression will be return to the user.    
<br /> 


### **Training Machine Learning Model**
We used tensorflow to train the model that recognize mathematical expression and deploy it on the Internet.  
Then, we use httr library as API to send image of mathematical expression from user to this model and obtain the result predicted by model.  
<br />

### **About this MathX Recognizer**
In this shiny project, we shows descriptive, exploratory and predictive analysis on mathematical expression dataset.  
<br />

#### **Main Tab**
Type of Analysis: Predictive Analysis  <br />
Usage: Users input image of mathematical expression and our MathX Recognizer recognizes the mathematical expression and calculate the result of the expression.      
<br />

#### **Data Views Tab**
##### **Data Table**
Type of Analysis: Descriptive Analysis  <br />
Usage: Alter "First n rows" and press "Submit" to show the first n lines of our vectorized dataset.    
<br />

##### **Image View**
Type of Analysis: Descriptive Analysis  <br />
Usage: Alter "Visuaize which row?" and press "Submit" to show randomly picked mathematical expression. The image view and pixel view of the chosen mathematical expression is shown.    
<br />

##### **All Symbols Image**
Type of Analysis: Descriptive Analysis  <br />
Usage: Press "Submit" to show each randomly picked mathematical expression.    
<br />

##### **Data Summary**
Type of Analysis: Exploratory Data Analysis & Descriptive Analysis  <br />
Usage:   <br />
1) Number of each mathematical expression  <br />
2) Dimension of our dataset  <br />
3) Summary Value of First Image  <br />
4) Structure of our dataset    
<br />

#### **Data Analysis Tab**
##### **Scatter Plot**
Type of Analysis: Exploratory Data Analysis  <br />
Usage: Find the relationship between pixels pair of image and determine how the pixels pair contribute to the type of mathematical expression    
<br />

##### **Scree Plot**
Type of Analysis: Exploratory Data Analysis  <br />
Usage: Alter "Amount of PCA Points" and press "Submit" to show the significance of pixel value that contribute to the type of mathematical expression.  
<br />
 


### **Data Science Process**
#### **Asking Questions**
1) Can I let computer recognizes mathematical expression through images? [predictive analysis]  <bt />
2) Can computer processes mathematical expression through images? [predictive analysis]  <br />
3) What is the relationship between pca (counted from pixel) and the mathematical expression? [exploratory analysis]  <br />
4) What are the mathematical expressions that our MathX Recognizer processes? [descriptive analysis]    
<br />

#### **Finding Data**
The data is obtained from this website: https://www.kaggle.com/datasets/xainano/handwrittenmathsymbols?resource=download  
From this source, the total images collected are 122280 with pixel of 2025 each.  
<br />


#### **Getting Data**
From image data of our mathematical expression [+ - x / 0 1 2 3 4 5 6 7 8 9 0 ( )], we vectorized the pixel of each image and store in matrix and data frame of dimension 17016 * 2025. 17016 is the number of dataset and 2025 is the pixel of each image of size 45 * 45. The process of data vectorization is in line 16 to line 27 in Project.R.      
Due to laptop processor constraints, we are only able to use approximately 13% of the original dataset, with 17016 images with pixel of 2025 each. This subset of data is extracted through functions in small_data.R and stored in small_matrix and small_y.  
<br />  

#### **Analysis Data**
Descriptive, Exploratory and Predictive Analysis    
Useful functions in visualizer.R:  
1) view_matrix(x) - show the first x row of vectorized dataset  
2) get_random(x)  - obtain a random number that points to selected mathematical expression that will be an input for randomized_image(random_pick) and get_randomized_image(random_pick)  
3) randomized_image(random_pick) - Output the selected line of dataset as image  
4) get_randomized_image(random_pick) - Output the selected line of dataset as a 45*45 matrix    
5) all_randomized_image() - Output randomized image of each mathematical expression    
6) y_summary() - Output Bar Plot of available mathematical expression  
7) eig_visualizer(ncp_num, scaled) - Output Scree Plot of PCA value of image dataset. ncp_num determine the first n pixel to be included in Scree Plot and scaled determine whether to use a scaled PCA value of dataset  
8) colour(index, cols, x, y, truth) - Output Scatter Plot and specified mathematical expression [index] being colored [cols]. x determine the first point of PCA value of image dataset and y determine the second point of PCA value of image dataset. truth is boolean that determine whether to use scaled PCA value of unscaled PCA value for Scatter Plot.  


<br />



