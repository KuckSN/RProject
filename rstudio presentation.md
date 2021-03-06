<style>
body{
    background-color: purple;
    background-image: url(https://t3.ftcdn.net/jpg/02/66/34/70/360_F_266347082_qnZLppWcnm5a4OA5cGNE1amPi01lpEmV.jpg);
    background-size: cover;
    background-position: center center;
    background-attachment: fixed;
    background-repeat: no-repeat;
    text-color: white;
}

.section .reveal .state-background{
  background-color: purple;
  background-image: url(https://t3.ftcdn.net/jpg/02/66/34/70/360_F_266347082_qnZLppWcnm5a4OA5cGNE1amPi01lpEmV.jpg);
  background-size: cover;
}

.section .reveal h1, h2, h3, h4, h5, h6{
    color: white;
    font-weight: bold;
  }

.reveal h1, .reveal h2, .reveal h3, .reveal h4, .reveal h5, .reveal h6{
  color: white;
  font-weight: bold;
  word-wrap: normal;
  -moz-hyphens: none;
}

.reveal{
  color:white;
}

.footer {
    color: black;
    background: #E8E8E8;
    position: fixed;
    top: 90%;
    text-align:center;
    width:100%;
}


.topleft {
    position: fixed;
    top:10%;
    left:10%;
}

.midcenter {
    position: fixed;
    top:20%;
    left: 90%;
}

links{
  color:dodgerblue;
}
</style>



MathX Recognizer
========================================================
author: 18K Shake It [Kuck Swee Nien  - Lim Wei Xin  - Nah Wan Jun  - Law Shiao Yin]
date: 15 Jun 2022
autosize: true

<div class="midcenter" style="height:170px; width:170px;margin-left:-150px; margin-top:-150px;">
<img src="Image.png" style="background-color:transparent; border:0px; box-shadow:none;"></img>
</div>

<div class="footer" style="margin-top:-150px;font-size:80%;border-radius:100px">
MathX Recognizer<br>
WIE2003 Group project using Machine Learning and Data Science Analysis Process on mathematical expression image data.</div>

Introduction
========================================================
class: background

This presentation reports the Group Project of WIE2003 Introduction to Data Science course guided by Dr Salimah.  

The goal of this project is to create a product that predicts the mathematical expression based on image input by user. The calculation is made and the result is presented to the user.  

This Shiny app is built entirely in R, with httr connected to our machine learning model created using Python.  

- The dataset is from Xai Nang called [**Handwritten Math Symbols Dataset**](https://www.kaggle.com/datasets/xainano/handwrittenmathsymbols?resource=download)
- This dataset contains over 100000 image of pixel 45*45
- It contains 17 expressions [+ - x / ( ) 0 1 2 3 4 5 6 7 8 9 =]

What Do MathX Recognizer Solve?
========================================================
##### *Asking Questions*
- Can computer predict mathematical symbols from images?
- How computer differentiate symbols through pixels?
- Who are the stakeholders?

##### *Cleaning Data*
- Image of 45*45 is vectorized into vector of 2025 and stored in matrix

##### *Steps in predicting Maths Symbol*
- Get image from user through httr, and preprocess it
- Apply automated label to each symbols on image
- Feed into model (CNN) and obtain output
- Feedback send through httr back to R.

Experience Creating MathX Recognizer
========================================================
This is a prototype for Mathematics Expression Recognition App! 

Due to memory, computation processing power limitation on my computer and also shinyapp.io restriction:
- The analysis use only 13% of the entire data provided.
- Since we have a remote model connected using **HTTR**, the prediction model is trained using 100% of the entire data.

It has been a challenging though fruitful experience to complete this project.

MathX Recognizer can be accessed at: [**MathX Recognizer on shinyapp.io**](https://shakeit-mathx.shinyapps.io/RProject/)


Inside MathX Recognizer 
========================================================
- The app starts with "Main" tab, that prompt users to upload image of mathematical expression.  
<img src="Main Tab.png" alt="Main Tab" style="height: 175px;">
- "Data Views" and "Data Analysis" tabs shows descriptive and exploratory analysis.
<img src="Main Tab.png" alt="Main Tab" style="height: 150px; position: absolute; left: 5%; bottom: 5%">
<img src="Main Tab.png" alt="Main Tab" style="height: 150px; position: absolute; left: 30%; bottom: 5%">
<img src="Main Tab.png" alt="Main Tab" style="height: 150px; position: absolute; left: 55%; bottom: 5%">
<img src="Main Tab.png" alt="Main Tab" style="height: 150px; position: absolute; left: 15%; bottom: -5%">
<img src="Main Tab.png" alt="Main Tab" style="height: 150px; position: absolute; left: 45%; bottom: -5%">
