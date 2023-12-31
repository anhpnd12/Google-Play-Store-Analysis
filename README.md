# Google-Playstore-Analysis

## 1. Introduction

Android is the widely favored operating system globally, with an extensive user base of more than 2.5 billion people across 190 countries. Over the last ten years, Google Play, the primary platform for Android applications, has witnessed remarkable growth, generating a substantial revenue of $38.6 billion in 2020 alone. The Play Store boasted an impressive collection of over 2.9 million apps available for download in 2020, which were collectively installed a staggering 108 billion times.

In Google Play Store market, users play a key role in driving revenue for app developers. The success of an app heavily depends on developers’ understanding of customer preferences and the ability to create popular apps. Failing to meet customer demand can waste both time and effort for businesses. In this Google Play Store project, I aim to understanding user preferences and the factors that contribute to user decision of downloading an app by answering the following questions:

###### ● 1. Which factors can impact user downloads in the Google Play Store?

###### ● 2. Does the pricing strategy of apps in the Google Play Store influence their user downloads?

###### ● 3. Is there a significant association between the most popular category’s app download and Rating, Reviews, and Type?

The overall flow of analysis steps consists of three steps: data cleaning, model execution and results interpretation. For analysis purpose, first I collect the data from Kaggle, an open-source secondary databases website. Then in the next step, I clean the data by removing invalid values and useless variables to minimize the error percentage. When the dataset is ready, I analyze the data using statistic models accordingly with each research question and interpret the results to provide a clear vision of the relationship among the areas of interest. Finally, I include my insights from the analysis for app developers in the last section called Summary of Findings.

## 2. Data Preprocessing

### 2.1 Data collection process

I have the dataset from Kaggle, includes 10,841 Google Play Store apps, collected via web scraping in 2019. It is the list of apps with information such as app name, category, rating, and more.

![Apps Information](./img/Image-1-Apps-information.png)
_Image 1: Apps information_

### 2.2 Understand data variables

* App: Application name
* Category (categorical): Category the app belongs to
* Rating (continuous): Overall user rating of the app (as when scraped)
* Reviews (continuous): Number of user reviews for the app (as when scraped)
* Size (continuous): Size of the app (as when scraped)
* Installs (continuous): Number of user downloads/installs for the app (as when scraped)
* Type (categorical): Paid or Free
* Price (continuous): Price of the app (as when scraped)
* Content Rating (categorical): Age group the app is targeted at - Children / Mature 21+ / Adult
* Genres (categorical): An app can belong to multiple genres (apart from its main category)

### 2.3 Data cleaning

The raw data contains 10,841 values, with too many invalid values and variables in the wrong format (continuous become categorical). Data cleaning includes the following steps:

* Eliminate irrelevant variables: First, I dropped some variables in the dataset to exclude those that are irrelevant to my analysis project. This step was done by common sense. I removed size, last updated date, current version, and android version because they are not useful. I also removed Genres as it is a subset of Category variable and there could be multiple values of Genre stored in one row, making it hard to analyze it.
* Remove NULL values: This is a very crucial step in every analysis and model, which on doing, improves the accuracy of my analysis.
* Remove invalid characters: The were multiple symbols in the continuous variables, making it hard to convert them into numeric. I removed all commas, characters, plus sign to easily convert.
* Type variable: There were 3 values (0, Free, Paid). I replaced 0 by Free to turn it into a binary variable
* Category variable: Dropped the invalid value called “1.9" which was an outliner with no installs.
* Convert continuous variables (Installs, Rating, Reviews, Price) to numeric format and categorical variables (Category, Type, Content.Rating) to factor format
* After cleaning, my dataset has 9,366 values, sufficient enough for my analysis.

## 3. Exploratory Data Analysis (EDA)

Speaking of spotting successful apps, almost everyone will relate to high installations (popularity), high rating and high number of reviews (customer feedback and satisfaction towards apps). Various factor can contribute to the chance to become ‘successful’ of an app. Now, let’s explore some plots/charts of the main variables in the dataset to gain an overview of Google Play Store apps.

![App’s rating distribution within the dataset](./img/Plot-1-Apps-rating-distribution-within-the-dataset.png)
_Plot 1: App’s rating distribution within the dataset_

In this dataset, apps’ rating are quite high. Most ratings concentrate in the range of (4-5).

![Number of installs by rating](./img/Plot-2-Number-of-installs-by-rating.png)
_Plot 2: Number of installs by rating_

While plot 1 depicts the distribution of ratings, plot 2 shows the relationship between Installs and Rating. The upward chart shows a positive relationship between Rating and Installs. Assumption is that, Rating may have statistical impacts on app installs, as the higher ratings are the more apps installed. However, as Rating come close to 5, the number of installations dropped. Another assumption can be made is that some rating ranges may have statistical impacts on app installs while some do not.

![Number of installs by review](./img/Plot-3-Number-of-installs-by-review.png)
_Plot 3: Number of installs by review_

Move to Review variable, I noticed the upward trend in the chart suggests that higher levels of reviews are associated with greater app installations. This can imply a potential correlation between user feedback and app popularity / review may have a statistical impact on app installs.

![Number of installs by category](./img/Plot-4-Number-of-installs-by-category.png)
_Plot 4: Number of installs by category_

Looking at the chart, some noticeable categories that have much higher number of installations than the rest are Game, Communication, etc which leads to an assumption that Apps in some specific categories may have a statistical effect with installs number.

![Number of installs by Type (Free/Paid)](./img/Plot-5-Number-of-installs-by-Type.png)
_Plot 5: Number of installs by Type (Free/Paid)_

![Number of installs by Price](./img/Plot-6-Number-of-installs-by-Price.png)
_Plot 6: Number of installs by Price_

Looking at chart 5, majority of app downloads are free app and in chart 6, prices in lower range associate with a higher number of app installations. Assumption is that, pricing strategy may affect user behavior in installing apps, and the cheaper the price is, the more apps are installed.

![Number of installs by Content.Rating (Age)](./img/Plot-7-Number-of-installs-by-Content.Rating.png)
_Plot 7: Number of installs by Content.Rating (Age)_

Finally, plot 7 illustrates the relationship between Install and Content.Rating. App for everyone has a much higher number of installations than the rest. This may implies a significant relationship between app for everyone vs installs.

Based on the assumptions in the EDA steps, I have some assumptions for my research question:

1. Factors that impact user downloads in the Google Play Store can be Type, Price, Review. Besides that, some category (not all), rating range (not all), Content.Rating everyone (not all) may also have significant impact to app downloads
2. Pricing strategy of apps in the Google Play Store influence user downloads
3. Haven’t gather enough sufficient information

## 4. Model (Analytical Methods)

#### 4.1 Question 1: Which factors impact user downloads in the Google Play Store?

##### Hypotheses:

* Null Hypothesis (H0): There is no significant relationship between the independent variables (Category, Rating, Reviews, Type, Content.Rating) and the dependent variable (Installs).
* Alternative Hypothesis (H1): There is a significant relationship between at least one of the independent variables (Category, Rating, Reviews, Type, Content.Rating) and the dependent variable (Installs).

##### Statistical Methods:

To test these hypotheses, I employ linear regression analysis. Multiple linear regression allows me to assess the relationship between multiple independent variables and a single dependent variable.

* Defining variables: include Category (categorical), Rating (continuous), Reviews (continuous), Type (categorical), Content.Rating (categorical) as independent variables to predict Installs dependent variable.
* Performance Metrics: p-values < 0.05 for individual coefficients indicate that the corresponding independent variables significantly impact user downloads (Installs)

#### 4.2 Question 2: Does the pricing strategy of apps in the Google Play Store influence their user downloads?

##### Hypotheses:

* Null Hypothesis (H0): The pricing strategy of apps does not significantly influence their user downloads.
* Alternative Hypothesis (H1): The pricing strategy of apps significantly influences their user downloads.

##### Statistical Methods:

To test these hypotheses, I use a combination of statistical methods, including:

* Two-Sample T-Test: Compare the mean user downloads (Installs) of paid and free apps to determine if there's a significant difference between the two groups.
* Linear Regression: Explore the relationship between user downloads and pricing among paid apps. Use price ranges as categorical variables to see how different price ranges influence user downloads.

##### Defining variables:

* For t-test, the dependent variable is the number of user downloads (Installs), the independent variable is app Type (paid or free).
* For linear regression, the dependent variable is Installs, the independent variable is price_ranges (a categorical variable created by subsetting all free apps from the data, then create price ranges from “Price” variable)

##### Performance Metrics:

* For t-test, I'll look at the p-value to determine if the difference in user downloads between paid and free apps is statistically significant (typically p-value < 0.05).
* For the linear regression, I'll evaluate the significance of the coefficients for the price range variables. A significant coefficient indicates that the price range has an impact on user downloads within paid apps.

#### 4.3 Question 3: Is there a significant association between the most popular category’s app download and Rating, Reviews, and Type?

##### Hypotheses:

* Null Hypothesis (H0): There is no significant association between the most popular app category's app download and Rating, Reviews, and Type.
* Alternative Hypothesis (H1): There is a significant association between the most popular app category's app download and at least one of the variables: Rating, Reviews, and Type.

##### Statistical Methods:

To test these hypotheses, I use linear regression to explore the relationship between Installs (dependent variable) and Rating, Reviews, and Type (independent variables) within the most popular category.

* Defining variables: The dependent variable is Installs while Rating, Reviews, and Type are independent variables
* Performance Metrics: Low p-values (< 0.05) for individual variables indicate significant associations

#### 5. Results interpretation

#### Question 1: Which factors impact user downloads in the Google Play Store?

Results of the multiple linear regression indicated that there was a collective significant effect between Category, Rating, Reviews, Type, Content.Rating and app Installs, (F(40, 9325) = 176.6, p < .001, R2 = .431).

Detail outcome:

* While most of the categories do not show significant impact, some few app categories that have a positive significant impact on app installs are: CategoryCOMMUNICATION (β = 53.27, p < .001), CategoryNEWS_AND_MAGAZINES (β = 27.50, p = .006), CategoryPRODUCTIVITY (β = 33.56, p < .001), CategoryTRAVEL_AND_LOCAL (β = 24.45, p = .01) and CategoryVIDEO_PLAYERS (β = 24.73, p = .01)
* Reviews also has a positive significant impact on the number of app installs (β = 18.17, p < .001). For each additional review, the number of downloads is estimated to increase by approximately 18.
* TypePaid is also significant (β = -74.76, p = 0.009). The negative coefficient suggests that paid apps tend to have approximately 74 times fewer user downloads compared to free apps.
* Rating (β = 2.40, p = .089) has a p-value close to significant level, suggest that there may have a marginal impact on the number of app installs
* Content.Rating have p-value larger than significant level, suggest that age group have no significant impact on the number of app installs in Google Play Store

##### Conclusion about factors influence user downloads:

The results reject H0, support H1. The number of reviews, app type (paid or free), and the app's category are substantial predictors of the number of app installations. Communication, news and magazines, productivity, travel & local, and video players categories associate with higher app installs.

The influence of the app's rating on installs, while not statistically significant, suggests a potential trend that need further exploration.

### 5.2 Question 2: Does the pricing strategy of apps in the Google Play Store influence their user downloads?

##### a. Two-Sample T-Test between Installs and Type:

The t-test revealed a significant difference in mean installs between paid and free apps in the Google Play Store, t(8731) = 18.884, p < .001. Free apps tend to have significantly higher mean user downloads (approximately 19 million) compared to Paid apps (approximately 113,000). This outcome supports H1, indicating that the pricing strategy does influence the user downloads of apps in the Google Play Store.

##### b. Linear regression between Installs and price_ranges:

Pre-processing:

* Subset dataset for paid apps only by excluding all free apps (Price = 0)
* Create a new categorical called “price ranges” from the “Price” variable from the subset dataset. The price range is based on the dataset summary above: "0.99-1.99", "1.99-2.99", "2.99-4.99", "4.99-13.91", "Above average" (13.91-400)

Results of the linear regression indicated that there was a significant effect between installs and price ranges, (F(4, 642) = 2.706, p = 0.02952, R2 = .017)

* The intercept (β = 118339, p = 0.0148) suggests that there's a positive significant relationship with number of installs when the price range is at $0.99-1.99
* $4.99-13.91 (β = 213711, p = 0.0194) also suggests that there's a positive significant relationship with number of installs when the price range is at $4.99-13.91
* The other price range categories, $1.99-2.99, $2.99-4.99, and “Above average”, do not show statistically significant effects on app installs

![Bar plot visualizes the relationship between price ranges and Installs](./img/Plot-8-Bar-plot-visualizes-the-relationship-between-price-ranges-and-Installs.png)
_Plot 8: Bar plot visualizes the relationship between price ranges and Installs, with $0.99-1.99 and $4.99-13.91 associates with higher download activity_

##### Conclusion:

My comprehensive analysis supports H1 hypothesis, revealing that the pricing strategy significantly influences the user downloads of apps within the Google Play Store. Additionally, within paid apps, specific price ranges $0.99-1.99 and $4.99-13.91 have significant influence on user downloads. These insights can be used to help app developers optimize their pricing strategies and optimize their revenue stream in the future.

### 5.3 Question 3: Is there a significant association between the most popular category’s app download and Rating, Reviews, and Type?

##### a. Pre-processing:

To determine the most popular app category, I decided to based that on these three criterias:

* Highest average number of Installs per category
* Highest average number of Reviews per category
* Highest average number of Ratings per category

![Chart visualizes average installs, reviews, and ratings among category](./img/Plot-9-Chart-visualizes-average-installs-reviews-and-ratings-among-category.png)
_Plot 9: Chart visualizes average installs, reviews, and ratings among category_

Winning 2 criterias among 3, the most popular app category is "Communication"

Subset the dataset to only include Communication apps

##### b. Finding

Results of the multiple linear regression indicated that there was a collective significant effect between Rating, Reviews, Type and app Installs, (F(3, 324) = 88.34, p < .001, R2 = .4449).

* In detail, Reviews is the only predictor shows positive, significant impact on the number of app installs (β = 18.69, p < .001), indicating that an increase in the number of reviews was associated with higher number of communication app installations
* Rating and Type do not appear to have a significant impact on app downloads within the 'Communication' category.

##### Conclusion:

The most popular app category is "Communication". The outcome suggests that Reviews significantly influence app downloads within the Communication category, reject null hypothesis and accept the alternative hypothesis. Reviews is associated with a positive increase in Communication apps.

## 6. Further exploration: Rating impacts on the number of installs

During the analysis for the first research question, I found that Rating demonstrated a p-value near the significant threshold (0.08), suggesting a marginal impact on Installs. A marginal p-value could be indicative of a potential trend that might become more pronounced when considering specific ranges of Rating variable. Therefore, I want to delve into Rating variable, especially High Rating apps for more insights to support the first research question on the factors that impact app downloads. I attempt to analyze using this sub-research question:

##### ● Do mobile apps with higher ratings exhibit a significantly higher number of installs?

##### Hypotheses:

* Null Hypothesis (H0): There is no significant difference in the number of app downloads among different rating ranges.
* Alternative Hypothesis (H1): There is a significant difference in the number of app downloads among different rating ranges.

##### Statistical Methods:

To test these hypotheses, I will use linear regression to determine if there's a significant difference in means among these groups.

* Defining variables: The dependent variable is Installs, while independent variable is the categorical rating_ranges, representing different ranges of app ratings
* Performance Metrics: Low p-values (< 0.05) indicates whether there's a significant difference among the groups

##### Pre-processing:

Create a new categorical called “rating ranges” from the “Rating” variable from the dataset. The rating range is based on the dataset summary above: "1-4", "4-4.192", "4.192-4.3", "4.3-4.5", "4.5-5"

##### Finding:

Results of the linear regression indicated that there was a significant effect between rating ranges and app installs, (F(4, 9361) = 30.43, p < .001, R2 = .01242).

* Rating_ranges4.192-4.3 (β = 19260577, p < .001) and Rating_ranges4.3-4.5 (β = 23525430, p < .001) are highly statistically significant. This indicates that apps with ratings falling within these ranges significantly impact app downloads. The positive coefficient (19260577 and 23525430) suggests a positive increase in installs when the rating range is at 4.192-4.3 and 4.3-4.5
* The other rating range categories, 4-4.192 and 4.5-5 categories do not show statistically significant effects on app installs

![CBar plot visualize the relationship between rating ranges and Installs](./img/Plot-10-Bar-plot-visualizes-the-relationship-between-rating-ranges-and-Installs.png)
_Plot 10: Bar plot visualize the relationship between rating ranges and Installs, with 4.192-4.3 and 4.3-4.5 associates with much higher download activity_

##### Conclusion:

The results provide evidence to reject the null hypothesis, indicating that there is a significant difference in the number of app downloads among different rating range categories. Specifically, apps falling within the 4.192-4.3 and 4.3-4.5 rating ranges exhibit a highly significant impact on app downloads and are associated with a positive increase in app installs.

This finding implies that the relationship between app ratings and app downloads varies across different rating ranges, and we cannot conclude mobile apps with higher ratings significantly exhibit a higher number of installs. Instead, Google Play Store app creators and developers should focus not only on achieving higher ratings but also on maintaining a certain range of ratings (4.192-4.5) to effectively impact app downloads.

## 7. Summary of Findings

In conclusion, my study's findings reveal significant associations between various factors (Category, Rating, Reviews, Type, Content.Rating) and app installations within the Google Play Store. From these insights, I have some recommendations for app developers and businesses to enhance their strategies in the competitive app landscape.

##### User Preference Factors and App Downloads:

Understanding factors impact users' behavior to download an app is crucial. Based on my findings, I recommend that app developers prioritize investing in those categories with a significant positive impact on installs, such as Communication, Productivity, Travel and Local, Video Players, and News and Magazines. By focusing on these categories, developers can target audiences that are more inclined to download apps.

Additionally, positive user review not only enhances user trust but also drives more app installations, so developers should also encourage users to leave positive reviews (through in-app pop-up, social media campaign).

Moreover, certain rating ranges, specifically within 4.192 and 4.5, have a significant positive impact on app installations. This may challenge the common assumption that higher ratings lead to higher number of installations, so app developers should not only focus on achieving higher ratings but also on maintaining app ratings within these specific impactful ranges.

##### Pricing Strategy Optimization:

Pricing strategy have a significant influence on user downloads. Free apps had a much higher average number of installations than paid apps. One of the reasons for this popularity is that 82% of apps in the Google Play Store were free to download. To maximize app downloads overall, consider offering free apps to significantly boost the number of downloads or build reputation before offering subscription, any upsell packages or paid apps.

To drive revenue stream through paid apps, the optimal price range is $0.99-$1.99 and $4.99-$13.91 which will maximize app installations. Adjusting prices within these ranges can attract more users, so developers can adjust price accordingly, also carefully analyze their app's value proposition and pricing relative in the market before launching apps.

##### Fostering Positive Reviews for Communication Apps:

Positive reviews can significantly influence app downloads, especially within the most popular category "Communication." Developers should actively engage with users and encourage them to provide positive feedback to enhance their app's reputation and drive more app installations in the Communication category.

##### Limitation

This study has several limitations. First is the data age, the data was sourced from 2018, which may not fully capture the current trends of the competitive Google Play Store market. Second is the sample size reductin, around 1.500 values was eliminated after cleaning the data. This reduction may introduce biases. It's important to acknowledge that the sample used for this study might not fully encompass the diversity and complexity of the Google Play Store.
