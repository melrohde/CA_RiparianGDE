# Detecting riparian vegetation reliance on groundwater using Sentinel-2 across California
The scripts in this repo show how Google Earth Engine and R were utilized to evaluate riparian vegetation reliance on groundwater across California from 2015 to 2019.  The following instructions show a step-by-step process.  This repo is accompanies the paper, "Groundwater dependence of riparian woodlands and the disrupting effect of anthropogenically altered streamflow" by M.M. Rohde, J. Stella, M.B. Singer, and D. Roberts (currently in preparation).

## Table of Contents
* [Introduction](#Introduction)
* [Getting Started](#GettingStarted)
* [Sentinel-2 NDVI Data](#Sentinel-2NDVIData)
* [Groundwater Level Data](#GroundwaterLevelData)
* [Streamflow Alteration Data](#StreamflowAlterationData)
* [Statistical Analyses](#StatisticalAnalyses)

## Introduction
 Until recently, technological limitations have hindered the assessment of groundwater influences on riparian ecosystem health at the spatial and temporal scales relevant to policy and management.  Here, we synthesize large, publicly-available datasets of mapped riparian vegetation communities, groundwater levels, and stream flow regimes to assess the influence of groundwater on riparian vegetation across California (USA).  We used Sentinel-2 satellite imagery to assess riparian vegetation health via NDVI (normalized difference vegetation index) at 10-m spatial resolution - a scale appropriate for characterizing narrow, highly fragmented riparian woodlands.  We link riparian NDVI responses to available field-based groundwater level data for riparian vegetation. 
 
 ## Getting Started
 ### Google Earth Engine
 [Google Earth Engine] (https://developers.google.com/earth-engine/) is an online, cloud-based platform that uses Google's computing infrastructure to catelog and process massive datasets in a relatively short amount of time that would be infeasbile on a personal computer.  Google Earth Engine is free for research, education, and non-profit usage. If you aren't already a Google Earth Engine user, you'll need to [sign-up] (https://signup.earthengine.google.com/).
 
 Google Earth Engine uses java in its [code editor] (https://developers.google.com/earth-engine/guides/playground), but can also be coded in [Python] (https://developers.google.com/earth-engine/guides/python_install) using an API.  [Tutorials] (https://developers.google.com/earth-engine/tutorials) and [guides] (https://developers.google.com/earth-engine/guides) exist online.  For technical questions, you can ask the Google Earth Engine Developers community by signing up for the [google group] (https://groups.google.com/g/google-earth-engine-developers).  
 
 The GEE code in this repo is written in java.
 
 ### R
 [R] (https://www.r-project.org/) is a free software environment for statistical computing and graphics. R can be downloaded on UNIX platforms, Windows, and MacOS.  The [RStudio] (https://rstudio.com/products/rstudio/download/) integrated development environment provides a set of integrated tools, including a console, syntax-highlighting editor, workspace manager, debugging support, history viewing, and plotting.  [Documentation] (https://cran.r-project.org/manuals.html) for using R is available, and with a simple internet search you can find loads of tutorials and support for most R programs.
 
 ## Sentinel-2 NDVI data from Google Earth Engine 
 ### Set Boundaries
 The vegetation polygons for this study were selected from California's Natural Communities Commonly Associated with Groundwater (NC dataset) using ArcGIS.  The [NC dataset] (https://gis.water.ca.gov/app/NCDatasetViewer/) contains 98,275 delineated vegetation polygons, and we identified 3 deciduous vegetation alliances (n= 22,167 polygons) representative of riparian environments along large-order streams in California: (1) Populus fremontii (cottonwood), (2) Salix goodingii (willow), and (3) Quercus lobata (valley oak). These three vegetation types are now publicly available as an asset. 
  ```<javascript>
 var Cottonwood = ee.FeatureCollection('users/melrohde/Public/FremontCottonwood_NCdataset');
 var Willow = ee.FeatureCollection('users/melrohde/Public/GoodingiiWillow_NCdataset');
 var ValleyOak = ee.FeatureCollection('users/melrohde/Public/ValleyOak_NCdataset');
 var veg = Cottonwood.merge(Willow).merge(ValleyOak);
  ```
  The study area includes riparian vegetation located within groundwater basins and subbasins by the [California Department of Water Resources] (https://water.ca.gov/Programs/Groundwater-Management/Bulletin-118), which was last updated in 2018 and are designated on the occurrence of alluvial or unconsolidated deposits. California is divided into 10 hydrologic regions that correspond to the state’s major drainage basins. Boundary shapefiles can be found at https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#boundaries . 
  ```<javascript>
var California = ee.FeatureCollection("TIGER/2016/States").filter(ee.Filter.eq('NAME', 'California'));
 // Source: https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#boundaries
 var HR = ee.FeatureCollection('users/melrohde/Public/HydrologicRegions'); 
 var Basins = ee.FeatureCollection('users/melrohde/Public/Basins');
 ```

### Supervised Classification

A supervised classification was performed to refine the vegetation polygons and identify pixels dominated by tree crowns versus bare ground.  Classifying tree crowns in Sentinel-2 imagery provides three main benefits: 1) eliminates the likelihood that dead trees from the 2012-2016 drought are included in the analyses; 2) removes the adverse impacts of soil reflectance on NDVI given the range of landscapes of the study vegetation types used in this study across California (e.g., deserts, coastal wetlands, agricultural land, urban) and relative differences in vegetation density across hydrological regions; and 3) can minimize NDVI inflation by selecting pixels dominated with vegetation crown in case deeper groundwater levels have caused opportunistic invasive species (e.g., Arundo donax, Tamarix spp.) to outcompete native vegetation under degraded ecosystem conditions.

#### Select an image to perform the supervised classification
The supervised classification was performed on a cloud-free, composite image of the 2018 growing season (April - September) by removing images with clouds and taking the median values of each pixel. 

  ```<javascript>
 function maskS2clouds(image) {
   var qa = image.select('QA60');

   // Bits 10 and 11 are clouds and cirrus, respectively.
   var cloudBitMask = 1 << 10;
   var cirrusBitMask = 1 << 11;

   // Both flags should be set to zero, indicating clear conditions.
   var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
       .and(qa.bitwiseAnd(cirrusBitMask).eq(0));

   return image.updateMask(mask).divide(10000);
 }

 var S2 = ee.ImageCollection('COPERNICUS/S2')
                   .filterDate('2018-04-01', '2018-06-30')
                   // Pre-filter to get less cloudy granules.
                   .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
                   .map(maskS2clouds)

 var image = S2.median();
 ````
 
 #### Create a training dataset
 A training dataset for the supervised classification was manually created in Google Earth Engine by hand-selecting tree and bare ground points.  Points were selected using a visual intepretation of the high-resultution satellite baselayer images (typically containing images from 2020). Figure 1 provides an example of how crown (dark blue) and bare ground (yellow) points were manually selected within a cottonwood-dominated vegetation polygon (light blue).

 ![Figure 1. SelectingPoints](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/SelectingPoints.png)


Within each vegetation polygon for a given vegetation type (i.e., cottonwood, willow, valley oak), tree crown and bare ground points were hand-selected. This resulted in 1200 training points across California (Figure 2).

![Figure 2. TrainingPoints](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/TrainingPoints.png)

When selecting training points, we attempted to select them uniformly across hydrologic regions and proportionately to the number of vegetation type polygons within each region. This was important for achieving a high accuracy supervised classification since soil reflectance and canopy density varied for each vegetation type across the state.

![Figure 3. TrainingPointTable](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/TrainingPointTable.png)

The training points (crown & bare ground) for each vegetation type were exported to an asset, and now are publicly available.

  ```<javascript>
var CottonwoodPoints = ee.FeatureCollection('users/melrohde/Public/CottonwoodTrainingPoints');

var WillowPoints = ee.FeatureCollection('users/melrohde/Public/WillowTrainingPoints');

var ValleyOakPoints = ee.FeatureCollection('users/melrohde/Public/ValleyOakTrainingPoints');

````
For each vegetation type, the training points were subdivided into a: (1) training set (~70% of the points) to assess the accuracy of the CART classifier, and (2) test set (~30% of the points) to validate the accuracy of the supervised classification.  


  ```<javascript>
//-------Subdivide training dataset into training and test points ----//
//Roughly 70% training, 30% testing
var split = 0.7;
````

#### Set up the supervised classification parameters and classifier
The training points selected within each vegetation type were used to train a Classification and Regression Trees (CART) classifier.  The classifier was then used to classify the composite Sentinel-2 satellite image (median of April 1 - Sept 30, 2018) using Bands 2 (blue  - 490 nm), 3 (green - 560 nm), 4 (red - 665 nm), 8 (NIR - 842 nm), 11 (SWIR - 1610 nm), and 12 (SWIR - 2190 nm).  For more information on setting up supervised classifications in Google Earth Engine, follow these [instructions] (https://developers.google.com/earth-engine/guides/classification).

  ```<javascript>
 //-----Identify parameters for supervised classification-----//

 // For prediction, use these bands (blue, green, red, NIR, SWIR)
 var bands = ['B2','B3','B4','B8','B11','B12']

 // This property of the table stores the land cover labels.
 var label = 'class';

 //Create a function to extract all reflectance info from training points plotted 
 var Training_IC = function(image,featureCollection){
   return image.select(bands).sampleRegions({
     // Get the sample from the polygons FeatureCollection.
     collection: featureCollection,
     // Keep this list of properties from the polygons.
     properties: [label],
     // Set the scale to get Sentinel pixels in the polygons.
     scale:10
   })
 };

 //Map the function over the image collection with the training points ('Training')
 //to get training data.  This returns class points with properties and band reflectance data.

 var CottonwoodSet = Training_IC(image,CottonwoodPoints);

 var WillowSet = Training_IC(image,WillowPoints);

 var ValleyOakSet = Training_IC(image,ValleyOakPoints);

 //The RandomColumn() method will add a column of uniform random
 //numbers in a column names 'random' by default
 var CottonwoodSample = CottonwoodSet.randomColumn();

 var WillowSample = WillowSet.randomColumn();

 var ValleyOakSample = ValleyOakSet.randomColumn();

 // Reserve Data for testing and validation.
 var CottonwoodTraining = CottonwoodSample.filter(ee.Filter.lt('random', split)); 
 var CottonwoodValidation = CottonwoodSample.filter(ee.Filter.gt('random', split));

 var WillowTraining = WillowSample.filter(ee.Filter.lt('random', split));
 var WillowValidation = WillowSample.filter(ee.Filter.gt('random', split));

 var ValleyOakTraining = ValleyOakSample.filter(ee.Filter.lt('random', split));
 var ValleyOakValidation = ValleyOakSample.filter(ee.Filter.gt('random', split));
````

#### Perform the supervised classification

  ```<javascript>
 //----------Train the classifier------------//
 var CottonwoodCARTtrained = ee.Classifier.cart().train(CottonwoodTraining, label, bands);
 var WillowCARTtrained = ee.Classifier.cart().train(WillowTraining, label, bands);
 var ValleyOakCARTtrained = ee.Classifier.cart().train(ValleyOakTraining, label, bands);

 //-----------Classify the image------------//
 var CottonwoodCARTclassified = image.select(bands).classify(CottonwoodCARTtrained);
 var WillowCARTclassified = image.select(bands).classify(WillowCARTtrained);
 var ValleyOakCARTclassified = image.select(bands).classify(ValleyOakCARTtrained);


 //-------Clip classified image to featurecollection -----------//
 var Cottonwood_CART = CottonwoodCARTclassified.clip(Cottonwood);
 var Willow_CART = WillowCARTclassified.clip(Willow);
 var ValleyOak_CART = ValleyOakCARTclassified.clip(ValleyOak);

````
#### Assess the accuracy
To assess the accuracy of the CART classifier a confusion matrix was created. The confusion matrix represents the resubsitution accuracy and describes how well the CART classifier was able to correctly label resubstituted training data (i.e., data the classifer had already seen). Since a supervised classification was performed separately for each vegetation type, the accuracy was evaluated for each supervised classification.

  ```<javascript>
 //---------Accuracy Test with Confusion Matrix--------//
 var CottonwoodTrainAccuracy = CottonwoodCARTtrained.confusionMatrix();
 //print('Cottonwood Resubstitution Confusion matrix: ', CottonwoodTrainAccuracy);
 //print('Cottonwood Training overall accuracy: ', CottonwoodTrainAccuracy.accuracy());

 var WillowTrainAccuracy = WillowCARTtrained.confusionMatrix();
 //print('Willow Resubstitution Confusion matrix: ', WillowTrainAccuracy);
 //print('Willow Training overall accuracy: ', WillowTrainAccuracy.accuracy());

 var ValleyOakTrainAccuracy = ValleyOakCARTtrained.confusionMatrix();
 //print('Valley Oak Resubstitution Confusion matrix: ', ValleyOakTrainAccuracy);
 //print('Valley Oak Training overall accuracy: ', ValleyOakTrainAccuracy.accuracy());
 
 //Classify the validation data
 var CottonwoodValidated = CottonwoodValidation.classify(CottonwoodCARTtrained);
 //print(CottonwoodValidated,'Cottonwood Validated')

 var WillowValidated = WillowValidation.classify(WillowCARTtrained);
 //print(WillowValidated, 'Willow Validated');

 var ValleyOakValidated = ValleyOakValidation.classify(ValleyOakCARTtrained);
 //print(ValleyOakValidated, 'Valley Oak Validated');


 // Get a confusion matrix representing expected accuracy.
 var CottonwoodTestAccuracy = CottonwoodValidated.errorMatrix('class', 'classification');
 //print('Cottonwood Validation error matrix: ', CottonwoodTestAccuracy);
 //print('Cottonwood Validation overall accuracy: ', CottonwoodTestAccuracy.accuracy());

 var WillowTestAccuracy = WillowValidated.errorMatrix('class', 'classification');
 //print('Willow Validation error matrix: ', WillowTestAccuracy);
 //print('Willow Validation overall accuracy: ', WillowTestAccuracy.accuracy());

 var ValleyOakTestAccuracy = ValleyOakValidated.errorMatrix('class', 'classification');
 //print('Valley Oak Validation error matrix: ', ValleyOakTestAccuracy);
 //print('Valley Oak Validation overall accuracy: ', ValleyOakTestAccuracy.accuracy());
````

The accuracy of the CART classifiers was >97%.
![Figure 4. CARTAccuracy](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/CARTAccuracy.png)

The validation accuracy for the supervised classifications with the test set was >90%.
![Figure 5. CARTAccuracy](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/CARTValidation.png)

#### Mask out bare ground pixels
  ```<javascript>
//-------Create a mask for vegetation crown class-------//
var CottonwoodRaster_mask = Cottonwood_CART.eq(1);
var WillowRaster_mask = Willow_CART.eq(1);
var ValleyOakRaster_mask = ValleyOak_CART.eq(1);

//------Clip out portions of the original vegetation polygon -------//
//------that have been classified as having bare soil ------//
var CottonwoodClassified = Cottonwood_CART.updateMask(CottonwoodRaster_mask);
var WillowClassified = Willow_CART.updateMask(WillowRaster_mask);
var ValleyOakClassified = ValleyOak_CART.updateMask(ValleyOakRaster_mask);

print(CottonwoodClassified,'CottonwoodClassified');
//Map.addLayer(CottonwoodClassified,{palette:['green']},'CottonwoodClassified');

print(WillowClassified,'WillowClassified');
//Map.addLayer(WillowClassified,{palette:['green']},'WillowClassified');

print(ValleyOakClassified,'ValleyOakClassified');
//Map.addLayer(ValleyOakClassified,{palette:['green']},'ValleyOakClassified');
````

Here is an example that illustrates how bare ground pixels were masked out of a cottonwood vegetation polygon (light blue- left image), such that only crown-dominated pixels (green) within the vegetation polygon (light blue - right image) are included in the study.
![Figure 6. CARTAccuracy](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/SupervisedClassification_Cottonwood.png)
 
 ### Compile NDVI data
 Once the boundaries have been established and the bare ground mask created from the supervised classification, we are now ready to extract NDVI data from each vegetation polygon.  Sentinel-2 satellite imagery from June 23, 2015 through September 30, 2020 were collected for this study. Once the image collection is retained, the masks are applied to remove any pixels with cloud cover or bare ground within the vegetation polygons.  NDVI is then computed for every remaining pixel using the red (band 4) and infrared (band 8) reflectance bands. For each vegetation polygon, the median NDVI value is computed for the crown-dominated pixels for each Sentinel-2 image.  A NDVI time-series is created for each vegetation polygon, by appending the median NDVI value and image date to the vegetation feature collection.  The final NDVI dataset is exported to google drive.  For instructions on how to use the Google Drive API in Google Earth Engine, follow [these instructions] (https://developers.google.com/drive/api/v3/about-sdk).
 
 #### Set Inputs
 
  ```<javascript>
 //--------------SET SCALE-----------------------------//
 var scale_NDVI = 10;

 //---------------SET DATE RANGE-----------------------//
 var startDate = ee.Date.fromYMD(2015,6,23);
 var endDate = ee.Date.fromYMD(2020,9,30);

 ```
 
 #### Create Functions

##### Add date to images
  ```<javascript>
 var addDate = function(image){
    var date = image.date().format('YYYYMMdd');
    return image.set('date', ee.Number.parse(date));
 };
 ```

##### Cloud masking        
Function to mask clouds using the Sentinel-2 QA band.

   ```<javascript>
 function maskS2clouds(image) {
   var qa = image.select('QA60');

  //Bits 10 and 11 are clouds and cirrus, respectively.
   var cloudBitMask = 1 << 10;
   var cirrusBitMask = 1 << 11;

  //Both flags should be set to zero, indicating clear conditions.
   var mask = qa.bitwiseAnd(cloudBitMask).eq(0).and(
            qa.bitwiseAnd(cirrusBitMask).eq(0));

   // Return the masked and scaled data, without the QA bands.
      return image.updateMask(mask).divide(10000)
      .select("B.*")
      .copyProperties(image, ["system:time_start"])
 }
  ```

##### Bare Ground Masking
Function to mask bare ground in vegetation polygons after supervised classification
   ```<javascript>
 function maskBGcottonwood (image){
   var mask = Cottonwood_CART.eq(1)
   return image.updateMask(mask)
 }

 function maskBGwillow (image){
   var mask = Willow_CART.eq(1)
   return image.updateMask(mask)
 }

 function maskBGvalleyoak (image){
   var mask = ValleyOak_CART.eq(1)
   return image.updateMask(mask)
 }

 //function maskBGliveoak (image){
   //var mask = LiveOak_CART.eq(1)
   //return image.updateMask(mask)
 //}
  ```
  ##### Derive Vegetation Index //
 This function derives NDVI from a S2 image, and adds it to the image stack.
   ```<javascript>
 function addNDVI(image) {
     return image
           .addBands(image.normalizedDifference(['B8', 'B4'])
                     .rename('NDVI')); //change new band name from 'nd' to 'NDVI'
   }
   ```  

##### Extract Images
This function searches a Sentinel-2 TOA Collection according to a date range and masks out cloud coverage, returning each image's NDVI variable "Icol" is the name of image collection (string), where "bounds" is the shp file .
    
  ```<javascript>  
 function NDVIcol(startdate, enddate, bounds, BGmask) {
   //Assign proper cloud and collection filter
   var Icol = 'COPERNICUS/S2';

   //Search Image Collection and select images.  
   var IC = ee.ImageCollection(Icol)
     .filterBounds(bounds) //reduce search to bounds
     .filterDate(startDate, endDate)//reduce search to date range
     //Pre-filter to get less cloudy granules.
     .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
     //Apply Cloud Masking Function
     .map(maskS2clouds)
     //Apply supervised classification mask to remove bare ground pixels;        
     .map(BGmask);
     
   //Add date in YYYYMMdd format to each image
   IC = IC.map(addDate); 

   // compute NDVI    
   IC = IC    
     .map(addNDVI); //calculate NDVI for each image and add it to stack

   //Number of Images in Collection
   var Icount = IC.size();
   // print('size of collection ', Icol, Icount);
   
   //return a collection
   return ee.ImageCollection(IC);
 }
   ```  
   ##### Remove duplicate images
 Sentinel-2 produces multiple images, resulting sometimes 4x more images than the actual size. 
   This is bad for any statistical analysis.  This function mosaics images by time. 

```<javascript>  
 function mosaicByTime(images) {  
   var TIME_FIELD = 'system:time_start';

   var distinct = images.distinct([TIME_FIELD]);

   var filter = ee.Filter.equals({ leftField: TIME_FIELD, rightField: TIME_FIELD });
   var join = ee.Join.saveAll('matches');
   var results = join.apply(distinct, images, filter);

   // mosaic
   results = results.map(function(i) {
     var mosaic = ee.ImageCollection.fromImages(i.get('matches')).sort('date').mosaic();
     
     return mosaic.copyProperties(i).set(TIME_FIELD, i.get(TIME_FIELD));
   });
   
   return ee.ImageCollection(results);
 }
   ```  

##### NDVI Time Series
This function appends the NDVI timeseries to each polygon in the feature collection.
  ```<javascript>  
 function getRegions(Icol, features) {
   return Icol.map(function(image){
     var medians = image.reduceRegions({       
       reducer: ee.Reducer.median(),    
       collection: features.select("POLYGON_ID"),
       scale: scale_NDVI
     }).filter(ee.Filter.notNull(["median"])) 
     .map(function(f) {
       return f.set('date', image.date().format())
     })
     return medians    
   }).flatten()
 }
   ```  

### Search for images and return NDVI collection for each vegetation shapefile
  ```<javascript> 
 var Col_Cottonwood = NDVIcol(startDate, endDate, Cottonwood.geometry().bounds(), maskBGcottonwood);
 Col_Cottonwood = mosaicByTime(Col_Cottonwood);

 var Col_Willow = NDVIcol(startDate, endDate, Willow.geometry().bounds(), maskBGwillow);
 Col_Willow = mosaicByTime(Col_Willow);

 var Col_ValleyOak = NDVIcol(startDate, endDate, ValleyOak.geometry().bounds(), maskBGvalleyoak);
 Col_ValleyOak = mosaicByTime(Col_ValleyOak);
  ```  

### Export Data
A csv file containing an NDVI timeseries for each vegetation polygon is exported directly to Google Drive for further analyses.  Depending on the file size (which in this study was >10 GB per csv file), you may need to upgrade your google drive storage. 

  ```<javascript> 
var ts_table_Cottonwood = getRegions(Col_Cottonwood.select('NDVI'), Cottonwood);
Export.table.toDrive({
      collection: ts_table_Cottonwood,
      description: "Cottonwood_NDVI",
      folder: '<YOUR FOLDER NAME>',
      fileFormat: 'CSV'
  });
  

var ts_table_Willow = getRegions(Col_Willow.select('NDVI'), Willow);
Export.table.toDrive({
      collection: ts_table_Willow,
      description: "Willow_NDVI",
      folder: '<YOUR FOLDER NAME>',
      fileFormat: 'CSV'
  });
  
  
var ts_table_ValleyOak = getRegions(Col_ValleyOak.select('NDVI'), ValleyOak);
Export.table.toDrive({
      collection: ts_table_ValleyOak,
      description: "ValleyOak_NDVI",
      folder: '<YOUR FOLDER NAME>',
      fileFormat: 'CSV'
  });
  ``` 
  ## Groundwater Level Data
  Statewide groundwater level data from June 2015 through September 2020 were downloaded from the California Department of Water Resources [SGMA Data Viewer](https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#gwlevels) (Figure 7).  Periodic groundwater level observation data from designated monitoring wells were included, and production wells were excluded to prevent pumping effects on the dataset.  Most periodic groundwater level measurements are taken manually twice per year (late summer/early fall and late spring) to capture the annual range in groundwater elevations.  However, wells with more frequent periodic observations (e.g., monthly, weekly, daily) are also included in the dataset. 
  
  ![Figure 7. SGMA_DataViewer](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/SGMA_DataViewer.png)
   
  
  After downloading the groundwater level data and well location data, the data was further analysed using R version 3.6.1.  Refer to "CASGEM_WellSelection.R" in the repo.
  
  ``` 
  #Load CASGEM data locations
  df<- read.csv('~/CASGEM/Station.csv', header=TRUE, sep =",")
  df$SITE_CODE <-as.character(df$SITE_CODE)
  ```  
  
  To select observation wells that monitor groundwater levels in shallow aquifers the following selection criteria was applied: 
  
  1. well have a top perforated depth of <30 meters 
  ```
  ######################################################################
  #Select Wells where the top perforated zone is <100 feet. 
  #(roots typically do not penetrate confining layers or extend below ~70 feet below the ground surface Stromberg 2013 and Fan et al 2017)
  ######################################################################
  df_ShallowWells <- df[which(df$TOP_PRF<98.4),]     #1007 wells
  df_ShallowWells <- df_ShallowWells[order(df_ShallowWells$SITE_CODE),]

  ShallowWells<- data.frame(df_ShallowWells$SITE_CODE,df_ShallowWells$LATITUDE,df_ShallowWells$LONGITUDE,
                  df_ShallowWells$WELL_TYPE, df_ShallowWells$WELL_DEPTH, df_ShallowWells$TOP_PRF,
                  df_ShallowWells$BOT_PRF, df_ShallowWells$GSE, df_ShallowWells$RPE, df_ShallowWells$HR,
                  df_ShallowWells$BASIN_CODE, df_ShallowWells$BASIN_NAME)
  colnames(ShallowWells)<- c('SITE_CODE','LATITUDE','LONGITUDE','WELL_TYPE','WELL_DEPTH','TOP_PRF',
                             'BOT_PRF','GSE','RPE','HR','BASIN_CODE','BASIN_NAME')

  ShallowWellNames<-data.frame(ShallowWells$SITE_CODE,ShallowWells$HR)
  colnames(ShallowWellNames)<-c("SITE_CODE","HR")
  ShallowWellNames$SITE_CODE<- as.character(ShallowWellNames$SITE_CODE)

  ```
  2. groundwater levels measured between June 23, 2015 and September 30, 2020 to correspond with available Sentinel-2 satellite imagery data for the study period 
  ```
  #Load CASGEM periodic data
  df<- read.csv('~/CASGEM/GroundwaterElevation.csv', header=TRUE, sep =",")
  df$date <- as.Date(df$MSMT_DATE, format = '%Y-%m-%d')
  df$SITE_CODE<-as.character(df$SITE_CODE)


  CASGEM<-data.frame(df$SITE_CODE, df$date, df$WSE, df$GSE_WSE)
  colnames(CASGEM)<-c("SITE_CODE",'date','WSE','GSE_WSE')
  CASGEM$SITE_CODE<-as.character(CASGEM$SITE_CODE)

  SelectWells<- left_join(ShallowWellNames, CASGEM, by="SITE_CODE")
  length(unique(SelectWells$SITE_CODE)) #1007 wells
  SelectWells <- SelectWells[order(SelectWells$SITE_CODE,SelectWells$date),]

  #Select data within study period (June 23, 2015 to Sept 30, 2020)
  SelectWells<- SelectWells[which(SelectWells$date >="2015-06-23" & SelectWells$date <= "2020-09-30"),]
  length(unique(SelectWells$SITE_CODE)) #803 wells
  summary(SelectWells)
  
  #Remove Well locations that aren't within the study period.
  x<-unique(SelectWells$SITE_CODE)
  xx<-data.frame(x," ")
  colnames(xx)<-c("SITE_CODE","blank")
  SelectLocations<- left_join(xx,ShallowWells, by="SITE_CODE")
  ```
  Export groundwater level data from selected well locations.
  ```#Export csv file
  write.csv(SelectLocations, '~/CASGEM_SelectLocations.csv')

  #Export csv file
  write.csv(SelectWells, '~/CASGEM_SelectElev.csv')
  ```

### Determine depth-to-groundwater for vegetation
Groundwater level data from monitoring wells were associated with each vegetation polygon located less than 1 km away. The closest well to each vegetation polygon within a 1km radius was determined in ArcGIS using a spatial join (Figure 8). 
![Figure 8. Spatial Join in ArcGIS to associate each vegetation polygon with the closest monitoring well within a 1 km radius](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/SpatialJoin.png). 

Since monitoring wells are often installed upland from river corridors, the depth-to-groundwater measurements taken at wells are generally deeper than depth-to-groundwater under riparian vegetation, which tend to occupy low-lying depressions in the land surface.  Figure 9 provides an illustration of how interpolation of depth-to-groundwater within California varies in resolution depending on whether contours were: (left) interpolated using depth-to-groundwater measurements determined at each well, or (right) interpolated using water surface elevations at each well and correcting for land surface elevation.

![Figure 9. Depth-to-groundwater mapping (Source: [TNC, 2019](https://groundwaterresourcehub.org/public/uploads/pdfs/TNC_NCdataset_BestPracticesGuide_2019.pdf))](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/DTWmap.png)

To correct for this, depth-to-groundwater was calculated for vegetation polygons by subtracting the water surface elevation in the associated monitoring well from the averaged land surface elevation of each vegetation polygon.  This approach assumes that the water surface elevations are constant over a 1 km distance, but corrects for variations in land topography that are typical in riparian environments (Figure 10).  

![Figure 10. Depth-to-groundwater method (Source: [TNC, 2019](https://groundwaterresourcehub.org/public/uploads/pdfs/TNC_NCdataset_BestPracticesGuide_2019.pdf))](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/DTWmethod.png)

Land surface elevations for vegetation polygons were determined from the U.S. Geological Survey’s [⅓ arc-second digital elevation model (DEM) dataset](https://viewer.nationalmap.gov/basic/#/), which is the highest resolution (~10 m) DEM dataset for the conterminous United States. Elevation at each vegetation polygon was calculated using the zonal statistics to table function in ArcGIS.  This yielded the mean elevation data from the 32-float point tiff DEM raster for each vegetation polygon.  

### Link depth-to-groundwater and NDVI data

Because the groundwater level and NDVI data were collected at different dates and frequencies (e.g., biannual groundwater levels versus NDVI ~10 days), NDVI data were linearly interpolated to derive a daily NDVI measurement for each vegetation polygon, which could then be matched with observed groundwater level data. Because the NDVI files were so large, it was easier to run a separate interpolating R script for each vegetation type (willow, cottonwood, valley oak); see "NCdataset_paired_Willow.R", "NCdataset_paired_Cottonwood", and "NCdataset_paired_ValleyOak" in this repo.


```
#Interpolate NDVI data and create a new data frame with daily results
SentinelInterp <- function (veg_NDVI, veg_wells){
uniq_veg_NDVI<- unique(veg_NDVI$POLYGON_ID)

z<- setNames(data.frame(matrix(ncol = 3, nrow = 0)), c("POLYGON_ID", "date","NDVI"))
z$POLYGON_ID <- as.numeric(z$POLYGON_ID)
z$date <- as.Date(z$date)
z$NDVI <- as.numeric(z$NDVI)

veg_NDVI_interp<- data.frame()
for (i in 1:length(uniq_veg_NDVI)){
  z <- veg_NDVI[which(veg_NDVI$POLYGON_ID==uniq_veg_NDVI[i]),]
  zz <- data.frame(date = seq(z$date[1], z$date[nrow(z)], by =1))
  zzz <- merge(z, zz, by ="date", all=T) 
  zzz$LinApprox <- na.approx(zzz$NDVI)
  zzz$POLYGON_ID <- uniq_veg_NDVI[i]
  veg_NDVI_interp <- rbind(veg_NDVI_interp, zzz)
}
colnames(veg_NDVI_interp) <- c("date", "POLYGON_ID", "NDVI","NDVI_LinApprox")
veg_NDVI_interp$NDVI <- NULL
```

To avoid temporal autocorrelation due to polygons sampled repeatedly within a season, median values of NDVI and DTW values were calculated for each polygon according to season for all years in the study (2016-2020): spring (Apr-Jun) and summer (July-Sept).  Because all three vegetation types in this study are winter deciduous, only NDVI and groundwater data with paired dates from the growing season (spring and summer) were included for this study (n=3560) from vegetation polygons (n=747).
```
## Join NDVI data with Groundwater Level Data ##
results<- left_join(veg_NDVI_interp,veg_wells, by=c("POLYGON_ID","date"))
results <- na.omit(results)
results <-results[order(results$POLYGON_ID,results$date),]

results$month<- as.numeric(month(results$date))
for (i in 1:length(results$month)){
  if (results$month[i] >1 && results$month[i] <=3){
    results$season[i] <- "aJan-Mar"
    results$SEASON[i] <- 1
  }
  if (results$month[i] >3 && results$month[i] <=6) {
    results$season[i] <- "Apr-June"
    results$SEASON[i] <- 2
  }
  if (results$month[i] >6 && results$month[i] <=9) {
    results$season[i] <- "July-Sept"
    results$SEASON[i] <- 3
  }
  if (results$month[i] >9 && results$month[i] <=12){
    results$season[i] <- "Oct-Dec"
    results$SEASON[i] <- 4
  }
}
results$MonthYear <- format(as.Date(results$date), "%m-%Y")
results$Year <- format(as.Date(results$date), "%Y")
results
}
```

## Streamflow Alteration Data
The National Hydrography Dataset (NHD) was used to identify statewide drainage features such as rivers, streams, coastline, canals, pipelines, artificial paths, and connectors for all stream segments (identified by [NHDv2](https://nhdplus.com/NHDPlus/NHDPlusV2_home.php)) within delineated groundwater basins throughout California.  The NHD was spatially joined to the NC dataset to associate each vegetation polygon to the closest stream reach within a 1 km radius.  We classified all stream reaches adjacent to the riparian woodland polygons as natural, altered, artificial or off-stream in the study using the [US Geological Survey national flow modification data](https://pubs.er.usgs.gov/publication/cir1461), which classifies the degree of streamflow alteration along a particular reach. For this study, we classified flow modification into two groups: natural flow regimes (alteration probabilities from 0-50%) and altered flow regimes (>50%).


## Statistical Analyses
All statistics performed for these analyses can be found in the "Stats.R" file in the repo.

### Summary Statistics

Quantiles
```
quantile(boxdata$NDVI[which(boxdata$veg=="ValleyOak" & boxdata$HR_NAME== "North Coast")])
```
Sample size
```
length(boxdata$NDVI[which(boxdata$veg=="ValleyOak" & boxdata$HR_NAME== "North Coast")])
```
Interquartile range
```
IQR(boxdata$NDVI[which(boxdata$veg=="ValleyOak" & boxdata$HR_NAME== "North Coast")])
```

Comparisons of NDVI and DTW relationships among vegetation types were made using a linear mixed model (‘lme4’ and ‘lmerTest’ packages) with random effects for polygon (to account for repeat measurements in multiple years).  Post-hoc pairwise comparisons were made using a post-hoc Tukey estimated marginal means (‘emmeans’ package).  NDVI and DTW relationships were determined using a linear mixed model (‘lmer’ function) by designating season as a fixed effect and polygon id as a random effect.  

```
AllVeg_SeasonalMedian$veg<-as.factor(AllVeg_SeasonalMedian$veg)

##### Figure 2 #####
veg_model<- function(df){
  lmer(NDVI_LinApprox~DTW + veg + DTW:veg + (1|POLYGON_ID), data=df)
}

veg_output<-veg_model(AllVeg_SeasonalMedian)
summary(veg_output)
#emmeans(veg_output, list (pairwise~veg), adjust="tukey") # For a pairwise comparison of the intercepts
pairs(emtrends(veg_output,"veg",var="DTW"))
#difflsmeans(veg_output, test.effs = "veg")
plot(veg_output, main="All Veg")
```

Similarly, comparisons of NDVI and DTW relationships for vegetation along natural and altered streams were also made using a linear mixed model.
```
#####  Figure 3c - NDVI v. DTW plot of Natural and Altered Streams #####

Global_model1<- function(df) {
  lmer(NDVI_LinApprox~DTW + SEASON + DTW:SEASON + (1|POLYGON_ID), data=df)
}

AllVeg_SeasonalMedian$SEASON<-as.factor(AllVeg_SeasonalMedian$SEASON)
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]

#Altered
global_Altered<- Global_model1(df[which(df$NHD=="Altered"),])
summary(global_Altered)
plot(global_Altered, main="Altered")

#Natural
global_Natural<- Global_model1(df[which(df$NHD=="Natural"),])
summary(global_Natural)
plot(global_Natural, main="Natural")

Global_model2<- function(df) {
  lmer(NDVI_LinApprox~DTW + NHD + DTW:NHD + (1|POLYGON_ID), data=df)
}
AllVeg_SeasonalMedian$NHD<-as.factor(AllVeg_SeasonalMedian$NHD)
df<- AllVeg_SeasonalMedian[which(AllVeg_SeasonalMedian$COMID!=0),]
global_NHD<- Global_model2(df)
summary(global_NHD)
plot(global_NHD, main="NHD")
```
For comparing the NDVI across seasons and stream type(i.e., natural, altered), a two-way mixed ANOVA was used (‘lmer’ function) with season as a fixed effect and random effects for both polygon id and year.  
```
##### Figure 3b - Mixed ANOVA - BoxPlot of NDVI and Season for all veg#####
result<-lmer(NDVI_seasonal~NHD*SEASON + (1|POLYGON_ID) + (1|year), data=streamNDVI)
summary(result)
plot(result)
emmeans(result, list (pairwise~NHD*SEASON), adjust="tukey") # For a pairwise comparison of the intercepts
```
