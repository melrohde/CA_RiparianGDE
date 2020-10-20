# CA_RiparianGDE
The scripts in this rep show how Google Earth Engine and R were to evaluate groundwater reliance of riparian vegetation across California from 2015 to 2019.  The following instructions show a step-by-step process.  This repo is accompanied by the paper, "Seasonal and Regional variation in riparian vegetation reliance on groundwater detected using Sentinel-2 across California" by M.M. Rohde, J. Stella, M.B. Singer, and D. Roberts (currently in preparation).

## Table of Contents
* [Introduction] (#introduction)
* [Getting Started] (#getting-started)

## Introduction
 Until recently, technological limitations have hindered the assessment of groundwater influences on riparian ecosystem health at the spatial and temporal scales relevant to policy and management.  Here, we synthesize large, publicly-available datasets of mapped riparian vegetation communities, groundwater levels, and stream flow regimes to assess the influence of groundwater on riparian vegetation across California (USA).  We used Sentinel-2 satellite imagery to assess riparian vegetation health via NDVI (normalized difference vegetation index) at 10-m spatial resolution - a scale required to characterize the narrow, highly fragmented riparian woodlands.  We link riparian NDVI responses to available field-based groundwater level data for riparian vegetation. 
 
 ## Getting Started
 ### Google Earth Engine
 
 Supervised Classifications https://developers.google.com/earth-engine/guides/classification
 
 ## Set Boundaries
 Import select vegetation shapefiles from NC dataset (https://gis.water.ca.gov/app/NCDatasetViewer/)//
  ```<javascript>
 var Cottonwood = ee.FeatureCollection('users/melrohde/FremontCottonwood_NCdataset');
 var Willow = ee.FeatureCollection('users/melrohde/GoodingiiWillow_NCdataset');
 var ValleyOak = ee.FeatureCollection('users/melrohde/ValleyOak_NCdataset');
 var HR = ee.FeatureCollection('users/melrohde/HydrologicRegions'); // Source: https://sgma.water.ca.gov/webgis/?appid=SGMADataViewer#boundaries
 var California = ee.FeatureCollection("TIGER/2016/States") .filter(ee.Filter.eq('NAME', 'California'));
 var veg = Cottonwood.merge(Willow).merge(ValleyOak);
 ```
 ## Set Inputs
 
  ```<javascript>
 //--------------SET SCALE-----------------------------//
 var scale_NDVI = 10;

 //---------------SET DATE RANGE-----------------------//
 var startDate = ee.Date.fromYMD(2015,6,23);
 var endDate = ee.Date.fromYMD(2019,9,30);

 //---------------SET CLOUD COVERAGE PERCENTAGE--------//
 var cloudFrequency = 0.7; //(from 0 to 1)

 ```
 ## Supervised Classification

A supervised classification was performed to refine the vegetation polygons and identify pixels dominated by tree crowns versus bare ground.  Classifying tree crowns in Sentinel-2 imagery provides three main benefits: 1) eliminates the likelihood that dead trees from the 2012-2016 drought are included in the analyses; 2) removes the adverse impacts of soil reflectance on NDVI (11) given the range of landscapes of the study vegetation types used in this study across California (e.g., deserts, coastal wetlands, agricultural land, urban) and relative differences in vegetation density across hydrological regions; and 3) can minimize NDVI inflation by selecting pixels dominated with vegetation crown in case deeper groundwater levels have caused opportunistic invasive species (e.g., Arundo donax, Tamarix spp.) to outcompete native vegetation under degraded ecosystem conditions (12–14).


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
 A training dataset for the supervised classification was manually created in Google Earth Engine by hand-selecting tree and bare ground points.  Points were selected using a visual intepretation of the high-resultution satellite baselayer images (typically containing images from 2020). Here is an example of how crown (dark blue) and bare ground (yellow) points were selected within a cottonwood-dominated vegetation polygon (light blue).

 ![SelectingPoints](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/SelectingPoints.png)


Within each vegetation polygon for a given vegetation type (i.e., cottonwood, willow, valley oak), tree crown and bare ground points were hand-selected. This resulted in 1200 training points across California.

![TrainingPoints](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/TrainingPoints.png)


When selecting training points, I tried to evenly select them across hydrologic regions and since soil reflectance and canopy density varied for each vegetation type across the state.

![TrainingPointTable](https://github.com/melrohde/CA_RiparianGDE/blob/main/images/TrainingPointTable.png)

I exported the training points as an asset, and now they are publicly available.

  ```<javascript>
var AllPoints = ee.FeatureCollection('users/melrohde/Public/TrainingPoints_RiparianVeg');

````
For each vegetation type, the training points were subdivided into a: (1) training set (~70% of the points) to assess the accuracy of the CART classifier, and (2) test set (~30% of the points) to validate the accuracy of the supervised classification.  



The training points selected within each vegetation type were used to train a Classification and Regression Trees (CART) classifier (15).  The classifier was then used to classify a composite Sentinel-2 satellite image (median of April 1 - Sept 30, 2018) using Bands 2 (blue  - 490 nm), 3 (green - 560 nm), 4 (red - 665 nm), 8 (NIR - 842 nm), 11 (SWIR - 1610 nm), and 12 (SWIR - 2190 nm). The accuracy of the CART classifier was >97%  (Table S13) and the validation accuracy for the supervised classification with the test set was >90% (Table S14).


 //-----Identify parameters for supervised classification-----//

 // For prediction, use these bands (blue, green, red, NIR, SWIR)
 var bands = ['B2','B3','B4','B8','B11','B12']

 // This property of the table stores the land cover labels.
 var label = 'class';


 //-------Subdivide training dataset into training and test points ----//
 //Roughly 70% training, 30% testing
 var split = 0.7;



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



 //---------Accuracy Test with Confusion Matrix--------//
 // Get a confusion matrix representing resubstitution accuracy.
 // describes how well the classifier was able to correctly label resubstituted training data 
 //i.e., data the classifier had already seen. 
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


 
 ## Compiling NDVI data
 ```<javascript>
 var Cottonwood = ee.FeatureCollection('users/[your handle]/FremontCottonwood_NCdataset');

 ```
 
