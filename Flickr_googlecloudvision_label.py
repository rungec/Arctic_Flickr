#This script takes an text file listing the url for each flickr photo
#then uses Google Cloud Vision https://cloud.google.com/vision/ to tag each image
#It returns the tag, score, and MID code that can be used in Knowledge graph https://www.google.com/intl/bn/insidesearch/features/search/knowledge.html

#google credentials

#set up libraries
import os
import io
from google.cloud import vision_v1
from google.cloud.vision import types

# Set Google API authentication and vision API client
os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = "D:\Box Sync\Arctic\CONNECT\Paper_3_Flickr\Analysis\login_cloud\connect-flickr-fe480e750aa3.json"
client = vision_v1.ImageAnnotatorClient() #client for batching

requests = [] #I think a list of individual image requests

response = client.batch_annotate_images(requests)

e.g.
request = {
	'image': {
		'source': { 'image_uri': 'https//foo.com.image.jpg'
		}
	},
	"features": { "LABEL_DETECTION",
	"maxResults":10
	},
}



# Set file containing list of photo urls
photoDF = ""
rows = 

#Setup empty dataframe
ImageLabels = pd.DataFrame()
ImageIDs = []
MID = []
Description = []
Score = []

#Get tags for each photo

for currRow in rows:
	imageID = photoDF[currRow, ]
	image_uri = photoDF[currRow, ] #http/httpsurl
	
	response=client.label_annotations
	
	description
	score
	topicality
	mid



google.cloud.vision_v1.types.BatchAnnotateImagesRequest
requests



filename = os.path.basename(file).split('.')[0] # Get image ID
 image_file = io.open(ImageFolder+file, 'rb') # Open image
 content = image_file.read() # Read image into memory
 image = types.Image(content=content) # Does something
 response = client.label_detection(image=image) # Gets response from API for image
 labels = response.label_annotations # Get labels from response
 Nlabels = len(labels) # Get the number of labels that were returned
 for i in range(0, Nlabels): # For each label we will store the MID, label, and score
 ImageID.append(filename) # Keep track Image ID
 MID.append(labels[i].mid) # Store MID
 Description.append(labels[i].description) # Store label
 Score.append(labels[i].score) # Store score of label
# Put Image ID, MID, label, and score into data frame
ImageLabels["imageid"] = ImageID
ImageLabels["mid"] = MID
ImageLabels["desc"] = Description
ImageLabels["score"] = Score



