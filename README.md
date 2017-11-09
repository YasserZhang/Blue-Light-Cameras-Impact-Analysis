# Blue-Light-Cameras-Impact-Analysis
This project is sponsored by Rochester Police Department, and aimed at implementing statistical analysis of
the impact that blue light surveillance cameras have on crime activities within their viewfields.

### Project Introduction
A Blue Light camera is a surveillance camera with a lamp radiating blue light installed on
its top. The camera is usually put in an open area such as road intersection. In addition to
monitor crime activities within its viewshed, it is also expected to have an effect in reducing
crimes with the help of its symbolic blue light, which is recognizable even from a distance of
hundreds of meters.
By the end of 2008, Rochester Police Department purchased 50 cameras and strategically
installed them in areas with high crime density based on the past crime records by the end of
2008. Ever since then, more Blue Light cameras were brought in and installed around the city
at various points of time. After the latest batch of cameras were installed in July 2016, the total
number has increased up to 136.
Since the installation of the first Blue Light camera in 2008, the RPD has not yet conducted
effectiveness analysis on those cameras. The objective of this project is to help the RPD to
quantitatively evaluate how effective these cameras are in reducing crimes in their vicinity.
Under the request of the RPD, this project put its main focus on felony activities instead
of all crimes. Therefore all hypothesis tests in the analyss process are implemented on the
dataset of felony activities.

### Data Preparation
The data used in this project consist of three parts. The first part is records of crime activities
that were reported during 12 years from 2005 up to 2016, which was provided by Rochester
Police Department (RPD). The original dataset contains 12 RDATA files and a total of 363,215
observations or crime reports. Each RDATA file consists of one year's crime reports. The
dataset is cleaned and preprocessed through the followingmain steps.

The second part is geospatial information of 136 Blue Light Cameras, which was downloaded
from RPD Open Data platform. The main issue about this part is that the installation
time of each Blue Light camera was not well recorded. To our best knowledge, there are two
groups of cameras which have relatively accurate installation times on record. The first group
is the batch of 56 cameras that were installed in 2008 and early 2009, and the other consists
of 19 cameras that were installed in July 2016. Although it was reported that there were 27
cameras installed in 2016, only 19 were identified in the dataset downloaded from the open
data platform.

The last part of the data is geospatial data of the City of Rochester, which was downloaded
from OpenStreetMap under the Open Data Commons Open Database License (ODbL). Geographical
coordinates of 4,255 intersections to the south of Holy Sepulchre Cemetery in
Rochester are extracted from the data. A further filtering step is implemented on the intersection
data and picks out a total of 2,358 intersections which has a distance of more than 300
meters away from any of the 136 Blue Light cameras installed in the city. The resulted dataset
is used as control group against the camera group in following analysis processes.

### Methods
CRIME COUNT. In order to quantify a camera0s impact on its nearby crime activities we
first developed a measuring metric crime count. Crime count is the sumof reports of crime
activities that happened within the distance of r away from a location l during a period of time
t , where r is the radius of a circle centered at l , which stands for the geographical coordinates
of a camera or control intersection, and t is a time interval, which could be one month, three
months, etc. In this project r is fixed at 50 meters, while t is various across different hypothesis
tests.
CONTROL GROUP. As it can be seen in figures, crime activities were decreasing across the city.
Empirically speaking, many factors such as economy, demography, government policies and
even some unconceivable elements can be attributed to the decreasing trend. With respect to
this project, we must take into account the confounding effects of these factors on crime. To
overcome this challenge the control group which consists of 2,358 intersections are brought in
and the crime activities near control group are used as a benchmark in comparison with those
near cameras. Asmentioned earlier, a 300-meter threshold is set in the process of choosing
control intersection. It is so done in order to reduce the probability that members of the
control group are covered in the buffer zone of any cameraâ˘A ´Zs impact area.

HYPOTHESIS TESTS. If a Blue Light camera has solid impact on nearby crimes, then a decreasing
pattern of crime is supposed to be observable along a certain period of time. Otherwise,
the camera0s impact on crime should be considered as obscure. Based on this assumption
we designed a one-sided hypothesis in the following expression, where pr ior cr ime count
stands for a sequence of counts of crimes recorded prior to the stallation of a batch of cameras,
and post cr ime count for the post-installtion data.

### Conclusion
From 2005 to 2016, the City of Rochester saw a steady decreasing trend in crime. For all-crime
activities, a peak-shifting pattern in all-crime density distribution was observed since the
installation of the first batch of Blue Light cameras, while similar pattern did not show up in
the case of felony activities. By conducting a series of hypothesis tests on felony records, we
found that the crime count in a Blue Light camera’s viewshed within the time interval following
its installation was statistically significant different from the value in the same period one year
earlier. In constrast, when it came to long termpatterns, more Blue Light cameras had seen
statistically significant deceasing trend in felony activities within their viewshed. In another
word, it takes time for a Blue Light camera to have impact on felony activities.
