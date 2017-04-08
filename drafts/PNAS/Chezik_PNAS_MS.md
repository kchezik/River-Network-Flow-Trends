---
author:
- 'Kyle A. Chezik'
- 'Sean C. Anderson'
- 'Jonathan W. Moore'
bibliography:
- 'ms.bib'
title: 'River networks dampen long-term hydrological signals of climate change'
---

As billions are spent adapting to climate change by such means as
building dikes or improving stormwater drainage [@Narain:2011], certain
habitats and processes provide a natural defense system that mitigates
climate change impacts [@Jones:2012]. For example, coastal habitats such
as oyster reefs, mangrove forests, and eelgrass beds shield 67% of the
United States coastline and 1.4 million people from sea level rise and
storms [@Arkema:2013]. Protection and enhancement of these natural
systems has become a key component of many climate mitigation strategies
[@Guerry:2015], as they return multiple cost-effective services in
contrast to engineered alternatives [@Jones:2012]. For instance,
revitalization of the Mississippi delta not only improves storm
protection, but also increases production and sustainability of
fisheries and wildlife resources, protects the water supply, and reduces
the impact of wastewater effluent [@Conservation:1998]. The value of
climate buffering habitats stands only to increase as average global
temperatures climb. The Paris Agreement of 2016 aims to limit global
temperature rise to 1.5 [@Hulme:2016], a goal that concedes a minimum
doubling of observed warming [@Hartmann:2013]; therefore, an urgent
challenge and opportunity is discovering, maintaining and restoring
systems that naturally buffer against the oncoming impacts of climate
change.

In the face of changing global precipitation patterns [@Donat:2016],
river networks may offer an unappreciated defense against shifting flow
regimes under climate change[@Hartmann:2013; @Palmer:2009]. Earlier
snowmelt [@Rauscher:2008], reduced snow pack [@McCabe:2014], shifts from
snow to rain, and changes in the annual distribution of precipitation
are all impacting stream flow [@Hartmann:2013], resulting in flashier
flows with increased frequency of flooding [@Hirabayashi:2013], longer
periods of drought and critically low flows for wildlife and agriculture
[@Melillo:2014]. However, we hypothesize that large free-flowing river
networks may naturally dampen these signals of climate change by
integrating across varied landscapes and different manifestations of
climate. Processes that aggregate across asynchronous components tend to
dampen the aggregate, a process known as the portfolio effect
[@Doak:1998]. Climate is expressed differently and asynchronously over
the landscape [@George:2015] and on account of their branching
architecture and directional flow, river networks integrate these varied
climate manifestations [@Peterson:2013], potentially dampening the local
climatic response of sub-catchments. Large free-flowing rivers may have
diverse climate portfolios, with sub-catchments acting as assets in
their portfolio, thereby dampening local climate impacts. Portfolio
effects in rivers are already known to dampen short-term variation
[@Moore:2015; @Yeakel:2014] but attenuation of long-term climate driven
shifts have yet to be examined.

Here we consider whether river networks attenuate local long-term
climate change through analysis of hydrometric data from one of the
largest free-flowing rivers in North America. Located in British
Columbia, Canada, the Fraser River drains an area approximately the size
of the United Kingdom ($\sim$217,000 km^2^) and in a rare combination is
fairly well monitored while also having no dams on its main stem
[@Vorosmarty:2010] (Fig. \[fig:1\]). This watershed drains interior high
plateau, coastal mountains, and the Canadian Rockies, and thus
integrates a diverse mosaic of landscapes, weather, and climate. As with
other mid-latitude rivers [@Bindoff:2013], the Fraser River has
expressed increasing trends in discharge volume and variability over the
last several decades [@Dery:2012; @Morrison:2002]. The river’s discharge
is monitored by hydrometric gauge stations throughout the watershed. We
analyzed 38 years of data collected concurrently at 55 of these stations
and applied a novel analytical approach to examine whether this large
river exhibits signals of long-term climate dampening.

![image](Fig1_Map.pdf){width="17.8cm"}

##Detecting Climate Change in the Fraser River Basin
We focused on changes in hydrology as our riverine manifestation of
climate signaling. As climate change alters precipitation patterns from
snow to rain in temperate regions, rivers are generally predicted to
have lower low-flows and higher high-flows, as well as earlier snowmelt
[@Nijssen:2001]. These manifestations of rising temperatures are
impacting species [@Xenopoulos:2006] and people [@Hirabayashi:2013].
Extracting annual and monthly metrics of flow from Fraser River basin
gauge station records, we used rolling average maximum- and minimum-flow
estimates to capture trends in extreme discharge and rolling average
median-flow estimates to capture general annual and seasonal trends.
Because the timing of flow is particularly important to the phenology of
many organisms, we also calculated the day-of-year in which half the
annual flow was reached.

To quantify changes in flow we fit generalized least squares models with
an autoregressive error structure for each of our flow metrics between
1970 and 2007 for each site (*Materials and Methods*). Long-term changes
in flow metrics varied substantially across the watershed. For instance,
annual maximum flow decreased in 40 sites but increased in 15 over the
last four decades (-26 to 10% change$\cdot$decade^-1^, std. dev. = 6%).
Annual median flow showed similar variability but a greater tendency
towards rising flow trends with 49 increasing and only six decreasing
(-5 to 25% change$\cdot$decade^-1^ std. dev. = 5%). Thus, within the
Fraser River basin, there are a variety of long-term trends in flow,
suggesting a diversity of climate responses and landscape effects.

For a river network to act as a portfolio of climate and dampen the
effects of climate change, expressions of climate and their trends over
time need to vary throughout the network. Using the Western North
America Climate tool (ClimateWNA) [@Wang:2016], we quantified the
diversity in climate trends within the Fraser River basin over the years
concurrent with our hydrology trend estimates (Fig. \[fig:1\])
(*Materials and Methods*). We then summarized the climate portfolio of
each flow gauge by calculating the standard deviation in climate trends
within their catchments. Temperature and precipitation are likely to
drive changing hydrology, therefore we included variables in our climate
index that capture changes in extreme and mean temperature, as well as
changes in precipitation volume and physical state. The standard
deviations of these climate variables were scaled between 0 and 1 and
summed into a general climate index for each catchment. Following our
hypothesis, this climate portfolio index increases with area
(Fig. \[fig:2\]), rapidly asymptoting in catchments greater than
40,000 km^2^.

![Climate portfolio index as a function of catchment area. The mean
trend line (blue) was estimated using robust linear regression and
standard error estimates (grey) were calculated using the weighted
standard deviation
estimate.[]{data-label="fig:2"}](Fig2_ClimPort.pdf){width=".8\linewidth"}

##Quantifying the River Network Portfolio Effect 
Climate portfolio diversity is largely a function of area
(Fig. \[fig:2\]). Larger areas contain greater landscape complexity and
thus a greater variety of climatic expressions. The influence of any
individual sub-basin on downstream flow trends is also a function of
area. Larger catchments simply aggregate more water such that their
climate portfolio will carry more weight on downstream dampening. The
interplay between climate portfolio and catchment area on the impact of
downstream flow trend dampening makes area a more robust response
variable than simply the climate portfolio index. Therefore, to test
whether networks dampen climate signals we regressed site-specific trend
estimates (e.g., % change$\cdot$decade^-1^) onto each site’s catchment
area using a generalized least squares model (*Materials and Methods*).
To quantify attenuation, we modeled the change in trend variability with
the change in catchment area using an exponential variance function
[@Pinheiro:2000]. The variance function allowed us to predict the range
of flow trend values that would be expected as watershed area increased.
A decreasing range of flow trends with increasing watershed area
indicates a dampening network effect.

In the observed data, small watersheds exhibited greater variability
around their trend estimates than large watersheds, likely because of
greater short-term variation in small catchments (Fig. \[fig:3\])
[@Moore:2015]. This relationship between watershed size and trend
certainty may pull small watershed trend estimates away from zero,
thereby creating the appearance of decreasing trend variability among
sites as watershed size increases. Using a null-hypothesis framework
(*Materials and Methods*) we simulated time-series for each site with no
underlying trend. These simulations were generated by a random walk
process with the observed standard deviation and autocorrelation
parameters ($\hat{\upsigma}$, $\hat{\upphi}$) at our sites. Using the
same generalized least squares model applied to the observed data, we
estimated trends for 1000 simulated time series at each site for each
response variable (e.g., Fig. S3). This simulation process, a form of
parametric bootstrapping, created distributions of null-expectations
with which we compared our observed results.

![image](Fig3_AnnFun.pdf){width="17.8cm"}

##Results 
###Climate Portfolios and Flow Trend Dampening 
Long-term changes in hydrology were less variable in larger catchments,
as predicted, demonstrating between 92 and 96% dampening of flow trends
moving from headwater catchments to the network’s confluence. In other
words, the largest catchments were approximately 10 times more stable in
their climate response than the smallest catchments. For instance,
trends ranged between a 6% reduction and 19% increase in median flow per
decade among small watersheds, while large watersheds ranged between 0.8
and 1.6% change per decade. Similar attenuation was seen in flow timing
with a 94% reduction in the day-of-year to half-annual-flow trends such
that small watersheds were reaching half their annual flow between 8
days later and 25 days earlier while large watersheds have only shifted
2 to 4 days earlier between 1970 and 2007.

Our null model approach illustrates that support for a river network
portfolio effect is greater than 90% for all four annual flow trend
metrics and as high as 98% for median-annual-flow trends
(Fig. \[fig:3\]). Trend attenuation was 3.0 (0.7–9.4 this and hereafter
are 90% intervals), 4.8 (1.1–13.8), 3.4 (0.9–9.0) and 5.3(1.4–14.8)
times greater than the null model for annual-flow timing, and minimum,
maximum and median annual-flow respectively. These statistics provide
strong support for the hypothesis that river network portfolio effects
contribute to climate dampening in large rivers.

###Seasonally Shifting Flow Trend Dampening
Regional flow-trends and attenuation strength varied by season. Analysis
of monthly trends revealed that winter flows are getting higher and
summer flows are getting lower over time, but that these trends have
been more variable in smaller catchments (Fig. \[fig:4\], *SI Appendix*,
Figs. S1,S2). For example, winter maximum flows in small catchments
ranged from nearly no change to dramatic 47% increases per decade with
decreases only as large as 9%. Despite these extreme locations, on
average the basin has only experienced moderate winter maximum flow
increases of 5-9%. Summer flows exhibited the opposite, with decreases
in high flows over time. However, there was weak evidence of attenuation
in the spring (Fig. \[fig:4\], *SI Appendix*,Figs. S1,S2). Overall,
climate portfolios tended to be greater during stable periods of the
year and decrease during seasonal transitions (*SI Appendix*, Fig. S3).
In the spring the likelihood of network-driven attenuation of maximum
flow trends was as low as 25% (Fig. \[fig:4\]). The deterioration of the
network’s attenuation strength was coincident with the spring freshet,
when snowmelt drives high flows across the basin. Synchronization events
such as the freshet may subvert river network dampening mechanisms by
homogenizing the region’s response to seasonally driven climate shifts.

![image](Fig4_MaxMonth.pdf){width="17.8cm"}

Despite considerable dampening relative to smaller sub-basins, the
furthest downstream reaches of the Fraser River are still exhibiting
small but detectable signals of climate change. Late fall, winter and
early spring flows are generally increasing while late spring, summer
and early fall flows are decreasing (Fig. \[fig:4\], *SI Appendix*
Figs. S1,S2). These findings generally support previous studies
demonstrating earlier spring runoff [@Dery:2012], decreasing summer and
fall flows [@Stahl:2006] and increased winter flows [@Healey:2011].
Together, this body of work indicates basin-wide effects of climate
change, where shifts in winter precipitation from snow to rain is
resulting in increased winter runoff and decreased snow pack
contributions to summer flows. Our work suggests that because the basin
is responding heterogeneously to the shifting climate and integrating
climate trends, the downstream impacts of climate change on flow are
greatly reduced. In contrast to previously studied types of climate
mitigation where habitats modify climate drivers (e.g., carbon storage)
or physically absorb climate change impacts (e.g., mangroves), network
climate dampening smooths out extreme climatic trends thereby reducing
the impact of local extremes.

##Discussion
Climate change is already causing economic and conservation challenges
for river systems worldwide[@Palmer:2009]. For example, sockeye salmon
in the Fraser River have seen as much as 80% pre-spawn mortality in
years with later run-off and elevated temperatures, leading managers to
close the fishery in 2013 and 2015. In the absence of climate dampening
the impacts of these temperature extremes could have been dramatically
worse. As climate change progresses, river hydrology will continue to
shift, further stressing riverine ecosystems and subsequently demanding
responsive management. However, river networks provide an
underappreciated defense against these climate-change impacts as they
are uniquely organized to leverage locally filtered expressions of
climate into stability. A simple product of form and gravity, stability
emerges as river networks integrate landscape heterogeneity, creating a
downstream portfolio of climate that smooths local extremes and tempers
long-term flow trends. Therefore, larger rivers should have flow regimes
that are less sensitive to local climate trends.

Our study provides insight into the spatial scaling of climate change.
We focused on a vast watershed with a remarkably diverse climate
portfolio, yet we would still expect that smaller rivers could dampen
some degree of climate variability. For instance, based on our data, we
predict that rivers draining a catchment of 60,000 km^2^ would have 66%
less variability in the rate of change in hydrology than a smaller river
draining  5000 km^2^. While this dampening is less than the 10-fold
decrease observed in the entire 220,000 km^2^ Fraser River, smaller
watersheds can offer a defense against the impacts of climate change.

There have been extensive efforts to predict the hydrologic sensitivity
of rivers due to climate change through downscaled climate projections
[@Nijssen:2001]. Here we demonstrate that large rivers may have an
inherent ability to absorb local climate change. But just as the
destruction of coastal habitats degrades their natural capacity to
mitigate sea-level rise or storm events [@Arkema:2013], the management
of river basins likely impacts the ability for river networks to
attenuate climate trends. For example, dams synchronize the flow regimes
of rivers [@Poff:2007] and logging increases the magnitude and frequency
of extreme events such as high and low flows through increased runoff
rates [@Zhang:2014]. These anthropogenic activities may magnify the
impacts of climatic shifts such as the transition from snow- to
rain-dominated precipitation in some basins. The climate cost of these
watershed activities should be considered in environmental decision
making. Climate change is a global challenge; here we suggest large
rivers dampen local change by leveraging landscape diversity into a
portfolio of climate, an important tool in the climate-mitigation
toolbox.

##Materials and Methods
###Data 
We downloaded daily flow data from Environment Canada’s HYDAT database.
Within HYDAT we selected Fraser River basin hydro-metric gauge stations
that were not observed to be dam influenced and which collected data in
each month between 1970 and 2007 where no month was missing more than 5
days data. We selected this 38-year range because this period maximized
the number of gauge stations in the Fraser River basin (*n*=55)
operating concurrently over a long enough time period in which climate
trends may be observable. We interpolated missing data using a linear
model in log-space, but of the 13,879 days of data only 23 days were
estimated. These daily data were smoothed using a five day rolling
average before summarizing into annual and monthly response variables.
This smoothing technique was used to reduce the influence of erroneous
and unusually extreme values. Because dams control flow and stabilize
flow trends, we removed sites where daily flow exhibited unusually high
autocorrelation and low variability relative to other sites of similar
size, and were observed downstream of, and atypically close to, a dam
(*n*=3). The downstream distance of gauge stations from dams was
determined using GIS spatial layers available in the *BC Data Catalog*,
maintained by the provincial government of British Columbia. Finally, we
summarized the daily data into annual and monthly maximum, median and
minimum flow estimates and the day-of-year to half-annual-flow.

###River discharge trend analysis 
We estimated annual and monthly flow trends for each response variable
at each gauge station using a generalized least squares (GLS) model with
an AR1 autocorrelation function:
$$\mathrm{flow}_{s,t} = a_s + b_s \mathrm{year}_{s,t} + \epsilon_{s,t}, \quad 
  \epsilon_{s,t} \sim \mathcal{N}(\upphi \epsilon_{s,t-1}, \sigma_\mathrm{flow}^2) \label{eq1},$$
where $\mathrm{flow}_{s,t}$ represents a flow metric \[log(maximum),
log(minimum), log(median), logit(day-of-year to half-annual-flow)\] at
each site ($s$) and time point ($t$), and $\mathrm{year}_{s,t}$
represents the time in years (e.g., 1970, 1971, …, 2007) with 1988
subtracted to approximately center the predictor. Parameters $b_{s}$
(slope) and $a_{s}$ (intercept) represent the estimated mean effect of
time on $\mathrm{flow}_{s,t}$ and the estimate of $\mathrm{flow}_{s,t}$
at time zero (i.e., 1988) respectively. Error ($\epsilon_{s,t}$) of the
current time step for a given site was allowed to be correlated with
that of the previous time step by $\upphi$ and was assumed to be
normally distributed with a variance of $\sigma^{2}$. To ensure our
model estimates and simulations remained within the calendar year we
logit transformed our flow-timing response variable (i.e., day-of-year
to half-annual-flow) after scaling the data between 0 and 1 (i.e.,
dividing by 365).

###Basin trend attenuation analysis 
To measure network driven dampening of climate signalling, we regressed
site-specific trend estimates (e.g., % change$\cdot$decade^-1^) onto
each site’s catchment area using a generalized least squares model:
$$\hat{b}_{s} = c + d\sqrt{\mathrm{area}_{s}} + \eta_{s}, \quad
  \eta_{s} \sim \mathcal{N}(0, f(\mathrm{area}_{s})) \label{eq2},$$
where $\hat{b}_{s}$ represents a flow trend at a given site ($s$) and
$\mathrm{area}_{s}$ represents site $s$’s watershed area. Fitted $d$ and
$c$ parameters represent the mean effect of
$\sqrt{\mathrm{watershed area}}$ on flow trends and the mean basin-wide
flow trend at a theoretical watershed area of zero, respectively. To
quantify attenuation, we modeled the change in trend variability with
the change in catchment area using an exponential variance function
[@Pinheiro:2000 p. 211]:
$$f(\mathrm{area}_{s}) = \sigma_b^2 \exp(2\updelta\sqrt{\mathrm{area}_{s}}) \label{eq3},$$
where the variance of the estimated error ($\eta$) changes exponentially
with increasing $\sqrt{\mathrm{area}}$. This variance function allowed
us to predict the range of flow trend values that would be expected as
watershed area increased.

###River Attnuation Null Model Simulations 
In the observed data, small watersheds exhibited greater variability
around their trend estimates than large watersheds, likely because of
greater short-term variation in small catchments [@Moore:2015]. This
relationship between watershed size and trend certainty may pull small
watershed trend estimates away from zero, thereby creating the
appearance of decreasing trend variability among sites as watershed size
increases. Using a null-hypothesis framework we simulated time-series
for each site with no underlying trend. These simulations were generated
by a random walk process with the observed standard deviation and
autocorrelation parameters ($\hat{\upsigma}$, $\hat{\upphi}$) at our
sites. Using the same GLS model and AR1 correlation structure as applied
to the observed data (eq. \[eq1\]), we estimated trends for 1000
simulated time series at each site for each response variable (e.g.,
Fig. S4). This simulation process, a form of parametric bootstrapping,
created distributions of null-expectations with which we compared our
observed results.

We then applied equation \[eq2\] to each of the 1000 basin-wide
simulations (e.g. Fig. S5), resulting in 1 observed variance exponent
parameter ($\hat{\updelta}$) and 1000 simulated $\hat{\updelta}$ for
each flow metric. By comparing our observed attenuation with our
basin-wide null-model simulations we addressed the potential that more
variable flows in smaller catchments contribute to the observed pattern
of flow trends as a function of watershed area (Fig. \[fig:3\]). This
null-model approach asks how likely our observation is due to a sampling
effect versus a network portfolio effect.

###Dampening summary statistics 
We calculated attenuation certainty as the percentage of simulated
$\hat{\updelta}$ that were less than the observed $\hat{\updelta}$. We
calculated attenuation strength as the ratio between the standard
deviation at the smallest and largest watersheds as defined by the
exponential variance function (i.e.,
$\sqrt{f(\mathrm{area}_1)} / \sqrt{f(\mathrm{area}_2)}$). To compare the
degree of attenuation observed with the null-simulated attenuation, we
compared this ratio with the same ratio calculated from the
null-simulated data (i.e.,
$\left( \sqrt{f(\mathrm{area}_1)} / \sqrt{f(\mathrm{area}_2)} \right) / \left( \sqrt{f(\mathrm{area}_{\mathrm{null} 1})} / \sqrt{f(\mathrm{area}_{\mathrm{null} 2})} \right)$).
Finally, to estimate how the day-of-year to half-annual-flow was
changing, we estimated the steepest point of each site’s logistic curve
and multiplied this slope value by 365 to revert from our 0–1 scale to
our original annual range of 0–365. To get a decadal rate we multiplied
these results by 3.8 (number of decades in 38 years). We estimated the
steepest part of the logistic curve using the \*divide by four\*
rule\[@Gelman:2008\] (dividing the coefficient by four equals the first
derivitive of the logistic curve at its steepest point).

###Climate 
In order to develop a climate index for each flow-gauge catchment we
needed to estimate climate trends across the Fraser River basin and then
spatially summarise them by flow-gauge catchment. Using Whitebox
Geospatial Analysis Tools, a freely available and open source geospatial
analysis software [@Lindsay:2016], we delineated the Fraser River basin
and the catchment areas of each flow-gauge station. Digital elevation
models provided by the provincial government of British Columbia, CA and
state goverment of Washinton, USA were pre-processed with a breaching
algorithm and stream burned to facilitate proper flow path and
accumulation models. We then used Climate WNA to estimate historic
climate values on an evenly spaced 1 km^2^ grid across the Fraser Basin
for each year and month in our study. Climate variables included the
mean annual temperature (MAT), and precipitation (MAP), extreme minimum
(EMT) and maximum (EXT) temperature and precipitation as snow (PAS).
Climate variable trends were calculated at each grid point using a
modified eq. \[eq1\], where time points were no longer considered and
our response variable becomes $\mathrm{climate}_{s}$.

###Code and Data Repository 
All flow data munging and analysis was done using R v3.3.2
([www.r-project.org/](www.r-project.org/)). Spatial analysis of
watersheds were done using GDAL/OGR v1.11.5, and Whitebox GAT v3.4
([www.uoguelph.ca/\~hydrogeo/Whitebox/](www.uoguelph.ca/~hydrogeo/Whitebox/))
with visualization help from QGIS v2.18.2
([www.qgis.org/](www.qgis.org/)). Climate data were gathered using
ClimateWNA v5.4 ([www.climatewna.com](www.climatewna.com)) and summary
statitics were calculated using R and the *rasterstats* module
(<https://github.com/perrygeo/python-rasterstats.git>) in Python v2.7.
All code can be found on [www.github.com/](www.github.com/) at
[github.com/kchezik/River-Network-Flow-Trends.git](github.com/kchezik/River-Network-Flow-Trends.git).
Raw HYDAT data can be obtained via Environment Canada
(<http://collaboration.cmc.ec.gc.ca/cmc/hydrometrics/www/>) or a
simplified version can be found on GitHub.

###Acknowledgments 
This study could not have been done without the decades of flow data
collected by those at Environment Canada. K.A. Chezik and J.W. Moore
were supported by the Liber Ero Chair of Coastal Science and Management
and Simon Fraser University. S.C. Anderson was supported by the David H.
Smith Conservation Research Fellowship.

\newpage

## References 
