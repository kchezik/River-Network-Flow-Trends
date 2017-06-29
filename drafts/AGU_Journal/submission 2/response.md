# River networks dampen long-term hydrological signals of climate change
## K. A. Chezik, S. C. Anderson, J. W. Moore
### 26-Jun-2017


### Response to Reviewer 1

1. **Comment:** (Fig. 1 caption): "within the Fraser River basin overlaid on a digital elevation model of British Columbia" The DEM is not of British Columbia but only of part of it. I would instead suggest either, "within the Fraser River basin in British Columbia overlaid on a digital elevation "model" or "within the Fraser River basin overlaid on a digital elevation model within British Columbia"

**Response:** The reviewer's second suggestion for clarity was applied to the manuscript. We now make clear the DEM is *within* BC, Canada.

2. **Comment:** Figure 1: I still think it would be nice to show the location of dams on this figure, but maybe too distracting for the key message of the paper.

**Response:** We understand the reviewer's concern here but we do feel adding dams to the map would distract and even obscure the information pertinent to understanding the studies findings. Largely, plotting the location of dams adds little information beyond assuring the reader visually that dams of significant size are not in proximity to our sites, but in many instances, without being able to zoom to specific sites and streams, the map would be unclear if a site and dam are on the same flow path. We do feel we have provided well commented code on GitHub that will assure the curious/skeptical reader that we have done our due diligence on this topic. Furthermore, the location of our sites and the spatial layers are freely available and can be accessed for analysis.

3. **Comment:** L 145: "climate_s,t" I think should be "C_s,t" to parallel the description of equation 1.

**Response:** We thank the reviewer for catching this mistake. We have corrected the equation per the reviewer's suggestion.

4. **Comment:** L 164: "can have" would be more accurate, because one could think of a large catchment in a different landscape not having a diverse climate portfolio (agricultural Midwest). Or "here have" to be specific that you are talking about the Fraser Basin.

**Response:** We have made it clear that we are speaking about the Fraser River basin by using "*here have*" per the reviewers suggestion.

5. **Comment:** L167: say "GLS model" as you defined GLS earlier.

**Response:** Replaced generalized least squares with "*GLS*" per the reviewers suggestion.

6. **Comment:** L 172: Why 5700 km^2^? I assume this is the mean watershed area of the sites you are considering but the text does not make this clear.

**Response:** Yes the reviewer is correct, the y-intercept is at the mean watershed sqrt-area. We adjusted the language to make clear that the centered response variable corresponds to the y-intercept at ~5700 km^2^.

### Response to Reviewer 2

**General Comments:** The presented analysis represents a robust synthesis of substantial amounts of hydrological data. What is not always clear within the analysis though are the physical justifications for the chosen metrics, and instead the analysis seems to be almost entirely driven by regression of descriptive metrics that are not necessarily physically justified. As an example, if the yearly probability density function for the rivers is well approximated by a log-normal or similar functional form then the median is a physically justified value, however if the underlying probability density function is better represented by an anomalous distribution then the median does not really capture the physics of the process. This limits the transferability of the model to other catchments and the extent to which we may treat the analysis as predictive rather then descriptive. Another example of this is the use of monthly statistics which while informative and interesting to look at don't represent time periods relevant for river or climatic processes. In this case, figure 4 would be more informative if binned by season, rather then month (from the monthly data there appear to be two groups Oct-April and June-Sept., with May being a bit of an outlier). As a summary, the statistical regression models are only useful if the descriptive statistics they are based on capture the physics of the system in question. The chosen parameters may very well be the best parameters for this analysis, however it was not clear to me when reading the manuscript if they were indeed the correct parameters to describe the data. It would be beneficial if the authors could clarify their reasoning on this, either in the main text or supplementary material.

**Response:** Our choice of response metrics was largely driven by what we have observed in the literature and what metrics would be of interest to a wider audience. As described on lines 102 to 111, extremes in flow and the timing of flow are important metrics to humans and species. Because the Fraser is still a snow driven system, the shape of the annual discharge curve is largely driven by the spring freshet and dry summers, with cross site variation occurring largely in the late fall and winter months depending on coastal or interior location. Therefore, the reason we took the median flow value was that it was more robust to inter-annual fluctuations in precipitation than the mean across sites. We include a new plot in the supplementary information showing the distribution of flow for each year, by site (Figure S9). This should make clear that the distribution of the flow data underlying our summary metrics is largely normal or captured by a log-normal distribution because each year has a single distinct peak in flow.

1. **Comment:** Key point 2: How large is a 'large sub-catchment'? It might just be better to say 'larger' if the effect does indeed just generally scale with sub-catchment size.

**Response:** We made the change per the reviewers suggestion. '*Large*' has become '*Larger*' to demonstrate a relative sense of the word.

2. **Comment:** Ln. 89: Why a five day rolling window? Could you provide the reasoning behind this choice.

**Response:** The five day rolling average was used simply because it is a period of timing commonly applied as a smoothing function to reduce the impacts of extreme and unusual or erroneous events. An example reference by Dery et al. (2009) has been added. 

3. **Comment:** Ln. 96: Why monthly response variables? If your time series has no structure on a monthly then this seems arbitrary.

**Response:** We did not bin by season because we feel the seasonal effect is more intuitive in a monthly format. In figure 4, we are able to more simply see the continuity of seasonal shifts if we have a more fine scale time variable than if we simply have two bins. Arguably, this format also allows for more informative cross site comparisons if another system were to be studied or if this system were to be studied at different periods, allowing for the observation of timing shifts in the seasonal waxing and waning of dampening.

4. **Comment:** Ln. 168. could you explain the reasoning behind taking the square root of catchment area? It is not clear why this was done. Is this supposed to represent a length scale for each site assuming an idealized geometry?

**Response:** We used the square root to spread out the catchments that are relatively small and reduce the leverage of the very large catchments. Also, given that area is in squared units, it makes sense to square root this response variable rather than log-transform or use some other transformation.

5. **Comment:** Equation 4: Please define delta.
6. **Comment:** Ln. 208: Please define delta^hat.

**Response:** Added a clarifying statement describing delta which should also make clear the meaning of delta_hat.

7. **Comment:** Ln. 234: It isn't clear what is meant by global climate here, the change for these sites is still regional (the data still represent a single river basin that is not particularly large by river standards).

**Response:** The reference to "*global climate*" is saying that while climate is variable from place to place, general shifts in global (i.e., general or mean temperature) climate (Holocene to Anthropocene), result in a variety of local climate responses. See list of changes to see how we have clarified this sentence.

8. **Comment:** Figure 2. What is the point of the fitted trend line? Is it just to guide the eye? The fitted trend line (if we are to take it at face value) suggests that the asymptotic behavior occurs for catchments substantially larger 40,0000 km^2^, closer to 250,000 km^2^. The data are more in line with a value of 40,000 km^2^, but then why include the fitted line?

**Response:** The fitted line is simply to guide the eye and provide for some sense of uncertainty in the observed relationship. This is not necessary and is relatively obvious. We have removed the fitted line.
