# Mortgage-Discrimination-Measure
## Overview
Mortgage discrimination alters the distribution of investment, opportunity, and economic advantage—key contributors of health disparities. Leveraging Home Mortgage Disclosure Act data, we assessed mortgage denial risk in 380 U.S. urban areas. We estimated the risks by census tract–relative to the urban-specific average—using a Bayesian spatial model with conditionally autoregressive distributions fitted with integrated nested Laplace approximation. This approach borrows information through spatial and non-spatial smoothing, resulting in stable estimates in the presence of sparse data. The method, publicly accessible, allows researchers to apply our approach, fostering deeper insights into mortgage lending discrimination and systematic neighborhood disinvestment. By providing a robust and transparent method for mortgage discrimination, our novel approach can inform interventions to promote equitable lending practices in these areas. The persistence of differential mortgage approvals by place, even five decades after the enactment of the 1968 Fair Housing Act, underscores the need for more extensive interdisciplinary inquiries. Such investigations are crucial to understanding the potential health inequities that may have arisen due to the lack of transparency and accountability in the enforcement of fair housing policies. By making the analytic approach publicly available, researchers will be able to adapt our methods to their needs and advance the understanding of the impact of mortgage discrimination on affected communities. This will facilitate the replication and extension of our findings and encourage collaborative research efforts to address this pressing issue.

## Approach 
We developed a metric to measure mortgage discrimination using 2010–2014 data from the HMDA. This approach was informed by previously published methodologies by Gee et al., Mendez et al., and Beyer et al. After instituting our eligibility criteria, there were 16,816,010 mortgage applications across 60,399 census tracts nested within 380 MSAs in the contiguous US used to calculate our measure. 

We estimated the relative risk of mortgage denial within each census tract compared to the average of the respective MSA using a hierarchical Bayesian spatial model. The hierarchical component of this model explicitly accounts for the clustering of loans within the same census tract and potential correlation between census tracts within the same MSA. Moreover, it allows adjustment by individual level (i.e., loan-level) covariates, such as loan-to-income ratio and applicant sex, while estimating residual census-tract level risk of denial. We employed a Besag, York, and Mollie (BYM) model to incorporate these considerations. The BYM model decomposes the census-tract spatial residual random effect into a sum of an unstructured random effect modeled using a normal distribution, and a spatially structured random effect modeled using a conditional autoregressive specification (CAR). The BYM model assumes that census tract-specific risks of mortgage denial may vary from one another and that those differences can be described by estimating random effects that explicitly account for spatial relatedness and random effects that are spatially independent.

## Considerations
All census tracts within the 380 MSAs in the contiguous US were included for completeness and transportability, irrespective of their population size or liviability. This resulted in some tract-level estimates of mortgage denial risk that are implausible (e.g., the census tract of the Statue of Liberty) or unstable estimates, even while using Bayesian approach. We suggest care be taken when applying these estimates. Moreover, researchers are welcome to replicate this analysis within their geographic areas of interest and adjust the process to institute additional or different exclusion criteria. 

Link to preprint: 
https://www.researchsquare.com/article/rs-4419606/v1
