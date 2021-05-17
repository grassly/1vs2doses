### Impact of strategies to prioritise vaccination with one dose versus delivering second dose

#### Nicholas Grassly 17 May 2021

### Summary
This is a model of the direct impact of vaccination on mortality due to COVID-19 when timely delivery of a second dose is prioritised or when the second dose is delayed to allow more people to be immunised with a first dose. The model requires estimates of the efficacy of one and two doses of vaccine, waning of efficacy, and a definition of groups prioritised for vaccination, coverage targets and their relative exposure and risk of mortality (these may just be specific age-groups, or include other groups such as health workers). The model takes as input the vaccine supply and weekly delivery constraints on the number of doses that can be administered. Parameters can also be altered according to whether a variant of concern (VoC) is prevalent that may affect vaccine efficacy. The model also allows for a proportion of the population to already be immune and for disease incidence to be constant, increasing or decreasing over time. It does not account for the impact of COVID-19 mortality on demography or the acquisition of natural immunity during the simulation period.

### Model description

The number of susceptible and naturally immune people in risk group $a$ in week $t$ are given by $S_{a,t}$ and $R_{a,t}$. The number vaccinated in week $t$ with one dose (and who have not yet received a second dose) are given by $vs1_{a,t}$ and $vr1_{a,t}$, and those vaccinated with a second dose by $vs2_{a,t}$ and $vr2_{a,t}$ respectively. The cumulative number of susceptible people immunised with one or two doses by week $t$ are given by $VS1_{a,t}=\sum_{i=0}^{t}vs1_{a,i}$ and $VS2_{a,t}=\sum_{i=0}^{t}vs2_{a,i}$ respectively, and the same notation used for naturally immune people ($VR1_{a,t}$, $VR2_{a,t}$). The total population in risk group $a$ in week $t$ is $N_{a,t}=S{a,t}+VS1{a,t}+VS2{a,t}+VR1{a,t}+VR2{a,t}$. 

The number of deaths averted by vaccination in risk group $a$ in week $t$ is given by the sum

$$\text{Deaths averted} = \lambda_{a,t} \rho_a \left[ \sum_{\tau=2}^{t} vs1_{a,t-\tau} es1_{\tau}  +  \sum_{\tau=1}^{t} \left( vr1_{a,t-\tau} er1_{\tau} + vs2_{a,t-\tau} es2_{\tau} + vr2_{a,t-\tau} er2_{\tau} \right) \right]  $$
where $\lambda_{a,t}$ is the force of infection experience in week $t$ by risk group $a$, $\rho_{a}$ is the IFR for risk group $a$ and vaccine efficacy $\tau$ weeks after vaccination for one dose given to a susceptible persion is $es1_\tau$, for one dose given to a naturally immune persion is $er1_\tau$, etc. We only sum vaccine effects from 2 weeks after the first dose given to a susceptible person and from 1 week after the second dose or after a first dose given to someone with natural immunity. We allow vaccine efficacy after one or two doses to wane or remain constant, depending on the vaccine and whether VoCs with immune escape properties (e.g. B.1.351) are circulating. 

The relative risk of infection by priority group $r_a$  is assumed constant over time, such that $\lambda_{a,t}=\lambda_tr_a$. The force of infection over time may be given by country-specific projections or simply assumed constant, increasing or decreasing. We ignore depletion of vaccinated groups due to mortality and acquisition/boosting of immunity due to natural infection over the time period of the simulation $T$, initially assumed to be 52 weeks.

### Vaccination strategies

The number of doses supplied in week $t$ is $V_t$ and the maximum number of doses that the country has capacity to administer in week $t$ is $D_t$. Excess supply in a given week is carried over to the next week.

_Prioritise second dose_  Immunise all those who have received a single dose $\tau$ weeks ago ($vs1_{a,t-\tau}$ and $vr1{a,t-\tau}$) for $\tau>\text{12 weeks}$ (the maximum interval recommended by WHO SAGE), prioritising according to the priority groups ($a=1$ then $a=2$ etc.) and within each group according to those vaccinated longest ago. Continue until the weekly delivery capacity or vaccine supply is exhausted, or if vaccine remains, then start providing first doses according to the prioritisation scheme until these conditions are met.

_Prioritise first dose_  Immunise everyone with a single dose according to the priority group order until vaccine delivery or supply constraints are met.

_Mixed strategies_ Immunise everyone in risk group 1 to $x$ (e.g. health workers, 70+) with a single dose before offering second doses according to the prioritise second doses strategy.

_Others_ Can be specified as required.

# 1vs2doses
 
