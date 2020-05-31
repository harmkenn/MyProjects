---
title: "Ballistic Trajectory with Drag"
author: "Ken Harmon"
date: "2020 May 28"
output:
  html_document:
    keep_md: yes
    code_folding: hide
    fig_height: 6
    fig_width: 12
    fig_align: center
  pdf_document: default
editor_options:
  chunk_output_type: console
---

# {.tabset .tabset-fade}






Ballistic Trajectory with Drag

*No Drag*

Firing Artillery is an interesting proposition. Hitting the target with as few
rounds as possible is very important. Each round fired comes at a cost. Besides
the monetary cost, each round draws unwanted attention from the enemy.
Therefore, each round needs to be carefully considered. Each target needs to
provide enough payoff to outweigh the cost and the calculations for each round
needs to be meticulous and timely in order to have as little adjustment as
possible and requiring subsequent rounds.

The initial physics behind firing artillery seems pretty straight forward.
Initial velocity at some angle: the initial velocity vector V

![](media/bdc6aece00f34d9fb2f177df1c70e766.png)

We analyze the components of that initial vector separately Vx and Vy.

$$V_{x} = Vcos\theta$$
$$V_{y} = Vsin\theta$$

If there were no drag on the round, Vx would remain constant and Vy would be
affected only by gravity. For this paper, $$x_{0} = y_{0} = t_{0} = x_{f} =
y_{f} = 0$$

$$\frac{\text{dx}}{\text{dt}} = \ Vcos\theta,\ \ x = Vtcos\theta,\ \
\frac{\text{dy}}{\text{dt}} = \ Vsin\theta - gt,\ \ y = Vtsin\theta -
\frac{1}{2}gt^{2}$$

So, for a given initial velocity, in order to hit the target, we need x to equal
the range (r) to the target at the same time y equals the height of the target
(zero in this case).

$$\frac{r}{\text{Vcosθ}} = t = \frac{2Vsin\theta}{g} \rightarrow r =
\frac{2V^{2}\text{sinθcosθ}}{g} = \frac{V^{2}sin2\theta}{g}$$

So, for a target at a given range and a round fired at a set velocity, we know
the angle that must be shot. $$\theta = \frac{1}{2}arcsin\left(
\frac{\text{rg}}{V^{2}} \right)$$

We also know the maximum height (H) of the round happens at half the range and
half the time.

$$If\ halftime = \frac{\text{Vsinθ}}{g}$$
, then 
$$H =\frac{V^{2}\sin^{2}\theta}{2g}$$

For example, to hit a target at 15km with an initial velocity of 671 m/s, the
initial angle needs to be .1664 radians, 9.54 degrees, or 169.7 mils. Time of
flight is 22.668. Max height is 629.87 m at 11.3339 seconds. Impact angle is
-169.7 mils. Impact velocity is 671 m/s.

This is all very predictable, but there is a great deal of drag so things
change.

*Drag*

If there is drag on the round, there are many variables that affect the velocity
of the round. Velocity in the x direction is no longer constant. I get these
formulas from Peter Chudinov from the journal of Physics[^1].

[^1]: https://iopscience.iop.org/article/10.1088/1742-6596/1287/1/012032

The drag constant $$k = \frac{\rho_{a}c_{d}S}{2mg} = \frac{1}{V_{t}^{2}}$$;

ρ*a* is the air density, *cd* is the drag factor for a sphere, *S* is the
cross-section area of the object, and *Vt* is the terminal velocity.

I will treat k as a constant even though ρ changes with altitude which may be
significant with artillery shots; in this case k could become a function. All
variables are functions of θ. Theta is the angle of trajectory that varies
throughout the flight.

Launch angle: $$\text{θϵ}\left\lbrack 0,\frac{\pi}{2} \right\rbrack$$ Impact
angle: $$- \text{θϵ}\left\lbrack 0,\frac{\pi}{2} \right\rbrack$$

![](media/09aa336ef56776834ee2eced9ecb01ff.png)

Here are the functions I am going to use to predict artillery shots given and
initial velocity and launch angle.

These first two equations I can compute directly from θ.

1.  $$f\left( \theta \right) = \frac{\text{sinθ}}{\cos^{2}\theta} + ln\left(
    \tan\left( \frac{\theta}{2} + \frac{\pi}{4} \right) \right)$$

2.  $$V\left( \theta \right) = \frac{V_{0}\cos\theta_{0}}{\text{cosθ}\sqrt{1 +
    kV_{0}^{2}\cos^{2}\theta_{0}\left( f\left( \theta_{0} \right) - f\left(
    \theta \right) \right)}}$$

The following formulas I cannot compute directly but I can get a numerical
solution for each θ.

The velocity in the x and y directions is now based off of instantaneous
velocity and trajectory angle; not just initial velocity and launch angle.

1.  $$\frac{\text{dx}}{\text{dt}} = Vcos\theta,
    \frac{\text{dy}}{\text{dt}} = Vcos\theta,
    \frac{\text{dθ}}{\text{dt}} = \frac{- gcos\theta}{V},
    \frac{\text{dV}}{\text{dt}} = - gsin\theta - gkV^{2}$$

Now convert all these so they are in terms of θ:

1.  $$\frac{\text{dV}}{\text{dθ}} = Vtan\theta + \frac{kV^{3}}{\text{cosθ}},\ \
    \ \ \frac{\text{dx}}{\text{dθ}} = \frac{V^{2}}{- g},\ \ \ \
    \frac{\text{dy}}{\text{dθ}} = \frac{V^{2}}{- g}tan\theta,\ \ \ \
    \frac{\text{dt}}{\text{dθ}} = \frac{V}{- gcos\theta}$$

2.  $$x = \int_{\theta_{0}}^{\theta}{\frac{V^{2}}{- g}\text{dθ}},\ \ \ \ y =
    \int_{\theta_{0}}^{\theta}{\frac{V^{2}}{- g}\text{tanθ dθ}},\ \ \ \ t =
    \int_{\theta_{0}}^{\theta}{\frac{V}{- gcos\theta}\text{dθ}}$$

Once I have and instantaneous altitude, I can alter k for appropriate air
pressure.

*Attempt at prediction*

Actual shot data that I am trying to match: Initial velocity 682 m/s at an
initial angle of 0.4174391238457 radians (425.2 mils). My prediction needs
to hit a level target at 15000 m in 42.1 seconds while reaching a maximum
altitude of 2295 m. Also, impact angle of - 0.6695519342963 radians (-682
mils) with and impact velocity of 310 m/s.


```r
V0 <- 682 # initial velocity in m/s for M795 with M232A1 4H
am0 <- 425.2 # QE in mils for a level 15000 m shot
th0 <- am0 * pi / 3200 # initial angle in radians
x0 <- 0 #Initial x
y0 <- 0 # initial y
t0 <- 0 # initial time
g <- 9.80665 # gravitational force in m/s/s
# All functions are in terms of angle of the trajectory
press <- data.frame(cbind(
  alt = c(0,200,500,1000,1500,2000,2500,3000,3500,
          4000,4500,5000,6000,7000,8000,9000),
  rho = c(1.2250,1.2133,1.1844,1.1392,1.0846,1.0320,
          .9569,.8632,.7768,.6971,.5895,.4664,.3612,.2655,.1937,.1413)))
c.press <- glm(rho~poly(alt,6,raw=TRUE), data = press)
rho <- as.numeric(predict(c.press, data.frame(alt = y0), type = "response"))
pk <- .00000155
k <- rho*pk # is the drag constant at 0 alt

#Lets build a table along the trajectory
ths <- seq(th0, -1, by = -.02)

f0 = sin(th0)/(cos(th0))^2 + log(tan(th0/2+pi/4))
allth <- th0
allf <- f0
allk <- k
allV <- V0
allt <- t0
allx <- x0
ally <- y0
allrho <- rho


for (th in ths) {
  allth <- c(allth,th)
  allk <- c(allk,k)
  f = sin(th)/(cos(th))^2 + log(tan(th/2+pi/4))
  allf <- c(allf,f)
  V = V0*cos(th0)/(cos(th)*sqrt(1+k*(V0*cos(th0))^2*(f0-f)))
  allV <- c(allV,V)
  tint <- integrate(function(j) {V0*cos(th0)/((cos(j))^2*sqrt(1+k*(V0*cos(th0))^2*(f0-f)))},th0,th)
  t <- t0 - 1/g*as.numeric(tint[1])
  allt <- c(allt,t)
  xint <- integrate(function(j) {(V0*cos(th0)/(cos(j)*sqrt(1+k*(V0*cos(th0))^2*(f0-f))))^2},th0,th)
  x <- x0 - 1/g*as.numeric(xint[1])
  allx <- c(allx,x)
  yint <- integrate(function(j) {tan(j)*(V0*cos(th0)/(cos(j)*sqrt(1+k*(V0*cos(th0))^2*(f0-f))))^2},th0,th)
  y <- y0 - 1/g*as.numeric(yint[1])
  ally <- c(ally,y)
  rho <- as.numeric(predict(c.press, data.frame(alt = y), type = "response"))
  allrho <- c(allrho,rho)
  k <- pk * rho
}
traj <- data.frame(cbind(allth,allk,allf,allV,allt,allx,ally,allrho))
trajp <- traj[2:nrow(traj),]%>%filter(ally>=0)
trajp %>% ggplot(aes(allx,ally))+geom_point()+
    coord_fixed(ratio = 1)
```

![](Trajectory2_files/figure-html/try-1.png)<!-- -->

```r
colnames(trajp) <- c("theta","k","f","V m/s","t s","x m","y m","pressure")
pander(trajp)
```


------------------------------------------------------------------------------
   theta         k           f       V m/s    t s     x m     y m    pressure 
----------- ----------- ----------- ------- ------- ------- ------- ----------
  0.4174     1.907e-06    0.9153      682      0       0       0       1.23   

  0.3974     1.907e-06    0.8636     663.5   1.48    905.7   390.9     1.19   

  0.3774     1.845e-06    0.8132     647.4   2.888   1738    729.9    1.161   

  0.3574      1.8e-06      0.764     632.8   4.233   2510    1025     1.136   

  0.3374     1.762e-06    0.7159     619.7   5.525   3231    1283     1.113   

  0.3174     1.725e-06    0.6687     607.9   6.77    3910    1509      1.09   

  0.2974     1.689e-06    0.6225     597.2   7.975   4553    1708     1.068   

  0.2774     1.655e-06    0.5772     587.5   9.145   5167    1882     1.046   

  0.2574     1.622e-06    0.5326     578.7   10.28   5755    2034     1.026   

  0.2374     1.591e-06    0.4887     570.6   11.4    6319    2166     1.008   

  0.2174     1.562e-06    0.4454     563.2   12.48   6864    2280     0.9908  

  0.1974     1.536e-06    0.4027     556.4   13.54   7389    2377     0.9759  

  0.1774     1.513e-06    0.3606      550    14.58   7895    2459     0.9631  

  0.1574     1.493e-06    0.3188     544.1   15.6    8384    2525     0.9525  

  0.1374     1.476e-06    0.2775     538.5   16.6    8855    2576     0.944   

  0.1174     1.463e-06    0.2365     533.2   17.58   9308    2613     0.9379  

  0.09744    1.454e-06    0.1958     528.2   18.53   9743    2637     0.9339  

  0.07744    1.448e-06    0.1553     523.4   19.47   10159   2647     0.9322  

  0.05744    1.445e-06    0.1151     518.7   20.38   10555   2644     0.9327  

  0.03744    1.446e-06    0.07493    514.2   21.27   10932   2629     0.9352  

  0.01744    1.45e-06     0.03488    509.8   22.14   11287   2601     0.9398  

 -0.002561   1.457e-06   -0.005122   505.5   22.99   11622   2562     0.9463  

 -0.02256    1.467e-06   -0.04513    501.3   23.82   11937   2512     0.9545  

 -0.04256    1.479e-06    -0.0852    497.2   24.62   12230   2452     0.9643  

 -0.06256    1.495e-06    -0.1254    493.1   25.4    12502   2381     0.9754  

 -0.08256    1.512e-06    -0.1657    489.2   26.16   12755   2301     0.9877  

  -0.1026    1.531e-06    -0.2062    485.4   26.9    12989   2212     1.001   

  -0.1226    1.552e-06    -0.247     481.6   27.62   13205   2115     1.015   

  -0.1426    1.573e-06    -0.2881    478.1   28.33   13404   2010     1.029   

  -0.1626    1.596e-06    -0.3295    474.6   29.01   13588   1899     1.044   

  -0.1826    1.618e-06    -0.3713    471.3   29.69   13759   1781     1.059   

  -0.2026    1.641e-06    -0.4136    468.2   30.35   13918   1657     1.074   

  -0.2226    1.664e-06    -0.4565    465.3    31     14067   1528     1.088   

  -0.2426    1.686e-06    -0.4999    462.6   31.64   14207   1393     1.102   

  -0.2626    1.708e-06    -0.5439    460.1   32.27   14338   1253     1.116   

  -0.2826    1.73e-06     -0.5887    457.8   32.9    14462   1108     1.129   

  -0.3026    1.75e-06     -0.6343    455.7   33.52   14580   957.6    1.142   

  -0.3226    1.771e-06    -0.6807    453.8   34.13   14690   802.6    1.155   

  -0.3426    1.791e-06    -0.7281    452.1   34.74   14792   642.6    1.169   

  -0.3626    1.811e-06    -0.7765    450.5   35.34   14884   477.6    1.183   

  -0.3826    1.833e-06    -0.826      449    35.93   14964   307.6    1.198   

  -0.4026    1.857e-06    -0.8767    447.5   36.5    15025   132.9    1.215   
------------------------------------------------------------------------------

