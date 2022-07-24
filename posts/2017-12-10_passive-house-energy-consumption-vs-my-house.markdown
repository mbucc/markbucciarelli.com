---
title: Passive House Energy Consumption vs. My House
date: December 10, 2017
tags: eco
---

How does my house's energy usage compare to the
[passive house](https://en.wikipedia.org/wiki/Passive_house) energy
usage standard?

A passive house is a super-insulated house that uses very little energy
and leaks very little air.  The passive house standards are
straightforward:

1. The building must be designed to have an annual heating and
cooling demand as calculated with the passive house Planning Package
of not more than 15 kWh/m2 (4,755 BTU/sq ft; 5.017 MJ/sq ft) per
year in heating or cooling energy OR be designed with a peak heat
load of 10 W/m2 (1.2 hp/1000 sq ft).

2. Total primary energy (source energy for electricity, etc.)
consumption (primary energy for heating, hot water and electricity)
must not be more than 60 kWh/m2 (19,020 BTU/sq ft; 20.07 MJ/sq ft)
per year.

3. The building must not leak more air than 0.6 times the house
volume per hour (n50 ≤ 0.6 / hour) at 50 Pa (0.0073 psi) as tested
by a blower door, or alternatively when looked at the surface area
of the enclosure, the leakage rate must be less than 0.05 cubic
feet per minute.

Our heating and cooling system powered by natural gas, and electricity
powers the fan that distributes the hot or cold air.  I don't know
how to split out the portion of the electric bill used by this
electric fan, so I'll use the total primal energy standard.

ref: <https://en.wikipedia.org/wiki/Passive_house#Standards_2>

Our total energy consumption consists of electricity and natural gas.

## Electricity: primary energy = 8,370 kWh / year

* Electricity usage: 5,161 kWh for last 12 months
* Primary energy [resource mix](https://www.iso-ne.com/about/key-stats/resource-mix) in New England, as of 2016 (rounded to the nearest integer):
    1. natural gas: 49%
    2. nuclear: 31%
    3. renewables: 10%
    4. hydro: 7%
    5. coal: 2%
    6. oil: 1%
* Generation [efficiency](http://insideenergy.org/2015/11/06/lost-in-transmission-how-much-electricity-disappears-between-a-power-plant-and-your-plug/) and transmission loss:
    1. natural gas generation efficiency: 45%
    2. all others: 34%
    3. transmission losses [for Massachusetts](http://insideenergy.org/2015/11/06/electricity-losses-state-by-state-interactive/): 9%


```bash
$ # natural gas primary energy in kWh:
$ echo "((5161*.49)/(1-0.09))/(1-0.49)"|bc
5449
$ # renewable + hydro primary energy
$ echo 0
0
$ # all other primary energy less hydro and renewables
$ echo "((5161*(1 - .49 - .1 - 0.07))/(1-0.09))/(1-0.34)"|bc
2921
$ # total primary energy used for electricity:
$ echo 2921+5449|bc
8370
```

The total primary energy used for our electricity consumption is 8,370 kWh / year.  (To be pedantic, New England imports roughly 17% from New York, and we are assuming their losses are the same.)

## Natural Gas: primary energy = 28,626 kWh / year


* 977 Therms used over last 12 months.
* There is not enough data to accurately calculate [distribution losses](http://www.mass.gov/eea/docs/dpu/gas/icf-lauf-report.pdf) in Massachusetts.  The two distribution companies analyzed in the linked study had "lost and unaccounted for" (LAUF) gas of at most 2.6%.
* There are 29.3 kWh in a Therm.

So, total  from natural gas usage is 28,626 kWh.

## Conclusion: 159 kWh primary energy/square-meter

Our house is roughly 2,500 square feet, which is 232 square meters.  So, our primary energy usage / square-meter is <code>echo "(28626+8370)/232" | bc</code> = 159.

So, we use nearly three times the energy of a passive house.  I actually expected our house to come out much worse.  Perhaps the result is biased because last winter was so mild and/or because our house is less than 20 years old.

So, how big of a solar array would you need to fully power a (much smaller) 1,200 square foot, off-grid, passive house?

* 1,200 square feet = 111 square meters
* 111 * 60 kWh = 6,660 kWh/yr
* 6,660 kWh / 365 days = 18 kWh/day

Since average inverter efficiency is 95%, we can just use the 18 kWh/day figure.  At a cost of $3/Watt installed, that's a one-time cost of $55,000.  Add two Telsa powerwalls for an additional $15,000 ($1,000 install cost swag per) to cover for cloudy days and we're up to $70,000 one-time cost for energy.  This works out to $527/month.

This seems high to me, and in an extreme case ([Colorado man builds state’s most energy efficient house](https://inhabitat.com/colorado-man-single-handedly-builds-states-most-energy-efficient-house/)), the passive house needs only a small propane "hydronic" heating system, even in a wintry place like the Colorado Rockies.  From the analysis of my house, heating represents over 75% of the primary energy usage, so given the Colorado data point, I'm optimistic you could do much better than 18 kWh/day in the Northeast.

To match our currently month gas and electric bills, we would need to get by on one powerwall (estimated $7,500 installed) and an array of 6 kWh.  (The monthly payment on a loan of $25,000 over 15 years at a 4.25% interest rate is $188/month.)

Even with drastically dropping cost/Watt of solar, and the benefits of building a super-insulated house, it is still tough to have it make purely financial sense over a 15-year period.  It depends on how much you value not using fossil fuels.




