install.packages("SCperf")
install.packages("triangle")
install.packages("Rmisc")
library("SCperf")
library("triangle")
library("Rmisc")
# ------ PART 1-------
# Annual demand (D)
annual_demand = 15000

# Unit cost (C)
each_unit_cost = 80

#Holding cost(H)
holding_cost = 0.18 * each_unit_cost

#ordering cost(S)
order_cost = 220

#Calculating EOQ

EconomicOrderQuantity <- EOQ(annual_demand,order_cost,holding_cost)
EconomicOrderQuantity

# Calculating annual ordering cost

annual_ordering_cost <- annual_demand/EconomicOrderQuantity[1]*order_cost
annual_ordering_cost

# Calculating annual holding cost

annual_holding_cost <- EconomicOrderQuantity[1]/2*holding_cost
annual_holding_cost

#Calculating total cost
total_inventory_cost <- (annual_demand/EconomicOrderQuantity[1]*order_cost)+(EconomicOrderQuantity[1]/2*holding_cost)
total_inventory_cost

#optimizing EOQ value
intervalEOQ <- c(500,750) #setting the interval

#defining the function
fEOQ <- function(EconomicOrderQuantity) {
  (EconomicOrderQuantity[1]/2*holding_cost)+(annual_demand/EconomicOrderQuantity[1]*order_cost)
}

#using the optimise function to get the optimal EOQ value.
optimize_EOQ <- optimise(f=fEOQ , interval= intervalEOQ, lower=min(intervalEOQ), upper = max(intervalEOQ), maximum = FALSE, tol=.Machine$double.eps^0.5)
optimize_EOQ

# Printing the required EOQ and Min Total Cost
print(paste("Optimized EOQ is:", round(optimize_EOQ[[1]], 0)))
print(paste("Minimum Total Cost is:", round(optimize_EOQ[[2]], 0)))


# ----- Part 2 ------

min_annual_demand = 13000
max_annual_demand = 17000
mode_annual_demand = 15000

# Calculating EOQ for each 1000 demand values.
demand_range = round(rtriangle(n=1000, min_annual_demand, max_annual_demand, mode_annual_demand))
Reorder_append_values <- c()
for (val in demand_range)
{
  Reorder_level = round(sqrt((2*val*order_cost)/holding_cost))
  print(Reorder_level)
  Reorder_append_values <- c(Reorder_append_values, Reorder_level)
}
print(Reorder_append_values)

# Calculating holding cost for each EOQ.
each_holding_cost_append <- c()
for (i in Reorder_append_values)
{
  each_holding_cost = i*holding_cost/2
  print(each_holding_cost)
  each_holding_cost_append <- c(each_holding_cost_append, each_holding_cost)
}
each_holding_cost_append

# Calculating total ordering cost for each EOQ.

annual_ordering_cost_each <- demand_range* order_cost / Reorder_append_values

# Calculating annual total cost
annual_total_cost <- annual_ordering_cost_each + each_holding_cost_append
#Calculating total number of orders annually
 total_no_orders <- demand_range / Reorder_append_values
# Creating data frame
 inventory_data <- data.frame(demand_range, Reorder_append_values, each_holding_cost_append, annual_ordering_cost_each, annual_total_cost, total_no_orders)
inventory_data 

# Calculating confidence intervals and the type of probability distribution.
#for total annual cost
CI(annual_total_cost, ci = 0.95)
p1 <- density(annual_total_cost)
plot(p1)
#for EOQ
CI(Reorder_append_values, ci=0.95)
p2 <- density(Reorder_append_values)
plot(p2)
# for number of orders annually
CI(total_no_orders, ci= 0.95)
p3 <- density(total_no_orders) 
plot(p3)
