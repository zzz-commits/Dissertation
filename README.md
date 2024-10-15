import gurobipy as gp
from gurobipy import GRB

# Sets
months = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
suppliers = ['A', 'B', 'C', 'D', 'E']
products = ['18/10', '18/8', '18/0']

# Parameters
demand = {
    '18/10': [25, 25, 0, 0, 0, 50, 12, 0, 10, 10, 45, 99],
    '18/8': [10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 10],
    '18/0': [5, 20, 80, 25, 50, 125, 150, 80, 40, 35, 3, 100]
}
cost_per_supplier = {'A': 5, 'B': 10, 'C': 9, 'D': 7, 'E': 8.5}
max_supply_per_supplier = {'A': 90, 'B': 30, 'C': 50, 'D': 70, 'E': 20}
holding_cost_per_product = {'18/10': 20, '18/8': 10, '18/0': 5}
chromium_content = {'A': 18, 'B': 25, 'C': 15, 'D': 14, 'E': 0}
nickel_content = {'A': 0, 'B': 15, 'C': 10, 'D': 16, 'E': 10}
required_chromium = {'18/10': 18, '18/8': 18, '18/0': 18}
required_nickel = {'18/10': 10, '18/8': 8, '18/0': 0}
production_capacity = 100

# Model
model = gp.Model("StainlessSteelProduction")

# Decision variables: amount bought from each supplier per month
buy = model.addVars(months, suppliers, vtype=GRB.CONTINUOUS, name="buy")

# Decision variables: amount produced of each product per month
produce = model.addVars(months, products, vtype=GRB.CONTINUOUS, name="produce")
inventory = model.addVars(months, products, vtype=GRB.CONTINUOUS, name="inventory")

# Objective function: minimize buying costs and holding costs
buying_cost = gp.quicksum(buy[m, s] * cost_per_supplier[s] for m in months for s in suppliers)
holding_cost = gp.quicksum(inventory[m, p] * holding_cost_per_product[p] for m in months for p in products)

model.setObjective(buying_cost + holding_cost, GRB.MINIMIZE)

# Constraints
for m in months:
    # Demand satisfaction constraint
    if m == 'Jan':  # No inventory carried over from the previous month
        for p in products:
            model.addConstr(produce[m, p] == demand[p][months.index(m)])
    else:
        prev_month = months[months.index(m) - 1]
        for p in products:
            model.addConstr(produce[m, p] + inventory[prev_month, p] - inventory[m, p] == demand[p][months.index(m)])

    # Production capacity constraint
    model.addConstr(gp.quicksum(produce[m, p] for p in products) <= production_capacity)

    # Supplier max supply constraint
    for s in suppliers:
        model.addConstr(buy[m, s] <= max_supply_per_supplier[s])


    total_chromium_needed = gp.quicksum(produce[m, p] * required_chromium[p] / 100 for p in products)
    total_nickel_needed = gp.quicksum(produce[m, p] * required_nickel[p] / 100 for p in products)
    
    model.addConstr(gp.quicksum(buy[m, s] * chromium_content[s] / 100 for s in suppliers) >= total_chromium_needed)
    model.addConstr(gp.quicksum(buy[m, s] * nickel_content[s] / 100 for s in suppliers) >= total_nickel_needed)



# Solve the model
model.optimize()

# Output results
if model.status == GRB.OPTIMAL:
    print(f'Optimal total cost: {model.objVal}')
    for m in months:
        print(f'\nMonth: {m}')
        for s in suppliers:
            print(f'  Buy from {s}: {buy[m, s].x:.2f} kg')
        for p in products:
            print(f'  Produce {p}: {produce[m, p].x:.2f} kg')
            print(f'  Inventory of {p} at end of month: {inventory[m, p].x:.2f} kg')
