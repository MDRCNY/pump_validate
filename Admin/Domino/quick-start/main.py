# This is a sample Python script

# Import dependencies
import random
import matplotlib.pyplot as plt
import json
import os

# Print to console/stdout
print("Hello, {0}!".format(os.environ['DOMINO_PROJECT_OWNER']))


# Define a helper function to generate a random number
def random_number(start, stop):
    return random.uniform(start, stop)


# Plot the values of random points
x = random.sample(range(1000), 100)
xbins = [0, len(x)]
plt.bar(range(0, 100), x)
plt.show()
plt.savefig('results/myHistogramFromPython.png', format='png')

# Generate and save some key statistics to dominostats.json
#   learn more at http://support.dominodatalab.com/hc/en-us/articles/204348169
r2 = round(random_number(0, 1), 4)
p = round(random_number(0, 1), 4)
with open('dominostats.json', 'w') as f:
    f.write(json.dumps({"R^2": r2, "p-value": p}))
