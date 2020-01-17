# This is a sample Python model

# Import dependencies
import random


# Define a helper function to generate a random number:
def random_number(start, stop):
    return random.uniform(start, stop)


# Define a function to create an API
# To call, use {"data": {"start": 1, "stop": 100}}
# Learn more at http://support.dominodatalab.com/hc/en-us/articles/204173149
def my_model(start, stop):
    return dict(a_random_number=random_number(start, stop))
