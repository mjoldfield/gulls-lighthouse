import matplotlib.pyplot as plt
import random
import math
from scipy import stats

def random_a(x, y):
    theta = random.uniform(-math.pi / 2.0, math.pi / 2.0)
    a = x + y * math.tan(theta)
    return a

def random_as(x,y,n):
    xs = []
    for i in range(n):
        xs.append(random_a(x,y))
    return xs

def simulate(x,y,stem):
    data = random_as(x,y,10000)
    desc = stats.describe(data)
    print desc

    # save description of data
    with open(stem + '-stats.txt', 'w') as f:
        f.write(str(desc))

    # plot histogram of counts
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.axis([-10,10,0,800])
    ax.hist(data, range=(-10.0, 10.0), bins=100)

    fig.savefig(stem + '.svg')

    plt.close(fig)

    # plot histogram of fraction, and overlay the Cauchy
    fig = plt.figure()
    ax = fig.add_subplot(111)

    ax.axis([-10,10,0,0.4])
    n, bins, patches = ax.hist(data, range=(-10.0, 10.0), bins=100, alpha=0.2, normed=1)

    cauchy = map(lambda a: y / (math.pi * ((a-x)**2 + y**2)), bins)

    ax.plot(bins, cauchy, 'r--', linewidth=2)

    fig.savefig(stem + '-c.svg')

    plt.close(fig)
    

simulate(1.0,1.0,'hist_1_1')    
simulate(1.0,2.0,'hist_1_2')    
