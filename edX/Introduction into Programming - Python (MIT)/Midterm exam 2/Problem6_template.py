import random
import pylab

# Global Variables
MAXRABBITPOP = 1000
CURRENTRABBITPOP = 500
CURRENTFOXPOP = 30

def rabbitGrowth():
    """ 
    rabbitGrowth is called once at the beginning of each time step.

    It makes use of the global variables: CURRENTRABBITPOP and MAXRABBITPOP.

    The global variable CURRENTRABBITPOP is modified by this procedure.

    For each rabbit, based on the probabilities in the problem set write-up, 
      a new rabbit may be born.
    Nothing is returned.
    """
    global CURRENTRABBITPOP
    global MAXRABBITPOP
    if random.random() < (1.0 - CURRENTRABBITPOP/float(MAXRABBITPOP)):
        CURRENTRABBITPOP += 1
            
def foxGrowth():
    """ 
    foxGrowth is called once at the end of each time step.

    It makes use of the global variables: CURRENTFOXPOP and CURRENTRABBITPOP,
        and both may be modified by this procedure.

    Each fox, based on the probabilities in the problem statement, may eat 
      one rabbit (but only if there are more than 10 rabbits).

    If it eats a rabbit, then with a 1/3 prob it gives birth to a new fox.

    If it does not eat a rabbit, then with a 1/10 prob it dies.

    Nothing is returned.
    """
    global CURRENTFOXPOP
    global CURRENTRABBITPOP
    global MAXRABBITPOP

    if CURRENTRABBITPOP > 10:
        if random.random() < CURRENTRABBITPOP/float(MAXRABBITPOP):
            CURRENTRABBITPOP -= 1
            if random.random() < 1/3.0:
                CURRENTFOXPOP += 1
        else:
            if random.random() < 1/10.0:
                CURRENTFOXPOP -= 1
    else:
        if random.random() < 1/10.0:
                CURRENTFOXPOP -= 1
        
def runSimulation(numSteps=200):
    """
    Runs the simulation for `numSteps` time steps.

    Returns a tuple of two lists: (rabbit_populations, fox_populations)
      where rabbit_populations is a record of the rabbit population at the 
      END of each time step, and fox_populations is a record of the fox population
      at the END of each time step.

    Both lists should be `numSteps` items long.
    """
    rabbits = []
    foxes = []
    global CURRENTFOXPOP
    global CURRENTRABBITPOP 
    for step in range(numSteps):
        rabbitGrowth()
        foxGrowth()
        rabbits.append(CURRENTRABBITPOP)
        foxes.append(CURRENTFOXPOP)
    return (rabbits, foxes)
##    return rabbits[-1], foxes [-1]
def plotRabFox(RabAndFoxes):
    """
    RabAndFoxes - tuple of two lists, first - rabbits' populations, second - foxes'
    """
    rabbits = RabAndFoxes[0]
    foxes = RabAndFoxes[1]
    pylab.xlabel('Time steps')
    pylab.ylabel('Population')
    pylab.plot(rabbits,label = 'Rabbits')
    pylab.plot(foxes,label = 'Foxes')
    pylab.legend(loc='best')
    pylab.show()

def poly(RabAndFoxes):
    
    rabbits = RabAndFoxes[0]
    foxes = RabAndFoxes[1]
    pylab.xlabel('Time steps')
    pylab.ylabel('Population')
    coeffR = pylab.polyfit(range(len(rabbits)), rabbits, 2)
    coeffF = pylab.polyfit(range(len(foxes)), foxes, 2)
##    print coeffR
##    print coeffF
    pylab.plot(polyval(coeffR, range(len(rabbits))))
    pylab.plot(polyval(coeffF, range(len(foxes))))
    
    pylab.legend(loc='best')
    pylab.show()


