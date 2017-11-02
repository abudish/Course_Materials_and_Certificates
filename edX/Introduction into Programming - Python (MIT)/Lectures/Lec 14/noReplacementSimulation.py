import random

def noReplacementSimulation(numTrials):
    '''
    Runs numTrials trials of a Monte Carlo simulation
    of drawing 3 balls out of a bucket containing
    3 red and 3 green balls. Balls are not replaced once
    drawn. Returns the a decimal - the fraction of times 3 
    balls of the same color were drawn.
    '''

     
    
    yes = 0.0
    for i in range(numTrials):
        bucket = ['R', 'R', 'R', 'B', 'B', 'B']
        for j in range(3):
            ball = random.choice(bucket)
            bucket.remove(ball)
        if bucket == ['R', 'R', 'R'] or bucket == ['B', 'B', 'B']:
            yes += 1
    return yes/float(numTrials)
