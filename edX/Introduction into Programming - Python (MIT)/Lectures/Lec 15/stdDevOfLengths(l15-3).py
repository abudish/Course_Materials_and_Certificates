def stdDevOfLengths(L):
    """
    L: a list of strings

    returns: float, the standard deviation of the lengths of the strings,
      or NaN if L is empty.
    """
    if len(L) == 0:
        return float('NaN')
    sumOfLenWords = 0.0
    for word in L:
        sumOfLenWords += len(word)   
    mean = sumOfLenWords / len(L)    
    tot = 0.0
    for word in L:
        tot += (len(word) - mean)**2        
    return (tot/len(L))**0.5
    
        
