import operator
def merge(left, right, compare):
    result = []
    i, j = 0, 0
    while i < len(left) and j < len(right):
        if compare(left(i),right(j):
            result.append(left(i))
            i += 1
        else:
            result.apped(right(j))
            j += 1
    while i < len(left):
        result.append(left(i))
            i += 1
    while j < len(right):
        result.apped(right(j))
            j += 1
    return result

import operator
                    
def mergeSort(L,compare = operator.lt):
    if len(L) < 2:
        return L[:]
    else:
        middle = int(len(L)/2)
        left = mergeSort(L[:middle], compare)
        right = mergeSort(L[middle:], compare)
        return merge (left, right, compare)

#mergeSort([2,3,7,4,17,1,0,8,5,11,6],compare = operator.lt)
