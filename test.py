def rec(n):
    sum = 0
    for i in range(n+1):
        sum = sum + i
    return sum
    
    
print(rec(1000000))