def rec_num_sum(n):
    if n == 1:
        return 1
    else:
        return n+rec_num_sum(n)

def num_sum(n):
    sum = 0
    for i in range(n+1):
        sum = sum + i
    return sum
    
    
print(rec_num_sum(1000000))