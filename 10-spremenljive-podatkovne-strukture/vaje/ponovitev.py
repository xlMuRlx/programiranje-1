def index_min (lower, upper, seznam):
    min = seznam[lower]
    stevec = lower
    for i in range(lower, upper + 1):
        if seznam[i] < min:
            min = seznam[i]
            stevec = i
    return stevec


def zamenjaj (seznam, i, j):
    m = seznam[i]
    seznam[i] = seznam[j]
    seznam[j] = m
    return seznam


def selection_sort (seznam):
    n = len(seznam)
    stevec = 0
    while stevec < n:
        nov_index = index_min(stevec, n-1, seznam)
        zamenjaj(seznam, stevec, nov_index)
        stevec +=1
    return seznam