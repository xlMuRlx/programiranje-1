# ===== TRETJA NALOGA ===== #

# a)
def simetricen(niz):
    return niz == niz[::-1]


# b)
from functools import lru_cache

@lru_cache(maxsize=None)
def stevilo_delov(niz, f):
    if niz == "" :
        return 0
    if f(niz) :
        return 1
    opcije = []
    for i in range(1, len(niz)):
        opcija = stevilo_delov(niz[:i], f) + stevilo_delov(niz[i:], f)
        opcije.append(opcija)
    return min(opcije)
    

# c)
@lru_cache(maxsize=None)
def razdeli(niz, f):
    if niz == "" :
        return (0, [niz])
    if f(niz) :
        return (1, [niz])
    opcije = []
    delitve = []
    for i in range(1, len(niz)):
        opcija1, delitev1 = razdeli(niz[:i], f)
        opcija2, delitev2 = razdeli(niz[i:], f)
        opcije.append(opcija1 + opcija2)
        delitve.append(delitev1 + delitev2)
    minimum = len(niz)
    for i in range(0, len(opcije)):
        if opcije[i] <= minimum:
            index = i
            minimum = opcije[i]
    return (opcije[index], delitve[index])



# d)
def vsotno_simetricen(niz):
    n = len(niz)
    seznam = [int(c) for c in niz]
    meja = int((n/2) // 1)
    return sum(seznam[:meja]) == sum(seznam[meja:])
