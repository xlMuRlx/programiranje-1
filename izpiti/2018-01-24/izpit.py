# ===== ČETRTA NALOGA ===== #

test = [[2, 4, 1, 1], 
        [3, 2, 0, 5], 
        [8, 0, 7, 2]]


from functools import lru_cache

def lisjacek(n, matrika):
    max_vrstica = len(matrika)
    max_stolpec = len(matrika[0])

    @lru_cache(maxsize=None)
    def lisjacek2(k, vrstica, stolpec):
        if (k == 0) or (vrstica >= max_vrstica) or (stolpec >= max_stolpec):
            return 0
        if stolpec + 1 == max_stolpec:
            pomozni = matrika[vrstica]
            return pomozni[len(pomozni) - 1]
        else:
            desno = lisjacek2 (k - 1, vrstica, stolpec + 1)
            spodaj = lisjacek2 (k - 1, vrstica + 1, 0)
            sadje = matrika[vrstica][stolpec]
            sadje += max(desno, spodaj)
            return sadje
        
    return lisjacek2(n, 0, 0)


# b)
# Funkcija ima časovno zahtevnost O(mn), če gre za mxn matriko.
