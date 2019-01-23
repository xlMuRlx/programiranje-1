# ===== TRETJA NALOGA ===== #

from functools import lru_cache


def tipkanje(niz):

    @lru_cache(maxsize=None)
    def tipkanje2(niz, levi, desni):
        if niz == "":
            return 0
        elif desni <= levi:
            return float('inf')
        elif (levi < 0) or (desni > 9):
            return float('inf')
        elif niz[0] == str(levi) or niz[0] == str(desni):
            return 1 + tipkanje2(niz[1:], levi, desni)
        else:
            levi_levo = tipkanje2(niz, levi - 1, desni)
            levi_desno = tipkanje2(niz, levi + 1, desni)
            desni_levo = tipkanje2(niz, levi, desni - 1)
            desni_desno = tipkanje2(niz, levi, desni + 1)
            return min(levi_levo, levi_desno, desni_levo, desni_desno)
    
    return tipkanje2(niz, 0, 9)




def narasca(seznam):
    k = -float('inf')
    for i in range(0, len(seznam)):
        if seznam[i] < k:
            return False
        k = seznam[i]
    return True


def so_vmes(seznam):
    n = len(seznam)
    if seznam == []:
        return True
    k = seznam[0]
    m = seznam[n-1]
    for i in range(0, n):
        if (seznam[i] < k) or (seznam[i] > m):
            return False
    return False




def najdaljsi(seznam):

    @lru_cache(maxsize=None)
    def najdaljsi2(zacetek, konec):
        koncne_opcije = []
        if zacetek >= konec:
            return (0, zacetek, konec)
        else:
            for i in range(zacetek, konec):
                opcije = []
                if narasca(seznam[i:konec]):
                    dodatek = (konec-i, i, konec)
                else:
                    opcije.append(najdaljsi2(i+1, konec))
                    dodatek = max(opcije)
                koncne_opcije.append(dodatek)

            for i in range(zacetek, konec):
                opcije = []
                if narasca(seznam[zacetek:konec-i]):
                    dodatek = (konec-i-zacetek, zacetek, konec-i)
                else:
                    opcije.append(najdaljsi2(zacetek, konec-i-1))
                    dodatek = max(opcije)
                koncne_opcije.append(dodatek)

        return max(koncne_opcije)
    
    dolzina, zacetek, konec = najdaljsi2(0,len(seznam))
    return (seznam[zacetek], seznam[konec-1])

        