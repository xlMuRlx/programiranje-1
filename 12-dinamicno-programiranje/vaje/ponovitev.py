test = [[1,2,0], 
    	[2,4,5], 
        [7,0,1]]

from functools import lru_cache


def max_cheese (matrika):

   # @lru_cache(maxsize=None)
    def max_cheese2(stolpec, vrstica):
        max_vrstica = len(matrika) - 1
        max_stolpec = len(matrika[0]) - 1
        if (vrstica >= max_vrstica) or (stolpec >= max_stolpec):
            return 0
        else:
            spodaj = max_cheese2 (vrstica + 1, stolpec)
            desno = max_cheese2 (vrstica, stolpec + 1)
            sir = matrika[vrstica][stolpec]
            sir += max (spodaj, desno)
            return sir
    return max_cheese2(0, 0)





articles = [
    ("yoghurt", 0.39, 0.18),
    ("milk", 0.89, 1.03),
    ("coffee", 2.19, 0.2),
    ("butter", 1.49, 0.25),
    ("yeast", 0.22, 0.042),
    ("eggs", 2.39, 0.69),
    ("sausage", 3.76, 0.50),
    ("bread", 2.99, 1.0),
    ("Nutella", 4.99, 0.75),
    ("juice", 1.15, 2.0)
]


def best_value(articles, max_w):

    @lru_cache(maxsize=None)
    def best_val(w):
        opcije = []
        for item in articles:
            (ime, cena, teza) = item
            if (w - teza) < 0:
                pass
            else:
                opcija = cena + best_val(w - teza)
                opcije.append(opcija)
        if opcije:
            return max(opcije)
        else:
            return 0

    return best_val(max_w)




def best_value_unique(articles, max_w):

    @lru_cache(maxsize=None)
    def best_val_unique(w, taken):
        opcije = []
        i = 0
        for item in articles:
            (ime, cena, teza) = item
            i += 1
            if ((w - teza) < 0) or taken[i] == "1":
                pass
            else:
                new_taken = taken[:i] + "1" + taken[i+1:]
                opcija = cena + best_val_unique(w - teza, new_taken)
                opcije.append(opcija)
        if opcije:
            return max(opcije)
        else:
            return 0

    return best_val_unique(max_w, "0" * len(articles))