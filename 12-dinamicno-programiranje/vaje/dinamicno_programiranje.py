# 1. naloga

test_matrix = [[1, 2, 0], [2, 4, 5], [7, 0, 1]]

def max_cheese (cheese_matrix):
    def max_cheese2 (vrstica, stolpec, cheese_matrix):
        max_vrstica = len(cheese_matrix) - 1
        max_stolpec = len(cheese_matrix[0]) - 1
        vrstica = 0
        stolpec = 0
        if (vrstica >= max_vrstica) or (stolpec >= max_stolpec):
            return 0
        else:
            desno = max_cheese2(vrstica, stolpec + 1, cheese_matrix)
            spodaj = max_cheese2(vrstica + 1, stolpec, cheese_matrix)
            our_cheese = cheese_matrix[vrstica][stolpec]
            our_cheese += max(desno, spodaj)
    max_cheese2 (0, 0, cheese_matrix)




# 3. naloga

from functools import lru_cache

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

    @lru_cache(maxsize=None) # memoizacija v Pythonu
    def best_val(w):
        options = []
        for item in articles:
            (name, price, weight) = item
            if (w - weight) < 0:
                pass
            else:
                option = best_val(w - weight) + price
                options.append(option)
        if options:
            return max(options)
        else:
            return 0
    return best_val(max_w)



def best_value_unique(articles, max_w):
    # taken je string, pri katerem taken[n] = "0" označuje da n-ti izdelek še ni bil izbran

    @lru_cache(maxsize=None)
    def best_val(w, taken):
        options = []
        for i, item in enumerate(articles):
            (name, price, weight) = item
            if (w - weight < 0) or (taken[i] == "1"):
                pass
            else:
                new_taken = taken[:i] + "1" + taken[i+1:]
                option = best_val(w - weight, new_taken) + price
                options.append(option)
        if options:
            return max(options)
        else:
            return 0
    return best_val(max_w, "0" * len(articles))