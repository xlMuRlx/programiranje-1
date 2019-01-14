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



def alternating_towers (visina):
    