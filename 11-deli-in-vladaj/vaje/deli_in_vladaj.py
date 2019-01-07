##############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
##############################################################################

def zamenjaj(a, prvi, drugi):
    pomozni = a[prvi]
    a[prvi] = a[drugi]
    a[drugi] = pomozni
    return a

# Števca označimo z rdeči in modri za lažjo povezavo s skico v zvezku
def pivot(a, start, end):
    pivot = a[start]
    rdeci = start + 1
    while a[rdeci] < pivot:
        rdeci += 1
    modri = rdeci + 1
    while modri <= end:
        if a[modri] < pivot:
            zamenjaj(a, rdeci, modri)
            rdeci += 1
            modri = rdeci + 1
        modri += 1
    zamenjaj(a, start, rdeci-1)
    return rdeci-1



##############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#   >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#   >>> quicksort(a)
#   [2, 3, 4, 5, 10, 11, 15, 17, 18]
##############################################################################

def quicksort_part(a, start, end):
    n = len(a)
    k = pivot(a, 0, n-1)
    if n == 1:
        return a
    else:
        quicksort_part(a, 0, k-1)
        quicksort_part(a, k+1, n-1)


def quicksort(a):
    n = len(a)
    quicksort_part(a, 0, n-1)



##############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
##############################################################################

def kth_element(a, k):
    n = len(a)
    l = pivot(a, 0, n)
    if k == l:
        return a[k]
    elif k < l:
        kth_element(a[0:l], k)
    else:
        kth_element(a[l:n-1], k-l)