#!/usr/bin/python
# -*- coding: utf-8 -*-
import random as rd
import matplotlib.pyplot as plt
import numpy as np

#fonctions utiles  sur les piles
def creerPile():
    return[]
    
def depiler(p):
    assert len(p)>0
    return p.pop()
    
def sommet(p):
    assert len(p)>0
    return p[-1]
    
def empiler(v,p):
    p.append(v)
 
 
def estVide(p):
    return len(p)==0    

#labyrinthe    
def visiter(c):
    (x,y)=c                                         #  coordonnées de  l'argument
    if x in range(n) and y in range(n):     
     #on n'écrit pas à l'extérieur de la matrice
        atteinte[x][y]=True
    
def estAtteinte(c):
    (x,y)=c
    if x in range(n) and y in range(n) :
        return atteinte[x][y]
     # les cases extérieures sont déjà atteintes
    else:
        return True     
    
    
#on détermine les positions adjacentes à c non encore visitées
#le résultat est renvoyé sous forme d'un tableau qui contient de 0 à 4 éléments

def choix(c):
    (x,y)=c
    t=[]
    def ajouter(p):
        if not estAtteinte(p):t.append(p)
    ajouter((x-1,y))
    ajouter((x+1,y))
    ajouter((x,y-1))
    ajouter((x,y+1))
    return t
    
def tirage(L):
    n=len(L)
    assert n>0
    return L[rd.randint(0,n-1)]
    
#programme principal
n=int(input('Taille du labyrinthe?')) 

plt.clf()
plt.axis([-1,n,-1,n])

atteinte=[[False]*n for i in range(n)]
pile=creerPile()
empiler( (0,0),pile)
visiter((0,0))

#La matrice chemin est là en vue de dessiner les "solutions"
#C'est une matrice vide dont les éléments seront des tuples 
#chemin[c] contient la case à partir de laquelle on
#a atteint la case c 
chemin=np.empty((n,n),tuple) 

while not estVide(pile):
    caseActuelle = sommet(pile)
    voisinsNonVisites = choix(caseActuelle)
    if len(voisinsNonVisites) >0:
        caseSuivante = tirage(voisinsNonVisites)
        visiter(caseSuivante)
        x, y = caseActuelle
        z, t = caseSuivante
        chemin[z, t] = caseActuelle
        plt.plot([x,z], [y,t],'b',lw=200/n)
        empiler(caseSuivante, pile)
    else:
        depiler(pile)

    
    
plt.show() 

#construction de la solution
def solution (debut,fin,couleur):
    case=fin
    sol=creerPile()    
    empiler( fin,sol)
    while case!=debut:
        x,y =case 
        prec=  chemin[x,y] 
        empiler( prec,sol)
        case=prec
        
    X=[a[0] for a in sol]
    Y=[a[1] for a in sol]
    
    plt.plot(X,Y,couleur,linewidth=50/n)
    plt.show()

solution((0,0),(n-1,n-1), 'r') 