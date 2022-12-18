# Traducció automàtica de documents LaTeX

Aquest document descriu com instal·lar i usar una eina de línia de comanda que permet traduïr
petits fitxers LaTeX.
L'eina s'ha provat en el sistema operatiu OSX 13.0.1


## El servei de traducció
A hores d'ara l'eina utiliza la [API](https://api.translated.com/v2) de [translated](https://translated.com/welcome)
per a fer la traducció.
Aquest servei imposa un límit diari de mots traduïts (vegeu [especificació de l'usuari](#com-especifiquem-un-usuari))



## Instal·lació

Us caldrà tenir instal·lada [l'eina stack de Haskell](https://docs.haskellstack.org/en/stable/).
Feu 

    stack build

Per tal d'instal·lar l'eina a un lloc més accessible haureu de fer

    stack install

En fer-ho us indicarà a quin lloc ha instal·lat l'eina.
Comproveu que aquesta localització és dins del vostre `PATH`

També podeu usar l'eina des de dins de _stack_ sense instal·lar-la, fent:

    stack exec hatexmt-exe --

i posant desprès del `--` les opcions de l'eina . (Mireu la secció d'us.)


## Us
Les opcions del eina es poden obtenir fent:

    hatexmt-exe -h

Us dóna:
````
Usage: hatexmt [-uUSER] [-t][-m SRC:DEST] files...
  -u[USER]     --user[=USER]    User for translation service. If not given, use MT_USER environment variable
  -t           --translate      Translate file
  -m SRC:DEST  --mark=SRC:DEST  Mark file segments.
````
El flux normal de treball seria:

1. **Marquem** el fitxer a traduir amb l'opció `-m` especificant la llengua d'origen (_l1_) i destinació (_l2_) com  _l1_`:`_l2_
on _l1_ i _l2_ són els [codis iso](https://en.wikipedia.org/wiki/List_of_ISO_639-1_codes) de les llengües en el format de 2 lletres.
Com a exemple, si volem traduïr de català a anglès haurem de posar `-m ca:en ` i seguidament el nom del fitxer.

2. Això genera un nou fitxer amb extensió `.marked.tex`. Allí podreu trobar els segements a traduir marcats com `<<`_l1_`:`_l2_`>>`.
(O sigui \<\<ca:en\>\> en el cas de l'exemple). D'aquests senyals ens direm **marques**.  
 Aquí tenim l'oportunitat de corregir, suprimir o afegir alguna d'aquestes marques.
 Només es traduirà el segment si té aquesta marca al començament.
 Això pot ser útil, per exemple, per a deixar sense traduir cites literals.

3. **Traduïm** el fitxer marcat amb l'opció `-t` seguida del nom del fixer **marcat** (no l'original).

4. Això ens dóna un fitxer amb extensió `.trans.tex` amb els segments traduits.
Val a dir que el servei de traducció imposa un limit diari de mots als usuaris del servei.
Aquesta és la raó principal per afegir un pas de **marcat** abans de la traducció:
Amb aquest esquema de dos passos, si la traducció no acaba perquè arribem al límit,
podem rependre la traducció l'endemà repetint la mateixa comanda.
Això és així perquè el procés de traducció elimina les marques dels segments traduits, però deixa inalterades la resta de marques.
El procés de traducció acaba quan totes les marques han estat eliminades.

5. Ara toca revisar tot el text ja que la traducció automàtica insereix de tant en tant fragments certament xocants.

### Com especifiquem un usuari

De moment, aquest límit és més alt si amb la comanda passes la teva adreça d'e-mail.
Per tal de donar aquesta adreça podem:

* Passar l'opció `-u `_\<adreça de correu\>_ en demanar la traducció (opció `-t`)
  exemple

```
    hatexmt -u nom@adrec-mail.com -t fitxer-marcat.tex
```


* O bé assignar, al començament de la sessió, l'adreça d'email a la variable d'entorn `MT_USER` com ara:

```
    export MT_USER=nom@adrec-mail.com
````

### Exemple d'us

Al directori **test/samples** hi ha un fitxer en català que volem traduir a l'anglès. Fem:

1. Marcatge:
```
    hatexmt-exe -m ca:en test/samples/cat1.tex
```

2. Traducció
```
    hatexmt-exe -t test/samples/cat1.marked.tex
```

3. El text traduït es trobarà a **test/samples/cat1.marked.trans.tex**

### El fixer de configuració
El fitxer **config** ha d'estar present al directori des d'on engeguem la traducció.
Aquest és un fixter en format [yaml](https://yaml.org/)  que especifica quines _comandes_ i _entorns_ LaTeX han de ser marcats per a traducció.
Consta de dues seccions, encapçalades per `commands:` i `environments:` on es donen els noms de les comandes i entorns respectivament
que cal considerar.
Cal tenir en compte que la interpretació de les dues seccions és oposada. És a dir:

* Allò que és llistat sota `commands:` **és** marcat per a traducció

* Allò que és llistat sota `environments:` **no és** marcat per a traducció








