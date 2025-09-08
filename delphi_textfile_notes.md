# Delphi/Pascal: Teksleêr- en Stringnutsgoed (Notas)


---

## Voordat ons begin

Moet nie **'n Real** gebruik nie, ooit. Gebruik net **Dubbel**. Dit kan jou help om baie *race conditions* te vermy.

As jy **programmeer** en met **text files** werk, doen **altyd**:

```pascal
Try
  /// al jou kode oor die file kom onder hierdie try
Finally
  CloseFile(..);
End;
```

---

## Algemene funksies

### `Inc(<variable>)`

```delphi
Inc(<variable>);

// kom ons sê ek het 'n var X en X se waarde is 0.
// As ek dan sê Inc(X) dan is dit so goed as wat ek sê X := X + 1;
// So, basies plus een by X se huidige waarde en sit die nuwe waarde in X.
// In ander tale sê ons nie Inc(X) nie; ons sê net X++
```

### `EOF(<FILE>)`

```delphi
EOF(<FILE>);

// EOF is 'n funksie; jy stuur vir hom 'n file en hy stuur 'n waar/false Boolean terug.
// Hierdie Boolean word bepaal deur of Delphi se cursor aan die einde van die file is of nie.
```

### `Pos('£', sEen)`

```delphi
Pos('£', sEen);

// Pos is 'n funksie: jy stuur vir hom dit waarvoor jy soek en die string waarin hy moet soek (geskei deur 'n ,).
// Dan stuur hy vir jou die index van die eerste element waarmee jy hom geroep het in die string waarmee jy hom geroep het.
// Eenvoudig: Jy stuur vir hom 'n string of char, dan die string waarin hy hierdie string of char moet soek.
// Kom ons sê ek het 'n string sEen = 'Hi£'
// Dan sê ek:
X := Pos('£', sEen);
// Nou sal X se waarde 3 wees, want die £ is in sEen by index 3 (£ is die 3de char).
```

---

## `Delete(X, Y, Z)`

```delphi
Delete(X, Y, Z);
// X is 'n string en Y en Z is ints.
// Wat doen wat?
// Y gee die index van die karakter waar hy moet begin delete.
// Z gee die hoeveelheid karakters wat hy moet delete.

// Voorbeeld 1
X := 'ABCDEFG';
Y := 1;
Z := 3;
Delete(X, Y, Z);
// Nou is X = 'DEFG' want die delete funksie het by die eerste index (Y) begin karakters
// delete totdat hy 'n totaal van 3 (Z) karakters gedelete het.

// Voorbeeld 2
X := 'ABCDEFG';
Y := 3;
Z := 3;
Delete(X, Y, Z);
// Nou is X = 'ABCG' want die delete funksie het by die 3de index (Y) begin karakters
// delete totdat hy 'n totaal van 3 (Z) karakters gedelete het.
```

---

## `Copy(X, Y, Z)`

```delphi
// Copy is 'n funksie om 'n sekere deel van 'n string te kry.
// Jy roep hom met X (die string waarvan jy 'n sekere deel soek),
// Y die index waar hy moet begin copy, Y is 'n int,
// Z die hoeveelheid char wat hy moet copy, Z is 'n int.

// Voorbeeld 1
X := 'Programming';
Y := 4;
Z := 3;

T := Copy(X, Y, Z);
// T se waarde is nou 'gra' want Copy het begin by die 4de char van X (char 4 is 'g')
// en toe copy hy daardie char en die volgende 2 (hy hou aan copy totdat hy 3 (Z)
// hoeveelheid char gekopieer het).
// T se waarde is nou 'gra' want Copy het begin copy by 'g' en toe hou hy aan totdat hy
// 'n totaal van Z hoeveelheid char gekopieer het.

// Voorbeeld 2
X := 'ABCDEFG';
Y := 2;
Z := 4;

T := Copy(X, Y, Z);
// T se waarde is nou 'BCDE' want Copy het begin by die 2de char van X (char 2 is 'B').
// Copy begin by die 2de char te copy want Y gee vir hom die index van die char waar hy
// moet begin copy; en toe copy hy daardie char en die volgende 3 (hy hou aan copy totdat
// hy 4 (Z) hoeveelheid char gekopieer het).
// T se waarde is nou 'BCDE' want Copy het begin copy by 'B'
// ('B' is die 2de char en ons het gesê hy moet begin copy by die char by index 2 (Y))
// en hy moet aanhou copy totdat hy 4 (Z) hoeveelheid char gekopieer het.
// X is die string waaruit hy moet copy.
```


