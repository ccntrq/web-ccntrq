---
title: Herausforderung 190 - „Regex Weekly Challenge” und „Zerstückeln und dekodieren"
body-title: Herausforderung 190 - <em>Regex Weekly Challenge</em> und <em>Zerstückeln und dekodieren</em>
lang: de
---

[English version](/pages/perl-weekly-challenge/challenge-190.html)

## Aufgabe #1 -  _Regex Weekly Challenge_ (Großbuchstabenermittlung)

[Originale Beschreibung (englisch)](https://theweeklychallenge.org/blog/perl-weekly-challenge-190/#TASK1)

**Eingereicht von:** [Mohammad S Anwar](http://www.manwar.org)

Du erhältst eine Zeichenkette bestehend ausschließlich aus den alphabetischen
Zeichen: `A..Z` und `a..z`.

Schreibe ein Skript, das herausfindet, ob Großbuchstaben angemessen verwendet
werden, indem es prüft, ob mindestens eine der folgenden Regeln erfüllt ist:

1. Nur der erste Buchstabe ist ein Großbuchstabe, alle anderen sind Kleinbuchstaben.
2. Jeder Buchstabe ist ein Kleinbuchstabe.
3. Jeder Buchstabe ist ein Großbuchstabe.

##### Beispiel 1

```
Eingabe: $s = 'Perl'
Ausgabe: 1
```

##### Beispiel 2

```
Eingabe: $s = 'TPF'
Ausgabe: 1
```

##### Beispiel 3

```
Eingabe: $s = 'PyThon'
Ausgabe: 0
```

##### Beispiel 4

```
Eingabe: $s = 'raku'
Output: 1
```

### Lösung

Willkommen zur dieswöchigen Ausgabe der _Regex Weekly Challenge_ wo alles darum
geht, textuelle Anforderungen in reguläre Ausdrücke umzuwandeln:

```perl
sub capital_detection ($s) {
    return $s =~ s/[[:^alpha:]]//gr =~ m/^(?:
      [[:upper:]][[:lower:]]+ | # 1) Only first letter is capital and all others are small.
      [[:lower:]]+ |            # 2) Every letter is small.
      [[:upper:]]+ |            # 3) Every letter is capital.
      )$/x
      ? 1    #
      : 0;
}
```

Der erste Regex `s/[[:^alpha:]]//gr` entfernt alle nicht alphabetischen Zeichen aus dem Eingabestring. Diese sind in der Challenge nicht relevant. Die zweite Regex prüft ob eine der drei Bedingungen aus der Beschreibung auf die verbleibenden Buchstaben zutrifft.

1. Nur der erste Buchstabe ist ein Großbuchstabe, alle anderen sind Kleinbuchstaben.
2. Jeder Buchstabe ist ein Kleinbuchstabe.
3. Jeder Buchstabe ist ein Großbuchstabe.

[Vollständiger Quellcode](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-190/alexander-pankoff/perl/ch-1.pl)

<br/>

---

<br/>

## Aufgabe #2 - _Zerstückeln und dekodieren_ (Dekodierte Liste)

[Originale Beschreibung (englisch)](https://theweeklychallenge.org/blog/perl-weekly-challenge-190/#TASK1)

**Eingereicht von:** [Mohammad S Anwar](http://www.manwar.org)

Du erhältst eine kodierte Zeichenkette, die aus einer Folge von numerischen
Zeichen besteht: 0..9, `$s`.

Schreibe ein Skript, das alle gültigen verschiedenen Dekodierungen in sortierter
Reihenfolge findet.

> Die Kodierung erfolgt durch einfache Abbildung von A,B,C,D,... auf 1,2,3,4,... usw.

##### Beispiel 1

```
Eingabe: $s = 11
Ausgabe: AA, K

11 kann als (1 1) oder (11) dekodiert werden, d.h. als AA oder K
```

##### Beispiel 2

```
Eingabe: $s = 1115
Ausgabe: AAAE, AAO, AKE, KAE, KO

Mögliche dekodierte Daten sind:
(1 1 1 5) => (AAAE)
(1 1 15) => (AAO)
(1 11 5) => (AKE)
(11 1 5) => (KAE)
(11 15) => (KO)
```

##### Beispiel 3

```
Eingabe: $s = 127
Ausgabe: ABG, LG

Mögliche dekodierte Daten sind:
(1 2 7) => (ABG)
(12 7) => (LG)
```

### Lösung

Diese Challenge habe ich in zwei Schritten gelöst:

1. Ermitteln aller möglichen Stückelungen des Eingabestrings mit substrings der
   längen 1 oder 2, wobei substrings mit einer länge von 2 die zusätzliche
   Beschränkung haben, dass sie kleiner oder gleich 26 sein müssen.
2. Umwandeln der in Schritt 1 ermittelten Stückelungen in die möglichen String Decodierungen.

Der erste Schritt wird von der rekursiven Routine `possible_decodings` erledigt:

```perl
sub possible_decodings ( $str, $cur = 0, $acc = [] ) {
    return ($acc) if $cur >= length $str;
    my @decodings =
      possible_decodings( $str, $cur + 1, [ @$acc, substr( $str, $cur, 1 ) ] ); # 1)

    my $next_two = substr( $str, $cur, 2 ); # 2)

    if ( length $next_two == 2 && $next_two <= 26 ) { # 3)
        push @decodings,
          possible_decodings( $str, $cur + 2, [ @$acc, $next_two ] );
    }

    return @decodings;
}
```

Der aktuelle Buchstabe der verarbeitet wird ist immer Teil einer möglichen
Dekodierung. In 1) wird dieser ermittelt und an den rekursiven Aufruf für die
nächste Stelle (`$cur + 1`) übergeben. Dadurch werden alle weiteren
Dekodierungen ermittelt, die mit diesem Buchstaben beginnen. In 2) und 3) holen
wir uns die 2 Buchstaben an der aktuellen und nächsten Stelle im String. Diese
sind nur dann Teil einer möglichen Dekodierung, wenn überhaupt noch ein nächster
Buchstabe existiert und sie zusammen numerisch kleiner oder gleich 26 sind. Wenn
ja, übergeben wir diese an den rekursiven Aufruf für die übernächste Stelle
(`$cur + 2`), um so alle möglichen Dekodierungen zu finden die mit diesen beiden
Buchstaben beginnen. Diese fügen wir ans Ende der Dekodierungen aus 1) an. Durch
das sorgfältige Auswählen der Rekursionsreihenfolge liefert diese Routine die
Dekodierungen genau in der von der Aufgabe geforderten Reihenfolge.

Der 2. Schritt besteht nun daraus, die in Schritt 1 ermittelten Dekodierungen in
Strings umzuwandeln. Dazu addieren wir den Versatz des Buchstaben 'A' in der
ASCII-Tabelle auf die ermittelten Werte und wandeln das Ergebnis via `chr` in
einen Buchstaben um.

```perl
sub decoded_list ($encoded) {
    my $offset = ord('A') - 1;
    return map {
        join( '', map { chr( $_ + $offset ) } @$_ )
    } possible_decodings($encoded);
}
```

[Vollständiger Quellcode](https://github.com/manwar/perlweeklychallenge-club/blob/master/challenge-190/alexander-pankoff/perl/ch-2.pl)
