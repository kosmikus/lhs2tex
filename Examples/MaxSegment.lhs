% lhs2TeX -verb MaxSegment.lhs > MaxSegment.tex
% lhs2TeX -math -align 33 MaxSegment.lhs > MaxSegment.tex

\documentclass{article}
\usepackage[german]{babel}

%-------------------------------=  --------------------------------------------

%include ../lhs2TeX.sty
%include ../lhs2TeX.fmt

%-------------------------------=  --------------------------------------------

%if style == math
%format alpha			=  "\alpha "
%format a1
%format an 	   		= a "_n"
%elif style == verb
%format a1       		= "a$_1$"
%format an       		= "a$_n$"
%endif

\begin{document}

So ist |\x -> x + 1| die Nachfolgerfunktion.

Diese Aufgabe ist dem Artikel `Proofs as Programs' von Bates und
Constable entnommen.  Gegeben ist eine Folge von ganzen Zahlen |[a1,
..., an]|. Finde die zusammenh"angende Teilfolge, deren Summe maximal
ist unter allen zusammenh"angenden Teilfolgen. F"ur die Folge

> seg				:: [Integer]
> seg				=  [-3, 2, -5, 3, -1, 2]

ist die Teilfolge |[3, -1, 2]| mit der Summe |4| maximal.
	Die Funktion |segments x| berechnet  alle zusammenh"angenden
Segmente der Liste |x|.

> type Segment alpha		=  [alpha]

> segments			:: [a] -> [Segment a]
> segments x			=  s ++ t
>     where (s, t)		=  segments' x

Ist das Ergebnis der Hilfsfunktion |segments x| das Tupel |(s, t)|, so
enth"alt |s| alle echten Pr"afixe von |x| und |t| alle "ubrigen
Segmente von |x|.

> segments'			:: [a] -> ([Segment a], [Segment a])
> segments' []			=  ([], [])
> segments' (a : x)		=  ([a] : [ a : y | y <- s ], s ++ t)
>     where (s, t)		=  segments' x

Die Funktion |maxSegment x| berechnet auf effiziente Weise die L"osung
f"ur das oben genannte Problem, d.h. sie realisiert die folgende
Spezifikation.

< maxSegment			:: (Num a, Ord a) => [a] -> a
< maxSegment x			=  maximum [ sum s | s <- segments x ]

Die Vorgehensweise entspricht im wesentlichen dem oben geschilderten
Verfahren. Beachte, da"s |maxSegment []| undefiniert ist.

> maxSegment			:: (Num a, Ord a) => [a] -> a
> maxSegment x			=  snd (maxSegment' x)
>
> maxSegment'			:: (Num a, Ord a) => [a] -> (a, a)
> maxSegment' [a]		=  (a, a)
> maxSegment' (a : x)		=  (n, m `max` n)
>     where (l, m)		=  maxSegment' x
>           n			=  a `max` (a + l)

Zur "Ubung: erweitere |maxSegment|, so da"s nicht nur die Summe,
sondern zus"atzlich, das dazugeh"orige Segment zur"uckgegeben wird.

< min a b | a <= b	       	=  a		-- vordefiniert
<         | otherwise		=  b

\end{document}
