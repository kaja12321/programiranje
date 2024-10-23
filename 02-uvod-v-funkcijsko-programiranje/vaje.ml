(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.]
let j = [0.; 1.; 0.]
let k = [0.; 0.; 1.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)


let razteg k = List.map(( *. ) k)
let razteg2 k v = List.map(fun x -> x *. k) v

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let sestej x y = List.map2( +. ) x y

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let vsota = List.fold_left ( +. ) 0.

let skalarni_produkt x y = vsota @@ List.map2 ( *. ) x y

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma x = sqrt (skalarni_produkt x x)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)


let projeciraj a b = razteg (skalarni_produkt a b) (razteg (1.0 /. norma b) b)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina = "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">" 

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let rec ul = ()

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)
let razdeli_vrstico vrstica =
    let indeks = String.index vrstica ',' in
    let prvi = String.sub vrstica 0 indeks
    and drugi = String.sub vrstica (indeks + 1) (String.length vrstica - indeks - 1) in
    (String.trim prvi, String.trim drugi)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)
let pretvori_v_seznam_parov niz =
    niz
    |> String.split_on_char '\n'
    |> List.map razdeli_vrstico
(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f = List.map(fun (x, y) -> (x, f y))

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let rec izracunaj_skupni_znesek = ()
