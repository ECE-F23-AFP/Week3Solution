module Week3.Starter


// 3.5
type Solution = | TwoRoots of float*float
                | OneRoot of float
                | NoRoot

let solve (a,b,c) =
    let sqrtD =
        let d = b*b-4.0*a*c
        if d < 0.0 || a = 0.0
        then failwith ""
        else sqrt d
    try
        let result = ((-b + sqrtD) / (2.0*a), (-b-sqrtD) / (2.0*a))
        if (fst result = snd result)
        then OneRoot (fst result)
        else TwoRoots result
    with
        Failure _ -> NoRoot
        
        
// 3.1
let orderTime (t1: int*int*string, t2) =
    let (_, _, f1) = t1
    let (_, _, f2) = t2
    if (f1 = f2)
    then
        if t1 < t2 then t1 else t2
    else
        if f1 < f2 then t1 else t2

type Time = { f: string; hour: int; min: int; }

let orderTimeR (t1: Time, t2) =
    if (t1 < t2) then t1 else t2



// 4.20
let rec isMember x = function
    | y::ys -> x=y || isMember x ys
    | [] -> false
    
let colMap m =
    let rec areNb c1 c2 l =
        match l with
        | y::ys -> (c1, c2)=y || (c2,c1)=y || areNb c1 c2 ys
        | []    -> false
    let rec canBeExBy col c =
        match col with
        | []       -> true
        | c'::col' -> not(areNb c' c m) && canBeExBy col' c
    let rec extColoring cols c =
        match cols with
        | []         -> [[c]]
        | col::cols' -> if canBeExBy col c
                        then (c::col)::cols'
                        else col::extColoring cols' c
    let addElem x ys = if isMember x ys then ys else x::ys
    let rec countries = function
        | []          -> []
        | (c1, c2)::xs -> addElem c1 (addElem c2 (countries xs))
    let rec colCntrs = function
        | []    -> []
        | c::cs -> extColoring (colCntrs  cs) c

    colCntrs (countries m)


// 4.21
type Neighbours =
    | Island of string
    | Mainland of (string * string)

let rec areNb1 c1 c2 l =
    match l with
    | y::ys -> match y with
                | Island _   -> false
                | Mainland y -> (c1, c2)=y || (c2,c1)=y || areNb1 c1 c2 ys
    | []    -> false
let rec canBeExBy col c m =
    match col with
    | []       -> true
    | c'::col' -> not(areNb1 c' c m) && canBeExBy col' c m
let rec extColoring cols c m =
    match cols with
    | []         -> [[c]]
    | col::cols' -> if canBeExBy col c m
                    then (c::col)::cols'
                    else col::extColoring cols' c m
let addElem x ys = if isMember x ys then ys else x::ys
let rec countries = function
    | []              -> []
    | x::xs -> match x with
               | Island(c)        -> addElem c (countries xs)
               | Mainland(c1, c2) -> addElem c1 (addElem c2 (countries xs))
let rec colCntrs m = function
    | []    -> []
    | c::cs -> extColoring (colCntrs m cs) c m

let colMapWithIslands m =
    colCntrs m (countries m)
    
// 4.22
// 1)
let rec polyMul c = function
    | [] -> []
    | x::xs -> x*c :: (polyMul c xs)
    
// 2)
let polyMulX p = 0::p

// 3)


// 4)
let printPoly p =
    let rec printPolyRec c = function
        | []               -> ""
        | x::xs when c = 0 -> sprintf "%d + %s" x (printPolyRec (c+1) xs)
        | x::xs when x = 0 -> printPolyRec (c+1) xs
        | x::xs when x = 1 -> sprintf "x^%d + %s" c (printPolyRec (c+1) xs)
        | x::xs            -> sprintf "%dx^%d + %s" x c (printPolyRec (c+1) xs)
        
    printPolyRec 0 p


// 4.23
type Client = {name: string; telephone: string; sex: string; yob: int; interests: string list}

let rec request (c: Client) clients =
    let rec commonInterest ci1 = function
        | [] -> false
        | i::xs -> List.contains i ci1 || (commonInterest ci1 xs) 
    match clients with
    | [] -> []
    | oc::cs when oc.sex <> c.sex &&
                  (oc.yob < c.yob+10 && oc.yob > c.yob - 10)
                  && commonInterest c.interests oc.interests
                   -> oc :: (request c cs)
    | _::cs        -> request c cs

        
