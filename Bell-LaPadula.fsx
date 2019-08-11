(* 
  Bell-LaPadula Confidentiality Model
  https://en.wikipedia.org/wiki/Bell%E2%80%93LaPadula_model

  Simple implementation in F#
  Valer Bocan, PhD - August 11th, 2019
*)

// Define confidentiality levels
type SecurityLevel = TopSecret = 2 | Secret = 1 | Public = 0

// Documents
let documents = [
  ("Document #1", SecurityLevel.TopSecret)
  ("Document #2", SecurityLevel.Secret)
  ("Document #3", SecurityLevel.Public)
]

// Subjects
let subjects = [
  ("Jack", SecurityLevel.Public)
  ("Jane", SecurityLevel.Secret)
  ("Maria", SecurityLevel.TopSecret)
]

// Create the cartesian product of documents and subjects
let crossproduct l1 l2 =
  [
    for x in l1 do
          for y in l2 do
            yield (x, y)
  ]

// Bell-LaPadula - Simple security property
// Subject at a given security level may not read an object at a higher security level  
let CheckReadRights subj doc =
  if snd(doc) > snd(subj)
    then sprintf "%A can't read from %A" subj doc
    else sprintf "%A can read from %A" subj doc

// Bell-LaPadula - Star property
// Subject at a given security level may not write to any object at a lower security level.
let CheckWriteRights subj doc =
  if snd(doc) < snd(subj)
    then sprintf "%A can't write to %A" subj doc
    else sprintf "%A can  write to %A" subj doc
    
let readrights = crossproduct subjects documents |> List.map (fun (x,y) -> CheckReadRights x y )

let writerights = crossproduct subjects documents |> List.map (fun (x,y) -> CheckWriteRights x y )

printfn "Displaying read rights:\n%A\n" readrights
printfn "Displaying write rights:\n%A" writerights
