
let
  makeRec = 
    \(name : Text) -> \(zipCode : Text) -> \(age : Natural) ->
    { name = name
    , zipCode = zipCode
    , age = age
    }
in
    [ makeRec "Alice" "000-0001" 20
    , makeRec "Bob" "000-0002" 30
    , makeRec "George" "000-0003" 40
    ]