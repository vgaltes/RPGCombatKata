module Combat

type Action =
    | Attack of int
    | Heal of int

type FighterType =
    | Melee
    | Range

type Character = {Name:string; Health : int; Level: int; Type: FighterType; Factions: string list}

type Thing = {Name: string; Health: int}

type Player =
    | Character of Character
    | Thing of Thing
    static member GetHealth x = 
        match x with
        | Character c -> c.Health
        | Thing t -> t.Health

[<Literal>]
let HealthLimit = 1000

let sameCharacterNotAllowed characterFrom characterTo distance amount =
    if characterFrom = characterTo then None
    else Some(amount)


let sameCharacterAllowed characterFrom characterTo distance amount =
    if characterFrom = characterTo then Some(amount)
    else None


let distance characterFrom distance amount =
    let effectiveRange = 
        match characterFrom.Type with
        | Melee -> 2
        | Range -> 20
        
    if distance > effectiveRange then None
    else Some(amount)


let level characterFrom characterTo distance amount =
    match characterFrom.Level - characterTo.Level with
    | d when d <= -5 -> Some( amount / 2 )
    | d when d >= 5 -> Some(amount + amount / 2)
    | _ -> Some(amount)


let noLessThanZero health amount =
    if amount > health then Some (health)
    else Some (amount)


let noMoreThanOneThousand health amount =
    if amount < 1000 - health then Some (amount)
    else Some (1000 - health)

let sameFactionNotAllowed characterFrom characterTo distance amount =
    let setFrom = set characterFrom.Factions
    let setTo = set characterTo.Factions

    if setFrom |> Set.isEmpty && setTo |> Set.isEmpty then Some(amount)
    elif Set.intersect setFrom setTo |> Set.isEmpty then Some(amount)
    else None

let sameFactionOrSameCharacterAllowed characterFrom characterTo distance amount =
    if characterFrom = characterTo then Some(amount) //There's another way to do this?
    else
        let setFrom = set characterFrom.Factions
        let setTo = set characterTo.Factions

        if not ( Set.intersect setFrom setTo |> Set.isEmpty ) then Some(amount)
        else None

let noHealIfDead health amount =
    if health = 0 then None
    else Some(amount)

let attackRules = [
    sameCharacterNotAllowed
    sameFactionNotAllowed
    level
]

let environmentalAttackRules = [
    distance
]


let attackLimitRules =[
    noLessThanZero
]

let healRules = [
    sameFactionOrSameCharacterAllowed
]

let healLimitRules = [
    noMoreThanOneThousand
    noHealIfDead
]

let rec calculateHealthDifference value rules characterFrom characterTo distance =
    match rules with
    | [] -> value
    | x::xs ->
        let result = x characterFrom characterTo distance value
        match result with
        | None -> 0
        | Some v -> calculateHealthDifference v xs characterFrom characterTo distance


let rec applyLimitRules value rules health =
    match rules with
    | [] -> value
    | x::xs ->
        let result = x health value
        match result with
        | None -> 0
        | Some v -> applyLimitRules v xs health

let rec applyEnvironmentalRules value rules characterFrom distance =
    match rules with
    | [] -> value
    | x::xs ->
        let result = x characterFrom distance value
        match result with
        | None -> 0
        | Some v -> applyEnvironmentalRules v xs characterFrom distance

let interactWith characterFrom playerTo action distance =
    let calculateDamage characterFrom playerTo action distance =
        match action with
        | Attack amount -> 
            match playerTo with
            | Character c ->
                let damage = calculateHealthDifference amount attackRules characterFrom c distance
                let damage' = applyEnvironmentalRules damage environmentalAttackRules characterFrom distance
                applyLimitRules damage' attackLimitRules c.Health
            | Thing t ->
                let damage = applyEnvironmentalRules amount environmentalAttackRules characterFrom distance
                applyLimitRules damage attackLimitRules t.Health

        | Heal amount ->
            match playerTo with
            | Character c -> 
                let healing = calculateHealthDifference amount healRules characterFrom c distance
                applyLimitRules healing healLimitRules c.Health
            | Thing t -> 0

    let damage = calculateDamage characterFrom playerTo action distance

    match action with 
    | Attack _ ->
        match playerTo with
        | Character c -> Player.Character {c with Health = c.Health - damage}
        | Thing t -> Player.Thing {t with Health = t.Health - damage}
    | Heal _ ->
        match playerTo with
        | Character c -> Player.Character {c with Health = c.Health + damage}
        | Thing t -> Player.Thing t