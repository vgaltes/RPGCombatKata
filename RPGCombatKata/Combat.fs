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

let sameCharacterNotAllowed characterFrom playerTo distance amount =
    match playerTo with
    | Thing _ -> Some(amount)
    | Character c ->
        if characterFrom = c then None
        else Some(amount)


let sameCharacterAllowed characterFrom playerTo distance amount =
    if characterFrom = playerTo then Some(amount)
    else None


let distance characterFrom (playerTo: Player) distance amount =
    let effectiveRange = 
        match characterFrom.Type with
        | Melee -> 2
        | Range -> 20
        
    if distance > effectiveRange then None
    else Some(amount)


let level characterFrom playerTo distance amount =
    match playerTo with
    | Thing _ -> Some(amount)
    | Character c ->
        match characterFrom.Level - c.Level with
        | d when d <= -5 -> Some( amount / 2 )
        | d when d >= 5 -> Some(amount + amount / 2)
        | _ -> Some(amount)


let noLessThanZero characterFrom playerTo distance amount =
    let health =
        match playerTo with
        | Character c -> c.Health
        | Thing t -> t.Health

    if amount > health then Some (health)
    else Some (amount)


let noMoreThanOneThousand characterFrom playerTo distance amount =
    match playerTo with
    | Thing t -> None
    | Character c ->
        if amount < 1000 - c.Health then Some (amount)
        else Some (1000 - c.Health)

let sameFactionNotAllowed characterFrom playerTo distance amount =
    match playerTo with
    | Thing _ -> Some(amount)
    | Character c ->
        let setFrom = set characterFrom.Factions
        let setTo = set c.Factions

        if setFrom |> Set.isEmpty && setTo |> Set.isEmpty then Some(amount)
        elif Set.intersect setFrom setTo |> Set.isEmpty then Some(amount)
        else None

let sameFactionOrSameCharacterAllowed characterFrom playerTo distance amount =
    match playerTo with
    | Thing _ -> None
    | Character c ->
        if characterFrom = c then Some(amount) //There's another way to do this?
        else
            let setFrom = set characterFrom.Factions
            let setTo = set c.Factions

            if not ( Set.intersect setFrom setTo |> Set.isEmpty ) then Some(amount)
            else None

let noHealIfDead characterFrom playerTo distance amount =
    match playerTo with
    | Thing _ -> None
    | Character c ->
        if c.Health = 0 then None
        else Some(amount)

let attackRules = [
    sameCharacterNotAllowed
    sameFactionNotAllowed
    level
    distance
    noLessThanZero
]

let healRules = [
    sameFactionOrSameCharacterAllowed
    noMoreThanOneThousand
    noHealIfDead
]

let applyDamage damage player =
    match player with
    | Thing t -> Player.Thing {t with Health = t.Health - damage}
    | Character c -> Player.Character {c with Health = c.Health - damage}


let applyHeal healing player =
    match player with
    | Thing t -> Player.Thing {t with Health = t.Health + healing}
    | Character c -> Player.Character {c with Health = c.Health + healing}


let rec applyRules value rules characterFrom playerTo distance =
    match rules with
    | [] -> value
    | x::xs ->
        let result = x characterFrom playerTo distance value
        match result with
        | None -> 0
        | Some v -> applyRules v xs characterFrom playerTo distance

let interactWith characterFrom playerTo action distance =
    match action with
    | Attack amount ->
        let damage = applyRules amount attackRules characterFrom playerTo distance
        playerTo |> applyDamage damage 
    | Heal amount ->
        let healing = applyRules amount healRules characterFrom playerTo distance
        playerTo |> applyHeal healing