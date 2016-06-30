module CombatTests

open Xunit
open Combat
open Swensen.Unquote

let characterFrom = {Name = "MySelf"; Health = 800; Level = 1; Type = Melee; Factions = []}

[<Fact>]
let ``When the damage received is higher than the actual health, health drops to 0 and the character dies`` () =
    let character = {Name = "Another"; Health = 10; Level = 1; Type = Melee; Factions = []}

    let characterAfterAttack = interactWith characterFrom (Player.Character character) (Attack 20) 2

    test <@ characterAfterAttack |> Player.GetHealth = 0 @>

[<Fact>]
let ``When the character is dead he cannot be healed`` () =
    let character = {Name = "Another";Health = 0; Level = 1; Type = Melee; Factions = []}

    let characterAfterHealing = interactWith character (Player.Character character) (Heal 20) 2

    test <@ characterAfterHealing |> Player.GetHealth = 0 @>


[<Fact>]
let ``The character cannot be healed over 1000`` () =
    let character = {Name = "Another";Health = 990; Level = 1; Type = Melee; Factions = []}

    let characterAfterHealing = interactWith character (Player.Character character) (Heal 20) 2

    test <@ characterAfterHealing |> Player.GetHealth = 1000 @>


[<Fact>]
let ``The character cannot deal damage to himself`` () =
    let cFrom = Player.Character characterFrom
    let characterAfterAttack = interactWith characterFrom cFrom (Attack 20) 2

    test <@ cFrom = characterAfterAttack @>

[<Fact>]
let ``Two character with the same "game" properties are two different characters`` () =
    let anotherCharacter = {characterFrom with Name = "Another"}

    test <@ anotherCharacter <> characterFrom @>

[<Fact>]
let ``The character can heal himself`` () =
    let characterAfterHealing = interactWith characterFrom (Player.Character characterFrom) (Heal 20) 2

    test <@ characterAfterHealing |> Player.GetHealth  = 820 @>

[<Fact>]
let ``The character cannot heal his enemies`` () =
    let anotherCharacter = Player.Character {Name="Another"; Health = 780; Level = 1; Type = Melee; Factions = []}

    let characterAfterHealing = interactWith characterFrom anotherCharacter (Heal 20) 2

    test <@ characterAfterHealing = anotherCharacter @>

[<Fact>]
let ``If the target is less than 5 levels above the player, the full damage will be applied`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 780 @>

[<Fact>]
let ``If the target is 5 levels above the player, the damage applied will be reduced by 50%`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 6; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 790 @>

[<Fact>]
let ``If the target is more than 5 levels above the player, the damage applied will be reduced by 50%`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 7; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 790 @>

[<Fact>]
let ``If the target is 5 levels below the player, the damage applied will be boosted by 50%`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 6; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 770 @>


[<Fact>]
let ``If the target is more than 5 levels below the player, the damage applied will be boosted by 50%`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 7; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 770 @>        

[<Fact>]
let ``If the fighter is of Melee type and is closer or equal than 2 meters his attack has effect`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 780 @>


[<Fact>]
let ``If the fighter is of Melee type and is further than 2 meters his attack has no effect`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 3

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 800 @>

[<Fact>]
let ``If the fighter is of Range type and is closer or equal than 20 meters his attack has effect`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Range; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 20

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 780 @>


[<Fact>]
let ``If the fighter is of Range type and is further than 20 meters his attack has no effect`` () =
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Range; Factions = []}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 21

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 800 @>
    

[<Fact>]
let ``A player cannot damage another player of the same faction`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}
    let targetCharacter = Player.Character {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}

    let targetCharacterAfterAttack = interactWith sourceCharacter targetCharacter (Attack 20) 2

    test <@ targetCharacter = targetCharacterAfterAttack @>


[<Fact>]
let ``A player can damage another player of another faction`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction2"]}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter) (Attack 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 780 @>


[<Fact>]
let ``A player cannot heal another player of another faction`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}
    let targetCharacter = Player.Character {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction2"]}

    let targetCharacterAfterHeal = interactWith sourceCharacter targetCharacter (Heal 20) 2

    test <@ targetCharacter = targetCharacterAfterHeal @>


[<Fact>]
let ``A player can heal another player of the same faction`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}
    let targetCharacter = {Name = "Target"; Health = 800; Level = 1; Type = Melee; Factions = ["Faction1"]}

    let targetCharacterAfterAttack = interactWith sourceCharacter (Player.Character targetCharacter)  (Heal 20) 2

    test <@ targetCharacterAfterAttack |> Player.GetHealth = 820 @>


[<Fact>]
let ``A player can attack a thing`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let targetThing = {Name = "Thing"; Health = 2000}

    let targetThingAfterAttack = interactWith sourceCharacter (Player.Thing targetThing) (Attack 20) 2

    test <@ targetThingAfterAttack |> Player.GetHealth = 1980 @>


[<Fact>]
let ``Distance is considered when attacking a thing`` () =
    let sourceCharacter = {Name = "Source"; Health = 800; Level = 1; Type = Melee; Factions = []}
    let targetThing = {Name = "Thing"; Health = 2000}

    let targetThingAfterAttack = interactWith sourceCharacter (Player.Thing targetThing) (Attack 20) 5

    test <@ targetThingAfterAttack |> Player.GetHealth = 2000 @>
