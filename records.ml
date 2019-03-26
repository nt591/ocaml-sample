type ptype = TNormal | TFire | TWater

type mon = {name: string; hp: int; ptype: ptype}

let charmander = {name = "Charmander"; hp = 39; ptype = TFire}

let charizard = {charmander with hp=120; name="Charizard"}