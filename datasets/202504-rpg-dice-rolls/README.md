# RPG Dice Rolls

A friend of mine logged some dice roll outcomes from an RPG video game
Pathfinder: Wrath of the Righteous. I'm not too familiar with these games or
rulesets, but there are commonalities with the more common Dungeons & Dragons.

## Understanding the dataset

In this dataset, six entities are tracked, and dice rolls marked. This appears
to be a combat situation. The entities are as follows:

- Cleric
- Paladin
- Shaman
- Hunter
- Centipede
- Giant Fly

All characters except for "centipede" and "giant fly" are in the player's party,
meaning they are either controlled by the player or aligned with the player's
characters. "Centipede" and "giant fly" are therefore enemies in combat.

Each of the six entities are spread out over columns, and have their own
sub-columns for action types such as Attack, Damage, Skill, Initiative. Not all
entities have the same columns (for instance, "Hunter" entity lacks records for
Skill rolls), and not all columns use the same dice (in particular, the Damage
rolls use various die from 1d3, 1d4, 1d6, 1d8). As a matter of convention for
this particular sheet, any columns unlabeled with dice characteristics are
assumed to be 1d20.

Each row is an instance of a dice roll, so there is only one record per row.
There is an implicit sorting by time.
