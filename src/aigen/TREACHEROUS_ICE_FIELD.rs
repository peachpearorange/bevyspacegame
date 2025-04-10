```rust
use crate::game::loot::LootDrop; // Assuming LootDrop location
use crate::game::objects::{ObjectKind, ProceduralParams, RegionShape, Zone}; // Assuming Zone/ObjectKind/etc location
use crate::game::world_gen::ProceduralObjectType; // Assuming ProceduralObjectType location
use crate::types::{EnemyType, MySprite, ResourceType}; // Assuming enum locations

// Defines the potential resources found in procedurally generated harvestables
pub static ICE_FIELD_RESOURCES: &[LootDrop] = &[
    // Assuming LootDrop can represent the resource type and yield range
    LootDrop::Resource(ResourceType::RareIsotopes, 5..15),
];

pub const TREACHEROUS_ICE_FIELD: Zone = Zone {
    name: "Treacherous Ice Field",
    manual_objects: &[
        // Static Listening Post
        static_obj([1500.0, -500.0, 200.0], MySprite::ListeningPost, 50.0),
    ],
    procedural_params: Some(ProceduralParams {
        region: RegionShape::Ring {
            inner_radius: 800.0,
            outer_radius: 2500.0,
            height: 300.0,
        },
        density: 0.000005, // Density of objects per cubic unit
        scale: (15.0, 45.0), // Represents radius range for generated objects
        seed: 8472,
        // Assuming the procedural generator uses this to determine the resource
        // type and yield for the Harvestable objects it creates.
        // The visual representation (ice asteroid) might be inferred or set elsewhere.
        possible_loot: ICE_FIELD_RESOURCES,
        // Explicitly stating object type if ProceduralParams supports it (Example field)
        // object_type_filter: Some(ProceduralObjectType::Harvestable),
        // object_template: ObjectKind::Harvestable { ... } // Another potential mechanism
    }),
};
```
