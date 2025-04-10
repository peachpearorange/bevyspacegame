```rust
use crate::{Item, MySprite, Name, Vec3};
use bevy::prelude::Component;
use rust_utils::{vec, HashMap}; // Assuming HashMap is used internally or needed

// --- Synthesized Types based on Prompt Requirements & Context Style ---

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Item {
    SPACECAT,
    Person,
    Spice,
    COFFEE,
    SpaceCOIN,
    Crystal,
    DiHydrogenMonoxide,
    Rock,
    SpaceMinerals,
    RareIsotopes, // Added based on description
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ObjectKind {
    Static {
        sprite: MySprite,
    },
    Harvestable {
        sprite: MySprite,
        item: Item,
        hp: u32,
    },
    // Add other kinds as needed based on context/future descriptions
}

// Helper const fns for ObjectKind creation
pub const fn static_obj(sprite: MySprite) -> ObjectKind {
    ObjectKind::Static { sprite }
}

pub const fn harvestable(sprite: MySprite, item: Item, hp: u32) -> ObjectKind {
    ObjectKind::Harvestable { sprite, item, hp }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Region {
    Sphere { radius: f32 },
    Ring { inner_radius: f32, outer_radius: f32 },
    Box { dimensions: [f32; 3] },
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct ProceduralParams {
    pub proc_object_type: ObjectKind,
    pub region: Region,
    pub density: f32,       // Objects per unit volume/area
    pub scale: (f32, f32), // Min/Max scale multiplier
    pub seed: u64,
    pub possible_loot: &'static [(Item, f32)], // Item and drop chance
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ZoneVisualEffect {
    CrackingIce,
    ElectricalStorm,
    NebulaGas,
    // Add others as needed
}

#[derive(Component, Clone, Debug)]
pub struct Zone {
    pub name: &'static str,
    pub manual_objects: &'static [([f32; 3], ObjectKind)],
    pub procedural_params: Option<ProceduralParams>,
    pub visual_effect: Option<ZoneVisualEffect>,
    // Add other fields like faction_control, etc. if needed from context/future prompts
}

// --- Static Definitions ---

pub static RARE_ISOTOPE_LOOT: [(Item, f32); 1] = [(Item::RareIsotopes, 0.8)]; // 80% chance

// --- Main Zone Definition ---

pub const TREACHEROUS_ICE_FIELD: Zone = Zone {
    name: "Treacherous Ice Field",
    manual_objects: &[([0.0, 0.0, 0.0], static_obj(MySprite::IMAGEN3SPACESTATION))], // Listening Post
    procedural_params: Some(ProceduralParams {
        proc_object_type: harvestable(MySprite::ICEASTEROID, Item::RareIsotopes, 100),
        region: Region::Ring { inner_radius: 100.0, outer_radius: 150.0 },
        density: 0.05,
        scale: (0.8, 2.3),
        seed: 12345,
        possible_loot: &RARE_ISOTOPE_LOOT,
    }),
    visual_effect: Some(ZoneVisualEffect::CrackingIce),
};
```
