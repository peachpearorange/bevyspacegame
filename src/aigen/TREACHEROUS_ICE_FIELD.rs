use crate::{Faction, SpawnableTemplate, Visuals, Zone};

pub const TREACHEROUS_ICE_FIELD: Zone = Zone {
    name: "Treacherous Ice Field",
    manual_objects: &[
        // 3 Static Listening Posts (using SpaceStation as placeholder)
        ([0.0, 0.0, 80.0], SpawnableTemplate::SpaceStation),
        ([-70.0, 0.0, -40.0], SpawnableTemplate::SpaceStation),
        ([70.0, 0.0, -40.0], SpawnableTemplate::SpaceStation),

        // ~30 Ice Asteroids (manually placed in a rough disk r=100)
        ([10.0, 5.0, 20.0], SpawnableTemplate::IceAsteroid),
        ([-30.0, -2.0, 50.0], SpawnableTemplate::IceAsteroid),
        ([45.0, 10.0, -15.0], SpawnableTemplate::IceAsteroid),
        ([-60.0, -8.0, -30.0], SpawnableTemplate::IceAsteroid),
        ([0.0, 15.0, -70.0], SpawnableTemplate::IceAsteroid),
        ([80.0, 0.0, 10.0], SpawnableTemplate::IceAsteroid),
        ([-10.0, -5.0, 90.0], SpawnableTemplate::IceAsteroid),
        ([25.0, 8.0, 65.0], SpawnableTemplate::IceAsteroid),
        ([-55.0, -12.0, 10.0], SpawnableTemplate::IceAsteroid),
        ([70.0, 3.0, -60.0], SpawnableTemplate::IceAsteroid),
        ([-85.0, 6.0, 35.0], SpawnableTemplate::IceAsteroid),
        ([15.0, -9.0, -85.0], SpawnableTemplate::IceAsteroid),
        ([95.0, -4.0, -5.0], SpawnableTemplate::IceAsteroid),
        ([-20.0, 12.0, 80.0], SpawnableTemplate::IceAsteroid),
        ([50.0, -6.0, 40.0], SpawnableTemplate::IceAsteroid),
        ([-75.0, 2.0, -70.0], SpawnableTemplate::IceAsteroid),
        ([35.0, -15.0, -25.0], SpawnableTemplate::IceAsteroid),
        ([-40.0, 7.0, 95.0], SpawnableTemplate::IceAsteroid),
        ([60.0, -1.0, -90.0], SpawnableTemplate::IceAsteroid),
        ([90.0, 11.0, 30.0], SpawnableTemplate::IceAsteroid),
        ([-5.0, -18.0, 5.0], SpawnableTemplate::IceAsteroid),
        ([40.0, 4.0, -45.0], SpawnableTemplate::IceAsteroid),
        ([-65.0, -10.0, 60.0], SpawnableTemplate::IceAsteroid),
        ([10.0, 9.0, -55.0], SpawnableTemplate::IceAsteroid),
        ([85.0, -7.0, -40.0], SpawnableTemplate::IceAsteroid),
        ([-90.0, 1.0, 70.0], SpawnableTemplate::IceAsteroid),
        ([20.0, -11.0, 15.0], SpawnableTemplate::IceAsteroid),
        ([-50.0, 14.0, -80.0], SpawnableTemplate::IceAsteroid),
        ([75.0, -3.0, 55.0], SpawnableTemplate::IceAsteroid),
        ([-25.0, 0.0, -95.0], SpawnableTemplate::IceAsteroid),
    ],
    faction_control: Faction::Wanderers, // Assuming neutral control
};
