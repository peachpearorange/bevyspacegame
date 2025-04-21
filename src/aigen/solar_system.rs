
// please write your code here:

// I'll create a streamlined solar system with the requested elements - just two planets, an asteroid field, and two NPCs.

use crate::*;
type Obj = Object;

// Define a dialogue tree for the asteroid field scout
const ICE_FIELD_SCOUT_DIALOGUE: DialogueTree = &[
    ("A", &[(
        "B",
        "Hello there. What are you doing out here?",
        "Scout: \"Monitoring this treacherous ice field. These asteroids aren't just frozen rocks - they're unpredictable.\"",
        None
    )]),
    ("B", &[
        (
            "C",
            "Any tips for navigating safely?",
            "Scout: \"Keep your distance from the larger ones, and watch for the bluish glow - those are unstable. Keep your comms open.\"",
            None
        ),
        (
            "D",
            "Find anything valuable out here?",
            "Scout: \"The ice contains pure water and some rare frozen gases. Valuable, but dangerous to extract without proper equipment.\"",
            None
        )
    ]),
    ("C", &[(
        "END",
        "Thanks for the advice. I'll be careful.",
        "Scout: \"Safe travels. If you get in trouble, emergency frequency is 121.3. We monitor it around the clock.\"",
        None
    )]),
    ("D", &[(
        "END",
        "Sounds like a challenging job.",
        "Scout: \"It is, but the payoff is worth it. Pure water fetches premium prices in the outer colonies.\"",
        None
    )]),
    DIALOGUE_END
];

// Define dialogue tree for the space station researcher
const RESEARCHER_DIALOGUE: DialogueTree = &[
    ("A", &[(
        "B",
        "What's your research about?",
        "Researcher: \"I'm studying the unique gravitational interactions between our two planets. The proximity creates fascinating phenomena.\"",
        None
    )]),
    ("B", &[
        (
            "C",
            "Has anyone tried living on both planets?",
            "Researcher: \"Yes! We have several exchange programs. Adapting from the ice world to the lava planet is quite challenging for the body.\"",
            None
        ),
        (
            "D",
            "Are there any dangers from this proximity?",
            "Researcher: \"Occasional gravitational anomalies can disrupt navigation. Always check your ship's gravity compensators before traveling between them.\"",
            None
        )
    ]),
    ("C", &[(
        "END",
        "The exchange programs sound interesting.",
        "Researcher: \"They've been crucial for developing technologies that work in both extreme environments. Visit the trading station if you're interested in seeing some.\"",
        None
    )]),
    ("D", &[(
        "END",
        "I'll be careful with my navigation systems.",
        "Researcher: \"Good idea. Safe travels! And if you're heading to the ice field, watch out for the unstable asteroids.\"",
        None
    )]),
    DIALOGUE_END
];

// Our mini solar system with just two planets and a couple of NPCs
pub const SOLAR_SYSTEM: &[([f32; 3], Zone)] = &[
    // The central zone with the sun and player start position
    ([0.0, 0.0, 0.0], CENTRAL_SYSTEM),

    // Ice planet and surroundings
    ([0.0, 0.0, 1500.0], ICE_PLANET_ZONE),

    // Lava planet and surroundings
    ([1200.0, -200.0, -800.0], LAVA_PLANET_ZONE),

    // Treacherous ice field between the planets
    ([600.0, -100.0, 300.0], ICE_FIELD_ZONE),
];

// The central area of our solar system
const CENTRAL_SYSTEM: Zone = {
    Zone {
        name: "Solar Core Region",
        objects: &[
            // The central star
            ([0.0, 0.0, 0.0], Obj::Sun),

            // Player starting position - a bit away from the sun
            ([0.0, 100.0, 300.0], Obj::Player),

            // Central space station
            ([0.0, 0.0, 500.0], Obj::SpaceStation),

            // A researcher who can explain the system
            ([5.0, 0.0, 505.0], Obj::TalkingPerson {
                name: "System Researcher",
                sprite: MySprite::GPT4O_SPACE_MAN,
                dialogue_tree: RESEARCHER_DIALOGUE
            }),

            // Some space coins as starter resources
            ([10.0, 100.0, 310.0], Obj::SpaceCoin),
            ([15.0, 105.0, 305.0], Obj::SpaceCoin),

            // HP box for emergencies
            ([0.0, 110.0, 320.0], Obj::HpBox),
        ],
        warp_gate_coords: [0.0, 50.0, 400.0],
        faction_control: Some(Faction::EXPLORERS)
    }
};

// Ice planet zone
const ICE_PLANET_ZONE: Zone = {
    Zone {
        name: "Frostpeak Planet",
        objects: &[
            // The ice planet itself
            ([0.0, 0.0, 0.0], Obj::Planet {
                sprite: MySprite::ICEPLANET,
                radius: 100.0,
                population: 2_500_000,
                name: "Frostpeak"
            }),

            // Orbital research station
            ([0.0, 120.0, 120.0], Obj::SpaceObject {
                name: "Frostpeak Research Station",
                scale: 4.0,
                can_move: false,
                visuals: Visuals::sprite(MySprite::SPACESTATION)
            }),

            // Ice mining vessel
            ([-60.0, 40.0, 140.0], Obj::SpaceObject {
                name: "Ice Mining Vessel",
                scale: 2.5,
                can_move: true,
                visuals: Visuals::sprite(MySprite::GPT4O_MINING_SHIP)
            }),

            // Supplies container
            ([20.0, 115.0, 130.0], Obj::TreasureContainer),

            // HP Box
            ([5.0, 125.0, 125.0], Obj::HpBox),
        ],
        warp_gate_coords: [0.0, 150.0, 150.0],
        faction_control: Some(Faction::EXPLORERS)
    }
};

// Lava planet zone
const LAVA_PLANET_ZONE: Zone = {
    Zone {
        name: "Magma Core",
        objects: &[
            // The lava planet itself
            ([0.0, 0.0, 0.0], Obj::Planet {
                sprite: MySprite::LAVAPLANET,
                radius: 80.0,
                population: 1_200_000,
                name: "Magma Core"
            }),

            // Orbital heat-resistant trading station
            ([0.0, 100.0, 100.0], Obj::SpaceObject {
                name: "Magma Core Trading Hub",
                scale: 4.0,
                can_move: false,
                visuals: Visuals::sprite(MySprite::GPT4O_TRADING_STATION)
            }),

            // Mining ship specialized for extreme heat
            ([80.0, 50.0, 120.0], Obj::SpaceObject {
                name: "Heat-Resistant Mining Vessel",
                scale: 2.5,
                can_move: true,
                visuals: Visuals::sprite(MySprite::GPT4O_MINING_SHIP)
            }),

            // Valuable goods container
            ([10.0, 95.0, 105.0], Obj::TreasureContainer),

            // HP Box
            ([-5.0, 105.0, 95.0], Obj::HpBox),
        ],
        warp_gate_coords: [0.0, 130.0, 130.0],
        faction_control: Some(Faction::TRADERS)
    }
};

// Ice field with asteroids
const ICE_FIELD_ZONE: Zone = {
    Zone {
        name: "Treacherous Ice Field",
        objects: &[
            // A listening post
            ([0.0, 15.0, 80.0], Obj::SpaceObject {
                name: "Listening Post Alpha",
                scale: 3.5,
                can_move: false,
                visuals: Visuals::sprite(MySprite::SPACESTATION)
            }),

            // A scout stationed at the listening post
            ([5.0, 15.0, 75.0], Obj::TalkingPerson {
                name: "Ice Field Scout",
                sprite: MySprite::GPT4O_SPACE_MAN,
                dialogue_tree: ICE_FIELD_SCOUT_DIALOGUE
            }),

            // HP box for emergencies
            ([10.0, 15.0, 70.0], Obj::HpBox),

            // Ice asteroids in a small field
            ([30.0, 5.0, 30.0], Obj::IceAsteroid),
            ([50.0, -5.0, 60.0], Obj::IceAsteroid),
            ([70.0, 10.0, 20.0], Obj::IceAsteroid),
            ([20.0, -10.0, 50.0], Obj::IceAsteroid),
            ([40.0, 0.0, 40.0], Obj::IceAsteroid),
            ([60.0, 15.0, 10.0], Obj::IceAsteroid),
            ([25.0, -5.0, 70.0], Obj::IceAsteroid),
            ([55.0, 5.0, 25.0], Obj::IceAsteroid),
            ([35.0, -15.0, 45.0], Obj::IceAsteroid),
            ([15.0, 20.0, 35.0], Obj::IceAsteroid),
        ],
        warp_gate_coords: [0.0, 10.0, 0.0],
        faction_control: Some(Faction::EXPLORERS)
    }
};

// Export the zones for the main game
pub const ZONES: &[([f32; 3], Zone)] = SOLAR_SYSTEM;
