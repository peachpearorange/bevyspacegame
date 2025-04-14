use crate::*;

type Obj = Object;

// Define dialogue tree for the ice field scout
const ICE_FIELD_SCOUT_DIALOGUE: DialogueTree = &[
    ("A", &[(
        "B",
        "Hello there, what are you doing out here?",
        "Scout: \"Keeping an eye on this treacherous ice field. Glad to see a friendly face out here in the cold void.\"",
        None
    )]),
    ("B", &[
        (
            "C",
            "What's so dangerous about this area?",
            "Scout: \"These ice asteroids aren't just frozen rocks - they're unpredictable. Some contain volatile compounds that can destabilize when disturbed. Plus, pirates use this field for ambushes.\"",
            None
        ),
        (
            "D",
            "What are these listening posts for?",
            "Scout: \"We monitor all traffic through this sector. The Traders Guild wants to establish a safer route, and the Space Police need intel on pirate movements.\"",
            None
        ),
        (
            "E",
            "Find anything valuable out here?",
            "Scout: \"The ice asteroids contain pure water and some rare frozen gases. Valuable, but dangerous to extract without proper equipment.\"",
            None
        )
    ]),
    ("C", &[
        (
            "F",
            "Any tips for navigating safely?",
            "Scout: \"Keep your distance from the larger asteroids, and watch for the bluish glow - those are the most unstable ones. And keep your comms open to our listening posts.\"",
            None
        ),
        (
            "G",
            "Have there been many accidents here?",
            "Scout: \"Too many. Last month we lost a mining vessel - hit an unstable pocket and the whole asteroid fractured. That's why we've increased surveillance.\"",
            None
        )
    ]),
    ("D", &[
        (
            "H",
            "Who maintains these posts?",
            "Scout: \"Joint operation between Traders and Space Police. I'm technically a contractor for both - keeps things neutral and information flowing both ways.\"",
            None
        ),
        (
            "I",
            "Can your systems detect pirates hiding in the field?",
            "Scout: \"Sometimes. They've gotten clever with thermal masking, but we track movement patterns and energy signatures. The ice creates unique distortion patterns when disturbed.\"",
            None
        )
    ]),
    ("E", &[
        (
            "J",
            "How do you extract resources safely?",
            "Scout: \"Specialized drills with thermal regulators. Too hot and you vaporize the valuable gases, too cold and the equipment freezes. It's a delicate balance.\"",
            None
        ),
        (
            "K",
            "Are there any mining operations I can join?",
            "Scout: \"The Traders Guild runs sanctioned operations on the outer rim of the field. They're always looking for skilled pilots. Just check in at Listening Post Alpha before you start.\"",
            None
        )
    ]),
    ("F", &[(
        "END",
        "Thanks for the advice. I'll be careful.",
        "Scout: \"Safe travels. If you get in trouble, emergency frequency is 121.3. We monitor it around the clock.\"",
        None
    )]),
    ("G", &[(
        "END",
        "That's terrible. I'll definitely stay alert.",
        "Scout: \"Good. We don't need more casualties. The coordinates of recent incidents are uploaded to your nav system. Avoid those areas.\"",
        None
    )]),
    ("H", &[(
        "END",
        "Interesting arrangement. Seems effective.",
        "Scout: \"It works. Neither side fully trusts the other, but they both trust the data. In deep space, reliable information is more valuable than fuel.\"",
        None
    )]),
    ("I", &[(
        "END",
        "I'll keep an eye out for suspicious movement.",
        "Scout: \"Appreciated. If you spot anything, ping Listening Post Beta. They handle security responses. And watch your back - not every threat comes from pirates.\"",
        None
    )]),
    ("J", &[(
        "END",
        "Sounds like a challenging job.",
        "Scout: \"It is, but the payoff is worth it. Pure water fetches premium prices in the outer colonies, and the rare gases are essential for high-end shield technology.\"",
        None
    )]),
    ("K", &[(
        "END",
        "I might check that out. Thanks for the tip.",
        "Scout: \"No problem. Tell them Outpost Scout Vega sent you. Might improve your contract terms a bit. Just don't mention that to my supervisors.\" *winks*",
        None
    )]),
    DIALOGUE_END
];

pub const ZONE: Zone = {
    // Create a listening post object
    let listening_post = |name: &'static str| Obj::SpaceObject {
        name,
        scale: 3.5,
        can_move: false,
        visuals: Visuals::sprite(MySprite::SPACESTATION)
    };
    
    // Create ice asteroids with random sizes
    let ice_asteroid = Obj::IceAsteroid;
    
    Zone {
        name: "Frostbite Ice Field",
        objects: &[
            // Three listening posts stationed around the ice field
            ([0.0, 15.0, 80.0], listening_post("Listening Post Alpha")),
            ([-75.0, 0.0, -50.0], listening_post("Listening Post Beta")),
            ([90.0, -10.0, -30.0], listening_post("Listening Post Gamma")),
            
            // A scout stationed at the first listening post
            ([5.0, 15.0, 75.0], Obj::TalkingPerson {
                name: "Ice Field Scout",
                sprite: MySprite::GPT4O_SPACE_MAN,
                dialogue_tree: ICE_FIELD_SCOUT_DIALOGUE
            }),
            
            // A mining ship
            ([-30.0, 0.0, 60.0], Obj::SpaceObject {
                name: "Ice Mining Vessel",
                scale: 2.5,
                can_move: false,
                visuals: Visuals::sprite(MySprite::GPT4O_MINING_SHIP)
            }),
            
            // Scattered HP box for emergencies
            ([45.0, 5.0, 20.0], Obj::HpBox),
            
            // Ice asteroids in a disk shape with radius 100
            ([0.0, 0.0, 100.0], ice_asteroid),
            ([35.0, 5.0, 95.0], ice_asteroid),
            ([70.0, -5.0, 70.0], ice_asteroid),
            ([95.0, 10.0, 35.0], ice_asteroid),
            ([100.0, 0.0, 0.0], ice_asteroid),
            ([95.0, -10.0, -35.0], ice_asteroid),
            ([70.0, 5.0, -70.0], ice_asteroid),
            ([35.0, -5.0, -95.0], ice_asteroid),
            ([0.0, 10.0, -100.0], ice_asteroid),
            ([-35.0, 0.0, -95.0], ice_asteroid),
            ([-70.0, -5.0, -70.0], ice_asteroid),
            ([-95.0, 5.0, -35.0], ice_asteroid),
            ([-100.0, 10.0, 0.0], ice_asteroid),
            ([-95.0, -10.0, 35.0], ice_asteroid),
            ([-70.0, 5.0, 70.0], ice_asteroid),
            ([-35.0, -5.0, 95.0], ice_asteroid),
            
            // Additional ice asteroids to reach 30 total, with varied positions within the disk
            ([50.0, 0.0, 30.0], ice_asteroid),
            ([-20.0, 5.0, 60.0], ice_asteroid),
            ([80.0, -8.0, -25.0], ice_asteroid),
            ([-55.0, 12.0, -30.0], ice_asteroid),
            ([-40.0, -6.0, 0.0], ice_asteroid),
            ([25.0, 7.0, -45.0], ice_asteroid),
            ([10.0, -10.0, 75.0], ice_asteroid),
            ([-75.0, 6.0, 15.0], ice_asteroid),
            ([60.0, -5.0, -65.0], ice_asteroid),
            ([-10.0, 8.0, -85.0], ice_asteroid),
            ([45.0, -12.0, 50.0], ice_asteroid),
            ([-50.0, 10.0, -70.0], ice_asteroid),
            ([-60.0, -8.0, 50.0], ice_asteroid),
            ([30.0, 6.0, 60.0], ice_asteroid),
            
            // A couple of space pirates lurking in the field
            ([65.0, -3.0, -60.0], Obj::SpacePirate),
            ([-80.0, 5.0, 30.0], Obj::SpacePirate),
            
            // Some helpful objects
            ([-15.0, 0.0, -40.0], Obj::TreasureContainer),
            ([40.0, 8.0, -25.0], Obj::SpaceCoin)
        ],
        warp_gate_coords: [0.0, 10.0, 0.0],
        faction_control: Some(Faction::EXPLORERS) // Joint control, but primarily Explorer territory
    }
};
