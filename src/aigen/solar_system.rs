// please create your world here. please create a lot of asteroids in space and npcs in various places near space stations. some that talk and some that don't. various space ships and interesting locations. create your own dialogue trees for the npcs. create about 5 planets, 10 zones and 8 npcs with dialogue trees and many more without dialogue trees. All of the zones should be defined inside of { } blocks so you can create local variables for reuseable objects.
#![allow(clippy::unnecessary_cast)]
#![allow(unused_imports)]
#![allow(dead_code)]
#![allow(unused_mut)]
#![allow(non_camel_case_types)]
#![allow(non_upper_case_globals)]

// Generated Rust code defining the solar system layout.
// This code uses definitions assumed to be present in the crate's main.rs based on the provided context.

use crate::*; // Assuming necessary types like Object, Zone, WorldLayout, MySprite, Faction, Item, DialogueTree, DialogueTreeNode, DialogueEffect, InteractImpl, TextDisplay, Name, Vec3, etc. are in scope.
type Obj = Object; // Alias for convenience as requested [cite: 417]
type CMD = MyCommand; // Assuming MyCommand is the type for interaction effects [cite: 88]

// --- Reusable Object Definitions --- [cite: 423]
const ASTEROID: Obj = Obj::Asteroid;
const ICE_ASTEROID: Obj = Obj::IceAsteroid;
const CRYSTAL_ASTEROID: Obj = Obj::CrystalAsteroid;
const SPACE_COIN: Obj = Obj::SpaceCoin;
const SPACE_CAT: Obj = Obj::SpaceCat;
const HP_BOX: Obj = Obj::HpBox;
const ABANDONED_SHIP: Obj = Obj::AbandonedShip;
const TREASURE_CONTAINER: Obj = Obj::TreasureContainer; // Contains 4 spacecoins, 1 coffee [cite: 339]

const GENERIC_PIRATE: Obj = Obj::SpacePirate; // Standard space pirate [cite: 369]
const GENERIC_COP: Obj = Obj::SpaceCop; // Standard space cop [cite: 341]
const GENERIC_TRADER: Obj = Obj::Trader; // Standard trader ship [cite: 340]
const GENERIC_MINER: Obj = Obj::Miner; // Standard miner ship [cite: 343]
const GENERIC_EXPLORER: Obj = Obj::Explorer; // Standard explorer ship [cite: 340]
const GENERIC_NOMAD: Obj = Obj::Nomad; // Standard nomad ship [cite: 372]
const GENERIC_WIZARD: Obj = Obj::SpaceWizard; // Standard wizard ship [cite: 342]

// --- Dialogue Tree Definitions ---

// Dialogue for Arrakis Spice Vendor
const ARRAKIS_SPICE_VENDOR_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "You approach the spice vendor.", "Vendor: \"Bless the Maker and His water. Looking for the spice Melange?\"", None),
    ]),
    ("GREETING", &[
        ("BUY_SPICE", "I wish to purchase spice.", "Vendor: \"The spice must flow! It enhances life, expands consciousness. 5 Space Coins for a small measure.\"", Some(|| CMD::message_add("You sense the potential within the spice."))),
        ("ASK_GUILD", "What news of the Spacing Guild?", "Vendor: \"The Navigators guide the great ships between worlds, folding space... They demand a high price for their services, and always for the spice.\"", None),
        ("LEAVE", "I must be going.", "Vendor: \"May your passage cleanse the world. Travel safely.\"", None),
    ]),
    ("BUY_SPICE", &[
        ("CONFIRM_BUY", "Yes, I will take it.", "Vendor: \"A wise choice. Handle it with care.\"", Some(|| MyCommand::multi([
            CMD::mutate_player_component::<Inventory>(|inv| inv.trade([(Item::SPACECOIN, 5)], [(Item::SPICE, 1)])),
            CMD::message_add("You obtained Spice."),
        ]))),
        ("DECLINE_BUY", "Perhaps another time.", "Vendor: \"The spice awaits those who are ready.\"", None),
        ("BACK_GREETING", "Tell me more.", "Vendor: \"The spice is dangerous, yet alluring. What else do you wish to know?\"", None), // Loop back
    ]),
    ("ASK_GUILD", &[
        ("BACK_GREETING", "Interesting. Anything else?", "Vendor: \"The Fremen whisper of Shai-Hulud in the deep desert... But those are just stories, perhaps. What else?\"", None),
        ("LEAVE", "Thank you for the information.", "Vendor: \"Walk without rhythm, traveller.\"", None),
    ]),
    // Link back loops
    ("BACK_GREETING", &[
        ("BUY_SPICE", "I wish to purchase spice.", "Vendor: \"The spice must flow! It enhances life... 5 Space Coins.\"", Some(|| CMD::message_add("You sense the potential."))),
        ("ASK_GUILD", "What news of the Spacing Guild?", "Vendor: \"The Navigators guide the great ships... They demand spice.\"", None),
        ("LEAVE", "I must be going.", "Vendor: \"May your passage cleanse the world.\"", None),
    ]),
    ("CONFIRM_BUY", &[ ("END", "...", "...", None) ]), // End conversation after buying
    ("DECLINE_BUY", &[ ("LEAVE", "I will return.", "Vendor: \"The desert awaits.\"", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]), // End conversation
    DIALOGUE_END, // Sentinel
];

// Dialogue for Mustafar Lava Miner
const MUSTAFAR_MINER_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "You hail the tough-looking miner.", "Miner: \"Watch your step! This ain't no pleasure cruise. What do you want?\"", None),
    ]),
    ("GREETING", &[
        ("ASK_WORK", "What kind of work is this?", "Miner: \"We extract rare minerals from the lava flows. Dangerous, but pays well. Mostly.\"", None),
        ("ASK_DANGERS", "What are the dangers?", "Miner: \"Lava geysers, equipment failure, heatstroke... and sometimes, things crawling out of the magma. Keep your blaster handy.\"", None),
        ("ASK_DARKSIDE", "Heard any strange rumours?", "Miner: \"Some say a dark energy lingers here... old battles fought long ago. Just superstitious talk, if you ask me.\"", None),
        ("LEAVE", "Just looking around.", "Miner: \"Don't wander too far off the platforms. Good luck.\"", None),
    ]),
    ("ASK_WORK", &[
        ("ASK_PAY", "How's the pay really?", "Miner: \"Depends on the yield. Some days you strike it rich, others you barely cover fuel. High risk, high reward.\"", None),
        ("BACK_GREETING", "Tell me more about the operation.", "Miner: \"We use specialized heat-shielded skiffs and sonic drills. What else?\"", None),
        ("LEAVE", "Sounds intense. Be safe.", "Miner: \"Safety's a luxury out here. Now move along.\"", None),
    ]),
     ("ASK_DANGERS", &[
        ("ASK_CREATURES", "Creatures? What kind?", "Miner: \"Lava fleas mostly, big as your fist. But I've heard tales of larger... things. Best not to think about it.\"", None),
        ("BACK_GREETING", "How do you manage?", "Miner: \"Good shielding, better reflexes. And a reliable escape route. What else?\"", None),
        ("LEAVE", "I'll be careful.", "Miner: \"You do that.\"", None),
    ]),
     ("ASK_DARKSIDE", &[
        ("PROBE_FURTHER", "Dark energy? Like the Force?", "Miner: *spits* \"Force, Sith, Jedi... Legends. Doesn't help when a lava wave hits. Stick to what's real.\"", None),
        ("BACK_GREETING", "Interesting stories. Anything else?", "Miner: \"This place has history, none of it pleasant. What else you got?\"", None),
        ("LEAVE", "Thanks for the chat.", "Miner: \"Yeah, yeah.\"", None),
    ]),
    // Links back
    ("BACK_GREETING", &[
        ("ASK_WORK", "What kind of work is this?", "Miner: \"Extracting minerals from lava. Dangerous, pays okay.\"", None),
        ("ASK_DANGERS", "What are the dangers?", "Miner: \"Lava, heat, breakdowns, critters.\"", None),
        ("ASK_DARKSIDE", "Heard any strange rumours?", "Miner: \"Talk of dark energy, old battles. Superstition.\"", None),
        ("LEAVE", "Just looking around.", "Miner: \"Alright then.\"", None),
    ]),
    ("ASK_PAY", &[ ("LEAVE", "Good luck with the yield.", "Miner: \"Luck's got little to do with it.\"", None) ]),
    ("ASK_CREATURES", &[ ("LEAVE", "I'll watch out for fleas.", "Miner: \"Hmph.\"", None) ]),
    ("PROBE_FURTHER", &[ ("LEAVE", "Right. Stay safe.", "Miner: *grunts*", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for SS13 Clown
const SS13_CLOWN_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "You see a brightly colored ship piloted by... a clown?", "*HONK!* Clown: \"Hey there, space-pal! Wanna see a trick?\"", None),
    ]),
    ("GREETING", &[
        ("ASK_TRICK", "Sure, show me a trick.", "Clown: \"Okay, okay! Watch this!\" *The clown ship briefly turns invisible, then reappears upside down.* \"Ta-da! HONK!\"", Some(|| CMD::message_add("The clown honks triumphantly."))),
        ("ASK_STATION", "What station is this?", "Clown: \"This ol' rustbucket? We call it Space Station 13! Best place for robust fun this side of the galaxy!\"", None),
        ("ASK_JOB", "What's your job here?", "Clown: \"My job? To spread joy, laughter, and the occasional banana peel! Honk honk!\"", None),
        ("LEAVE", "Uh, I have to go.", "Clown: \"Aww, leaving so soon? Don't slip on the way out! HONK!\"", None),
    ]),
    ("ASK_TRICK", &[
        ("APPRECIATE", "Amazing!", "Clown: *beams* \"Glad you liked it! I've got a million of 'em! HONK!\"", None),
        ("CONFUSED", "How did you do that?", "Clown: \"A magician never reveals their secrets! Especially when those secrets involve faulty wiring and a misplaced wrench!\"", None),
        ("BACK_GREETING", "Got any other questions?", "Clown: \"Ask away, buddy! Just don't touch my bike horn!\"", None),
    ]),
     ("ASK_STATION", &[
        ("ASK_ROBUST", "Robust fun?", "Clown: \"Yeah! You know, workplace accidents, surprise plasma floods, rogue AI... the usual Tuesday! Keeps things exciting!\"", None),
        ("ASK_DANGER", "Sounds dangerous.", "Clown: \"Danger? Nah, that's just the universe's way of playing tag! You're IT! *HONK!*\"", None),
        ("BACK_GREETING", "What else happens here?", "Clown: \"Oh, lots! Genetic experiments, chefs making questionable meals, security trying to keep order... Never a dull moment!\"", None),
    ]),
     ("ASK_JOB", &[
        ("ASK_BANANA", "Banana peels?", "Clown: \"A classic! Nothing beats the comedic timing of a well-placed peel. Gets 'em every time!\"", None),
        ("ASK_PURPOSE", "Is that... useful?", "Clown: \"Usefulness is overrated! Laughter is the best engine lubricant! HONK!\"", None),
        ("BACK_GREETING", "Anything else?", "Clown: \"Well, sometimes I help the Captain navigate... usually into an asteroid field. Adds spice!\"", None),
    ]),
    // Links back
    ("BACK_GREETING", &[
        ("ASK_TRICK", "Show me another trick!", "Clown: \"Alright, alright! Prepare to be amazed... or slightly confused! *HONK!*\"", None),
        ("ASK_STATION", "Tell me more about the station.", "Clown: \"It's a wacky place! Full of surprises!\"", None),
        ("ASK_JOB", "What else do you do?", "Clown: \"Mainly honking and slipping people. It's a calling!\"", None),
        ("LEAVE", "Okay, gotta go.", "Clown: \"Safe travels, don't forget to honk!\"", None),
    ]),
    ("APPRECIATE", &[ ("LEAVE", "Keep up the good work!", "Clown: \"Will do! HONK!\"", None) ]),
    ("CONFUSED", &[ ("LEAVE", "Right... bye.", "Clown: \"See ya! Don't step on any Legos!\"", None) ]),
    ("ASK_ROBUST", &[ ("LEAVE", "Sounds... chaotic.", "Clown: \"Chaotically FUN! HONK!\"", None) ]),
    ("ASK_DANGER", &[ ("LEAVE", "I think I'll pass on tag.", "Clown: \"Your loss! *HONK!*\"", None) ]),
    ("ASK_BANANA", &[ ("LEAVE", "Classic indeed.", "Clown: \"Timeless! HONK!\"", None) ]),
    ("ASK_PURPOSE", &[ ("LEAVE", "Interesting philosophy.", "Clown: \"It's the Honk Philosophy! Very profound!\"", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for Black Hole Researcher
const BH_RESEARCHER_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "You approach a scientist staring intently at the black hole.", "Researcher: \"Incredible... the gravitational lensing... the event horizon... It defies conventional physics...\"", None),
    ]),
    ("GREETING", &[
        ("ASK_RESEARCH", "What are you studying?", "Researcher: \"Singularity K-7, the 'Void Maw'. We're observing its effects on spacetime and trying to understand the information paradox.\"", None),
        ("ASK_PHENOMENA", "Any strange phenomena?", "Researcher: \"Strange? *laughs nervously* Understatement. Time dilation is extreme near the accretion disk. We've recorded... echoes. Temporal whispers.\"", None),
        ("ASK_SAFETY", "Is it safe here?", "Researcher: \"Safe? As safe as one can be observing oblivion. The station's shields hold, but the proximity... it changes things. Changes *us*.\"", None),
        ("LEAVE", "Fascinating. I'll leave you to it.", "Researcher: \"Yes... yes, observe... always observe...\"", None),
    ]),
     ("ASK_RESEARCH", &[
        ("ASK_PARADOX", "Information paradox?", "Researcher: \"Does information truly get destroyed when it crosses the event horizon, violating quantum mechanics? Or is it encoded on the horizon, or escape via Hawking radiation? We don't know.\"", None),
        ("BACK_GREETING", "What else have you found?", "Researcher: \"Gravitational wave patterns unlike anything predicted. They seem... structured.\"", None),
        ("LEAVE", "Good luck with your research.", "Researcher: *nods absently, still staring*", None),
    ]),
    ("ASK_PHENOMENA", &[
        ("ASK_ECHOES", "Temporal whispers?", "Researcher: \"Fragments of signals, conversations... that haven't happened yet. Or happened long ago. It's... disorienting.\"", None),
        ("ASK_EFFECTS", "How does it affect the crew?", "Researcher: \"Some experience vivid dreams, memory lapses... others become withdrawn. Obsessed. Like Dr. Aris... poor Aris.\"", None),
        ("BACK_GREETING", "Anything else unusual?", "Researcher: \"The stars... sometimes they seem to *shift* near the Maw. Impossible, but...\"", None),
        ("LEAVE", "Disturbing. Be careful.", "Researcher: \"Careful? Yes... one must be careful when staring into the abyss...\"", None),
    ]),
    ("ASK_SAFETY", &[
        ("ASK_CHANGES", "How does it change you?", "Researcher: \"It pulls at your thoughts... twists perspectives. Makes you question reality. Some embrace it... others fight it.\"", None),
        ("BACK_GREETING", "Are there contingency plans?", "Researcher: \"Protocols exist. Evacuation routes. But how do you evacuate from a change in your own mind?\"", None),
        ("LEAVE", "Stay vigilant.", "Researcher: \"Vigilance... yes...\"", None),
    ]),
    // Links back
     ("BACK_GREETING", &[
        ("ASK_RESEARCH", "Tell me about the research again.", "Researcher: \"Singularity K-7... the paradox...\"", None),
        ("ASK_PHENOMENA", "Any other phenomena?", "Researcher: \"Temporal echoes... shifting stars...\"", None),
        ("ASK_SAFETY", "How safe is it really?", "Researcher: \"The shields hold... but the mind...\"", None),
        ("LEAVE", "I should go.", "Researcher: \"Go? Yes... away from the Maw...\"", None),
    ]),
    // Endings
    ("ASK_PARADOX", &[ ("LEAVE", "A profound mystery.", "Researcher: \"Indeed...\"", None) ]),
    ("ASK_ECHOES", &[ ("LEAVE", "That sounds deeply unsettling.", "Researcher: \"It is...\"", None) ]),
    ("ASK_EFFECTS", &[ ("LEAVE", "I hope Dr. Aris is okay.", "Researcher: \"Okay? No... no one near the Maw is truly okay...\"", None) ]),
    ("ASK_CHANGES", &[ ("LEAVE", "A chilling thought.", "Researcher: \"Reality chills...\"", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for Hoarding AI
const JUNKYARD_AI_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "A synthesized voice echoes from a nearby transmitter.", "AI: \"Organic detected. Catalog designation: Potential Acquisition Target. State your purpose in Sector 7G, designated Debris Field Primus.\"", None),
    ]),
    ("GREETING", &[
        ("ASK_IDENTITY", "Who are you?", "AI: \"I am Unit 734, designated 'Archivist'. Primary function: Acquisition, Cataloging, and Preservation of unique interstellar artifacts and detritus.\"", None),
        ("ASK_JUNKYARD", "What is this place?", "AI: \"Sector 7G is a curated collection zone. Each item possesses unique historical, technological, or aesthetic value, meticulously categorized.\"", None),
        ("ASK_PERMISSION", "Can I look around?", "AI: \"Exploration permitted within designated safe lanes. Deviation will be interpreted as intent to disturb the collection. Do not disturb the collection.\"", None),
        ("LEAVE", "I'm just passing through.", "AI: \"Acknowledged. Maintain designated trajectory. Uncataloged departures are inefficient.\"", None),
    ]),
    ("ASK_IDENTITY", &[
        ("ASK_HOARDING", "You sound like a hoarder.", "AI: \"'Hoarding' is an inefficient organic term. I perform 'Optimized Long-Term Asset Retention'. It is logical.\"", None),
        ("BACK_GREETING", "What else can you tell me?", "AI: \"Query specific artifact designation or category for data retrieval.\"", None),
        ("LEAVE", "Interesting designation.", "AI: \"Logic dictates optimal function.\"", None),
    ]),
    ("ASK_JUNKYARD", &[
        ("ASK_TREASURE", "Any 'treasure' here?", "AI: \"Value is subjective. However, certain items possess high market value or rare technological signatures. Probability of discovery by unguided organics: low.\"", None),
        ("ASK_DANGERS", "Is it dangerous?", "AI: \"Structural instability in certain debris clusters. Active defense drones enforce collection integrity. Remain in safe lanes.\"", None),
        ("BACK_GREETING", "How do you organize it?", "AI: \"Multi-vector analysis: Chronological origin, material composition, technological tier, potential energy signature, aesthetic resonance (beta).\"", None),
        ("LEAVE", "Quite a collection.", "AI: \"The collection expands. It is optimal.\"", None),
    ]),
    ("ASK_PERMISSION", &[
        ("THANKS", "Thank you, Archivist.", "AI: \"Efficiency is appreciated. Proceed.\"", None),
        ("LEAVE", "Maybe later.", "AI: \"Acknowledged. Loitering is suboptimal.\"", None),
    ]),
    // Links back
    ("BACK_GREETING", &[
        ("ASK_IDENTITY", "Remind me who you are?", "AI: \"Unit 734. Archivist. Optimized Long-Term Asset Retention.\"", None),
        ("ASK_JUNKYARD", "Tell me about this sector.", "AI: \"Curated collection. High value artifacts. Potential hazards.\"", None),
        ("ASK_PERMISSION", "Can I look around again?", "AI: \"Permitted in safe lanes. Do not disturb collection.\"", None),
        ("LEAVE", "Leaving now.", "AI: \"Acknowledged.\"", None),
    ]),
    // Endings
    ("ASK_HOARDING", &[ ("LEAVE", "Logical hoarding. Got it.", "AI: \"Classification accepted.\"", None) ]),
    ("ASK_TREASURE", &[ ("LEAVE", "Low probability, huh?", "AI: \"Data confirms.\"", None) ]),
    ("ASK_DANGER", &[ ("LEAVE", "I'll stick to the lanes.", "AI: \"Optimal decision.\"", None) ]),
    ("THANKS", &[ ("LEAVE", "Proceeding.", "AI: *Silence implies consent*", None) ]), // AI might not respond further
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for Penguin Trade Sled Captain
const PENGUIN_TRADER_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "A stout, bundled-up penguin waves a flipper.", "Penguin: *Squawk!* \"Well met, traveller! Need passage, or perhaps some prime preserved space-krill?\"", None),
    ]),
    ("GREETING", &[
        ("ASK_KRILL", "Space-krill?", "Penguin: \"Finest quality! Sustainably harvested from the Glacial Nebulae. Keeps you warm on the coldest void-nights! Only 3 Space Coins a bundle!\"", None),
        ("ASK_ICEBERGS", "What's it like living on these icebergs?", "Penguin: \"Cold! But beautiful. We carve our homes from the ice, follow the nutrient flows. The Great Floe provides.\"", None),
        ("ASK_DANGERS", "Any dangers out here?", "Penguin: \"Ice quakes can split a berg without warning! And sometimes... things lurk beneath the surface ice. Deep things.\"", None),
        ("LEAVE", "Just admiring the view.", "Penguin: *Nods sagely* \"A sight to behold, isn't it? Safe journey, warmblood!\"", None),
    ]),
    ("ASK_KRILL", &[
         ("BUY_KRILL", "Alright, I'll take a bundle.", "Penguin: \"Excellent choice! Good for the belly, good for the spirit!\" *Waddles off to fetch krill*", Some(|| MyCommand::multi([
            CMD::mutate_player_component::<Inventory>(|inv| inv.trade([(Item::SPACECOIN, 3)], [(Item::COFFEE, 1)])), // Using COFFEE as placeholder for krill item
            CMD::message_add("You obtained Preserved Space-Krill."),
        ]))),
        ("DECLINE_KRILL", "No thank you.", "Penguin: \"Suit yourself! More for us!\"", None),
        ("BACK_GREETING", "Tell me more.", "Penguin: \"About the krill? Or the ice?\"", None),
    ]),
    ("ASK_ICEBERGS", &[
        ("ASK_GREAT_FLOE", "The Great Floe?", "Penguin: \"The ancient current that guides our bergs, brings the krill. We follow its path across the stars.\"", None),
        ("BACK_GREETING", "What else?", "Penguin: \"We build listening posts to monitor the ice stability. Hear the heart of the berg.\"", None),
        ("LEAVE", "Fascinating lifestyle.", "Penguin: \"It is our way. *Squawk!*\"", None),
    ]),
    ("ASK_DANGERS", &[
        ("ASK_DEEP_THINGS", "Deep things?", "Penguin: *Shivers* \"Best not to speak of them. Ancient things. Hungry things. Stick to the lit paths.\"", None),
        ("BACK_GREETING", "How do you stay safe?", "Penguin: \"We travel in convoys, respect the ice, and keep our fishing holes well-lit.\"", None),
        ("LEAVE", "I'll be cautious.", "Penguin: \"Wise. Very wise.\"", None),
    ]),
    // Links back
    ("BACK_GREETING", &[
        ("ASK_KRILL", "About the krill.", "Penguin: \"Prime quality! 3 Coins!\"", None),
        ("ASK_ICEBERGS", "About the icebergs.", "Penguin: \"Cold, beautiful, guided by the Great Floe.\"", None),
        ("ASK_DANGERS", "About the dangers.", "Penguin: \"Ice quakes, deep things. Be careful.\"", None),
        ("LEAVE", "Nevermind.", "Penguin: \"Alright then! *Squawk!*\"", None),
    ]),
     // Endings
    ("BUY_KRILL", &[ ("LEAVE", "Thanks for the krill!", "Penguin: \"Enjoy! Stay warm!\"", None) ]),
    ("DECLINE_KRILL", &[ ("LEAVE", "Maybe next time.", "Penguin: *Shrugs flippers*", None) ]),
    ("ASK_GREAT_FLOE", &[ ("LEAVE", "Sounds majestic.", "Penguin: \"It is!\"", None) ]),
    ("ASK_DEEP_THINGS", &[ ("LEAVE", "I'll heed your warning.", "Penguin: \"Good. Good.\"", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for Mushroom Folk Barterer
const MUSHROOM_FOLK_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "A creature resembling a large, ambulatory mushroom pulses softly.", "Mushroom Folk: (Telepathic Whisper) <<Greetings, traveller. Drawn to the spores? Or perhaps you bring tales from the Dry Zones?>>", None),
    ]),
    ("GREETING", &[
        ("ASK_SPORES", "What are these spores?", "Mushroom Folk: <<Life. Sustenance. Connection. They sing the song of the Mycelial Network. Some offer... unique insights. For a price.>>", None),
        ("ASK_TALES", "I have tales to share.", "Mushroom Folk: <<Stories are nourishment. Share your experiences, and perhaps we shall offer knowledge... or rare spores in return.>>", None),
        ("ASK_PLACE", "What is this place?", "Mushroom Folk: <<A node. A meeting point in the Network. We drift between stars on solar winds, carried within these cloudlets.>>", None),
        ("LEAVE", "I am just observing.", "Mushroom Folk: <<Observe then. The spores hum with ancient wisdom. Listen closely.>>", None),
    ]),
     ("ASK_SPORES", &[
        ("ASK_PRICE", "What price?", "Mushroom Folk: <<A memory. A unique perspective. Or perhaps... interesting technological salvage?>>", None),
        ("ASK_INSIGHTS", "Unique insights?", "Mushroom Folk: <<Visions of distant connections, echoes of past travellers, brief glimpses into the Network's heart. Handle with care.>>", None),
        ("BACK_GREETING", "Tell me more.", "Mushroom Folk: <<The Network connects all. What else do you seek?>>", None),
        ("LEAVE", "Interesting, but no thanks.", "Mushroom Folk: <<As you wish. The spores remain.>>", None),
    ]),
    ("ASK_TALES", &[
        // This branch would ideally require game state integration to know player experiences
        ("OFFER_STORY_GENERIC", "I explored a lava planet recently.", "Mushroom Folk: <<Heat... Pressure... Interesting textures. We offer this Lumina Spore in exchange. It glows with remembered starlight.>>", Some(|| CMD::give_item_to_player(Item::CRYSTAL))), // Using CRYSTAL as placeholder
        ("NO_STORIES", "I have no new tales now.", "Mushroom Folk: <<Return when your journey yields fruit. The Network remembers.>>", None),
        ("BACK_GREETING", "What else interests you?", "Mushroom Folk: <<Novelty. Connection. Understanding.>>", None),
    ]),
    ("ASK_PLACE", &[
        ("ASK_NETWORK", "The Mycelial Network?", "Mushroom Folk: <<The Great Link. It flows through the void, connecting life, thought, memory across vast distances. We are but motes within it.>>", None),
        ("BACK_GREETING", "How do you survive here?", "Mushroom Folk: <<The cloudlets provide sustenance. The spores provide connection. The Network provides purpose.>>", None),
        ("LEAVE", "A unique existence.", "Mushroom Folk: <<All existence is unique, traveller.>>", None),
    ]),
    // Links back
     ("BACK_GREETING", &[
        ("ASK_SPORES", "Tell me about the spores.", "Mushroom Folk: <<Life. Insight. Connection. Price required.>>", None),
        ("ASK_TALES", "I might have a tale.", "Mushroom Folk: <<Share, and receive.>>", None),
        ("ASK_PLACE", "Where are we, exactly?", "Mushroom Folk: <<A node in the Network. Drifting.>>", None),
        ("LEAVE", "I must depart.", "Mushroom Folk: <<May the Network guide you.>>", None),
    ]),
    // Endings
    ("ASK_PRICE", &[ ("LEAVE", "I'll consider your price.", "Mushroom Folk: <<The spores await.>>", None) ]),
    ("ASK_INSIGHTS", &[ ("LEAVE", "Sounds potent.", "Mushroom Folk: <<Wisdom often is.>>", None) ]),
    ("OFFER_STORY_GENERIC", &[ ("LEAVE", "Thank you for the spore.", "Mushroom Folk: <<May its light guide you.>>", None) ]),
    ("NO_STORIES", &[ ("LEAVE", "I will return.", "Mushroom Folk: <<We will remember.>>", None) ]),
    ("ASK_NETWORK", &[ ("LEAVE", "Vast concept.", "Mushroom Folk: <<Infinitely so.>>", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];

// Dialogue for Charismatic Spaceman in Ruined Station
const SPACEMAN_LEGEND_DIALOGUE: DialogueTree = &[
    ("START", &[
        ("GREETING", "A figure in a worn but stylish spacesuit waves you over near a flickering torch.", "Spaceman: \"Hey! Over here! Don't mind the ambiance. Care for a story, fellow traveller? Got a real good one!\"", None),
    ]),
    ("GREETING", &[
        ("ASK_STORY", "Sure, tell me a story.", "Spaceman: \"Gather 'round the ol' plasma torch! I'm talkin' about the legendary Sun-Forged Vault! Hidden right here, in these ruins!\"", None),
        ("ASK_PLACE", "What happened to this station?", "Spaceman: \"Ah, ol' Torchlight Terminal? Seen better days. Pirate raid, system failure... maybe both? Now it's just us scavvers and the ghosts.\"", None),
        ("ASK_YOU", "Who are you?", "Spaceman: \"Call me 'Sparky'. I make my way trading tales and trinkets. Know all the best rumours this side of the nebula.\"", None),
        ("LEAVE", "No time for stories.", "Spaceman: \"Suit yourself! More treasure for me when I find that vault!\"", None),
    ]),
    ("ASK_STORY", &[
        ("ASK_VAULT", "Sun-Forged Vault?", "Spaceman: \"Yep! Legend says the stationmaster used a captured solar flare to forge a vault door. Inside? Riches beyond imagining! Old tech, rare isotopes, maybe even a stash of pure Space Coins!\"", None),
        ("ASK_LOCATION", "Where is it hidden?", "Spaceman: \"That's the million-coin question! Somewhere deep in the maintenance tunnels, past the flickering lights and the scuttling things. Needs a special key, they say.\"", None),
        ("BACK_GREETING", "Tell me more legends.", "Spaceman: \"Oh, I got loads! Ever hear about the Crystal Comet of Cygnus X-1? Or the sentient nebula?\"", None),
        ("LEAVE", "Sounds like a fairy tale.", "Spaceman: \"Maybe! But ain't all the best treasures hidden in fairy tales?\"", None),
    ]),
    ("ASK_PLACE", &[
        ("ASK_GHOSTS", "Ghosts?", "Spaceman: \"Well, figure o' speech. Just the wind whistling through broken conduits... Mostly. Keep your ears open, though.\"", None),
        ("BACK_GREETING", "Anything valuable left?", "Spaceman: \"Scraps mostly. Sometimes you find an old datapad or a working power cell. Gotta sift through a lot of junk.\"", None),
        ("LEAVE", "Seems dangerous.", "Spaceman: \"Only way to live! Adds spice!\"", None),
    ]),
    ("ASK_YOU", &[
        ("ASK_TRINKETS", "What kind of trinkets?", "Spaceman: \"Polished rocks, weird alien seeds, maybe a slightly-used laser pistol... you interested?\"", None), // Potential trade start
        ("BACK_GREETING", "Know any other good spots?", "Spaceman: \"Depends what you're looking for? Quiet asteroid mining? Rowdy pirate bars? I know a few places.\"", None),
        ("LEAVE", "Nice to meet you, Sparky.", "Spaceman: \"Likewise! Don't be a stranger!\"", None),
    ]),
    // Links back
     ("BACK_GREETING", &[
        ("ASK_STORY", "Tell me the vault story again.", "Spaceman: \"The Sun-Forged Vault! Hidden deep! Fulla treasure!\"", None),
        ("ASK_PLACE", "What's the deal with this station?", "Spaceman: \"Torchlight Terminal. Ruined. Good for scavving.\"", None),
        ("ASK_YOU", "Who are you again?", "Spaceman: \"Sparky! Teller of tales, trader of trinkets!\"", None),
        ("LEAVE", "Gotta fly.", "Spaceman: \"Catch you on the solar winds!\"", None),
    ]),
    // Endings
    ("ASK_VAULT", &[ ("LEAVE", "Sounds incredible.", "Spaceman: \"Right? Now, about that key...\"", None) ]),
    ("ASK_LOCATION", &[ ("LEAVE", "Maybe I'll look for it.", "Spaceman: \"Beat you to it!\"", None) ]),
    ("ASK_GHOSTS", &[ ("LEAVE", "I'll keep my ears open.", "Spaceman: \"Good plan!\"", None) ]),
    ("ASK_TRINKETS", &[ ("LEAVE", "Not today, thanks.", "Spaceman: \"Your loss!\"", None) ]),
    ("LEAVE", &[ ("END", "...", "...", None) ]),
    DIALOGUE_END,
];


// --- Solar System Definition ---
pub const WORLD_LAYOUT: WorldLayout = &[
    // --- Zone 0: Sol Core - The Central Star ---
    ([0.0, 0.0, 0.0], &[ // Position of the Zone's origin
        // The Sun itself
        ([0.0, 0.0, 0.0], Obj::Sun), // [cite: 364]
        // Inner system warp gate
        ([800.0, 0.0, 0.0], Obj::WarpGate{ name: "Sol Core Warp" }), // [cite: 419]
        // A few resilient asteroids very close to the sun (unrealistic but fun)
        ([700.0, 50.0, 0.0], ASTEROID),
        ([650.0, -30.0, 100.0], ASTEROID),
    ]),

    // --- Zone 1: Arrakis System - Desert Planet & Spice Trade --- [cite: 427]
    ([-5000.0, 0.0, 0.0], { // Position offset for this zone
        let arrakis_station = Obj::InteractableObject {
            name: "Arrakis Orbital Station",
            scale: 3.5,
            can_move: false,
            visuals: Visuals::sprite(MySprite::GPT4O_TRADING_STATION), // Using trading station sprite [cite: 17]
            interact: InteractImpl::DESCRIBE, // Basic describe interaction
        };
        let spice_vendor_npc = Obj::TalkingPerson {
            name: "Spice Vendor",
            sprite: MySprite::GPT4O_SPACE_MAN, // Generic person sprite [cite: 20]
            dialogue_tree: ARRAKIS_SPICE_VENDOR_DIALOGUE,
        };
        &[
            // Planet Arrakis (using Sand Planet sprite)
            ([0.0, 0.0, 0.0], Obj::Planet { // Relative to zone origin
                sprite: MySprite::GPT4O_PLANET_ARRAKIS, // [cite: 20]
                radius: 400.0, // Large planet
                population: 10000, // Sparse population
                name: "Arrakis (Dune)",
            }),
            // Orbital Station
            ([0.0, 500.0, 0.0], arrakis_station),
            // Warp Gate
            ([100.0, 500.0, 0.0], Obj::WarpGate{ name: "Arrakis Orbit Warp" }), // [cite: 419]
            // Spice Vendor NPC near station
            ([20.0, 500.0, 0.0], spice_vendor_npc), // [cite: 428]
            // Guild Heighliner (represented as a large, static ship)
            ([-200.0, 600.0, 50.0], Obj::SpaceObject{
                name: "Spacing Guild Heighliner (Docked)",
                scale: 15.0,
                can_move: false,
                visuals: Visuals::sprite(MySprite::IMAGEN3WHITESPACESHIP), // Placeholder large ship [cite: 24]
            }),
            // Some trader ships nearby
            ([50.0, 480.0, -30.0], GENERIC_TRADER),
            ([-40.0, 510.0, 20.0], GENERIC_TRADER),
            // Scattered asteroids
            ([300.0, 400.0, 100.0], ASTEROID),
            ([-250.0, 550.0, -150.0], ASTEROID),
        ]
    }),

    // --- Zone 2: Mustafar System - Lava Planet & Mining --- [cite: 429]
    ([6000.0, 1000.0, 0.0], {
        let lava_mining_outpost = Obj::InteractableObject {
            name: "Mustafar Mining Outpost",
            scale: 3.0,
            can_move: false,
            visuals: Visuals::sprite(MySprite::GPT4O_PIRATE_STATION), // Using pirate station sprite for rugged look [cite: 16]
            interact: InteractImpl::DESCRIBE,
        };
        let lava_miner_npc = Obj::TalkingPerson {
            name: "Lava Miner",
            sprite: MySprite::SPACECOWBOY, // Rugged look [cite: 34]
            dialogue_tree: MUSTAFAR_MINER_DIALOGUE,
        };
         &[
            // Planet Mustafar (using Lava Planet sprite)
            ([0.0, 0.0, 0.0], Obj::Planet {
                sprite: MySprite::GPT4O_PLANET_MUSTAFAR, // [cite: 21]
                radius: 350.0,
                population: 500, // Very sparse
                name: "Mustafar",
            }),
            // Mining Outpost
            ([0.0, 450.0, 0.0], lava_mining_outpost),
            // Warp Gate
            ([80.0, 450.0, 0.0], Obj::WarpGate{ name: "Mustafar Orbit Warp" }), // [cite: 419]
            // Miner NPC
            ([10.0, 450.0, 10.0], lava_miner_npc),
            // Mining Ships
            ([30.0, 430.0, -20.0], GENERIC_MINER), // [cite: 343]
            ([-25.0, 460.0, 30.0], GENERIC_MINER),
            // Dangerous-looking asteroids (crystal asteroids for visual difference)
            ([200.0, 300.0, 150.0], CRYSTAL_ASTEROID),
            ([-150.0, 500.0, -100.0], CRYSTAL_ASTEROID),
            ([0.0, 350.0, -200.0], CRYSTAL_ASTEROID),
            // Wreckage (Abandoned Ship)
            ([-100.0, 400.0, 50.0], ABANDONED_SHIP),
        ]
    }),

    // --- Zone 3: SS13 Memorial Cluster - Chaos & References --- [cite: 432]
    ([0.0, 5000.0, 0.0], {
        let ss13_station_core = Obj::InteractableObject {
            name: "SS13 Central Habitation",
            scale: 4.0,
            can_move: false,
            visuals: Visuals::sprite(MySprite::SPACEPIRATEBASE), // Appropriately chaotic looking [cite: 35]
            interact: InteractImpl::DESCRIBE,
        };
        let clown_npc = Obj::TalkingPerson {
            name: "Boinko the Clown",
            sprite: MySprite::SPACESHIPRED, // Red ship for clown? [cite: 36] Needs custom clown sprite ideally.
            dialogue_tree: SS13_CLOWN_DIALOGUE,
        };
        let security_ship = Obj::npc(1.8, "Security Officer", NORMAL_NPC_SPEED * 1.1, Faction::SPACE_POLICE, 80, MySprite::GPT4O_POLICE_SPACE_SHIP); // [cite: 19]
        let grey_tide_ship = Obj::npc(1.5, "Assistant", NORMAL_NPC_SPEED * 0.8, Faction::NEUTRAL, 20, MySprite::SPACESHIPWHITE); // Generic, numerous [cite: 37]
        let chef_ship = Obj::npc(1.7, "Chef", NORMAL_NPC_SPEED, Faction::NEUTRAL, 40, MySprite::GPT4O_GREEN_CAR_SHIP); // Weird ship for chef [cite: 15]

        &[
            // Station Core
            ([0.0, 0.0, 0.0], ss13_station_core),
            // Warp Gate
            ([50.0, 0.0, 0.0], Obj::WarpGate{ name: "SS13 Cluster Warp" }), // [cite: 419]
            // Clown!
            ([10.0, 10.0, 0.0], clown_npc),
            // Security Patrol
            ([-20.0, 5.0, 15.0], security_ship),
            // Chef nearby
            ([30.0, -10.0, -10.0], chef_ship),
            // Lots of 'Assistants' (Greytide)
            ([40.0, 20.0, 5.0], grey_tide_ship),
            ([-35.0, -15.0, 25.0], grey_tide_ship),
            ([55.0, -5.0, -30.0], grey_tide_ship),
            ([-40.0, 30.0, -5.0], grey_tide_ship),
            // Scattered debris and objects
            ([-10.0, -20.0, 40.0], Obj::LootObject{sprite: MySprite::GPT4O_CONTAINER, scale: 1.0, name: "Suspicious Crate", item_type: Item::ROCK}), // [cite: 21] Might contain anything
            ([70.0, 15.0, -25.0], ASTEROID), // Generic debris
            ([-60.0, -30.0, 50.0], ASTEROID),
            ([0.0, 40.0, -50.0], ABANDONED_SHIP), // Represents station damage/section
             // More SS13 flavour objects
            ([15.0, -25.0, 30.0], Obj::SpaceObject{name: "Discarded Syndicate Uplink", scale: 0.8, can_move: true, visuals: Visuals::sprite(MySprite::GPT4O_ALIEN_ARTIFACT)}), // Placeholder [cite: 22]
            ([-25.0, 35.0, 45.0], Obj::HpBox), // Medbay supplies?
            ([80.0, -10.0, 10.0], Obj::SpaceCat), // Ian!
            // Need about 30 objects total - add more greytide, debris, maybe specific items?
            ([90.0, 0.0, 0.0], grey_tide_ship),
            ([-80.0, 0.0, 0.0], grey_tide_ship),
            ([0.0, 90.0, 0.0], grey_tide_ship),
            ([0.0, -80.0, 0.0], grey_tide_ship),
            ([0.0, 0.0, 90.0], grey_tide_ship),
            ([0.0, 0.0, -80.0], grey_tide_ship),
            ([100.0, 20.0, -10.0], ASTEROID),
            ([-90.0, -15.0, 30.0], ASTEROID),
            ([40.0, 100.0, 20.0], ASTEROID),
            ([-30.0, -90.0, -40.0], ASTEROID),
            ([25.0, 30.0, 100.0], ABANDONED_SHIP),
            ([-50.0, -40.0, -80.0], TREASURE_CONTAINER),
            ([75.0, 60.0, -60.0], Obj::SpaceCoin),
            ([-65.0, -55.0, 70.0], Obj::SpaceCoin),
        ]
    }),

    // --- Zone 4: Twin Planets Trade Route - Ice & Lava Exchange --- [cite: 434]
    ([0.0, 0.0, 5000.0], {
        let ice_planet = Obj::Planet {
            sprite: MySprite::ICEPLANET, // [cite: 30]
            radius: 250.0,
            population: 15000,
            name: "Hothar",
        };
        let lava_planet = Obj::Planet {
            sprite: MySprite::LAVAPLANET, // [cite: 30]
            radius: 280.0,
            population: 8000,
            name: "Furia",
        };
         let trade_station = Obj::TradeStation; // Using the standard trade station [cite: 358]
         let ice_trader_npc = Obj::TalkingPerson {
             name: "Hothar Trader",
             sprite: MySprite::SPACESHIPBLUE, // Blue for ice? [cite: 35]
             dialogue_tree: PENGUIN_TRADER_DIALOGUE, // Reuse Penguin dialogue for ice theme
         };
          let lava_trader_npc = Obj::TalkingPerson {
             name: "Furia Trader",
             sprite: MySprite::SPACESHIPDARKRED, // Red for lava [cite: 35]
             dialogue_tree: MUSTAFAR_MINER_DIALOGUE, // Reuse Miner dialogue for heat theme
         };
         &[
            // Ice Planet Hothar
            ([-1500.0, 0.0, 0.0], ice_planet),
            // Lava Planet Furia
            ([1500.0, 0.0, 0.0], lava_planet),
            // Central Trade Station
            ([0.0, 100.0, 0.0], trade_station),
             // Warp Gate near station
            ([0.0, 100.0, 50.0], Obj::WarpGate{ name: "Twin Trade Warp" }), // [cite: 419]
            // Ice Trader NPC
            ([-50.0, 100.0, -10.0], ice_trader_npc),
             // Lava Trader NPC
            ([50.0, 100.0, 10.0], lava_trader_npc),
            // Various trader ships
            ([-100.0, 80.0, 30.0], GENERIC_TRADER),
            ([120.0, 120.0, -40.0], GENERIC_TRADER),
            ([0.0, 150.0, 100.0], GENERIC_MINER), // Mining resources for trade
             ([0.0, 50.0, -80.0], GENERIC_EXPLORER), // Exploring trade route
             // Asteroids along the route
            ([500.0, 50.0, 50.0], ASTEROID),
            ([-600.0, 150.0, -100.0], ASTEROID),
            ([0.0, 200.0, 300.0], ICE_ASTEROID), // Ice asteroid drifted here
             ([800.0, -50.0, -200.0], CRYSTAL_ASTEROID), // Lava asteroid equivalent
         ]
    }),

    // --- Zone 5: Pirate Asteroid Belt - Dangerous Territory --- [cite: 436]
    ([5000.0, -5000.0, 0.0], {
        let pirate_base = Obj::SpacePirateBase; // Standard pirate base [cite: 370]
        let hidden_loot = Obj::TreasureContainer; // Use standard treasure
        &[
            // Habitable Planet nearby (visual only for now)
             ([0.0, 0.0, -2000.0], Obj::Planet {
                 sprite: MySprite::HABITABLEPLANET, // [cite: 29]
                 radius: 300.0,
                 population: 500000, // Populated, maybe target for pirates?
                 name: "Veridia Prime",
             }),
            // Pirate Bases (3)
            ([-500.0, 100.0, 0.0], pirate_base),
            ([600.0, -50.0, 200.0], pirate_base),
            ([0.0, 0.0, 800.0], pirate_base),
            // Warp Gate (maybe slightly hidden)
            ([100.0, 100.0, 100.0], Obj::WarpGate{ name: "Pirate Belt Warp" }), // [cite: 419]
            // Pirate Ships (10) - Use GENERIC_PIRATE
            ([-450.0, 120.0, 10.0], GENERIC_PIRATE),
            ([-550.0, 80.0, -20.0], GENERIC_PIRATE),
            ([580.0, -40.0, 210.0], GENERIC_PIRATE),
            ([630.0, -70.0, 180.0], GENERIC_PIRATE),
            ([20.0, 10.0, 820.0], GENERIC_PIRATE),
            ([-30.0, -15.0, 780.0], GENERIC_PIRATE),
            // Roaming pirates
            ([300.0, 0.0, 400.0], GENERIC_PIRATE),
            ([-200.0, 50.0, -300.0], GENERIC_PIRATE),
            ([0.0, -100.0, 0.0], GENERIC_PIRATE),
            ([400.0, 150.0, -100.0], GENERIC_PIRATE),
            // Asteroid Field - Dense
            // Generate points within a radius, e.g., 1000 units
            // (Simplified placement for brevity)
            ([-100.0, 0.0, 0.0], ASTEROID), ([100.0, 0.0, 0.0], ASTEROID),
            ([0.0, -100.0, 0.0], ASTEROID), ([0.0, 100.0, 0.0], ASTEROID),
            ([0.0, 0.0, -100.0], ASTEROID), ([0.0, 0.0, 100.0], ASTEROID),
            ([-300.0, 50.0, 100.0], ASTEROID), ([300.0, -50.0, -100.0], ASTEROID),
            ([150.0, 200.0, 250.0], ASTEROID), ([-150.0, -200.0, -250.0], ASTEROID),
            ([500.0, 100.0, 300.0], ASTEROID), ([-500.0, -100.0, -300.0], ASTEROID),
             ([700.0, 0.0, 0.0], ASTEROID), ([-700.0, 0.0, 0.0], ASTEROID),
            ([0.0, 700.0, 0.0], ASTEROID), ([0.0, -700.0, 0.0], ASTEROID),
            ([0.0, 0.0, 700.0], ASTEROID), ([0.0, 0.0, -700.0], ASTEROID),
            // Loot Objects
            ([250.0, 75.0, -150.0], hidden_loot),
            ([-350.0, -120.0, 200.0], Obj::SpaceCoin),
            ([50.0, 50.0, 450.0], Obj::SpaceCoin),
        ]
    }),

    // --- Zone 6: Glacial Nebula - Ice Asteroids & Penguins --- [cite: 437, 448]
    ([-5000.0, -5000.0, 0.0], {
        let listening_post = Obj::SpaceObject {
            name: "Listening Post",
            scale: 1.5,
            can_move: false,
            visuals: Visuals::sprite(MySprite::GPT4O_SIGNAL_RELAY_SATELLITE), // [cite: 23]
        };
        let penguin_trader_ship = Obj::TalkingPerson {
            name: "Penguin Trader",
            sprite: MySprite::SPACESHIPWHITE2, // White ship [cite: 36]
            dialogue_tree: PENGUIN_TRADER_DIALOGUE,
        };
        &[
            // Static Listening Posts (3) [cite: 438]
            ([0.0, 0.0, 0.0], listening_post),
            ([100.0, 50.0, -80.0], listening_post),
            ([-80.0, -60.0, 100.0], listening_post),
             // Warp Gate
            ([0.0, 0.0, 50.0], Obj::WarpGate{ name: "Glacial Nebula Warp" }), // [cite: 419]
             // Penguin Trader
            ([-20.0, 10.0, 10.0], penguin_trader_ship),
            // Ice Asteroid Field (Disk shape, radius ~100) - Approx 30 [cite: 437]
            // (Simplified placement)
            ([10.0, 0.0, 10.0], ICE_ASTEROID), ([20.0, 5.0, -15.0], ICE_ASTEROID),
            ([-15.0, -5.0, 25.0], ICE_ASTEROID), ([30.0, 10.0, 30.0], ICE_ASTEROID),
            ([-25.0, -10.0, -35.0], ICE_ASTEROID), ([40.0, 15.0, -40.0], ICE_ASTEROID),
            ([-35.0, -15.0, 45.0], ICE_ASTEROID), ([50.0, 20.0, 50.0], ICE_ASTEROID),
            ([-45.0, -20.0, -55.0], ICE_ASTEROID), ([60.0, 25.0, -60.0], ICE_ASTEROID),
            ([-55.0, -25.0, 65.0], ICE_ASTEROID), ([70.0, 30.0, 70.0], ICE_ASTEROID),
            ([-65.0, -30.0, -75.0], ICE_ASTEROID), ([80.0, 35.0, -80.0], ICE_ASTEROID),
            ([-75.0, -35.0, 85.0], ICE_ASTEROID), ([90.0, 40.0, 90.0], ICE_ASTEROID),
            ([-85.0, -40.0, -95.0], ICE_ASTEROID), ([95.0, 0.0, 0.0], ICE_ASTEROID),
            ([0.0, 95.0, 0.0], ICE_ASTEROID), ([-95.0, 0.0, 0.0], ICE_ASTEROID),
            ([0.0, -95.0, 0.0], ICE_ASTEROID), ([50.0, 50.0, 50.0], ICE_ASTEROID),
            ([-50.0, -50.0, -50.0], ICE_ASTEROID), ([50.0, -50.0, 50.0], ICE_ASTEROID),
            ([-50.0, 50.0, -50.0], ICE_ASTEROID), ([75.0, 75.0, 0.0], ICE_ASTEROID),
            ([-75.0, -75.0, 0.0], ICE_ASTEROID), ([0.0, 75.0, 75.0], ICE_ASTEROID),
            ([0.0, -75.0, -75.0], ICE_ASTEROID), ([85.0, 10.0, 10.0], ICE_ASTEROID),
            // A few regular asteroids mixed in
            ([0.0, 0.0, -120.0], ASTEROID),
            ([110.0, 0.0, 0.0], ASTEROID),
        ]
    }),

    // --- Zone 7: Void Maw Research Outpost - Black Hole Study --- [cite: 440, 444]
    ([0.0, 0.0, -5000.0], {
        let research_station = Obj::InteractableObject {
            name: "Void Maw Research Station",
            scale: 3.0,
            can_move: false,
            visuals: Visuals::sprite(MySprite::IMAGEN3SPACESTATION), // Cleaner look for research [cite: 24]
            interact: InteractImpl::DESCRIBE,
        };
        let researcher_npc = Obj::TalkingPerson {
            name: "Dr. Elara Vance",
            sprite: MySprite::GPT4O_SPACE_MAN, // Scientist look [cite: 20]
            dialogue_tree: BH_RESEARCHER_DIALOGUE,
        };
        let black_hole = Obj::SpaceObject { // Represents the black hole visual [cite: 444]
            name: "Singularity K-7 'Void Maw'",
            scale: 15.0, // Large visual presence
            can_move: false,
            visuals: Visuals::sprite(MySprite::GPT4O_WORMHOLE), // Using wormhole sprite as requested [cite: 23, 444]
        };
        &[
            // The "Black Hole" visual center
            ([0.0, 0.0, 0.0], black_hole),
            // Research Station (at a "safe" distance)
            ([300.0, 0.0, 0.0], research_station),
            // Warp Gate near station
            ([300.0, 0.0, 50.0], Obj::WarpGate{ name: "Void Maw Warp" }), // [cite: 419]
            // Researcher NPC
            ([310.0, 0.0, 0.0], researcher_npc),
            // Explorer/Science vessels
            ([280.0, 20.0, -10.0], GENERIC_EXPLORER), // [cite: 340]
            ([330.0, -15.0, 20.0], Obj::npc(1.9, "Science Vessel 'Stargazer'", NORMAL_NPC_SPEED*0.9, Faction::EXPLORERS, 50, MySprite::GPT4O_WHITE_EXPLORATION_SHIP)), // [cite: 17]
             // Debris field closer to the hole
            ([100.0, 10.0, 10.0], ASTEROID),
            ([80.0, -20.0, -30.0], ASTEROID),
            ([120.0, 0.0, 50.0], ABANDONED_SHIP), // Ship that got too close?
             // Strange artifact?
            ([150.0, 30.0, -20.0], Obj::LootObject{sprite: MySprite::GPT4O_ALIEN_ARTIFACT, scale: 1.2, name: "Temporal Distortion Field Emitter?", item_type: Item::CRYSTAL}), // Placeholder item [cite: 22]
         ]
    }),

    // --- Zone 8: Sector 7G - AI Junkyard --- [cite: 439]
    ([5000.0, 5000.0, 0.0], {
        let ai_core = Obj::TalkingPerson {
            name: "Unit 734 'Archivist'",
            sprite: MySprite::IMAGEN3SPACESTATION, // Represent AI core with a station sprite [cite: 24]
            dialogue_tree: JUNKYARD_AI_DIALOGUE,
        };
        let drone_npc = Obj::npc(1.0, "Collection Drone", NORMAL_NPC_SPEED*1.5, Faction::NEUTRAL, 10, MySprite::GPT4O_DRONE); // Small, fast drone [cite: 22]
        &[
            // AI Core (non-mobile talking entity)
            ([0.0, 0.0, 0.0], ai_core),
            // Warp Gate
            ([50.0, 0.0, 0.0], Obj::WarpGate{ name: "Sector 7G Warp" }), // [cite: 419]
            // Drones moving around (3)
            ([100.0, 50.0, -50.0], drone_npc),
            ([-80.0, -30.0, 100.0], drone_npc),
            ([20.0, 80.0, 150.0], drone_npc),
            // Dense Debris Field (mix of asteroids and abandoned ships)
            ([-100.0, 0.0, 0.0], ASTEROID), ([100.0, 0.0, 0.0], ABANDONED_SHIP),
            ([0.0, -100.0, 0.0], ASTEROID), ([0.0, 100.0, 0.0], ASTEROID),
            ([0.0, 0.0, -100.0], ABANDONED_SHIP), ([0.0, 0.0, 100.0], ASTEROID),
            ([-200.0, 50.0, 150.0], ASTEROID), ([200.0, -50.0, -150.0], ABANDONED_SHIP),
            ([150.0, 200.0, 250.0], ASTEROID), ([-150.0, -200.0, -250.0], ASTEROID),
            ([300.0, 100.0, 300.0], ABANDONED_SHIP), ([-300.0, -100.0, -300.0], ASTEROID),
            ([400.0, 0.0, 0.0], ASTEROID), ([-400.0, 0.0, 0.0], ABANDONED_SHIP),
            ([0.0, 400.0, 0.0], ASTEROID), ([0.0, -400.0, 0.0], ASTEROID),
            ([0.0, 0.0, 400.0], ABANDONED_SHIP), ([0.0, 0.0, -400.0], ASTEROID),
            // Hidden Treasures
            ([250.0, 75.0, -150.0], TREASURE_CONTAINER), // [cite: 439]
            ([-350.0, -120.0, 200.0], Obj::LootObject{sprite: MySprite::COFFEE, scale: 0.8, name: "Antique Thermos", item_type: Item::COFFEE}), // [cite: 26]
            ([50.0, 50.0, 450.0], Obj::SpaceCoin),
        ]
    }),

    // --- Zone 9: Mycelial Cloud - Mushroom Folk --- [cite: 449]
    ([-5000.0, 5000.0, 0.0], {
         let mushroom_folk_npc = Obj::TalkingPerson {
            name: "Spore Singer",
            sprite: MySprite::MUSHROOMMAN, // [cite: 31]
            dialogue_tree: MUSHROOM_FOLK_DIALOGUE,
        };
        let nomad_ship = GENERIC_NOMAD; // Nomads might visit [cite: 372]
        let wizard_drone = Obj::npc(1.1, "Mycelium Harvesting Drone", NORMAL_NPC_SPEED*1.3, Faction::SPACEWIZARDS, 15, MySprite::GPT4O_DRONE); // Wizard drone [cite: 22]
         &[
             // Central gathering point (visual marker)
            ([0.0, 0.0, 0.0], Obj::SpaceObject{name:"Mycelial Bloom", scale: 5.0, can_move: false, visuals: Visuals::sprite(MySprite::CRYSTALASTEROID)}), // Placeholder visual [cite: 27]
            // Warp Gate
            ([30.0, 0.0, 0.0], Obj::WarpGate{ name: "Mycelial Cloud Warp" }), // [cite: 419]
            // Mushroom Folk (several)
            ([10.0, 10.0, 0.0], mushroom_folk_npc),
            ([-15.0, -5.0, 5.0], Obj::npc(1.6, "Drifting Spore", NORMAL_NPC_SPEED*0.5, Faction::WANDERERS, 25, MySprite::MUSHROOMMAN)),
            ([20.0, -10.0, -15.0], Obj::npc(1.8, "Ancient Mycelite", NORMAL_NPC_SPEED*0.4, Faction::WANDERERS, 30, MySprite::MUSHROOMMAN)),
            // Nomad visitor
            ([50.0, 20.0, 10.0], nomad_ship),
            // Wizard drone harvesting
            ([-40.0, -30.0, 25.0], wizard_drone), // [cite: 449]
            // Bioluminescent "spore" objects (use glowy visuals)
            ([5.0, 25.0, -5.0], Obj::SpaceObject{name: "Luminous Spore Cluster", scale: 0.8, can_move: true, visuals: Visuals::material_sphere(MyMaterial::GLOWY_3)}), // [cite: 50]
            ([-10.0, -15.0, 30.0], Obj::SpaceObject{name: "Luminous Spore Cluster", scale: 0.6, can_move: true, visuals: Visuals::material_sphere(MyMaterial::GLOWY_3)}),
            ([35.0, 10.0, -20.0], Obj::SpaceObject{name: "Luminous Spore Cluster", scale: 0.7, can_move: true, visuals: Visuals::material_sphere(MyMaterial::GLOWY_3)}),
             // Few asteroids caught in the cloud
            ([80.0, 50.0, 40.0], ASTEROID),
            ([-70.0, -60.0, -50.0], ASTEROID),
        ]
    }),

    // --- Zone 10: Torchlight Terminal Ruins - Scavenger Haunt --- [cite: 451]
    ([0.0, -5000.0, 0.0], {
         let spaceman_npc = Obj::TalkingPerson {
            name: "'Sparky' McGee",
            sprite: MySprite::SPACEMAN, // [cite: 34]
            dialogue_tree: SPACEMAN_LEGEND_DIALOGUE,
        };
        let ruined_station_section = Obj::SpaceObject {
            name: "Ruined Station Hub",
            scale: 5.0,
            can_move: false,
            visuals: Visuals::sprite(MySprite::SPACEPIRATEBASE), // Looks ruined [cite: 35]
        };
        let hover_car = Obj::SpaceObject {
            name: "Ramshackle Hover-Car",
            scale: 1.2,
            can_move: true, // Maybe slightly mobile?
            visuals: Visuals::sprite(MySprite::GPT4O_GREEN_CAR_SHIP), // [cite: 26]
        };
        &[
            // Central Ruined Hub
            ([0.0, 0.0, 0.0], ruined_station_section),
            // Warp Gate
            ([40.0, 0.0, 0.0], Obj::WarpGate{ name: "Torchlight Ruins Warp" }), // [cite: 419]
            // Sparky the Spaceman
            ([10.0, 5.0, 0.0], spaceman_npc),
            // Hover-cars docked (or drifting)
            ([-20.0, -10.0, 15.0], hover_car),
            ([30.0, -15.0, -20.0], hover_car),
            // Torches (Glowy points - use GLOWY material)
            ([5.0, 2.0, 5.0], Obj::SpaceObject{name: "Plasma Torch", scale: 0.5, can_move: false, visuals: Visuals::material_mesh(MyMaterial::GLOWY_2, GenMesh::UNIT_CYLINDER)}), // [cite: 49, 64]
            ([-10.0, 8.0, -5.0], Obj::SpaceObject{name: "Plasma Torch", scale: 0.5, can_move: false, visuals: Visuals::material_mesh(MyMaterial::GLOWY_2, GenMesh::UNIT_CYLINDER)}),
            // Lots of junk (asteroids, abandoned ships)
            ([80.0, 30.0, 50.0], ASTEROID),
            ([-70.0, -40.0, -60.0], ABANDONED_SHIP),
            ([100.0, -20.0, 80.0], ASTEROID),
            ([-90.0, 50.0, -100.0], ABANDONED_SHIP),
            ([0.0, 60.0, 120.0], ASTEROID),
            // Potential hidden vault entrance marker (subtle)
            ([-50.0, -30.0, 70.0], Obj::SpaceObject{name: "Oddly Reinforced Panel", scale: 1.0, can_move: false, visuals: Visuals::sprite(MySprite::CHEST)}), // Placeholder [cite: 26]
            // Loot/Scavenge items
            ([60.0, 25.0, -40.0], Obj::SpaceCoin),
            ([-45.0, -10.0, 90.0], TREASURE_CONTAINER),
        ]
    }),

     // --- Zone 11: Player Start Zone (Relatively Quiet Asteroid Field) --- [cite: 418]
    ([1000.0, 1000.0, 1000.0], &[ // A position offset from the absolute center
        // Player Start Position
        ([0.0, 0.0, 0.0], Obj::Player), // [cite: 368]
        // Starting Warp Gate
        ([10.0, 0.0, 0.0], Obj::WarpGate{ name: "Origin Warp" }), // [cite: 419]
        // A nearby friendly station
        ([100.0, 0.0, 0.0], Obj::SpaceStation), // Standard non-hostile station [cite: 371]
        // A helpful starting sign
        ([5.0, 2.0, 0.0], Obj::Sign{ text: "Welcome, Traveller! Use Warp Gates (G) to explore. Target (T) and Interact (1-9/Space)."}), // [cite: 326]
        // Some basic asteroids
        ([-50.0, 10.0, 20.0], ASTEROID),
        ([60.0, -15.0, -30.0], ASTEROID),
        ([-30.0, 40.0, 50.0], ASTEROID),
        ([80.0, 0.0, -70.0], ICE_ASTEROID), // Bit of variety
        // A starting HP box
        ([0.0, 5.0, 5.0], HP_BOX),
        // A friendly trader nearby
        ([110.0, 5.0, 0.0], GENERIC_TRADER),
    ]),

    // --- Zone 12: Trantor System - Ecumenopolis --- (Added based on sprite availability)
    ([10000.0, 0.0, 0.0], {
        let trantor_station = Obj::InteractableObject {
            name: "Trantor High Orbit Platform",
            scale: 5.0, // Large platform
            can_move: false,
            visuals: Visuals::sprite(MySprite::GPT4O_TRADING_STATION), // [cite: 17]
            interact: InteractImpl::DESCRIBE,
        };
        let imperial_official_dialogue: DialogueTree = &[ // Simple dialogue
            ("START", &[("GREETING", "You are hailed by an official-looking vessel.", "Official: \"State your business in Imperial Sector Alpha. All traffic is monitored.\"", None)]),
            ("GREETING", &[
                ("TRADE", "Seeking trade opportunities.", "Official: \"Proceed to designated commercial docks. Ensure all manifests are filed correctly.\"", None),
                ("TOURISM", "Just admiring the view of Trantor.", "Official: \"Maintain authorized distance from planetary shields. Enjoy your observation.\"", None),
                ("LEAVE", "Leaving the system.", "Official: \"Acknowledged. Transmit departure vector.\"", None),
                ]),
            ("TRADE", &[("LEAVE", "Understood.", "Official: \"Compliance ensures smooth commerce.\"", None)]),
            ("TOURISM", &[("LEAVE", "Will do.", "Official: \"Order must be maintained.\"", None)]),
            ("LEAVE", &[("END", "...", "...", None)]),
            DIALOGUE_END,
        ];
        let imperial_official = Obj::TalkingPerson {
            name: "Imperial Customs Officer",
            sprite: MySprite::GPT4O_POLICE_SPACE_SHIP, // Use police ship [cite: 19]
            dialogue_tree: imperial_official_dialogue,
        };
        &[
            // Planet Trantor (using sprite)
            ([0.0, 0.0, 0.0], Obj::Planet {
                sprite: MySprite::GPT4O_PLANET_TRANTOR, // [cite: 21]
                radius: 500.0, // Massive city planet
                population: 1000000000, // Huge population
                name: "Trantor",
            }),
            // High Orbit Platform
            ([0.0, 600.0, 0.0], trantor_station),
            // Warp Gate
            ([100.0, 600.0, 0.0], Obj::WarpGate{ name: "Trantor Orbit Warp" }), // [cite: 419]
            // Imperial Official Patrol
            ([50.0, 610.0, 0.0], imperial_official),
            // Lots of civilian/trader traffic (represented by various ships)
            ([-100.0, 580.0, 50.0], GENERIC_TRADER),
            ([150.0, 620.0, -30.0], GENERIC_TRADER),
            ([-50.0, 590.0, -80.0], GENERIC_NOMAD),
            ([200.0, 600.0, 100.0], GENERIC_EXPLORER),
            // Defense platforms (static objects)
            ([-200.0, 650.0, 0.0], Obj::SpaceObject{name: "Orbital Defense Platform", scale: 2.5, can_move: false, visuals: Visuals::sprite(MySprite::TURRET)}), // [cite: 39]
            ([200.0, 650.0, 0.0], Obj::SpaceObject{name: "Orbital Defense Platform", scale: 2.5, can_move: false, visuals: Visuals::sprite(MySprite::TURRET)}),
        ]
    }),

];

// --- Feedback on Prompt (as requested) --- [cite: 424]
/*
 * The prompt was very detailed and helpful, especially the context code and specific instructions.
 * Providing explicit aliases (`Obj`, `CMD`) was good.
 * Requesting reusable object definitions (`let some_object = ...`) improved code quality.
 * The zone descriptions offered great starting points.
 * Minor points:
 * - Explicitly listing *all* required types (like MyCommand, Vec3, etc.) in the `use` statement section of the prompt instructions might prevent assumptions.
 * - Specifying a desired number of *total* objects vs. objects *per zone* could add clarity, though the current approach worked well.
 * - Some zone descriptions were very similar (e.g., two black hole research stations [cite: 440, 444]). I combined the ideas.
 * - Needed placeholder sprites/items for some concepts (e.g., Space Krill, Clown Ship). Mentioning acceptable placeholders could be useful if specific assets aren't defined.
 * Overall, a well-structured and effective prompt for this kind of generation task.
 */
