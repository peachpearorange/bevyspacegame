use crate::*;

pub const SPHERICAL_SPACE_COW_DIALOGUE:DialogueTree = &[
  ("A", &[
    ("B", "Hello there, cow!", "Cow: \"Moo-stronaut reporting for duty!\"", None),
  ]),
  ("B", &[
    ("C", "Why are you in space?", "Cow: \"I'm here to study zero-gravity milk production!\"", None),
    ("D", "How did you become spherical?", "Cow: \"It's an evolutionary adaptation for space travel.\"", None),
    ("E", "Are you lost?", "Cow: \"No, space is my new pasture!\"", None),
  ]),
  ("C", &[
    ("F", "How's the milk production going?", "Cow: \"It's out of this world! Want to try some Moon Moo?\"", None),
    ("G", "Isn't it dangerous up here?", "Cow: \"No need to cow-er in fear, I've got my space suit!\"", None),
    ("H", "Who sent you here?", "Cow: \"Dr. Bovine von Lactose, the mad dairy scientist!\"", None),
  ]),
  ("D", &[
    ("I", "What are the advantages of being spherical?", "Cow: \"I can roll in any direction, and there are no corners to bump into!\"", None),
    ("J", "Are there other spherical animals in space?", "Cow: \"I've heard rumors of a cubical chicken, but that's just absurd.\"", None),
    ("K", "Can you change back to normal?", "Cow: \"Why would I? Being spherical is utterly amazing!\"", None),
  ]),
  ("E", &[
    ("L", "Don't you miss Earth?", "Cow: \"Sometimes, but the view up here is spe-cow-tacular!\"", None),
    ("M", "What do you eat in space?", "Cow: \"Cosmic ray grass and star dust. It's quite moo-tritious!\"", None),
    ("N", "How do you moo-ve around?", "Cow: \"I just roll with it! Newton's laws are my best friends.\"", None),
  ]),
  ("F", &[
    ("O", "Yes, I'd love to try some!", "Cow: \"Here's a glass of Moon Moo. It's extra frothy in zero-G!\"", None),
  ]),
  ("G", &[
    ("P", "What's the biggest danger you've faced?", "Cow: \"I once got caught in Saturn's rings. Talk about a tight spot!\"", None),
  ]),
  ("H", &[
    ("Q", "Can I meet this scientist?", "Cow: \"He's on the dark side of the moon. It's a bit of a trek!\"", None),
  ]),
  ("I", &[
    ("R", "Can you demonstrate your rolling?", "Cow: \"Sure! WATch me do a barrel roll!\"", None),
  ]),
  ("J", &[
    ("S", "A cubical chicken? That's crazy!", "Cow: \"I know, right? Geometry in space gets wild!\"", None),
  ]),
  ("K", &[
    ("T", "Do you ever get dizzy from being round?", "Cow: \"Nope, I'm always well-balanced!\"", None),
  ]),
  ("L", &[
    ("U", "What's your favorite view from space?", "Cow: \"The Milky Way, of course! It reminds me of home.\"", None),
  ]),
  ("M", &[
    ("V", "Does star dust taste good?", "Cow: \"It's a bit dry, but it makes my milk sparkle!\"", None),
  ]),
  ("N", &[
    ("W", "Can you explain the physics of your movement?", "Cow: \"It's all about conservation of moo-mentum!\"", None),
  ]),
  ("O", &[
    ("X", "Wow, it's delicious! Can I have the recipe?", "Cow: \"Sorry, it's a closely guarded secret of the cosmos.\"", None),
  ]),
  ("P", &[
    ("Y", "That sounds terrifying! How did you escape?", "Cow: \"I used my quick re-flex-es and dairy-ing escape plan!\"", None),
  ]),
  ("Q", &[
    ("Z", "Is he planning to send more animals to space?", "Cow: \"He's working on a flock of zero-gravity sheep as we speak!\"", None),
  ]),
  ("R", &[
    ("AA", "Impressive! Do you ever get motion sick?", "Cow: \"Nah, I've got a stomach of steel... er, four of them actually!\"", None),
  ]),
  ("S", &[
    ("AB", "Are there any other strange space animals?", "Cow: \"I've heard whispers of a dodecahedron dolphin, but that's just silly.\"", None),
  ]),
  ("T", &[
    ("AC", "You're full of jokes! Are all space cows this funny?", "Cow: \"Of course! Humor helps us cope with the uni-verse-al challenges.\"", None),
  ]),
  ("U", &[
    ("AD", "That's beautiful. Do you ever feel lonely up here?", "Cow: \"Sometimes, but then I remember I'm surrounded by stars... and star-struck fans like you!\"", None),
  ]),
  ("V", &[
    ("AE", "Sparkly milk sounds amazing! Can it grant superpowers?", "Cow: \"Only the power of good bone density and a happy tummy!\"", None),
  ]),
  ("W", &[
    ("AF", "You're quite the physicist! Ever thought of teaching?", "Cow: \"I've been thinking of starting a 'Moo-niversity' actually!\"", None),
  ]),
  ("X", &[
    ("END", "I understand. Thanks for sharing it with me!", "Cow: \"You're welcome! Remember, what happens in space, stays in space!\"", None),
  ]),
  ("Y", &[
    ("END", "You're quite the adventurer! Any other close calls?", "Cow: \"Well, there was this one time with a black hole... but that's a story for another day!\"", None),
  ]),
  ("Z", &[
    ("END", "Wow! What's next, pigs in orbit?", "Cow: \"Don't be silly! Everyone knows pigs can't fly... yet.\"", None),
  ]),
  ("AA", &[
    ("END", "You're amazing! Can I take a selfie with you?", "Cow: \"Of course! Let's make it a 'span-selfie' - spanning the cosmos!\"", None),
  ]),
  ("AB", &[
    ("END", "This is getting too weird. I think I need to go.", "Cow: \"Aw, don't have a cow, man! Stay a while and listen to more space tales!\"", None),
  ]),
  ("AC", &[
    ("END", "You're out of this world! Thanks for the chat!", "Cow: \"My pleasure! Remember, in space, everyone can hear you cream... your coffee!\"", None),
  ]),
  ("AD", &[
    ("END", "You're never alone with that attitude! Goodbye, space cow!", "Cow: \"Goodbye, Earth friend! May your dreams be as boundless as the universe!\"", None),
  ]),
  ("AE", &[
    ("END", "I'll take a gallon! This was fun, thanks!", "Cow: \"Come back anytime! The Milky Way's always open!\"", None),
  ]),
  ("AF", &[
    ("END", "SIGN me up for Astro-nomoo-my 101! Farewell!", "Cow: \"So long, and thanks for all the laughs! Keep reaching for the stars!\"", None),
  ]),
  DIALOGUE_END,
];
pub const TREACHEROUS_ICE_FIELD: Zone =
  Zone { name: "Treacherous Ice Field",
         objects: &[// ice planet in the distance
                    ([300.0, 20.0, -250.0],
                     Object::Planet { planet_type: PlanetType::ICEPLANET,
                                                 population: 20000,
                                                 radius: 102.0,
                                                 name: "Teraklon VI" }),
                    // 3 Static Listening Posts
                    ([0.0, 0.0, 80.0], Object::SpaceStation),
                    ([-70.0, 0.0, -40.0], Object::SpaceStation),
                    ([70.0, 0.0, -40.0], Object::SpaceStation),
                    // talking space cow
                    ([-10.0, -5.0, 90.0],
                     Object::TalkingPerson { name: "Space Cow",
                                                        sprite:
                                                          MySprite::GPT4O_SPHERICAL_COW,
                                                        dialogue_tree:
                                                          SPHERICAL_SPACE_COW_DIALOGUE }),
                    // 29 Ice Asteroids (manually placed in a rough disk r=100)
                    ([10.0, 5.0, 20.0], Object::IceAsteroid),
                    ([-30.0, -2.0, 50.0], Object::IceAsteroid),
                    ([45.0, 10.0, -15.0], Object::IceAsteroid),
                    ([-60.0, -8.0, -30.0], Object::IceAsteroid),
                    ([0.0, 15.0, -70.0], Object::IceAsteroid),
                    ([80.0, 0.0, 10.0], Object::IceAsteroid),
                    ([25.0, 8.0, 65.0], Object::IceAsteroid),
                    ([-55.0, -12.0, 10.0], Object::IceAsteroid),
                    ([70.0, 3.0, -60.0], Object::IceAsteroid),
                    ([-85.0, 6.0, 35.0], Object::IceAsteroid),
                    ([15.0, -9.0, -85.0], Object::IceAsteroid),
                    ([95.0, -4.0, -5.0], Object::IceAsteroid),
                    ([-20.0, 12.0, 80.0], Object::IceAsteroid),
                    ([50.0, -6.0, 40.0], Object::IceAsteroid),
                    ([-75.0, 2.0, -70.0], Object::IceAsteroid),
                    ([35.0, -15.0, -25.0], Object::IceAsteroid),
                    ([-40.0, 7.0, 95.0], Object::IceAsteroid),
                    ([60.0, -1.0, -90.0], Object::IceAsteroid),
                    ([90.0, 11.0, 30.0], Object::IceAsteroid),
                    ([-5.0, -18.0, 5.0], Object::IceAsteroid),
                    ([40.0, 4.0, -45.0], Object::IceAsteroid),
                    ([-65.0, -10.0, 60.0], Object::IceAsteroid),
                    ([10.0, 9.0, -55.0], Object::IceAsteroid),
                    ([85.0, -7.0, -40.0], Object::IceAsteroid),
                    ([-90.0, 1.0, 70.0], Object::IceAsteroid),
                    ([20.0, -11.0, 15.0], Object::IceAsteroid),
                    ([-50.0, 14.0, -80.0], Object::IceAsteroid),
                    ([75.0, -3.0, 55.0], Object::IceAsteroid),
                    ([-25.0, 0.0, -95.0], Object::IceAsteroid)],
         faction_control: Some(Faction::Explorers) };
