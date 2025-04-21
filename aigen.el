
;;; aigen.el -*- lexical-binding: t; -*-
;;

;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)

;; --- Helper function to build the context string for the prompt ---
(defun my/build-context-code-section (context-files)
  "Build a formatted string containing code from CONTEXT-FILES for the prompt."
  (if (null context-files)
      "" ; Return empty string if no files provided
    (let ((context-string "\n\n--- START CONTEXT CODE ---\nContext files provide definitions relevant to the generation task:\n\n"))
      (progn
        (dolist (f context-files)
          (let ((content (doom-file-read f)))
            (when content
              (setq context-string
                    (concat context-string
                            (format "**File: %s**\n```rust\n%s\n```\n\n"
                                    f ; Show the relative path provided
                                    content))))))
        (concat context-string "-- END CONTEXT CODE --\n\n")))))

;; --- The Solar System Generation Prompt ---
(defconst my/rust-solar-system-prompt
  "GENERATE RAW RUST CODE ONLY. Follow all rules precisely.

Using the provided CONTEXT CODE (core definitions from src/main.rs) and ZONE DESCRIPTIONS (inspiration for world features), generate a complete Rust file defining an entire solar system game world for a space game.

1. * Keep the code organized and well-structured.

2.  **File Structure:** Create a complete Rust file that defines:
    * `use` statements: Start with `use crate::*; type Obj = Object;`
    * A central star/sun as the system's center
    * Multiple planets with distinct characteristics
    * Space stations, asteroid fields, and other space features
    * The player character starting position and properties
    * NPCs distributed throughout the system
    * Any necessary helper data structures (dialogue trees, faction relationships, etc.)
    * Ultimately the world should be expressed using the WorldLayout type from main.rs, even if the examples use another data structure.

3.  **Required World Components:**
    * **Central Star:** Define a sun or central star as the gravitational center
    * **Planets:** Create 3-7 planets with unique environments
    * **Space Stations:** Include several space stations with different purposes
    * **Warp Gates:** Remember to place a warp gate near important locations so the player can practically get to them
    * **Asteroid Fields:** Define some asteroid fields
    * **Player:** Set up the initial player
    * **NPCs:** Populate the world with various NPCs, including talking characters
    * **Special Zones:** Include special areas like research stations, trading hubs, etc.

4.  **Incorporating Zone Descriptions:**
    * Use the provided zone descriptions as inspiration for different areas in your solar system
    * Adapt ideas from the zone prompts to create coherent regions (e.g., black hole research station, space prison, trading zones)
    * Connect these diverse areas into a unified solar system that makes spatial sense
    * Space planets and the sun out somewhat far. Planets and the sun should usually be perceived as large and far away relative to stuff the player can practically get close to through warp gates.

5.  **Validity & Style:** Ensure generated code is valid Rust, uses types/functions/variants from context correctly, and follows standard Rust formatting.

* You should use `let some_object = Obj::Something;` to create reuseable objects and then insert them in various places in the world.

6.  **Prompt Feedback (As Rust Comment):** If you have feedback on this prompt, need more information, or identify missing context definitions, include these comments *inside* the generated Rust code block using `//` or `/* */`.

--- START CONTEXT CODE ---
Context files provide definitions (src/main.rs) relevant to the generation task.
"
  "The detailed prompt for generating a complete solar system game world in Rust.")

(defconst my/zone-descriptions
  '(
    ;; --- Zone Description Collection --- take these as inspiration. You don't have to stick to them exactly
    "An area near planet Arrakis. Stations store spice and The Guild is there and other references to Dune. preferably with some dialogue that references the setting and stuff. other relevant stuff that you think makes thematic sense"
    "An area near planet Mustafar. Star-Wars-y stuff. other relevant stuff that you think makes thematic sense"
    "A floating island in space with a person living on it. get creative."
    "Space prison where the most dangerous criminals are contained. Get creative."
    "An area in space where there are many references to the video game Space Station 13. Anything that references ss13 is good. Get creative. Add about 30 objects."
    "An area in space where there are space stations that are in between a lava planet and an ice planet. The stations are used for trading between the planets. Also some NPCs that can be talked to and background objects. Get creative."
    "An area in space in an asteroid field with a few barn-stations and space cowboys and space cows that are used to make space milk."
    "An asteroid field with 10 space pirates and 3 space pirate stations. A habitable planet nearby. Also some space coins and loot objects."
    "A treacherous ice field with ice asteroids. place about 30 ice asteroids in a roughly disk shape with radius 100 . Manually place 3 static listening posts."
    "A massive space junkyard managed by an AI that has developed a hoarding personality. The AI collects interesting debris and categorizes it meticulously. Include hidden treasures among the junk, drone npcs moving around and a talking AI NPC."
    "A research station studying a black hole, with scientists who are making concerning discoveries. Strange phenomena occur near the event horizon, and some researchers appear to be mentally affected by proximity to it."
    "A space laboratory conducting questionable genetic experiments. Escaped specimens roam certain areas, and scientists are divided about whether to continue their work or destroy everything."
    "An abandoned mining facility in an asteroid field where something went wrong. Strange noises echo through the halls, and the few remaining automated systems give cryptic warnings. Include valuable ore deposits and signs of what happened."
    "A research station studying a black hole(use the wormhole sprite), with scientists who are making concerning discoveries. Strange phenomena occur near the event horizon, and some researchers appear to be mentally affected by proximity to it."
    "A dazzling belt of crystal asteroids refracts every nearby starlight beam. Shards drift like glassy snow crystal monsters patrol the larger chunks, jealously guarding luminous alien artifacts embedded in the rock. Hidden signal‑relay satellites record the strange photonic harmonics."
    "A cobalt nebula cloaks an arcane enclave where space wizards conduct experiments. Floating spell circles, crystalline bookshelves, and a grand wizard spaceship hover near a lone signal beacon inscribed with runes."
    "Colossal icebergs drift through vacuum, rigged with plasma sails and populated by waddling space‑penguins. Listening posts dot the bergs' surfaces, monitoring creaking ice while trade sleds ferry fishy cargo between frozen towers."
    "A micro‑cluster of breathable cloudlets bursts with bioluminescent mushrooms. Nomadic mushroom‑folk barter spores for stories, while wizard‑like drones harvest rare mycelium for distant alchemists."
    "A tangle of drifting arboreal platforms linked by thick vine‑cables. Treemonsters guard photosynthetic farms; visitors must appease them with water shipments delivered via sleek white exploration ships."
    "Caravans of ramshackle hover‑cars dock at a half‑ruined station lit only by blazing torches. A charismatic spaceman recounts legends of a hidden sun‑forged coin vault somewhere in the maintenance tunnels."
    )
  "Collection of zone descriptions to incorporate into the solar system.")

(defun my/format-zone-descriptions (descriptions)
  "Format a list of zone DESCRIPTIONS into a string for the prompt."
  (let ((result "\n--- ZONE DESCRIPTIONS (For Inspiration) ---\n"))
    (cl-loop for desc in descriptions
             for i from 1
             do (setq result (concat result (format "Zone %d: %s\n\n" i desc))))
    (concat result "--- END ZONE DESCRIPTIONS ---\n")))

(defun my/remove-first-and-last-line (str)
  "Remove the first and last lines from the multi-line string STR."
  (let* ((lines (split-string str "\n"))
         (middle-lines (butlast (cdr lines)))) ;; remove first and last
    (string-join middle-lines "\n")))

(defun my/generate-solar-system ()
  "Generate a complete Rust solar system game world via gptel, including context.

Output file: 'src/aigen/SOLAR_SYSTEM.rs' relative to project root."

  ;; --- Logic: Derive names, path, read context, build prompt ---
  (let* ((relative-output-file "src/aigen/SOLAR_SYSTEM.rs")
         (main-rs-content (doom-file-read "src/main.rs"))
         (example-rs-content (doom-file-read "src/aigen/SPACE_DAIRY_FARM.rs"))
         (black-hole-rs-content (doom-file-read "src/aigen/BLACK_HOLE_RESEARCH_STATION.rs"))
         (zone-descriptions-formatted (my/format-zone-descriptions my/zone-descriptions))
         (context-code-section
          (concat
           (format "\n\n**File: src/main.rs (Core Definitions)**\n```rust\n%s\n```\n\n"
                   (or main-rs-content "// Failed to read src/main.rs"))
           (format "**Example File 1: SPACE_DAIRY_FARM.rs**\n```rust\n%s\n```\n\n"
                   (or example-rs-content "// Failed to read example file"))
           (format "**Example File 2: BLACK_HOLE_RESEARCH_STATION.rs**\n```rust\n%s\n```\n\n--- END CONTEXT CODE ---\n\n"
                   (or black-hole-rs-content "// Failed to read example file"))))
         (full-prompt (concat my/rust-solar-system-prompt
                              context-code-section
                              zone-descriptions-formatted)))
    (progn
      ;; --- API Call ---
      (message "Sending request to GPTel for complete solar system generation...")
      (gptel-request
          full-prompt
        :callback
        (lambda (response info)
          (if (stringp response)
              (progn
                (message "GPTel Raw Response (Solar System):\n---\n%s\n---" response)
                (let* ((final-code (my/remove-first-and-last-line response)))
                  ;; Debug final code before writing
                  (message "Final code to write:\n---\n%s\n---" final-code)

                  ;; Write the final cleaned code
                  (condition-case err
                      (doom-file-write (concat (dir!) relative-output-file) final-code)
                    (file-error (message "ERROR during file write operation: %s" err)))

                  ;; Check file attributes AFTER write attempt
                  (let ((file-size (nth 7 (file-attributes relative-output-file t))))
                    (message "Processed GPTel response for solar system. Attempted write to %s (Reported Size: %s bytes)"
                             relative-output-file (or file-size "N/A")))))
            ;; Handle non-string responses
            (message "GPTel callback for solar system received non-string response: %S (Info: %S)"
                     response info)))
        )
      (message "Request initiated for solar system generation. Waiting for GPTel response...")
      )
    ))

;; Call the function to generate the solar system
(my/generate-solar-system)


*** One more thing: this is a test run. Generate a small solar system with only two planets and like one asteroid field and 2 npcs ***
