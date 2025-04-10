;;; aigen.el -*- lexical-binding: t; -*-
;;

;; (provide 'aigen)
;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)

;; --- Helper function to convert lowercase with spaces to SCREAMING_SNAKE_CASE ---
;; Still needed for the const name in the prompt and the filename derivation.
(defun my/lowercase-spaces-to-screaming-snake-case (s)
  "Convert lowercase string S with spaces to SCREAMING_SNAKE_CASE.
Example: \"my zone name\" -> \"MY_ZONE_NAME\""
  (when (and s (stringp s) (not (string-empty-p s)))
    (let* ((trimmed (string-trim s))
           ;; Replace one or more whitespace chars (space, tab, newline etc.)
           ;; with a single underscore.
           (underscored (replace-regexp-in-string "[[:space:]]+" "_" trimmed)))
      ;; Convert the result to uppercase.
      (upcase underscored))))

;; --- The Prompt Prefix ---
;; This still instructs the AI on the desired format, crucial if skipping extraction.
(defconst my/rust-zone-prompt-prefix
  "Generate Rust code for a space game zone definition based on the specific description provided below.

**Output Requirements:**
1.  **CODE ONLY:** Provide ONLY the raw Rust code block required. Start directly with `pub const` or necessary `pub static` definitions (like patrol routes or loot tables if needed). Do NOT include ```rust markdown, introductory text, explanations, or closing remarks.
2.  **Structure:** The code must define a single public Rust `const` variable named as specified below, using the `Zone` struct format previously established.
3.  **Content:**
    * Use `pub struct Zone { name: &'static str, manual_objects: &'static [([f32; 3], ObjectKind)], procedural_params: Option<ProceduralParams> }`.
    * Use the `ObjectKind` enum: `Static { sprite, radius }`, `Harvestable { sprite, radius, resource, yield_amount }`, `Enemy { sprite, radius, enemy_type, patrol_route }`, `LootContainer { sprite, radius, loot_table_id }`.
    * Use provided helper `const fn`s: `static_obj`, `harvestable`, `enemy`, `enemy_patrol`, `loot_box`. Assume they exist and are callable in `const` context.
    * Use `[f32; 3]` arrays for 3D coordinates.
    * Use `ProceduralParams` struct where needed, defining `region`, `density`, `scale`, `seed`, and `possible_loot: &'static [LootDrop]`. Define `pub static` loot tables (like `MY_LOOT: &[LootDrop] = &[...]`) if needed and reference them. Assume `LootDrop`, `RegionShape`, `ProceduralObjectType` exist.
    * Define and reference `pub static` arrays for patrol routes if needed (e.g., `pub static MY_ROUTE: &[[f32; 3]] = &[...];`). Ensure they are `pub` if referenced by the `pub const Zone`.
    * Assume the existence of enums: `MySprite`, `ResourceType`, `EnemyType`.
    * The final output must be a valid block of Rust code suitable for direct inclusion in a `.rs` file.

--- START ZONE DESCRIPTION ---
"
  "The prompt prefix string explaining Rust zone code generation rules.")




;; --- Main Function (Corrected Argument Passing) ---

;; --- Main Function (Corrected Argument Passing - Take 2) ---
;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)
;; We are not explicitly using json functions here for now

;; --- Helper function to convert lowercase with spaces to SCREAMING_SNAKE_CASE ---
(defun my/lowercase-spaces-to-screaming-snake-case (s)
  "Convert lowercase string S with spaces to SCREAMING_SNAKE_CASE.
Example: \"my zone name\" -> \"MY_ZONE_NAME\""
  (when (and s (stringp s) (not (string-empty-p s)))
    (let* ((trimmed (string-trim s))
           (underscored (replace-regexp-in-string "[[:space:]]+" "_" trimmed)))
      (upcase underscored))))

;; --- The Prompt Prefix ---
(defconst my/rust-zone-prompt-prefix
  "Generate Rust code for a space game zone definition based on the specific description provided below.

**Output Requirements:**
1.  **CODE ONLY:** Provide ONLY the raw Rust code block required. Start directly with `pub const` or necessary `pub static` definitions (like patrol routes or loot tables if needed). Do NOT include ```rust markdown, introductory text, explanations, or closing remarks.
2.  **Structure:** The code must define a single public Rust `const` variable named as specified below, using the `Zone` struct format previously established.
3.  **Content:**
    * Use `pub struct Zone { name: &'static str, manual_objects: &'static [([f32; 3], ObjectKind)], procedural_params: Option<ProceduralParams> }`.
    * Use the `ObjectKind` enum: `Static { sprite, radius }`, `Harvestable { sprite, radius, resource, yield_amount }`, `Enemy { sprite, radius, enemy_type, patrol_route }`, `LootContainer { sprite, radius, loot_table_id }`.
    * Use provided helper `const fn`s: `static_obj`, `harvestable`, `enemy`, `enemy_patrol`, `loot_box`. Assume they exist and are callable in `const` context.
    * Use `[f32; 3]` arrays for 3D coordinates.
    * Use `ProceduralParams` struct where needed, defining `region`, `density`, `scale`, `seed`, and `possible_loot: &'static [LootDrop]`. Define `pub static` loot tables (like `MY_LOOT: &[LootDrop] = &[...]`) if needed and reference them. Assume `LootDrop`, `RegionShape`, `ProceduralObjectType` exist.
    * Define and reference `pub static` arrays for patrol routes if needed (e.g., `pub static MY_ROUTE: &[[f32; 3]] = &[...];`). Ensure they are `pub` if referenced by the `pub const Zone`.
    * Assume the existence of enums: `MySprite`, `ResourceType`, `EnemyType`.
    * The final output must be a valid block of Rust code suitable for direct inclusion in a `.rs` file.

--- START ZONE DESCRIPTION ---
"
  "The prompt prefix string explaining Rust zone code generation rules.")


;; --- Main Function (Trying gptel-make-request) ---
;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)

;; --- Helper function ---
(defun my/lowercase-spaces-to-screaming-snake-case (s)
  "Convert lowercase string S with spaces to SCREAMING_SNAKE_CASE."
  (when (and s (stringp s) (not (string-empty-p s)))
    (let* ((trimmed (string-trim s))
           (underscored (replace-regexp-in-string "[[:space:]]+" "_" trimmed)))
      (upcase underscored))))

;; --- Prompt Prefix ---
(defconst my/rust-zone-prompt-prefix
  "Generate Rust code for a space game zone definition based on the specific description provided below.

**Output Requirements:**
1.  **CODE ONLY:** Provide ONLY the raw Rust code block required. Start directly with `pub const` or necessary `pub static` definitions (like patrol routes or loot tables if needed). Do NOT include ```rust markdown, introductory text, explanations, or closing remarks.
2.  **Structure:** The code must define a single public Rust `const` variable named as specified below, using the `Zone` struct format previously established.
3.  **Content:**
    * Use `pub struct Zone { name: &'static str, manual_objects: &'static [([f32; 3], ObjectKind)], procedural_params: Option<ProceduralParams> }`.
    * Use the `ObjectKind` enum: `Static { sprite, radius }`, `Harvestable { sprite, radius, resource, yield_amount }`, `Enemy { sprite, radius, enemy_type, patrol_route }`, `LootContainer { sprite, radius, loot_table_id }`.
    * Use provided helper `const fn`s: `static_obj`, `harvestable`, `enemy`, `enemy_patrol`, `loot_box`. Assume they exist and are callable in `const` context.
    * Use `[f32; 3]` arrays for 3D coordinates.
    * Use `ProceduralParams` struct where needed, defining `region`, `density`, `scale`, `seed`, and `possible_loot: &'static [LootDrop]`. Define `pub static` loot tables (like `MY_LOOT: &[LootDrop] = &[...]`) if needed and reference them. Assume `LootDrop`, `RegionShape`, `ProceduralObjectType` exist.
    * Define and reference `pub static` arrays for patrol routes if needed (e.g., `pub static MY_ROUTE: &[[f32; 3]] = &[...];`). Ensure they are `pub` if referenced by the `pub const Zone`.
    * Assume the existence of enums: `MySprite`, `ResourceType`, `EnemyType`.
    * The final output must be a valid block of Rust code suitable for direct inclusion in a `.rs` file.

--- START ZONE DESCRIPTION ---
")

(defconst my/rust-zone-prompt-prefix
  "aaaaa"
  )


;; --- Main Function (Using Plist with Mixed Keys) ---

;; --- Prompt Prefix ---
(defconst my/rust-zone-prompt-prefix
  "Generate Rust code for a space game zone definition based on the specific description provided below.

**Output Requirements:**
1.  **CODE ONLY:** Provide ONLY the raw Rust code block required. Start directly with `pub const` or necessary `pub static` definitions (like patrol routes or loot tables if needed). Do NOT include ```rust markdown, introductory text, explanations, or closing remarks.
2.  **Structure:** The code must define a single public Rust `const` variable named as specified below, using the `Zone` struct format previously established.
3.  **Content:**
    * Use `pub struct Zone { name: &'static str, manual_objects: &'static [([f32; 3], ObjectKind)], procedural_params: Option<ProceduralParams> }`.
    * Use the `ObjectKind` enum: `Static { sprite, radius }`, `Harvestable { sprite, radius, resource, yield_amount }`, `Enemy { sprite, radius, enemy_type, patrol_route }`, `LootContainer { sprite, radius, loot_table_id }`.
    * Use provided helper `const fn`s: `static_obj`, `harvestable`, `enemy`, `enemy_patrol`, `loot_box`. Assume they exist and are callable in `const` context.
    * Use `[f32; 3]` arrays for 3D coordinates.
    * Use `ProceduralParams` struct where needed, defining `region`, `density`, `scale`, `seed`, and `possible_loot: &'static [LootDrop]`. Define `pub static` loot tables (like `MY_LOOT: &[LootDrop] = &[...]`) if needed and reference them. Assume `LootDrop`, `RegionShape`, `ProceduralObjectType` exist.
    * Define and reference `pub static` arrays for patrol routes if needed (e.g., `pub static MY_ROUTE: &[[f32; 3]] = &[...];`). Ensure they are `pub` if referenced by the `pub const Zone`.
    * Assume the existence of enums: `MySprite`, `ResourceType`, `EnemyType`.
    * The final output must be a valid block of Rust code suitable for direct inclusion in a `.rs` file.

--- START ZONE DESCRIPTION ---
"
  "The prompt prefix string explaining Rust zone code generation rules.")


(defun my/generate-rust-zone (zone-description-prompt zone-location-name)
  "Generate Rust zone code via gptel (default model), save raw response. CORRECT CALLBACK.

Args:
  zone-description-prompt (String): Describes the specific zone content.
  zone-location-name (String): Descriptive name, lowercase with spaces.

Output file: 'src/aigen/ZONE_LOCATION_NAME_snake_case.rs' relative to project root.
Writes the RAW response from the AI directly to the file. Errors signal directly."

  ;; --- Input Validation ---
  (unless (and (stringp zone-description-prompt) (not (string-empty-p zone-description-prompt)))
    (error "Argument 'zone-description-prompt' must be a non-empty string"))
  (unless (and (stringp zone-location-name) (not (string-empty-p zone-location-name)))
    (error "Argument 'zone-location-name' must be a non-empty string"))

  ;; --- Logic: Derive names, path, prompt ---
  (let* ((const-name-snake-case (my/lowercase-spaces-to-screaming-snake-case zone-location-name))
         (relative-output-file (format "src/aigen/%s.rs" const-name-snake-case))
         (full-prompt (format "%s%s\n--- END ZONE DESCRIPTION ---\n\n**Required Const Name:** %s"
                              my/rust-zone-prompt-prefix
                              zone-description-prompt
                              const-name-snake-case)))

    ;; --- API Call ---
    (message "Sending request to GPTel for zone '%s' -> %s..." zone-location-name relative-output-file)
    (gptel-request
        full-prompt   ; Argument 1: The prompt string (positional)
      :callback      ; Keyword argument :callback
      ;; Lambda now accepts TWO arguments: response and info
      (lambda (response info)
        ;; Check if the main response is a string (the expected success case)
        (if (stringp response)
            ;; --- Write raw response directly ---
            (progn ; Use progn for multiple forms
              (make-directory (file-name-directory relative-output-file) :parents)
              (with-temp-file relative-output-file
                (insert response) ; Insert the response string
                (unless (eq (char-before (point-max)) ?\n)
                  (insert "\n"))) ; Ensure trailing newline
              (message "Raw GPTel response for '%s' written successfully to %s"
                       zone-location-name relative-output-file))
          ;; Handle cases where response is not a string (error, abort, stream end, etc.)
          (message "GPTel callback for '%s' received non-string response: %S (Info: %S)"
                   zone-location-name response info)))))

  (message "Request initiated for zone '%s'. Waiting for GPTel response..." zone-location-name))



(my/generate-rust-zone
 "A treacherous ice field with unstable, cracking ice asteroids (visual effect). Contains harvestable RareIsotopes within some asteroids. Procedurally generate ice asteroids in a ring shape. Manually place 1 static listening post."
 "treacherous ice field")


;; --- Example Programmatic Usage ---
;; (my/generate-rust-zone
;;  "A treacherous ice field with unstable, cracking ice asteroids (visual effect). Contains harvestable RareIsotopes within some asteroids. Procedurally generate ice asteroids in a ring shape. Manually place 1 static listening post (sprite: ListeningPost)."
;;  "glacial drift sector")
