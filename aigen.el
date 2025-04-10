;;; aigen.el -*- lexical-binding: t; -*-
;;

;; (provide 'aigen)
;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)

;; --- Helper function to convert lowercase with spaces to SCREAMING_SNAKE_CASE ---
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

(my/lowercase-spaces-to-screaming-snake-case "asdf sadf sa")

;; --- The Prompt Prefix ---
;; This explains the rules for generating the Rust code for a zone.
;; (This constant remains the same as before)
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

;; --- Function to Extract Code Block ---
;; (This helper function remains the same as before)
(defun my/extract-rust-code-block (response-string)
  "Extract Rust code block from RESPONSE-STRING. Prefers ```rust blocks."
  (let ((code response-string))
    ;; 1. Try to find explicit markdown block first
    (when (string-match "```rust\\s-*\\(?:\n\\)?\\(.*\\)```" code)
      (setq code (match-string 1 code)))
    ;; 2. Trim whitespace regardless
    (string-trim code)))

;; --- Main Function (Non-Interactive) ---
(defun my/generate-rust-zone (zone-description-prompt output-filename zone-location-name)
  "Generate Rust zone code using gptel and save it to a file. NON-INTERACTIVE.

Args:
  zone-description-prompt (String): Describes the specific zone content.
  output-filename (String): Path for the output Rust file (e.g., \"src/zones/my_zone.rs\").
  zone-location-name (String): Descriptive name for the zone, lowercase with spaces
                               (e.g., \"derelict station core\"). This will be
                               converted to SCREAMING_SNAKE_CASE for the const name.

Generates a detailed prompt, sends it via `gptel-request`, extracts
the Rust code from the response, and writes it to OUTPUT-FILENAME."
  ;; --- Input Validation ---
  (unless (and zone-description-prompt (stringp zone-description-prompt) (not (string-empty-p zone-description-prompt)))
    (error "Argument 'zone-description-prompt' must be a non-empty string"))
  (unless (and output-filename (stringp output-filename) (not (string-empty-p output-filename)))
    (error "Argument 'output-filename' must be a non-empty string"))
  (unless (and zone-location-name (stringp zone-location-name) (not (string-empty-p zone-location-name)))
    (error "Argument 'zone-location-name' must be a non-empty string"))

  ;; --- Logic ---
  (let* (;; Convert the descriptive name to Rust constant convention
         (const-name-snake-case (my/lowercase-spaces-to-screaming-snake-case zone-location-name))
         ;; Construct the final prompt for the AI
         (full-prompt (format "%s%s\n--- END ZONE DESCRIPTION ---\n\n**Required Const Name:**\n%s"
                              my/rust-zone-prompt-prefix
                              zone-description-prompt
                              const-name-snake-case))
         ;; Expand filename and ensure it's available in the callback
         (output-file (expand-file-name output-filename)))

    ;; --- API Call ---
    (message "Sending request to GPTel for zone %s..." const-name-snake-case)
    (gptel-request `(:prompt ,full-prompt
                     ;; :model "..." ; Optional: specify a model if needed/supported
                     :stream nil ; Ensure we get the full response in the callback
                     ;; Define the callback function to process the response
                     :callback (lambda (response-plist)
                                 (let ((response-content (plist-get response-plist :response)))
                                   (if (or (null response-content) (string-empty-p response-content))
                                       (message "GPTel returned an empty response for %s." const-name-snake-case)
                                     ;; --- File Writing on Success ---
                                     (condition-case err
                                         ;; Extract only the code part
                                         (let ((rust-code (my/extract-rust-code-block response-content)))
                                           (if (string-empty-p rust-code)
                                               (message "Could not extract Rust code block from GPTel response for %s." const-name-snake-case)
                                             ;; Ensure the target directory exists
                                             (make-directory (file-name-directory output-file) :parents)
                                             ;; Write the extracted code to the specified file
                                             (with-temp-file output-file ; Atomically write file
                                               (insert rust-code)
                                               ;; Optionally add newline if missing
                                               (unless (eq (char-before (point-max)) ?\n)
                                                 (insert "\n")))
                                             (message "Rust zone code for '%s' written successfully to %s" zone-location-name output-file)))
                                       ;; Handle file writing errors
                                       (error (message "Error writing file %s for zone '%s': %s" output-file zone-location-name err)))))))
      ;; Confirmation message that the request was initiated
      (message "Request initiated for zone '%s'. Waiting for GPTel response..." zone-location-name)))


  ;; --- Example Programmatic Usage ---
  ;; (let ((description "A quiet asteroid cluster rich in SiliconCrystal.
  ;;                    No enemies. Some harvestable IronOre asteroids too.
  ;;                    Procedurally generate asteroids in a sphere radius 5000
  ;;                    centered at origin, density 0.008.
  ;;                    Possible loot from asteroids: SiliconCrystal (p=0.7, 10-50), IronOre (p=0.4, 20-100).")
  ;;       (filename "~/my_game/src/zones/quiet_cluster.rs")
  ;;       (zonename "quiet cluster alpha"))
  ;;   (my/generate-rust-zone description filename zonename))

  ;; (let ((description "A dangerous nebula passage. Procedurally generate
  ;;                    PlasmaCloud effects (non-colliding) in a box region.
  ;;                    Manually place 3 PirateInterceptor enemies on patrol.
  ;;                    Include one hidden LootContainer with id 'nebula_cache'.")
  ;;       (filename "~/my_game/src/zones/nebula_passage.rs")
  ;;       (zonename "nebula passage gamma"))
  ;;   (my/generate-rust-zone description filename zonename))
