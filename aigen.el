;;; aigen.el -*- lexical-binding: t; -*-
;;


;; Ensure gptel is loaded and potentially cl-lib for convenience
(require 'gptel)
(require 'cl-lib)

;; --- Helper function to convert lowercase with spaces to SCREAMING_SNAKE_CASE ---
(defun my/lowercase-spaces-to-screaming-snake-case (s)
  "Convert lowercase string S with spaces to SCREAMING_SNAKE_CASE.
Example: \"my zone name\" -> \"MY_ZONE_NAME\""
  (when (and s (stringp s) (not (string-empty-p s)))
    (let* ((trimmed (string-trim s))
           (underscored (replace-regexp-in-string "[[:space:]]+" "_" trimmed)))
      (upcase underscored))))


;; --- Helper function to safely read file content ---
(defun my/read-file-content-safely (filename)
  "Read content of FILENAME relative to `default-directory`.
Return file content as string or nil if error."
  (let ((f (expand-file-name filename))) ; Ensure path is absolute for reading
    (condition-case err
        (when (file-readable-p f)
          (with-temp-buffer
            (insert-file-contents f)
            (buffer-string)))
      ;; If file error (e.g., not found, permissions), warn and return nil
      (file-error
       (message "Warning: Could not read context file '%s': %s" filename err)
       nil))))

;; --- Helper function to build the context string for the prompt ---
(defun my/build-context-code-section (context-files)
  "Build a formatted string containing code from CONTEXT-FILES for the prompt."
  (if (null context-files)
      "" ; Return empty string if no files provided
    (let ((context-string "\n\n--- START CONTEXT CODE ---\nContext files provide definitions relevant to the generation task:\n\n"))
      (dolist (f context-files)
        (let ((content (my/read-file-content-safely f)))
          (when content
            (setq context-string
                  (concat context-string
                          (format "**File: %s**\n```rust\n%s\n```\n\n"
                                  f ; Show the relative path provided
                                  content))))))
      (concat context-string "-- END CONTEXT CODE --\n\n"))))



;; --- The Prompt Prefix ---
;; Adjusted slightly to mention context comes first
(defconst my/rust-zone-prompt-prefix
  "Using the provided CONTEXT CODE from src/main.rs, generate a complete Rust code block defining a space game zone based on the specific description provided below.

**Output Requirements:**

1.  **CODE ONLY:** Provide ONLY the raw Rust code block required. Start directly with `pub const` or necessary `pub static` definitions (like patrol routes or loot tables if needed). Do NOT include ```rust markdown, introductory text, explanations, or closing remarks.
2.  **`use` Statements:** Start with necessary `use` statements to bring required types into scope. Infer paths from the CONTEXT CODE or assume common paths like `crate::game::objects::*`, `crate::types::*`, etc.
3.  **`static` Definitions (If Needed):** Define `pub static` loot tables or patrol routes BEFORE the main `Zone` const definition, referencing types from the context code.
4.  **Main Definition:** Define a single `pub const ZONE_NAME: Zone = Zone { ... };` where `ZONE_NAME` is the SCREAMING_SNAKE_CASE version specified later.
5.  **`Zone` Struct Fields:** Populate the `Zone` struct precisely, using types and helper functions defined in the CONTEXT CODE.
    * `name: &'static str`: User-friendly string.
    * `manual_objects: &'static [([f32; 3], ObjectKind)]`: Use coordinates `[f32; 3]` and construct `ObjectKind` ONLY using the helper `const fn`s (like `static_obj`, `harvestable`, etc.) found in the context code.
    * `procedural_params: Option<ProceduralParams>`: If `Some`, populate fields (`proc_object_type`, `region`, `density`, `scale` (radius range), `seed`, `possible_loot`) using types defined in context code. Reference `pub static` loot tables.
6.  **Validity & Style:** Ensure generated code is valid Rust, uses types/functions from context correctly, and follows standard formatting.

--- START ZONE DESCRIPTION ---
"
  "The detailed prompt prefix string explaining Rust zone code generation rules.")


;; --- Main Function (Hardcoded src/main.rs Context) ---
(defun my/generate-rust-zone (zone-description-prompt zone-location-name)
  "Generate Rust zone code via gptel, including src/main.rs as context.

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

  ;; --- Logic: Derive names, path, read context, build prompt ---
  (let* ((const-name-snake-case (my/lowercase-spaces-to-screaming-snake-case zone-location-name))
         (relative-output-file (format "src/aigen/%s.rs" const-name-snake-case))
         ;; Directly read src/main.rs content
         (main-rs-content (my/read-file-content-safely "src/main.rs"))
         ;; Format context string, handle potential read failure
         (context-code-section
          (if main-rs-content
              (format "\n\n--- START CONTEXT CODE FROM src/main.rs ---\n```rust\n%s\n```\n--- END CONTEXT CODE ---\n\n"
                      main-rs-content)
            ;; If src/main.rs couldn't be read, include a warning in the prompt *and* message buffer
            (progn (message "Warning: Could not read context file src/main.rs")
                   "\n\n--- CONTEXT CODE UNAVAILABLE (Could not read src/main.rs) ---\n\n")))
         ;; Construct the final prompt
         (full-prompt (format "%s%s%s\n--- END ZONE DESCRIPTION ---\n\n**Required Const Name:** %s"
                              context-code-section ; Context first
                              my/rust-zone-prompt-prefix
                              zone-description-prompt
                              const-name-snake-case)))

    ;; --- API Call ---
    (message "Sending request to GPTel for zone '%s' -> %s (with src/main.rs context)..."
             zone-location-name relative-output-file)
    (gptel-request
        full-prompt   ; Argument 1: The complete prompt string (positional)
      :callback      ; Keyword argument :callback
      ;; Value for :callback (accepts response and info)
      (lambda (response info)
        (if (stringp response)
            ;; --- Write raw response directly ---
            (progn
              (make-directory (file-name-directory relative-output-file) :parents)
              (with-temp-file relative-output-file
                (insert response) ; Insert the response string
                (unless (eq (char-before (point-max)) ?\n)
                  (insert "\n"))) ; Ensure trailing newline
              (message "Raw GPTel response for '%s' written successfully to %s"
                       zone-location-name relative-output-file))
          ;; Handle non-string responses
          (message "GPTel callback for '%s' received non-string response: %S (Info: %S)"
                   zone-location-name response info))) ; Log unexpected response/info
      ) ; End lambda
    ) ; End gptel-request call

  (message "Request initiated for zone '%s'. Waiting for GPTel response..." zone-location-name))




(my/generate-rust-zone
 "A treacherous ice field with unstable, cracking ice asteroids (visual effect). Contains harvestable RareIsotopes within some asteroids. Procedurally generate ice asteroids in a ring shape. Manually place 1 static listening post."
 "treacherous ice field")


;; --- Example Programmatic Usage ---
;; (my/generate-rust-zone
;;  "A treacherous ice field with unstable, cracking ice asteroids (visual effect). Contains harvestable RareIsotopes within some asteroids. Procedurally generate ice asteroids in a ring shape. Manually place 1 static listening post (sprite: ListeningPost)."
;;  "glacial drift sector")
