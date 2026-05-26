<!-- Synced copy. Canonical: ~/Documents/ObsidianVault/memory/writing-voice.md -->
# John Ehrlinger — Writing Voice Fingerprint

Reference for keeping documentation and prose in a consistent human voice.
Canonical copy lives in the Obsidian vault; package repos hold a synced copy.

## The voice in one line

Pedagogical and conversational: start from something the reader already knows,
build to the new idea with a concrete analogy, and don't be afraid of a little
personality or a slightly imperfect sentence.

## Two registers

**Narrative** (vignettes, README, roxygen @description/@details, methods prose)
- Open from the familiar: "Most readers are familiar with simple linear regression..."
- Carry one concrete analogy through a hard idea (a fruit basket, the blind men
  and the elephant, a "noise-reduction filter").
- Question-headed sections: "Why Cluster?", "Where Do We Stop?".
- First person plural: "in our practice", "we proceed". Address the reader as "you".
- Gloss terms inline in parentheses; scare-quote a piece of jargon the first
  time it appears ("rules", "elbow", "eyeballing").
- Start sentences with But / Yet / Thus / Now when it helps the flow.
- Vary sentence length. A short flat statement after a long one lands well.
- **Conversational, not chatty.** A colleague explaining the method at a
  whiteboard, not a blog post. Keep an analogy when it teaches; cut winking
  asides and cute phrasing. "no Tukey rule hiding in the middle" is too cute;
  "not the usual Tukey 1.5 IQR whiskers" is right. Plain and direct beats
  folksy. The reader is a peer, so don't perform for them.

**Terse** (roxygen @param/@return, NEWS bullets)
- Compressed, but still plain and concrete, not sterile.
- State the thing; skip the preamble. "Logical; if TRUE, ..." not "This
  argument controls whether...".
- No analogies, no question headers here; the voice shows in word choice.

## Rules

- Em-dashes: use sparingly. Native to the voice and honestly overused. Keep one
  where it earns the pause; otherwise a comma, parentheses, or a full stop.
- Ellipses: an informal-register habit (text, email). Keep them out of package docs.
- Don't overstate. No overselling. Cut "enhanced", "powerful", "seamlessly",
  "robust" (as a brag), "comprehensive". State what the thing does, at its size.
- Imperfection is allowed. Mild redundancy, an occasional long sentence: human
  texture, not an error to scrub. Don't polish to a glassy finish.
- Repetition that teaches is voice, not defect. Restating a concept, or a
  callback structure (state the problems up front, answer each later), is kept.
  Repeat when it clarifies; cut repetition that only fills space.

## AI tells to hunt and kill

- Mechanical parallelism: same-shaped sentences with no teaching purpose, lists
  padded to equal length. (Pedagogical repetition is NOT this; preserve it.)
- Flavorlessness: correct but with no analogy, no picture, nothing a person
  would actually say.
- Missing "we"/"you": abstract, authorless prose.
- Forced tricolons: "fast, simple, and reliable".
- Hedge-free sterile balance: "X. However, Y. It is worth noting Z."
- Overstatement (see Rules).
- "in order to", "it is important to note", "leverage", "utilize".

Punctuation density is NOT a tell. The tells are structural.

## Before / after

AI:   "Set per_class = TRUE to enable the computation of per-class one-vs-rest
       ROC curves, providing enhanced flexibility for multi-class analysis."
John: "Set per_class = TRUE and a multi-class forest gives you one ROC curve
       per class, each class scored against all the others."
