## Generate Fretboard Maps of Scales and Chords

This package draws "fretboard maps", positions of notes along the fretboard of a stringed instrument, such as a guitar. Typical uses would be to illustrate all the positions at which a particular chord or scale can be played, or relationships between scales, modes and chords. These functions have not been tested extensively except for 6-string guitar in standard tuning-- caveat emptor.

### Fretboard Map Functions:
Each of these functions generates a pdf file with sets of fretboards, chord or scale diagrams meant to illustrate particular relationships between the scales, chords and their notes.

* `chordShapePlots`: Plot major and minor chord shapes of a given root, individually and within the neck of a guitar with standard tuning. Optionally sevenths of each chord may be included. Six fretboards will be plotted.

* `scalePositionPlots`: Plots fretboards of a scale/mode, and its diatonic chords by position. On the first fretboard, all of the notes of the scale are shown for the whole neck. In the second column, the notes of the scale are shown broken out by CAGED position on the neck. Next the notes of the major pentatonic scale for each position are shown. The remaining seven columns show the CAGED chord shapes at each position, with the dominant 7th substituted for the diminished chord.

* `parallelModePlots`: Plots seven fretboard maps showing all the modes of a given tonic note,
ordered such that only a single note differs between successive modes.

### Low-Level Functions:

* `fretNotes`: Assign scale notes to positions of a fretboard given instrument parameters (number of strings, frets and tuning) and major or minor key.

* `diatonicChordNotes`: Find the notes of a diatonic chord within its scale, given the root note.

* `chordNotesByType`: Get notes of an arbitrary chord, by root note and chord type, where
"arbitrary" means not necessarily diatonic to the underlying scale that is specified by the first argument.

* `findPositions`: Find CAGED chord/scale positions along the fretboard for the present scale. (Position 1 is defined as the scale position where the chord build on the tonic has the E shape.)

* `makeFretboardString`: Convert a set of notes on a fretboard to a string representation.

* `drawNeck`: Calculate fret spacing and draw an empty fretboard.

* `plotFretboardByPosition`: Plot whole fretboard with positions separated by offset; join the frets in common.

* `drawNotes`: Add note markers and labels at particular locations to a fretboard plot.

