# Project Prompts

## 2025-03-23

- Please generate a skeleton Scala 3 project called "claude-counterpoint".
- Please generate the code to represent the Notes of the C major scale for G2 to G5.
- The C major scale has no sharps or flats. Can you remove them?
- Move NoteName, Note and Notes into another file called Note.scala
- The tests for Note and Notes should be moved to a file called NoteSpec
- Rename the Notes object to Note
- Create a class called Melody that contains a list of Notes. Provide an "add" method that prepends a note to the list.
- Make the notes field private and provide a method called "toList" that returns the reversed list of notes.
- Add a line to claude.md that indicates that no comments should be added to the code. Also remove all existing comments from the code.
- Add a method called "getValidNextNotes" to Melody that returns a set of notes according to the following rules:
