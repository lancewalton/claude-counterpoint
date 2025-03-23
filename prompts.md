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
- Please remove the getValidNextNotes method
- The Melody.toList method should reverse the list. Also, the companion object apply method should take a varargs of notes and create a list, reverse it and then pass it to the Melody constructor.
- Create an Interval class that calculates the interval between a pair of notes.
- The interval class should know whether the interval is simple or compound
Let's try a simpler approach to intervals
Create a method in Melody to give is a set of valid notes that can be added next. There are rules for this which we'll add incrementally. The first rule is that any note added must be within an octave of the previous note.
