# Instructions I Want Claude to Remember Between Invocations

* Append every prompt to the file called "prompts.md"
* Fully test all code produced
* Don't produce code unless it is needed
* Do not add any comments to the code
* Run all the tests to make sure everything works after (or during) doing the work for each prompt
* When the work is complete for each prompt, do "git add .", then commit using the pattern "Prompt: " followed by the prompt that caused the work to be done
* Keep the Main.scala file clean without debug printing code
* Keep parameters to higher order functions (filter, map, flatMap, etc.) simple by delegating complex logic to well-named methods

