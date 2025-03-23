package counterpoint

@main def run(): Unit = 
  println("Hello world!")
  println(s"Welcome to claude-counterpoint ${sys.props.getOrElse("user.name", "")}!")
