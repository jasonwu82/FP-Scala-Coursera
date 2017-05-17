package funsets

object Main extends App {
  import FunSets._

  println(contains(singletonSet(1), 1))
  println(contains(singletonSet(1), 2))

  var s = singletonSet(-10)
  for(i <- -9 to 10){
    s = union(s,singletonSet(i))
  }
  def f1(x:Int) = x*3
  printSet(map(s,f1))
}
