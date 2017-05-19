// lecture 4.1 exercise
abstract class Nat {
    def isZero: Boolean
    def predecessor: Nat
    def successor: Nat
    def + (that: Nat): Nat = {
        if(!that.isZero){
            new Succ(this)+(that.predecessor)
        }
        else this

    }
    def - (that: Nat): Nat = {
        if(!that.isZero){
            this.predecessor - that.predecessor
        }
        else this
    }
}

object Zero extends Nat{
    def isZero = true

    override def predecessor: Nat = throw new NoSuchElementException

    override def successor: Nat = new Succ(this)

}
class Succ(n: Nat) extends Nat{
    val pre = n
    def isZero = false

    override def predecessor: Nat = n

    override def successor: Nat = new Succ(this)

}
Zero.isZero
new Succ(Zero)
val one = Zero.successor
val two = one.successor
val three = two.successor
val checkZero = three - three
checkZero.isZero

val checkOne = three - two
checkOne.isZero
val checkZero2 = checkOne - one
checkZero2.isZero