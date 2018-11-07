import scala.math.pow

class LogicGenerator (init_size: Int){
  val size: Int = init_size
  
  def gen_func(rule_int: Int) : Array[Boolean] => Boolean = {
    val rule_array: Array[Boolean] = int_to_rule_bools(rule_int)
    x => {
      rule_array(LogicGenerator.bools_to_int(x))
    }
  }
  
  def int_to_rule_bools(rule_int: Int) : Array[Boolean] = {
    val ret : Array[Boolean] = new Array[Boolean](pow(2, this.size).intValue)
    var rule_var: Int = rule_int
    for(i <- (ret.length - 1) to 0 by -1){
      ret(i) = rule_var % 2 == 1
      rule_var = rule_var / 2
    }
    ret
  }
}

object LogicGenerator {
  def bools_to_int(input: Array[Boolean]) : Int = {
    var ret: Int = 0
    for(i <- input) {
      ret *= 2
      if(i) {
        ret += 1
      }
    }
    ret
  }
}

class Prover (init_size: Int, init_circuit: Array[Boolean] => Boolean){
  val circuit: Array[Boolean] => Boolean = init_circuit
  val size: Int = init_size
  
  def to_two_input(rule: Int) : (Boolean, Boolean) => Boolean = {
    val selector: Array[Boolean] = int_to_bools(rule)
    for(b <- selector){
      //println(b)
    }
    //println
    (x, y) => {
      val input: Array[Boolean] = new Array[Boolean](this.size)
      for( i <- 0 until input.length){
        if(selector(i)){
          input(i) = x
        }else{
          input(i) = y
        }
      }
      circuit(input)
    }
  }
  
  def int_to_bools(rule_int: Int) : Array[Boolean] = {
    var rule: Int = rule_int
    var ret: Array[Boolean] = new Array[Boolean](this.size)
    for(i <- (ret.length - 1) to 0 by -1){
      ret(i) = rule % 2 == 1
      rule /= 2
    }
    ret
  }
}

object main {
  def main(args: Array[String]) {
    val size = 3
    val a = new LogicGenerator(size)
    for(i <- 0 until 8){
      new Prover(size, a.gen_func(i))
    }
    for(i <- 0 until 8){
      val b :Boolean = new Prover(size, a.gen_func(12)).to_two_input(i)(false, true)
      println(b)
    }
  }
}
