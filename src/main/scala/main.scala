import scala.math.pow

object IntBoolsConverter{
  def int_to_bools(rule_int: Int, size: Int) : Array[Boolean] = {
    val ret : Array[Boolean] = new Array[Boolean](size)
    var rule_var: Int = rule_int
//    for(i <- (ret.length - 1) to 0 by -1){
    for(i <- 0 until ret.length){
      ret(i) = rule_var % 2 == 1
      rule_var = rule_var / 2
    }
    ret
  }
  
  def bools_to_int(input: Array[Boolean]) : Int = {
    var ret: Int = 0
    for(i <- input.reverse) {
      ret *= 2
      if(i) {
        ret += 1
      }
    }
    ret
  }
}

class LogicGenerator (init_size: Int){
  val size: Int = init_size
  
  def gen_func(rule_int: Int) : Array[Boolean] => Boolean = {
    val rule_array: Array[Boolean] = int_to_rule_bools(rule_int)
    x => {
      if(x.length != this.size) {
        //raise new Exception("Wrong Length of Array")
      }
      rule_array(LogicGenerator.bools_to_int(x))
    }
  }
  
  def int_to_rule_bools(rule_int: Int) : Array[Boolean] = {
    val ret : Array[Boolean] = new Array[Boolean](pow(2, this.size).intValue)
    var rule_var: Int = rule_int
//    for(i <- (ret.length - 1) to 0 by -1){
    for(i <- 0 until ret.length){
      ret(i) = rule_var % 2 == 1
      rule_var = rule_var / 2
    }
    ret
  }
}

object LogicGenerator {
  def bools_to_int(input: Array[Boolean]) : Int = {
    var ret: Int = 0
    for(i <- input.reverse) {
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
  
  def comform_all_two_inputs(): Array[Boolean] = {
    val ret: Array[Boolean] = new Array[Boolean](16)
    for(i <- 0 until ret.length){
      ret(i) = false
    }
    for(i <- 0 until ret.length){
      val func_bit: Int = Prover.func_to_int(to_two_input(i))
      ret(func_bit) = true
    }
    ret
  }
}

object Prover {
  def func_to_int(two_inputs: (Boolean, Boolean) => Boolean) : Int = {
    val ret : Array[Boolean] = new Array[Boolean](4)
    ret(0) = two_inputs(false, false)
    ret(1) = two_inputs(false, true)
    ret(2) = two_inputs(true, false)
    ret(3) = two_inputs(true, true)
    LogicGenerator.bools_to_int(ret.reverse)
  }
}

object main {
  def main(args: Array[String]) {
    val size = 3
    val a = new LogicGenerator(size)
    new Prover(size, a.gen_func(1)).comform_all_two_inputs()
  }
}
