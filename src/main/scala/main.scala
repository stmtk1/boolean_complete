import scala.math.pow

object IntBoolsConverter{
  def int_to_bools(rule_int: Int, size: Int) : Array[Boolean] = {
    val ret : Array[Boolean] = new Array[Boolean](size)
    var rule_var: Int = rule_int
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
    val rule_size: Int = pow(2, this.size).intValue
    val rule_array: Array[Boolean] = IntBoolsConverter.int_to_bools(rule_int, rule_size)
    x => {
      if(x.length != this.size) {
        //raise new Exception("Wrong Length of Array")
        // TODO
        // rase exception
      }
      rule_array(IntBoolsConverter.bools_to_int(x))
    }
  }
}

class Prover (init_size: Int, init_circuit: Array[Boolean] => Boolean){
  val circuit: Array[Boolean] => Boolean = init_circuit
  val size: Int = init_size
  
  def to_two_input(rule: Int) : (Boolean, Boolean) => Boolean = {
    val selector: Array[Boolean] = IntBoolsConverter.int_to_bools(rule, this.size)
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
    IntBoolsConverter.bools_to_int(ret.reverse)
  }
}

object main {
  def main(args: Array[String]) {
    val size = 3
    val a = new LogicGenerator(size)
    new Prover(size, a.gen_func(1)).comform_all_two_inputs()
  }
}
