import scala.math.pow

class Prover (rule_int: Int, init_size: Int){
  val circuit: Array[Boolean] => Boolean = Prover.gen_init_func(rule_int, init_size)
  val size: Int = init_size
  
  def to_two_input(rule: Int) : (Boolean, Boolean) => Boolean = {
    val selector: Array[Boolean] = Prover.int_to_bools(rule, this.size)
    (x, y) => {
      val input: Array[Boolean] = new Array[Boolean](this.size)
      for( i <- 0 until input.length){
        if(selector(i)){
          input(i) = y
        }else{
          input(i) = x
        }
      }
      circuit(input)
    }
  }
  
  def comform_all_two_inputs(): Int = {
    val ret: Array[Boolean] = new Array[Boolean](16)
    for(i <- 0 until ret.length){
      ret(i) = false
    }
    for(i <- 0 until ret.length){
      val func_bit: Int = Prover.func_to_int(to_two_input(i))
      ret(func_bit) = true
    }
    Prover.bools_to_int(ret)
  }
}

object Prover {
  def gen_init_func(rule: Int, size: Int) : Array[Boolean] => Boolean = {
    val rule_size: Int = pow(2, size).intValue
    val rule_array: Array[Boolean] = Prover.int_to_bools(rule, rule_size)
    x => {
      if(x.length != size) {
        throw new Exception("引数の長さ違います")
        //raise new Exception("Wrong Length of Array")
      }
      rule_array(Prover.bools_to_int(x))
    }
  }
  
  def func_to_int(two_inputs: (Boolean, Boolean) => Boolean) : Int = {
    val ret : Array[Boolean] = new Array[Boolean](4)
    ret(0) = two_inputs(false, false)
    ret(1) = two_inputs(true, false)
    ret(2) = two_inputs(false, true)
    ret(3) = two_inputs(true, true)
    Prover.bools_to_int(ret)
  }

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

object main {
  def main(args: Array[String]) {
    val size = 3
    val b :Int = new Prover(127, size).comform_all_two_inputs
    println(b)
  }
}
