import scala.math.pow

class Prover (rule_int: Int, init_size: Int){
  val circuit: Array[Boolean] => Boolean = Prover.gen_init_func(rule_int, init_size)
  val size: Int = init_size
  
  def to_two_input(rule: Int) : Array[Boolean] => Boolean = {
    val selector: Array[Boolean] = Prover.int_to_bools(rule, this.size)
    x => {
      val input: Array[Boolean] = new Array[Boolean](this.size)
      for( i <- 0 until input.length){
        if(selector(i)){
          input(i) = x(1)
        }else{
          input(i) = x(0)
        }
      }
      circuit(input)
    }
  }
  
  def comform_all_two_inputs(): Int = {
    // 計算量削減できる
    val ret: Array[Boolean] = new Array[Boolean](16)
    for(i <- 0 until ret.length){
      ret(i) = false
    }
    for(i <- 0 until ret.length){
      val func_bit: Int = Prover.func_to_int(to_two_input(i), 2)
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
  
  def func_to_int(func: Array[Boolean] => Boolean, size: Int) : Int = {
    //val ret : Array[Boolean] = new Array[Boolean](4)
    val size_of_input: Int = pow(2, size).intValue
    val ret : Array[Boolean] = new Array[Boolean](size_of_input)
    /*
    ret(0) = two_inputs(false, false)
    ret(1) = two_inputs(true, false)
    ret(2) = two_inputs(false, true)
    ret(3) = two_inputs(true, true)
    */
   for(i <- 0 until size_of_input){
     ret(i) = func(int_to_bools(i, size))
   }
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
  
  def to_three_input(rule1: Int, rule2: Int) : Array[Boolean] => Boolean = {
    val func1 = Prover.gen_init_func(rule1, 2)
    val func2 = Prover.gen_init_func(rule2, 2)
    x => {
      if(x.length != 3){
        throw new Exception("引数の大きさが違います")
      }
      val b: Boolean = func1(Array(x(1), x(2)))
      func2(Array(x(0), b))
    }
  }
  
  def composite_to_twos(rule1: Int, rule2: Int) : Int = {
    1
  }
}

object main {
  def main(args: Array[String]) {
    val func = Prover.to_three_input(7, 7)
    println(func(Array(false, false, false)))
    println(func(Array(true, false, false)))
    println(func(Array(false, true, false)))
    println(func(Array(true, true, false)))
    println(func(Array(false, false, true)))
    println(func(Array(true, false, true)))
    println(func(Array(false, true, true)))
    println(func(Array(true, true, true)))
  }
}
