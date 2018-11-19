import scala.math.pow

class Prover (rule_int: Int, init_size: Int){
  val size: Int = init_size
  val rule: Int = rule_int
  val circuit: Array[Boolean] => Boolean = gen_func()
  
  def this(init_circuit: Array[Boolean] => Boolean, init_size: Int){
    this(Prover.func_to_int(init_circuit, init_size), init_size)
  }
  
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
  
  def gen_func() : Array[Boolean] => Boolean = {
    val rule_size: Int = pow(2, this.size).intValue
    val rule_array: Array[Boolean] = Prover.int_to_bools(this.rule, rule_size)
    x => {
      if(x.length != this.size) {
        throw new Exception("引数の長さ違います")
        //raise new Exception("Wrong Length of Array")
      }
      rule_array(Prover.bools_to_int(x))
    }
  }
}

object Prover {
  def func_to_int(func: Array[Boolean] => Boolean, size: Int) : Int = {
    val size_of_input: Int = pow(2, size).intValue
    val ret : Array[Boolean] = new Array[Boolean](size_of_input)
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
    val func1 = new Prover(rule1, 2).circuit
    val func2 = new Prover(rule2, 2).circuit
    x => {
      if(x.length != 3){
        throw new Exception("引数の大きさが違います")
      }
      val b: Boolean = func1(Array(x(1), x(2)))
      func2(Array(x(0), b))
    }
  }
  
  def to_four_input(rule1: Int, rule2: Int, rule3: Int) : Array[Boolean] => Boolean = {
    val func1 = new Prover(rule1, 2).circuit
    val func2 = new Prover(rule2, 2).circuit
    val func3 = new Prover(rule3, 2).circuit
    x => {
      if(x.length != 4){
        throw new Exception("引数の大きさが違います")
      }
      val b1: Boolean = func1(Array(x(1), x(2)))
      val b2: Boolean = func2(Array(x(1), x(2)))
      func3(Array(b1, b2))
    }
  }
  
  def composite_to_twos(rule1: Int, rule2: Int, rule3: Int) : Int = {
    val func_rule = Prover.func_to_int(Prover.to_four_input(rule1, rule2, rule3), 4)
    new Prover(func_rule, 4).comform_all_two_inputs
  }
  
  def check_all_three() : Array[Int] = {
    val ret: Array[Int] = new Array[Int](255)
    for(i <- 0 until ret.length){
      ret(i) = new Prover(i, 3).comform_all_two_inputs()
    }
    var cont: Boolean = true // continue
    while(cont){
      cont = false
      for(i <- 0 until ret.length){
        val origin = ret(i)
        ret(i) |= combine_all_two(origin)
        if(ret(i) != origin){
          cont = true
        }
      }
    }
    ret
  }
  
  def combine_all_two(created_bit: Int) : Int = {
    var ret : Int = created_bit
    for(i <- 0 until 16 if ((created_bit >> i) & 1) == 1){
      for(j <- 0 until 16 if ((created_bit >> j) & 1) == 1){
        for(k <- 0 until 16 if ((created_bit >> k) & 1) == 1){
          ret |= composite_to_twos(i, j, k)
        }
      }
    }
    ret
  }
}

object main {
  def main(args: Array[String]) {
    val a = Prover.check_all_three()
    for(i <- 0 until a.length){
      print(i)
      print(" ")
      println(a(i))
    }
  }
}
