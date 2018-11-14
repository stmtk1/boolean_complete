import org.scalatest.FunSpec

class IntBoolsConverterTest extends FunSpec {
  describe("bools_to_int"){
    it("長さ1"){
      assert(Prover.bools_to_int(Array(false)) == 0)
      assert(Prover.bools_to_int(Array(true)) == 1)
    }
    
    it("長さ２"){
      assert(Prover.bools_to_int(Array(false, false)) == 0)
      assert(Prover.bools_to_int(Array(true, false)) == 1)
      assert(Prover.bools_to_int(Array(false, true)) == 2)
      assert(Prover.bools_to_int(Array(true, true)) == 3)
    }
    it("長さ３"){
      assert(Prover.bools_to_int(Array(false, false, false)) == 0)
      assert(Prover.bools_to_int(Array(true, false, false)) == 1)
      assert(Prover.bools_to_int(Array(false, true, false)) == 2)
      assert(Prover.bools_to_int(Array(true, true, false)) == 3)
      assert(Prover.bools_to_int(Array(false, false, true)) == 4)
      assert(Prover.bools_to_int(Array(true, false, true)) == 5)
      assert(Prover.bools_to_int(Array(false, true, true)) == 6)
      assert(Prover.bools_to_int(Array(true, true, true)) == 7)
    }
    
    it("長さ４"){
      assert(Prover.bools_to_int(Array(false, false, false, false)) == 0)
      assert(Prover.bools_to_int(Array(true, true, true, true)) == 15)
    }
  }
  
  describe("int_to_bools"){
    it("size 1"){
      val size : Int = 1
      assert(Prover.int_to_bools(0, size).sameElements(Array(false)))
      assert(Prover.int_to_bools(1, size).sameElements(Array(true)))
    }
    
    it("size 2"){
      val size : Int = 2
      assert(Prover.int_to_bools(0, size).sameElements(Array(false, false)))
      assert(Prover.int_to_bools(1, size).sameElements(Array(true, false)))
      assert(Prover.int_to_bools(2, size).sameElements(Array(false, true)))
      assert(Prover.int_to_bools(3, size).sameElements(Array(true, true)))
    }
    
    it("size 3"){
      val size : Int = 3
      assert(Prover.int_to_bools(0, size).sameElements(Array(false, false, false)))
      assert(Prover.int_to_bools(1, size).sameElements(Array(true, false, false)))
      assert(Prover.int_to_bools(2, size).sameElements(Array(false, true, false)))
      assert(Prover.int_to_bools(4, size).sameElements(Array(false, false, true)))
      assert(Prover.int_to_bools(7, size).sameElements(Array(true, true, true)))
    }
    
    it("size 4"){
      val size : Int = 4
      assert(Prover.int_to_bools(0, size).sameElements(Array(false, false, false, false)))
      assert(Prover.int_to_bools(1, size).sameElements(Array(true, false, false, false)))
      assert(Prover.int_to_bools(2, size).sameElements(Array(false, true, false, false)))
      assert(Prover.int_to_bools(3, size).sameElements(Array(true, true, false, false)))
      assert(Prover.int_to_bools(4, size).sameElements(Array(false, false, true, false)))
      assert(Prover.int_to_bools(5, size).sameElements(Array(true, false, true, false)))
      assert(Prover.int_to_bools(6, size).sameElements(Array(false, true, true, false)))
      assert(Prover.int_to_bools(7, size).sameElements(Array(true, true, true, false)))
      assert(Prover.int_to_bools(8, size).sameElements(Array(false, false, false, true)))
      assert(Prover.int_to_bools(9, size).sameElements(Array(true, false, false, true)))
      assert(Prover.int_to_bools(10, size).sameElements(Array(false, true, false, true)))
      assert(Prover.int_to_bools(11, size).sameElements(Array(true, true, false, true)))
      assert(Prover.int_to_bools(12, size).sameElements(Array(false, false, true, true)))
      assert(Prover.int_to_bools(13, size).sameElements(Array(true, false, true, true)))
      assert(Prover.int_to_bools(13, size).sameElements(Array(true, false, true, true)))
      assert(Prover.int_to_bools(14, size).sameElements(Array(false, true, true, true)))
      assert(Prover.int_to_bools(15, size).sameElements(Array(true, true, true, true)))
    }
  }

  describe("Int -> Int 結合テスト"){
    it("0 -> 0"){
      assert(Prover.bools_to_int((Prover.int_to_bools(0, 1))) == 0)
      assert(Prover.bools_to_int((Prover.int_to_bools(0, 2))) == 0)
      assert(Prover.bools_to_int((Prover.int_to_bools(0, 3))) == 0)
      assert(Prover.bools_to_int((Prover.int_to_bools(0, 4))) == 0)
    }
    
    it("1 -> 1"){
      assert(Prover.bools_to_int((Prover.int_to_bools(1, 1))) == 1)
      assert(Prover.bools_to_int((Prover.int_to_bools(1, 2))) == 1)
      assert(Prover.bools_to_int((Prover.int_to_bools(1, 3))) == 1)
      assert(Prover.bools_to_int((Prover.int_to_bools(1, 4))) == 1)
    }

    it("all true"){
      assert(Prover.bools_to_int((Prover.int_to_bools(3, 2))) == 3)
      assert(Prover.bools_to_int((Prover.int_to_bools(7, 3))) == 7)
      assert(Prover.bools_to_int((Prover.int_to_bools(15, 4))) == 15)
      assert(Prover.bools_to_int((Prover.int_to_bools(31, 5))) == 31)
    }
  }
  describe("Bools -> Bools 結合テスト"){
    describe("all false"){
      it("size 1"){
        val input: Array[Boolean] = Array(false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 1).sameElements(input))
      }
      
      it("size 2"){
        val input: Array[Boolean] = Array(false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 2).sameElements(input))
      }
      
      it("size 3"){
        val input: Array[Boolean] = Array(false, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
    }
    
    describe("all true"){
      it("size 1"){
        val input: Array[Boolean] = Array(true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 1).sameElements(input))
      }
      
      it("size 2"){
        val input: Array[Boolean] = Array(true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 2).sameElements(input))
      }
      
      it("size 3"){
        val input: Array[Boolean] = Array(true, true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
    }
    
    describe("先頭だけtrue"){
      it("size 2"){
        val input: Array[Boolean] = Array(true, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 2).sameElements(input))
      }
      
      it("size 3"){
        val input: Array[Boolean] = Array(true, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
      
      it("size 4"){
        val input: Array[Boolean] = Array(true, false, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 4).sameElements(input))
      }
      
      it("size 5"){
        val input: Array[Boolean] = Array(true, false, false, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 5).sameElements(input))
      }
    }
    
    describe("２番目だけtrue"){
      it("size 3"){
        val input: Array[Boolean] = Array(false, true, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
      
      it("size 4"){
        val input: Array[Boolean] = Array(false, true, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 4).sameElements(input))
      }
      
      it("size 5"){
        val input: Array[Boolean] = Array(false, true, false, false, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 5).sameElements(input))
      }
    }
    
    describe("先頭だけfalse"){
      it("size 2"){
        val input: Array[Boolean] = Array(false, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 2).sameElements(input))
      }
      
      it("size 3"){
        val input: Array[Boolean] = Array(false, true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
      
      it("size 4"){
        val input: Array[Boolean] = Array(false, true, true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 4).sameElements(input))
      }
    }
    
    describe("最後だけfalse"){
      it("size 3"){
        val input: Array[Boolean] = Array(true, true, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
      
      it("size 4"){
        val input: Array[Boolean] = Array(true, true, true, false)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 4).sameElements(input))
      }
    }
    
    describe("２番目だけfalse"){
      it("size 3"){
        val input: Array[Boolean] = Array(true, false, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 3).sameElements(input))
      }
      
      it("size 4"){
        val input: Array[Boolean] = Array(true, false, true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 4).sameElements(input))
      }
      
      it("size 5"){
        val input: Array[Boolean] = Array(true, false, true, true, true)
        assert(Prover.int_to_bools((Prover.bools_to_int(input)), 5).sameElements(input))
      }
    }
  }
}

class GenFuncTest extends FunSpec {
  describe("size1"){
    val one_true: Array[Boolean] = Array(true)
    val one_false: Array[Boolean] = Array(false)
    describe("ルール0は常にfalse"){
      val func: Array[Boolean] => Boolean = new Prover(0, 1).gen_func()
      assert(func(one_true) == false)
      assert(func(one_false) == false)
    }
    
    describe("ルール1はnot"){
      val func: Array[Boolean] => Boolean = new Prover(1, 1).gen_func()
      assert(func(one_true) == false)
      assert(func(one_false) == true)
    }
    
    describe("ルール2はidentity funtion"){
      val func: Array[Boolean] => Boolean = new Prover(2, 1).gen_func()
      assert(func(one_true) == true)
      assert(func(one_false) == false)
    }
    
    describe("ルール3は常にtrue"){
      val func: Array[Boolean] => Boolean = new Prover(3, 1).gen_func()
      assert(func(one_true) == true)
      assert(func(one_false) == true)
    }
  }
  
  describe("size2"){
    val all_true: Array[Boolean] = Array(true, true)
    val all_false: Array[Boolean] = Array(false, false)
    val true_false: Array[Boolean] = Array(true, false)
    val false_true: Array[Boolean] = Array(false, true)
    
    it("ルール0は常にfalse"){
      val func: Array[Boolean] => Boolean = new Prover(0, 2).gen_func()
      assert(func(all_true) == false)
      assert(func(false_true) == false)
      assert(func(true_false) == false)
      assert(func(all_false) == false)
    }
    
    it("ルール8はand"){
      val func: Array[Boolean] => Boolean = new Prover(8, 2).gen_func()
      assert(func(all_true) == true)
      assert(func(false_true) == false)
      assert(func(true_false) == false)
      assert(func(all_false) == false)
    }
    
    it("ルール14はor"){
      val func: Array[Boolean] => Boolean = new Prover(14, 2).gen_func()
      assert(func(all_true) == true)
      assert(func(false_true) == true)
      assert(func(true_false) == true)
      assert(func(all_false) == false)
    }
    
    it("ルール6はxor"){
      val func: Array[Boolean] => Boolean = new Prover(6, 2).gen_func()
      assert(func(all_true) == false)
      assert(func(false_true) == true)
      assert(func(true_false) == true)
      assert(func(all_false) == false)
    }
    
    it("ルール7はnand"){
      val func: Array[Boolean] => Boolean = new Prover(7, 2).gen_func()
      assert(func(all_true) == false)
      assert(func(false_true) == true)
      assert(func(true_false) == true)
      assert(func(all_false) == true)
    }
    
    it("ルール1はnor"){
      val func: Array[Boolean] => Boolean = new Prover(1, 2).gen_func()
      assert(func(all_true) == false)
      assert(func(false_true) == false)
      assert(func(true_false) == false)
      assert(func(all_false) == true)
    }
    
    it("ルール9はxnor"){
      val func: Array[Boolean] => Boolean = new Prover(9, 2).gen_func()
      assert(func(all_true) == true)
      assert(func(false_true) == false)
      assert(func(true_false) == false)
      assert(func(all_false) == true)
    }
    
    it("ルール10は先頭そのまま出力"){
      val func: Array[Boolean] => Boolean = new Prover(10, 2).gen_func()
      assert(func(all_false) == false)
      assert(func(true_false) == true)
      assert(func(false_true) == false)
      assert(func(all_true) == true)
    }
    
    it("ルール12は最後そのまま出力"){
      val func: Array[Boolean] => Boolean = new Prover(12, 2).gen_func()
      assert(func(all_false) == false)
      assert(func(true_false) == false)
      assert(func(false_true) == true)
      assert(func(all_true) == true)
    }
    
    it("ルール15は常にtrue"){
      val func: Array[Boolean] => Boolean = new Prover(15, 2).gen_func()
      assert(func(all_true) == true)
      assert(func(true_false) == true)
      assert(func(false_true) == true)
      assert(func(all_false) == true)
    }
  }
  
  describe("size 3"){
    val input0: Array[Boolean] = Array(false, false, false)
    val input1: Array[Boolean] = Array(false, false, true)
    val input2: Array[Boolean] = Array(false, true, false)
    val input3: Array[Boolean] = Array(false, true, true)
    val input4: Array[Boolean] = Array(true, false, false)
    val input5: Array[Boolean] = Array(true, false, true)
    val input6: Array[Boolean] = Array(true, true, false)
    val input7: Array[Boolean] = Array(true, true, true)

    it("常にtrue"){
      val func :Array[Boolean] => Boolean = new Prover(255, 3).gen_func()
      assert(func(input0) == true)
      assert(func(input1) == true)
      assert(func(input2) == true)
      assert(func(input3) == true)
      assert(func(input4) == true)
      assert(func(input5) == true)
      assert(func(input6) == true)
      assert(func(input7) == true)
    }
    
    it("常にfalse"){
      val func :Array[Boolean] => Boolean = new Prover(0, 3).gen_func()
      assert(func(input0) == false)
      assert(func(input1) == false)
      assert(func(input2) == false)
      assert(func(input3) == false)
      assert(func(input4) == false)
      assert(func(input5) == false)
      assert(func(input6) == false)
      assert(func(input7) == false)
    }
  }
}

class FuncToIntTest extends FunSpec {
  it("常にfalse"){
    val func : Array[Boolean] => Boolean = _ => false
    assert(Prover.func_to_int(func, 2) == 0)
  }
  
  it("norは1"){
    val func : Array[Boolean] => Boolean = x => { !(x(0) || x(1)) }
    assert(Prover.func_to_int(func, 2) == 1)
  }
  
  it("xorは6"){
    val func : Array[Boolean] => Boolean = x => { x(0) ^ x(1) }
    assert(Prover.func_to_int(func, 2) == 6)
  }
  
  it("nandは7"){
    val func : Array[Boolean] => Boolean = x => { !(x(0) && x(1)) }
    assert(Prover.func_to_int(func, 2) == 7)
  }
  
  it("nandは8"){
    val func : Array[Boolean] => Boolean = x => { x(0) && x(1) }
    assert(Prover.func_to_int(func, 2) == 8)
  }
  
  it("xnorは9"){
    val func : Array[Boolean] => Boolean = x => { !(x(0) ^ x(1)) }
    assert(Prover.func_to_int(func, 2) == 9)
  }
  
  it("norは14"){
    val func : Array[Boolean] => Boolean = x => { x(0) || x(1) }
    assert(Prover.func_to_int(func, 2) == 14)
  }
  
  it("常にtrueは15"){
    val func : Array[Boolean] => Boolean = _ => { true }
    assert(Prover.func_to_int(func, 2) == 15)
  }
  
  it("左そのまま"){
    val func : Array[Boolean] => Boolean = x => x(0)
    assert(Prover.func_to_int(func, 2) == 10)
  }
  
  it("右そのまま"){
    val func : Array[Boolean] => Boolean = x => x(1)
    assert(Prover.func_to_int(func, 2) == 12)
  }
}

class RuleToRuleCombineTest extends FunSpec {
  it("Int -> Int"){
    assert(Prover.func_to_int(new Prover(0, 2).circuit, 2) == 0)
    assert(Prover.func_to_int(new Prover(1, 2).circuit, 2) == 1)
    assert(Prover.func_to_int(new Prover(2, 2).circuit, 2) == 2)
    assert(Prover.func_to_int(new Prover(3, 2).circuit, 2) == 3)
    assert(Prover.func_to_int(new Prover(4, 2).circuit, 2) == 4)
    assert(Prover.func_to_int(new Prover(5, 2).circuit, 2) == 5)
    assert(Prover.func_to_int(new Prover(6, 2).circuit, 2) == 6)
    assert(Prover.func_to_int(new Prover(7, 2).circuit, 2) == 7)
    assert(Prover.func_to_int(new Prover(8, 2).circuit, 2) == 8)
    assert(Prover.func_to_int(new Prover(9, 2).circuit, 2) == 9)
    assert(Prover.func_to_int(new Prover(10, 2).circuit, 2) == 10)
    assert(Prover.func_to_int(new Prover(11, 2).circuit, 2) == 11)
    assert(Prover.func_to_int(new Prover(12, 2).circuit, 2) == 12)
    assert(Prover.func_to_int(new Prover(13, 2).circuit, 2) == 13)
    assert(Prover.func_to_int(new Prover(14, 2).circuit, 2) == 14)
    assert(Prover.func_to_int(new Prover(15, 2).circuit, 2) == 15)
  }
  
  describe("Func -> Func"){
    it("常にfalse"){
      val func: Array[Boolean] => Boolean = _ => false
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == false)
      assert(checked_func(Array(false, true)) == false)
      assert(checked_func(Array(true, true)) == false)
    }
    
    it("常にtrue"){
      val func: Array[Boolean] => Boolean =  _ => true
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == true)
      assert(checked_func(Array(true, false)) == true)
      assert(checked_func(Array(false, true)) == true)
      assert(checked_func(Array(true, true)) == true)
    }
    
    it("and"){
      val func: Array[Boolean] => Boolean = x => { x(0) && x(1) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == false)
      assert(checked_func(Array(false, true)) == false)
      assert(checked_func(Array(true, true)) == true)
    }
    
    it("or"){
      val func: Array[Boolean] => Boolean = x => { x(0) || x(1) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == true)
      assert(checked_func(Array(false, true)) == true)
      assert(checked_func(Array(true, true)) == true)
    }
    
    it("xor"){
      val func: Array[Boolean] => Boolean = x => { x(0) ^ x(1) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == true)
      assert(checked_func(Array(false, true)) == true)
      assert(checked_func(Array(true, true)) == false)
    }
    
    it("nand"){
      val func: Array[Boolean] => Boolean = x => { !(x(0) && x(1)) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == true)
      assert(checked_func(Array(true, false)) == true)
      assert(checked_func(Array(false, true)) == true)
      assert(checked_func(Array(true, true)) == false)
    }
    
    it("nor"){
      val func: Array[Boolean] => Boolean = x => { !(x(0) || x(1)) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == true)
      assert(checked_func(Array(true, false)) == false)
      assert(checked_func(Array(false, true)) == false)
      assert(checked_func(Array(true, true)) == false)
    }
    
    it("xnor"){
      val func: Array[Boolean] => Boolean = x => { !(x(0) ^ x(1)) }
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == true)
      assert(checked_func(Array(true, false)) == false)
      assert(checked_func(Array(false, true)) == false)
      assert(checked_func(Array(true, true)) == true)
    }
    
    it("左側をそのまま"){
      val func: Array[Boolean] => Boolean = x => x(0)
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == true)
      assert(checked_func(Array(false, true)) == false)
      assert(checked_func(Array(true, true)) == true)
    }
    
    it("右側をそのまま"){
      val func: Array[Boolean] => Boolean = x => x(1)
      val checked_func: Array[Boolean] => Boolean = new Prover(Prover.func_to_int(func, 2), 2).circuit
      assert(checked_func(Array(false, false)) == false)
      assert(checked_func(Array(true, false)) == false)
      assert(checked_func(Array(false, true)) == true)
      assert(checked_func(Array(true, true)) == true)
    }
  }
}

class ProverConstructorTest extends FunSpec {
  it("第２引数はsize"){
    assert((new Prover(0, 1)).size == 1)
    assert((new Prover(0, 2)).size == 2)
    assert((new Prover(0, 3)).size == 3)
  }
  
  describe("第一引数はルール"){
    it("0にして常にfalseになるか確認"){
      val func = new Prover(0, 2).circuit
      assert(func(Array(false, false)) == false)
      assert(func(Array(true, false)) == false)
      assert(func(Array(false, true)) == false)
      assert(func(Array(true, true)) == false)
    }
    
    it("size2で常にtrueになるか確認"){
      val func = new Prover(15, 2).circuit
      assert(func(Array(false, false)) == true)
      assert(func(Array(true, false)) == true)
      assert(func(Array(false, true)) == true)
      assert(func(Array(true, true)) == true)
    }
    
    it("size3で常にtrueになるか確認"){
      val func = new Prover(255, 3).circuit
      assert(func(Array(false, false, false)) == true)
      assert(func(Array(true, false, false)) == true)
      assert(func(Array(false, true, false)) == true)
      assert(func(Array(true, true, false)) == true)
      assert(func(Array(false, false, true)) == true)
      assert(func(Array(true, false, true)) == true)
      assert(func(Array(false, true, true)) == true)
      assert(func(Array(true, true, true)) == true)
    }
  }
  
  describe("sizeと引数のlengthが違うと例外発生"){
    it("size 1"){
      intercept[Exception]{
        new Prover(0, 1).circuit(Array(true, true))
      }
    }
    
    it("size 2"){
      intercept[Exception]{
        new Prover(0, 2).circuit(Array(true))
      }
    }
    
    it("size 3"){
      intercept[Exception]{
        new Prover(0, 3).circuit(Array(true))
      }
    }
  }
}

class ToTwoInputTest extends FunSpec {
  describe("size 3"){
    describe("3入力nand"){
      val p: Prover = new Prover(127, 3)
      it("ルール1はnandのまま"){
        val func = p.to_two_input(1)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == true)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール2はnandのまま"){
        val func = p.to_two_input(2)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == true)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール7はnot"){
        val func = p.to_two_input(7)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == true)
        assert(func(Array(false, true)) == false)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール0はnot"){
        val func = p.to_two_input(0)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == false)
      }
    }
    describe("3入力nor"){
      val p: Prover = new Prover(1, 3)
      it("ルール1はnorのまま"){
        val func = p.to_two_input(1)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == false)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール2はnorのまま"){
        val func = p.to_two_input(2)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == false)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール7はnot"){
        val func = p.to_two_input(7)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == true)
        assert(func(Array(false, true)) == false)
        assert(func(Array(true, true)) == false)
      }
      
      it("ルール0はnot"){
        val func = p.to_two_input(0)
        assert(func(Array(false, false)) == true)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == false)
      }
    }
    describe("一番左の入力だけに依存する関数"){
      val p = new Prover(240, 3)
      it("左に依存"){
        val func = p.to_two_input(0)
        assert(func(Array(false, false)) == false)
        assert(func(Array(true, false)) == true)
        assert(func(Array(false, true)) == false)
        assert(func(Array(true, true)) == true)
      }
      
      it("右に依存"){
        val func = p.to_two_input(7)
        assert(func(Array(false, false)) == false)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == true)
      }
      
      it("ルール1も右に依存"){
        val func = p.to_two_input(4)
        assert(func(Array(false, false)) == false)
        assert(func(Array(true, false)) == false)
        assert(func(Array(false, true)) == true)
        assert(func(Array(true, true)) == true)
      }
    }
  }
}

class ComfirmAllTwoInputTest extends FunSpec {
  it("nand"){
    // not, nandが実装できる
    assert(new Prover(127, 3).comform_all_two_inputs() == 168)
  }
  
  it("nor"){
    // not, norが実装できる
    assert(new Prover(1, 3).comform_all_two_inputs() == 42)
  }
  
  it("and"){
    // and, idが実装できる
    assert(new Prover(128, 3).comform_all_two_inputs() == 5376)
  }
  
  it("or"){
    // or, idが実装できる
    assert(new Prover(254, 3).comform_all_two_inputs() == 21504)
  }
  
  it("id"){
    // idが実装できる
    assert(new Prover(240, 3).comform_all_two_inputs() == 5120)
  }
  
  it("常にfalse"){
    assert(new Prover(0, 3).comform_all_two_inputs() == 1)
  }
  
  it("常にtrue"){
    assert(new Prover(255, 3).comform_all_two_inputs() == 32768)
  }
}

class ToThreeInputTest extends FunSpec {
  describe("一つ目がor"){
    it("定数関数(true)との合成は常にtrue"){
      val func = Prover.to_three_input(15, 14)
      assert(func(Array(false, false, false)) == true)
      assert(func(Array(true, false, false)) == true)
      assert(func(Array(false, true, false)) == true)
      assert(func(Array(true, true, false)) == true)
      assert(func(Array(false, false, true)) == true)
      assert(func(Array(true, false, true)) == true)
      assert(func(Array(false, true, true)) == true)
      assert(func(Array(true, true, true)) == true)
    }
    it("定数関数(false)との合成はid"){
      val func = Prover.to_three_input(0, 14)
      assert(func(Array(false, false, false)) == false)
      assert(func(Array(true, false, false)) == true)
      assert(func(Array(false, true, false)) == false)
      assert(func(Array(true, true, false)) == true)
      assert(func(Array(false, false, true)) == false)
      assert(func(Array(true, false, true)) == true)
      assert(func(Array(false, true, true)) == false)
      assert(func(Array(true, true, true)) == true)
    }
    
    it("順番変えると常にfalse"){
      val func = Prover.to_three_input(14, 0)
      assert(func(Array(false, false, false)) == false)
      assert(func(Array(true, false, false)) == false)
      assert(func(Array(false, true, false)) == false)
      assert(func(Array(true, true, false)) == false)
      assert(func(Array(false, false, true)) == false)
      assert(func(Array(true, false, true)) == false)
      assert(func(Array(false, true, true)) == false)
      assert(func(Array(true, true, true)) == false)
    }
    
    it("3入力or"){
      val func = Prover.to_three_input(14, 14)
      assert(func(Array(false, false, false)) == false)
      assert(func(Array(true, false, false)) == true)
      assert(func(Array(false, true, false)) == true)
      assert(func(Array(true, true, false)) == true)
      assert(func(Array(false, false, true)) == true)
      assert(func(Array(true, false, true)) == true)
      assert(func(Array(false, true, true)) == true)
      assert(func(Array(true, true, true)) == true)
    }
    
    it("andとの合成"){
      val func = Prover.to_three_input(8, 14)
      assert(func(Array(false, false, false)) == false)
      assert(func(Array(true, false, false)) == true)
      assert(func(Array(false, true, false)) == false)
      assert(func(Array(true, true, false)) == true)
      assert(func(Array(false, false, true)) == false)
      assert(func(Array(true, false, true)) == true)
      assert(func(Array(false, true, true)) == true)
      assert(func(Array(true, true, true)) == true)
    }
  }
}

class ComposeToTwoTest extends FunSpec {
  it("3入力and"){
    assert(Prover.composite_to_twos(8, 8) == 5376)
  }
  
  it("3入力or"){
    assert(Prover.composite_to_twos(14, 14) == 21504)
  }
  
  it("常にfalse"){
    assert(Prover.composite_to_twos(0, 0) == 1)
  }
  
  it("常にtrue"){
    assert(Prover.composite_to_twos(255, 255) == 32768)
  }
}

class CombineAllTwo extends FunSpec {
  it("andだけから構成されるのはandとid"){
    assert(Prover.combine_all_two(256) == 5376)
  }
  
  it("orだけから構成されるのはorとid"){
    assert(Prover.combine_all_two(16384) == 21504)
  }
  
  it("andとorで構成できるのは4種類"){
    assert(Prover.combine_all_two(16640) == 21760)
  }
}
