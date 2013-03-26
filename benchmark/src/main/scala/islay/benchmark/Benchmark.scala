package islay.benchmark

import annotation.tailrec
import com.google.caliper.Param
import islay.transform._
import scala.concurrent.Await
import scala.concurrent.duration._
import scala.xml.Text


// a caliper benchmark is a class that extends com.google.caliper.Benchmark
// the SimpleScalaBenchmark trait does it and also adds some convenience functionality
class Benchmark extends SimpleScalaBenchmark {

  // to make your benchmark depend on one or more parameterized values, create fields with the name you want
  // the parameter to be known by, and add this annotation (see @Param javadocs for more details)
  // caliper will inject the respective value at runtime and make sure to run all combinations
  @Param(Array("10", "100", "1000", "10000"))
  val length: Int = 0


  val xml = <div id="a">
              <div id="b"></div>
              <div id="c">
                <p id="d">Wibble</p>
                <p id="e">Wobble</p>
                <p id="f">Wubble</p>
                <div id="g">
                  <span id="h">Watusi</span>
                </div>
                <p id="i">Twist</p>
                <div id="j"></div>
                <div id="k">
                  <span id="l">Mashed Potato</span>
                </div>
              </div>
              <p id="l">Niagra Falls</p>
            </div>

  val duration = 100.millis

  val childSelector = c"div > span"
  val childTransform = childSelector ** "foo"

  override def setUp() {
    // set up all your benchmark data here
  }

  def timeChildSelector(reps: Int) = repeat(reps) {
    val future = childTransform.apply(xml)
    Await.result(future, duration)
  }


  override def tearDown() {
    // clean up after yourself if required
  }

}

