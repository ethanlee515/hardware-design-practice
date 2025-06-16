import os.Path
import mill._
import mill.scalalib._

object Cordic extends ScalaModule {
  def scalaVersion = "2.12.18"

  //override def millSourcePath = os.pwd / "cordic"
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.10.2a",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.10.2a",
    )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.10.2a")
}

object Calibrate extends ScalaModule {
  def scalaVersion = "2.12.18"

  //override def millSourcePath = os.pwd / "calibrate"
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.10.2a",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.10.2a",
    ivy"org.typelevel::spire:0.17.0"
    )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.10.2a")
}

object Scratch extends ScalaModule {
  def scalaVersion = "2.12.18"

/*
  override def millSourcePath = os.pwd / "scratch"
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.10.2a",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.10.2a",
    )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.10.2a")
*/
}
