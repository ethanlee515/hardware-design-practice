import os.Path
import mill._
import mill.scalalib._

object Main extends ScalaModule {
  def scalaVersion = "2.12.18"

  override def millSourcePath = os.pwd
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.10.2a",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.10.2a",
    )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.10.2a")
}

