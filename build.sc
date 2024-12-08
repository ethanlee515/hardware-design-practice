import os.Path
import mill._
import mill.scalalib._

object Main extends ScalaModule {
  def scalaVersion = "2.11.12"

  override def millSourcePath = os.pwd
  def ivyDeps = Agg(
    ivy"com.github.spinalhdl::spinalhdl-core:1.6.4",
    ivy"com.github.spinalhdl::spinalhdl-lib:1.6.4",
    )

  def scalacPluginIvyDeps = Agg(ivy"com.github.spinalhdl::spinalhdl-idsl-plugin:1.6.4")
}

