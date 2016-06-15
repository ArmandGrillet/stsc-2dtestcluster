package uicluster

// UI
import java.awt.{Color, Graphics2D, Paint}
import java.awt.image.BufferedImage
import javafx.scene.{chart => jfxsc}
import org.jfree.chart.axis.{NumberTickUnit, TickUnits}
import scala.collection.mutable.ListBuffer
import scala.util.control._
import scalafx.embed.swing.SwingFXUtils
import scalafx.event.ActionEvent
import scalafx.scene.chart._
import scalafx.scene.control._
import scalafx.scene.control.Alert._
import scalafx.scene.image.ImageView
import scalafx.scene.layout.AnchorPane
import scalafx.scene.Node
import scalafx.stage.FileChooser
import scalafx.stage.FileChooser.ExtensionFilter
import scalafxml.core.{NoDependencyResolver, FXMLLoader}
import scalafxml.core.macros.sfxml

// Logic
import breeze.linalg._
import breeze.stats.distributions.MultivariateGaussian
import breeze.plot._
import java.io.File
import stsc._

@sfxml
class Controller(private val root: AnchorPane, private val observations: TextField, private val distance: TextField, val dataset: ImageView, val clusters: ImageView) {

    private var displayedDataset = DenseMatrix.zeros[Double](0, 0)

    def run(event: ActionEvent) {
        var observationsInput = toInt(observations.text.value).getOrElse(0)
        var distanceInput = toDouble(distance.text.value).getOrElse(0.0)
        var ready = true

        if (ready && observationsInput < 10) {
            showAlert("The number of observations has to be more than 10", "The number of observations has to be more than 10.")
            ready = false
        }

        if (ready && distanceInput < 0) {
            showAlert("Distance has to be positive", "The distance between the two dataset has to be positive.")
            ready = false
        }

        if (ready) {
            val f1 = Figure()
            f1.visible = false
            f1.width = dataset.getBoundsInParent().getWidth().toInt
            f1.height = dataset.getBoundsInParent().getHeight().toInt
            val f2 = Figure()
            f2.visible = false
            f2.width = dataset.getBoundsInParent().getWidth().toInt
            f2.height = dataset.getBoundsInParent().getHeight().toInt

            val p1 = f1.subplot(0)
            p1.title = "Dataset"

            val sample1Vector = new MultivariateGaussian(DenseVector(0.0, 0.0),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(observationsInput)
            val sample2Vector = new MultivariateGaussian(DenseVector(0.0, distanceInput),DenseMatrix((1.0, 0.0), (0.0, 1.0))).sample(observationsInput)

            val sample1 = DenseMatrix.zeros[Double](observationsInput, 2)
            val sample2 = DenseMatrix.zeros[Double](observationsInput, 2)

            var i = 0
            for (i <- 0 until sample1Vector.length){
                sample1(i, 0) = sample1Vector(i)(0)
                sample1(i, 1) = sample1Vector(i)(1)

                sample2(i, 0) = sample2Vector(i)(0)
                sample2(i, 1) = sample2Vector(i)(1)
            }

            p1 += scatter(sample1(::, 0), sample1(::, 1), {(_:Int) => 0.03}, {(pos:Int) => Color.ORANGE})
            p1 += scatter(sample2(::, 0), sample2(::, 1), {(_:Int) => 0.03}, {(pos:Int) => Color.BLUE})

            dataset.image = SwingFXUtils.toFXImage(imageToFigure(f1), null)

            val samplesMatrix = DenseMatrix.vertcat(sample1, sample2)
            val result = Algorithm.cluster(samplesMatrix, 2, 2)._2

            val p21 = f2.subplot(0)
            p21.title = "Clusters"
            val colors = List(Color.ORANGE, Color.BLUE, Color.GREEN, Color.BLACK, Color.MAGENTA, Color.CYAN, Color.YELLOW)
            p21 += scatter(samplesMatrix(::, 0), samplesMatrix(::, 1), {(_:Int) => 0.03}, {(pos:Int) => colors(result(pos))}) // Display the observations.
            clusters.image = SwingFXUtils.toFXImage(imageToFigure(f2), null)
        }
    }

    private def imageToFigure(f: Figure): BufferedImage = {
        val image = new BufferedImage(f.width, f.height, BufferedImage.TYPE_INT_ARGB)
        val g2d = image.createGraphics()
        f.drawPlots(g2d)
        g2d.dispose
        return image
    }

    private def showAlert(header: String, content: String) {
        new Alert(AlertType.Error) {
            title = "Error"
            headerText = header
            contentText = content
        }.showAndWait()
    }

    private def showDataset() {
        // val f = Figure()
        // f.visible = false
        // f.width = dataset.getBoundsInParent().getWidth().toInt
        // f.height = dataset.getBoundsInParent().getHeight().toInt
        // val p = f.subplot(0)
        // p.title = "Dataset"
        // p += scatter(displayedDataset(::, 0), displayedDataset(::, 1), {(_:Int) => 0.01}, {(pos:Int) => Color.BLACK}) // Display the observations.
        // dataset.image = SwingFXUtils.toFXImage(imageToFigure(f), null)
        // qualities.text = ""
    }

    private def toDouble(s: String): Option[Double] = {
        try {
            Some(s.toDouble)
        } catch {
            case e: Exception => None
        }
    }

    private def toInt(s: String): Option[Int] = {
        try {
            Some(s.toInt)
        } catch {
            case e: Exception => None
        }
    }
}
