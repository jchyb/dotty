package dotty.tools.tasty

import java.util.UUID

import TastyFormat.{MajorVersion, MinorVersion, ExperimentalVersion, header, bestEffortHeader}

/**
 * The Tasty Header consists of four fields:
 * - uuid
 *   - contains a hash of the sections of the TASTy file
 * - majorVersion
 *   - matching the TASTy format version that last broke backwards compatibility
 * - minorVersion
 *   - matching the TASTy format version that last broke forward compatibility
 * - experimentalVersion
 *   - 0 for final compiler version
 *   - positive for between minor versions and forward compatibility
 *     is broken since the previous stable version.
 * - toolingVersion
 *   - arbitrary string representing the tooling that produced the TASTy
 * - isBestEffort
 *   - signifies if it is a best effort TASTy file, or a regular one
 */
sealed abstract case class TastyHeader(
  uuid: UUID,
  majorVersion: Int,
  minorVersion: Int,
  experimentalVersion: Int,
  toolingVersion: String,
  isBestEffort: Boolean
)

class TastyHeaderUnpickler(reader: TastyReader) {
  import TastyHeaderUnpickler._
  import reader._

  def this(bytes: Array[Byte]) = this(new TastyReader(bytes))

  /** reads and verifies the TASTy version, extracting the UUID */
  def readHeader(withBestEffortTasty: Boolean = false): UUID =
    readFullHeader(withBestEffortTasty).uuid

  /** reads and verifies the TASTy version, extracting the whole header */
  def readFullHeader(withBestEffortTasty: Boolean = false): TastyHeader = {

    val isBestEffort = {
      val readHeader = (for (i <- 0 until header.length) yield readByte()).toArray

      if (readHeader.sameElements(header)) false
      else if (!withBestEffortTasty) throw new UnpickleException("not a TASTy file")
      else if (readHeader.sameElements(bestEffortHeader)) true
      else throw new UnpickleException("not a TASTy or best effort TASTy file")
    }
    val fileMajor = readNat()
    if (fileMajor <= 27) { // old behavior before `tasty-core` 3.0.0-M4
      val fileMinor = readNat()
      val signature = signatureString(fileMajor, fileMinor, 0)
      throw new UnpickleException(signature + backIncompatAddendum + toolingAddendum)
    }
    else {
      val fileMinor = readNat()
      val fileExperimental = readNat()
      val toolingVersion = {
        val length = readNat()
        val start = currentAddr
        val end = start + length
        goto(end)
        new String(bytes, start.index, length)
      }

      val validVersion = TastyFormat.isVersionCompatible(
        fileMajor            = fileMajor,
        fileMinor            = fileMinor,
        fileExperimental     = fileExperimental,
        compilerMajor        = MajorVersion,
        compilerMinor        = MinorVersion,
        compilerExperimental = ExperimentalVersion
      )

      check(validVersion, {
        val signature = signatureString(fileMajor, fileMinor, fileExperimental)
        val producedByAddendum = s"\nThe TASTy file was produced by $toolingVersion.$toolingAddendum"
        val msg = (
          if (fileExperimental != 0) unstableAddendum
          else if (fileMajor < MajorVersion) backIncompatAddendum
          else forwardIncompatAddendum
        )
        signature + msg + producedByAddendum
      })

      val uuid = new UUID(readUncompressedLong(), readUncompressedLong())
      new TastyHeader(uuid, fileMajor, fileMinor, fileExperimental, toolingVersion, isBestEffort) {}
    }
  }

  def isAtEnd: Boolean = reader.isAtEnd

  private def check(cond: Boolean, msg: => String): Unit = {
    if (!cond) throw new UnpickleException(msg)
  }
}

object TastyHeaderUnpickler {

  private def toolingAddendum = (
    if (ExperimentalVersion > 0)
      "\nNote that your tooling is currently using an unstable TASTy version."
    else
      ""
  )

  private def signatureString(fileMajor: Int, fileMinor: Int, fileExperimental: Int) = {
    def showMinorVersion(min: Int, exp: Int) = {
      val expStr = if (exp == 0) "" else s" [unstable release: $exp]"
      s"$min$expStr"
    }
    val minorVersion = showMinorVersion(MinorVersion, ExperimentalVersion)
    val fileMinorVersion = showMinorVersion(fileMinor, fileExperimental)
    s"""TASTy signature has wrong version.
      | expected: {majorVersion: $MajorVersion, minorVersion: $minorVersion}
      | found   : {majorVersion: $fileMajor, minorVersion: $fileMinorVersion}
      |
      |""".stripMargin
  }

  private def unstableAddendum =
    """This TASTy file was produced by an unstable release.
      |To read this TASTy file, your tooling must be at the same version.""".stripMargin

  private def backIncompatAddendum =
    """This TASTy file was produced by an earlier release that is not supported anymore.
      |Please recompile this TASTy with a later version.""".stripMargin

  private def forwardIncompatAddendum =
    """This TASTy file was produced by a more recent, forwards incompatible release.
      |To read this TASTy file, please upgrade your tooling.""".stripMargin
}
