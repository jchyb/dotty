package dotty.tools.scaladoc
package tasty.comments

import java.nio.file.Path
import dotty.tools.scaladoc.Scaladoc.CommentSyntax

case class CommentSyntaxOverridesArgs(csFormats: PathBased[CommentSyntax], defaultFormat: CommentSyntax):
  def get(path: Option[Path]): CommentSyntax =
    path
      .flatMap(p => csFormats.get(p).map(_.elem))
      .getOrElse(defaultFormat)

object CommentSyntaxOverridesArgs:
  val usage =
    """
    |Comment Syntax Override arguments provide a way to override comment syntax used for specified paths.
    |
    |This setting accepts list of arguments in format:
    |args := arg{,arg}
    |arg := [path=]syntax
    |where `path` is a prefix of the path to source files that will have a specific comment syntax set and `syntax` specifies the one used.
    |
    |Available syntaxes:
    |markdown
    |wiki
    |
    """.stripMargin

  def load(args: List[String], defaultSyntax: CommentSyntax)(using CompilerContext): CommentSyntaxOverridesArgs = {
    PathBased.parse[CommentSyntax](args)(using CommentSyntax.CommentSyntaxParser) match {
      case PathBased.ParsingResult(errors, res) =>
        if errors.nonEmpty then report.warning(s"""
            |Got following errors during comment syntax overrides args parsing:
            |$errors
            |
            |$usage
            |""".stripMargin
        )
        CommentSyntaxOverridesArgs(res, defaultSyntax)
    }
  }