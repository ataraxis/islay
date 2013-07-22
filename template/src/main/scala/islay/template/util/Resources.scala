package islay.template.util

import java.io.FileNotFoundException
import java.lang.UnsupportedOperationException
import java.nio.ByteBuffer
import java.nio.channels.{AsynchronousFileChannel, CompletionHandler}
import java.nio.file.{FileSystems, Files, InvalidPathException, Path}
import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success}
import java.net.URI
import java.nio.file.FileSystemAlreadyExistsException


object Resources {

  val defaultClassLoader =
    Option(Thread.currentThread.getContextClassLoader) getOrElse getClass.getClassLoader

  val defaultRoot: Path = pathTo(".")

  def pathTo(context: String): Path = {
    val contextResource = defaultClassLoader.getResource(if (context.isEmpty) "." else context)
    if (contextResource == null)
      throw new FileNotFoundException(context)

    val contextUri = contextResource.toURI
    val (contextPath, fs) =
      if (contextUri.getScheme == "file")
        (contextUri.getPath, FileSystems.getFileSystem(contextUri.resolve("/")))
      else
        (context, nonFileSystem(contextUri))
    fs.getPath(contextPath)
  }

  private def nonFileSystem(uri: URI)= try {
    FileSystems.newFileSystem(uri, new java.util.HashMap[String, Nothing])
  } catch { case _: FileSystemAlreadyExistsException =>
    FileSystems.getFileSystem(uri)
  }

  /**
   * Resolves a path against a given root, disallowing path traversals beyond the root.
   *
   * @throws InvalidPathException if the resolved path does not have `root` as a parent
   */
  def resolve(root: Path, path: String): Path = {
    val normalizedRoot = root.toAbsolutePath.normalize
    val resolved = normalizedRoot.resolve(path.replaceFirst("^/+", ""))
    @tailrec
    def isAllowed(next: Path): Boolean =
      if (next == normalizedRoot) true
      else if (!next.normalize.startsWith(normalizedRoot)) false
      else isAllowed(next.getParent)
    if (!isAllowed(resolved))
      throw new InvalidPathException(path, "Path traversal not allowed")
    resolved.normalize
  }

  /**
   * Returns a resource's last modified time in milliseconds.
   */
  def lastModified(resource: Path)(implicit executor: ExecutionContext): Future[Long] =
    Future(Files.getLastModifiedTime(resource).toMillis)

  /**
   * Reads all bytes from the specified resource using non-blocking operations where possible.
   */
  def readAllBytes(resource: Path)(implicit executor: ExecutionContext): Future[Array[Byte]] = {

    Future {
      /* open() and size() are blocking */
      val channel = AsynchronousFileChannel.open(resource)
      (channel, channel.size())

    } flatMap { case (channel, size) =>

      val promise = Promise[Array[Byte]]
      val buffer = ByteBuffer.allocate(size.toInt)
      channel.read(buffer, 0, buffer, new CompletionHandler[Integer, ByteBuffer] {
        override def completed(result: Integer, attachment: ByteBuffer) {
          promise.complete(Success(attachment.array))
          channel.close()
        }
        override def failed(ex: Throwable, attachment: ByteBuffer) {
          promise.complete(Failure(ex))
          channel.close()
        }
      })

      promise.future

    } recover { case _: UnsupportedOperationException =>
      Files.readAllBytes(resource)
    }
  }

  def list(directory: Path)(implicit executor: ExecutionContext): Future[Seq[Path]] = {
    Future {
      val stream = Files.newDirectoryStream(directory)
      try
        collection.JavaConversions.iterableAsScalaIterable(stream).toSeq
      finally
        stream.close()
    }
  }
}