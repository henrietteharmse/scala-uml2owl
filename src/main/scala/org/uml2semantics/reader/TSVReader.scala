package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.model.*
import org.uml2semantics.reader.ReaderHelper.populateParentsWithTheirChildren

import java.io.File
import scala.collection.mutable

object TSVReader extends UMLClassDiagramReader:
  val logger = Logger("TSVReader")

  enum ClassesHeader:
    case Name, Curie, Definition, Parents

  enum ClassAttributesHeader:
    case Class, Curie, Name, ClassEnumOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition

  enum EnumerationsHeader:
    case Name, Curie, Definition

  enum EnumerationValuesHeader:
    case Enumeration, Name, Curie, Definition

  override def parseUMLClassDiagram(input: InputParameters): Unit =
    val ontologyPrefix = PrefixNamespace(input.ontologyPrefix)
    parseClasses(input.classesTsv, ontologyPrefix)
    val k= 0

  private def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): Unit =
    import ClassesHeader.*
    logger.info("Start")
    implicit object TsvFormat extends TSVFormat {}

    val reader = CSVReader.open(maybeTsvFile.get)
    val parentToChildrenMap = mutable.Map[String, mutable.Set[String]]()

    reader.allWithHeaders().foreach(header => {
      var classBuilder = UMLClass.builder(ontologyPrefix)
      logger.trace(s"m = $header")
      extractParents(parentToChildrenMap, Name, Curie, Parents, header)

      classBuilder
        .withNameAndCurie(header(Name.toString), header(Curie.toString))
        .withDefinition(header(Definition.toString))
        .build
    })
    populateParentsWithTheirChildren(parentToChildrenMap, ontologyPrefix)


  private def extractParents(parentToChildrenMap: mutable.Map[String, mutable.Set[String]],
                             Name: ClassesHeader.Name.type, Curie: ClassesHeader.Curie.type,
                             Parents: ClassesHeader.Parents.type, header: Map[String, String]): Unit = {
    val parentSet = {
      val maybeParent = header.get(Parents.toString)
      logger.trace(s"maybeParent.get=${maybeParent.get}")
      maybeParent.getOrElse("")
    }
      .split('|')
      .map(_.trim)
      .filter(_.nonEmpty)
      .toSet

    if parentSet.nonEmpty then
      val child = if header(Curie.toString).nonEmpty then header(Curie.toString) else header(Name.toString)
      parentSet.foreach(parent =>
        parentToChildrenMap.getOrElseUpdate(parent, mutable.HashSet()) += child
      )
  }



