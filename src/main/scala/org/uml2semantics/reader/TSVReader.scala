package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.InputParameters.*
import org.uml2semantics.model.*

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.Set

object TSVReader /*extends UMLClassDiagramReader*/:

  enum ClassesHeader:
    case Name, Curie, Definition, Children

  enum ClassAttributesHeader:
    case Class, Curie, Name, ClassEnumOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition

  enum EnumerationsHeader:
    case Name, Curie, Definition

  enum EnumerationValuesHeader:
    case Enumeration, Name, Curie, Definition


  def parseUMLClassDiagram(input: InputParameters): Unit =
    val ontologyPrefix = PrefixNamespace(input.ontologyPrefix)
    val classes = parseClasses(input.classesTsv, ontologyPrefix)

  private def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLClass.BuilderCache =
    import ClassesHeader.*
    val logger = Logger("parseClasses")
    logger.info("Start")
    implicit object TsvFormat extends TSVFormat {}

    val reader = CSVReader.open(maybeTsvFile.get)
    val builderCache = UMLClass.BuilderCache()

    reader.allWithHeaders().foreach(header => {
      logger.trace(s"m = $header")
      val childrenSet = {
        val maybeChildren = header.get(Children.toString)
        logger.trace(s"maybeChildren.get=${maybeChildren.get}")
        if maybeChildren.isDefined then
          maybeChildren.get
        else
          ""
      }
      .split('|')
      .map(_.trim)
      .map(child => {
        val curieOption = org.uml2semantics.model.Curie.fromString(child)
        val classIdentity = if curieOption.isEmpty then
          UMLClassIdentity(UMLClassName(Some(child)), UMLClassCurie(None), ontologyPrefix)
        else
          UMLClassIdentity(UMLClassName(None), UMLClassCurie(curieOption), ontologyPrefix)
        classIdentity
      })
      .toSet

      val builder = UMLClass.Builder(Some(ontologyPrefix), builderCache)
        .withName(header(Name.toString))
        .withCurie(header(Curie.toString))
        .withDefinition(header(Definition.toString))
        .withSpecializations(childrenSet)
    })

    builderCache