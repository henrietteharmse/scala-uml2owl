package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.*
import org.uml2semantics.{InputParameters}
import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache}

import java.io.File

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
    parseClasses(input.classesTsv, ontologyPrefix)
    val k= 0

  private def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): Unit =
    import ClassesHeader.*
    val logger = Logger("parseClasses")
    logger.info("Start")
    implicit object TsvFormat extends TSVFormat {}

    val reader = CSVReader.open(maybeTsvFile.get)

    reader.allWithHeaders().foreach(header => {
      var classBuilder = UMLClass.builder(ontologyPrefix)
      logger.trace(s"m = $header")
      val childrenSet = {
        val maybeChildren = header.get(Children.toString)
        logger.trace(s"maybeChildren.get=${maybeChildren.get}")
        maybeChildren.getOrElse("")
      }
      .split('|')
      .map(_.trim)
      .filter(_.nonEmpty)
      .toSet
      
      classBuilder
        .withNameAndCurie(header(Name.toString), header(Curie.toString))
        .withDefinition(header(Definition.toString))
        .withChildren(childrenSet)
        .build
    })


