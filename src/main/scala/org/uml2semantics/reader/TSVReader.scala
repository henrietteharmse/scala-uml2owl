package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.model.*
import org.uml2semantics.InputParameters
import org.uml2semantics.model.UMLClass.ClassBuilder
import org.uml2semantics.model.cache.{ClassBuilderCache, ClassIdentityBuilderCache}

import java.io.File
import scala.collection.{immutable, mutable}

object TSVReader /*extends UMLClassDiagramReader*/:

  enum ClassesHeader:
    case Name, Curie, Definition, Parents

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
    val parentToChildrenMap = scala.collection.mutable.Map[String, scala.collection.mutable.Set[String]]()

    reader.allWithHeaders().foreach(header => {
      var classBuilder = UMLClass.builder(ontologyPrefix)
      logger.trace(s"m = $header")
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
        parentSet.foreach(parent => {
          parentToChildrenMap.getOrElseUpdate(parent, scala.collection.mutable.HashSet()) += child
        })

      classBuilder
        .withNameAndCurie(header(Name.toString), header(Curie.toString))
        .withDefinition(header(Definition.toString))
        .build
    })

    var classesToRebuild = scala.collection.mutable.Set[ClassBuilder]()

//    parentToChildrenMap.foreach((parent, children) => {
//      val parentIdentifier: UMLClassCurie | UMLClassName =
//        UMLClassCurie(parent).getOrElse(UMLClassName(parent).get)
//      val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parentIdentifier).getOrElse(
//        UMLClassIdentity.builder(ontologyPrefix).withNameOrCurie(parent).build)
//      var classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
//        UMLClass.builder(ontologyPrefix)
//          .withNameOrCurie(parent))
//
//      classBuilder = classBuilder.withChildren(parent, children.toSet)
//      classesToRebuild += classBuilder
//    })
    parentToChildrenMap.foreach { (parent, children) =>
      val parentClassIdentity = ClassIdentityBuilderCache.getUMLClassIdentity(parent).getOrElse(
        UMLClassIdentity.builder(ontologyPrefix).withNameOrCurie(parent).build)
      val classBuilder = ClassBuilderCache.getUMLClassBuilder(parentClassIdentity).getOrElse(
          UMLClass.builder(ontologyPrefix).withNameOrCurie(parent))
        .withChildren(parent, children.toSet)
      classesToRebuild += classBuilder
    }

    classesToRebuild.foreach(classBuilder => {
      val umlClass = classBuilder.build
      ClassBuilderCache.cacheUMLClass(umlClass, classBuilder)
    })

