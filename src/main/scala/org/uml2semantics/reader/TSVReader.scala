package org.uml2semantics.reader

import com.github.tototoshi.csv.*
import com.typesafe.scalalogging.Logger
import org.uml2semantics.*
import org.uml2semantics.model.*

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.Set

enum ClassesHeader:
  case Name, Curie, Definition, ParentNames

enum AttributesHeader:
  case ClassName, Curie, Name, ClassOrPrimitiveType, MinMultiplicity, MaxMultiplicity, Definition

enum EnumerationsHeader:
  case Name, Curie, Definition

def parseClasses(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLClasses =
  import ClassesHeader.*
  val logger = Logger("TsvReader: parseClasses")
  logger.info("Start")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  val umlClasses = mutable.Set[UMLClass]()

  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val parentNamedElementsSet =
      val maybeParentNames = m.get(ParentNames.toString)
      logger.trace(s"maybeParentNames.get=${maybeParentNames.get}")
      if maybeParentNames.isDefined then
        maybeParentNames.get
      else ""
    .split('|').map(_.trim).toSet

    val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
      Some(org.uml2semantics.model.Curie(m(Curie.toString)))
    else
      None

    val umlClass = UMLClass(
      UMLClassIdentity(
        UMLClassName(m(Name.toString)),
        UMLClassCurie(curieOption),
        ontologyPrefix
        ),
      UMLClassDefinition(m(Definition.toString)),
      UMLClassParentNamedElements(parentNamedElementsSet)
    )
    logger.trace(s"umlClasses.getClass.hasCode=${umlClasses.getClass.hashCode}")
    umlClasses += umlClass
  })
  reader.close()
  val umlClassesByNamedElement = umlClasses.map(umlClass => (umlClass.classIdentity.classNamedElement, umlClass)).toMap
  logger.trace(s"umlClassesByNamedElement = $umlClassesByNamedElement")
  logger.info("Done")
  UMLClasses(umlClassesByNamedElement)
end parseClasses


def parseAttributes(maybeTsvFile: Option[File], ontologyPrefix: PrefixNamespace): UMLClassAttributes =
  import AttributesHeader.*
  val logger = Logger("TsvReader: parseAttributes")
  implicit object TsvFormat extends TSVFormat {}

  val reader = CSVReader.open(maybeTsvFile.get)
  val umlClassAttributes = mutable.Set[UMLClassAttribute]()

  logger.info("Start")
  reader.allWithHeaders().foreach(m => {
    logger.trace(s"m = $m")

    val classNamedElement = UMLClassIdentity.findClassNamedElement(m(AttributesHeader.ClassName.toString))
    logger.trace(s"classNamedElement = $classNamedElement")
    if classNamedElement.isDefined then
      logger.trace(s"mClassOrPrimitiveType.toString = {${m(ClassOrPrimitiveType.toString)}}")
      val curieOption: Option[Curie] = if m(Curie.toString).contains(":") then
        Some(org.uml2semantics.model.Curie(m(Curie.toString)))
      else
        None
      val umlClassAttribute = UMLClassAttribute(
        UMLClassAttributeIdentity(classNamedElement.get.classNamedElement,
          UMLClassAttributeName(m(Name.toString)),
          UMLClassAttributeCurie(curieOption),
          ontologyPrefix
        ),
        UMLClassAttributeType(m(ClassOrPrimitiveType.toString)),
        UMLMultiplicity(UMLCardinality(m(MinMultiplicity.toString)), UMLCardinality(m(MaxMultiplicity.toString))),
        UMLClassAttributeDefinition(m(Definition.toString))
      )
      umlClassAttributes += umlClassAttribute
  })
  reader.close()
  val umlClassAttributesById = umlClassAttributes.map(umlClassAttribute => (umlClassAttribute.attributeIdentity.attributeNamedElement, umlClassAttribute)).toMap
  logger.trace(s"umlClassAttributesById = $umlClassAttributesById")
  logger.info("Done")
  UMLClassAttributes(umlClassAttributesById)
end parseAttributes

def parseUMLClassDiagram(input: InputParameters): UMLClassDiagram =
  UMLClassDiagram(
    input.owlOntologyFile.get,
    OntologyIRI(input.ontologyIRI),
    PrefixNamespace(input.ontologyPrefix),
    parseClasses(input.classesTsv, PrefixNamespace(input.ontologyPrefix)),
    parseAttributes(input.attributesTsv, PrefixNamespace(input.ontologyPrefix)))


