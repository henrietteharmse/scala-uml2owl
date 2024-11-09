package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code
import org.uml2semantics.model.UMLCardinality.{>=, logger}
import sourcecode.Text

import java.io.File
import scala.:+
import scala.annotation.{tailrec, targetName}
import scala.collection.mutable

/**
 * The name UmlNamedElement is based on the UML specification where a NamedElement is a UML element that can have a
 * name.
 *
 */
sealed trait UMLNamedElement:
  def nonEmpty: Boolean

sealed trait UMLClassNamedElement extends UMLNamedElement:
  def getName: String

case class UMLClassName(name: String = "") extends UMLClassNamedElement:
  override def nonEmpty: Boolean = name.nonEmpty

  override def getName: String = name

case class UMLClassCurie(curieOption: Option[Curie] = None) extends UMLClassNamedElement:
  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def getName: String = curieOption.get.curie


private val FRAGMENT_SEPARATOR: String = "/"

sealed trait UMLClassAttributeNamedElement extends UMLNamedElement:
  def getName(classNamedElement: UMLClassNamedElement): String

case class UMLClassAttributeName(name: String = "") extends UMLClassAttributeNamedElement:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = name.nonEmpty

  override def getName(namedElement: UMLClassNamedElement): String =
    logger.debug(s"classNamedElement=$namedElement ${Code.source}")
    namedElement match
      case UMLClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR + name
      case _ => namedElement.getName + FRAGMENT_SEPARATOR + name

case class UMLClassAttributeCurie(curieOption: Option[Curie]) extends UMLClassAttributeNamedElement:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def getName(namedElement: UMLClassNamedElement): String =
    logger.debug(s"classNamedElement=$namedElement ${Code.source}")
    namedElement match
      case UMLClassCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR +
        curieOption.get.prefixReference.reference
      case _ => namedElement.getName + FRAGMENT_SEPARATOR + curieOption.get.prefixReference.reference


sealed trait UMLEnumerationNamedElement extends UMLNamedElement:
  def getName: String

case class UMLEnumerationName(name: String = "") extends UMLEnumerationNamedElement:
  override def nonEmpty: Boolean = name.nonEmpty

  override def getName: String = name

case class UMLEnumerationCurie(curieOption: Option[Curie] = None) extends UMLEnumerationNamedElement:
  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def getName: String = curieOption.get.curie

sealed trait UMLEnumerationValueNamedElement extends UMLNamedElement:
  def getName(enumerationNamedElement: UMLEnumerationNamedElement): String

case class UMLEnumerationValueName(name: String = "") extends UMLEnumerationValueNamedElement:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = name.nonEmpty

  override def getName(namedElement: UMLEnumerationNamedElement): String =
    logger.debug(s"enumerationNamedElement=$namedElement ${Code.source}")
    namedElement match
      case UMLEnumerationCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR + name
      case _ => namedElement.getName + FRAGMENT_SEPARATOR + name

case class UMLEnumerationValueCurie(curieOption: Option[Curie]) extends UMLEnumerationValueNamedElement:
  private val logger = Logger[this.type]

  override def nonEmpty: Boolean = curieOption.nonEmpty

  override def getName(namedElement: UMLEnumerationNamedElement): String =
    logger.debug(s"enumerationNamedElement=$namedElement ${Code.source}")
    namedElement match
      case UMLEnumerationCurie(curieOption) => curieOption.get.prefixReference.reference + FRAGMENT_SEPARATOR +
        curieOption.get.prefixReference.reference
      case _ => namedElement.getName + FRAGMENT_SEPARATOR + curieOption.get.prefixReference.reference


case class UMLClassAttributeIdentity(classNamedElement: UMLClassNamedElement,
                                     attributeName: UMLClassAttributeName = UMLClassAttributeName(),
                                     attributeCurie: UMLClassAttributeCurie = UMLClassAttributeCurie(None),
                                     ontologyPrefix: PrefixNamespace):
  private var _attributeIRI: UMLClassAttributeIRI = _
  private var _attributeNamedElement: UMLClassAttributeNamedElement = _
  private var _attributeLabel: String = _

  def attributeIRI : UMLClassAttributeIRI = _attributeIRI
  def attributeNamedElement: UMLClassAttributeNamedElement = _attributeNamedElement
  def attributeLabel: String = _attributeLabel
object UMLClassAttributeIdentity:
  private val logger = Logger[this.type]

  private var attributeIdentityByName: mutable.HashMap[UMLClassAttributeName, UMLClassAttributeIdentity] =
    mutable.HashMap[UMLClassAttributeName, UMLClassAttributeIdentity]()
  private var attributeIdentityByIRI: mutable.HashMap[UMLClassAttributeIRI, UMLClassAttributeIdentity] =
    mutable.HashMap[UMLClassAttributeIRI, UMLClassAttributeIdentity]()
  private var attributeIdentityByCurie: mutable.HashMap[UMLClassAttributeCurie, UMLClassAttributeIdentity] =
    mutable.HashMap[UMLClassAttributeCurie, UMLClassAttributeIdentity]()

  def apply(classNamedElement: UMLClassNamedElement,
            attributeName: UMLClassAttributeName = UMLClassAttributeName(),
            attributeCurie: UMLClassAttributeCurie = UMLClassAttributeCurie(None),
            ontologyPrefix: PrefixNamespace): UMLClassAttributeIdentity =
    logger.debug(s"classNamedElement=$classNamedElement, attributeName=$attributeName, attributeCurie=$attributeCurie, " +
      s"ontologyPrefix=$ontologyPrefix ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    require(classNamedElement.nonEmpty && (attributeName.nonEmpty || attributeCurie.nonEmpty),
      "An attribute must have a name or curie.")
    val classAttributeIdentity: UMLClassAttributeIdentity =
      new UMLClassAttributeIdentity(classNamedElement, attributeName, attributeCurie, ontologyPrefix)

    (attributeName, attributeCurie.curieOption) match
      case (UMLClassAttributeName(""), Some(curie)) =>
        logger.debug(s"case (UmlClassAttributeName(\"\"), Some(curie)) ${Code.source}")
        populate(classNamedElement, attributeCurie, ontologyPrefix, classAttributeIdentity, curie.prefixReference.reference)
        attributeIdentityByCurie += (attributeCurie -> classAttributeIdentity)
      case (UMLClassAttributeName(strAttributeName), Some(curie)) =>
        logger.debug(s"case (UmlClassAttributeName(strAttributeName), Some(curie)) ${Code.source}")
        populate(classNamedElement, attributeCurie, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attributeIdentityByCurie += (attributeCurie -> classAttributeIdentity)
        attributeIdentityByName += (attributeName -> classAttributeIdentity)
      case (UMLClassAttributeName(strAttributeName), None) =>
        logger.debug(s"case (UmlClassAttributeName(strAttributeName), None) ${Code.source}")
        populate(classNamedElement, attributeName, ontologyPrefix, classAttributeIdentity, strAttributeName)
        attributeIdentityByName += (attributeName -> classAttributeIdentity)

    attributeIdentityByIRI += (classAttributeIdentity.attributeIRI -> classAttributeIdentity)

    classAttributeIdentity

  private def populate(classNamedElement: UMLClassNamedElement,
                       attributeNamedElement: UMLClassAttributeNamedElement,
                       ontologyPrefix: PrefixNamespace,
                       attributeIdentity: UMLClassAttributeIdentity,
                       label: String): Unit = {
    logger.debug(s"classNamedElement=$classNamedElement, attributeNamedElement=$attributeNamedElement, " +
      s"ontologyPrefix=$ontologyPrefix, attributeIdentity=$attributeIdentity, label=$label ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    val attributeIRI: UMLClassAttributeIRI = UMLClassAttributeIRI(ontologyPrefix, classNamedElement, attributeNamedElement)
    attributeIdentity._attributeNamedElement = attributeNamedElement
    attributeIdentity._attributeIRI = attributeIRI
    attributeIdentity._attributeLabel = label
  }


case class UMLEnumerationValueIdentity(enumerationNamedElement: UMLEnumerationNamedElement,
                                       valueName: UMLEnumerationValueName = UMLEnumerationValueName(),
                                       valueCurie: UMLEnumerationValueCurie = UMLEnumerationValueCurie(None),
                                       ontologyPrefix: PrefixNamespace):
  var valueIRI: UMLEnumerationValueIRI = _
  var valueNamedElement: UMLEnumerationValueNamedElement = _
  var valueLabel: String = _

object UMLEnumerationValueIdentity:
  private val logger = Logger[this.type]

  private var valueIdentityByName: mutable.HashMap[UMLEnumerationValueName, UMLEnumerationValueIdentity] =
    mutable.HashMap[UMLEnumerationValueName, UMLEnumerationValueIdentity]()
  private var valueIdentityByIRI: mutable.HashMap[UMLEnumerationValueIRI, UMLEnumerationValueIdentity] =
    mutable.HashMap[UMLEnumerationValueIRI, UMLEnumerationValueIdentity]()
  private var valueIdentityByCurie: mutable.HashMap[UMLEnumerationValueCurie, UMLEnumerationValueIdentity] =
    mutable.HashMap[UMLEnumerationValueCurie, UMLEnumerationValueIdentity]()

  def apply(enumerationNamedElement: UMLEnumerationNamedElement,
            valueName: UMLEnumerationValueName = UMLEnumerationValueName(),
            valueCurie: UMLEnumerationValueCurie = UMLEnumerationValueCurie(None),
            ontologyPrefix: PrefixNamespace): UMLEnumerationValueIdentity =
    logger.debug(s"enumerationNamedElement=$enumerationNamedElement, valueName=$valueName, valueCurie=$valueCurie, " +
      s"ontologyPrefix=$ontologyPrefix ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    require(enumerationNamedElement.nonEmpty && (valueName.nonEmpty || valueCurie.nonEmpty),
      "A value must have a name or curie.")
    val enumerationValueIdentity: UMLEnumerationValueIdentity =
      new UMLEnumerationValueIdentity(enumerationNamedElement, valueName, valueCurie, ontologyPrefix)

    (valueName, valueCurie.curieOption) match
      case (UMLEnumerationValueName(""), Some(curie)) =>
        logger.debug(s"case (UMLEnumerationValueName(\"\"), Some(curie)) ${Code.source}")
        populate(enumerationNamedElement, valueCurie, ontologyPrefix, enumerationValueIdentity, curie.prefixReference.reference)
        valueIdentityByCurie += (valueCurie -> enumerationValueIdentity)
      case (UMLEnumerationValueName(strValueName), Some(curie)) =>
        logger.debug(s"case (UMLEnumerationValueName(strValueName), Some(curie)) ${Code.source}")
        populate(enumerationNamedElement, valueCurie, ontologyPrefix, enumerationValueIdentity, strValueName)
        valueIdentityByCurie += (valueCurie -> enumerationValueIdentity)
        valueIdentityByName += (valueName -> enumerationValueIdentity)
      case (UMLEnumerationValueName(strValueName), None) =>
        logger.debug(s"case (UMLEnumerationValueName(strValueName), None) ${Code.source}")
        populate(enumerationNamedElement, valueName, ontologyPrefix, enumerationValueIdentity, strValueName)
        valueIdentityByName += (valueName -> enumerationValueIdentity)

    valueIdentityByIRI += (enumerationValueIdentity.valueIRI -> enumerationValueIdentity)

    enumerationValueIdentity

  private def populate(enumerationNamedElement: UMLEnumerationNamedElement,
                       valueNamedElement: UMLEnumerationValueNamedElement,
                       ontologyPrefix: PrefixNamespace,
                       valueIdentity: UMLEnumerationValueIdentity,
                       label: String): Unit = {
    logger.debug(s"enumerationNamedElement=$enumerationNamedElement, valueNamedElement=$valueNamedElement, " +
      s"ontologyPrefix=$ontologyPrefix, valueIdentity=$valueIdentity, label=$label ${Code.source}")
    //    logger.debug(s"${Code.arguments} ${Code.source}")
    val valueIRI: UMLEnumerationValueIRI = UMLEnumerationValueIRI(ontologyPrefix, enumerationNamedElement, valueNamedElement)
    valueIdentity.valueNamedElement = valueNamedElement
    valueIdentity.valueIRI = valueIRI
    valueIdentity.valueLabel = label
  }


/**
 * @Todo - Add Covering parameter with permitted values {complete, incomplete}
 * @Todo - Add Disjoint parameter with permitted values {disjoint, overlapping}
 *
 * @param setOfParentNamedElements
 */
case class UMLClassParentNamedElements(setOfParentNamedElements: Set[UMLClassNamedElement])

object UMLClassParentNamedElements:
  private val logger = Logger[this.type]

  @targetName("fromSetOfStrings")
  def apply(setOfParentNamedElements: Set[String]): UMLClassParentNamedElements =
    logger.debug(s"setOfParentNamedElements=$setOfParentNamedElements Ids${Code.source}")
    logger.debug(s"setOfParentNamedElements.isEmpty=${setOfParentNamedElements.isEmpty}")
    val setOfParentUncertainClassNamedElements = setOfParentNamedElements
      .filterNot(s => s.isEmpty)
      .map(m => UMLClassIdentity.findClassNamedElement(m).get.classNamedElement)
    new UMLClassParentNamedElements(setOfParentUncertainClassNamedElements)

case class UMLClassesMap(umlClasses: Map[UMLClassNamedElement, UMLClass])

case class UMLClassAttributesMap(umlClassAttributes: Map[UMLClassAttributeNamedElement, UMLClassAttribute])

case class UMLEnumerationsMap(umlEnumerations: Map[UMLEnumerationNamedElement, UMLEnumeration])

case class UMLEnumerationValuesMap(umlEnumerationValues: Map[UMLEnumerationValueNamedElement, UMLEnumerationValue])

sealed trait UMLElementIRI:
  val iri: String

case class UMLClassIRI(override val iri: String) extends UMLElementIRI

object UMLClassIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace,
            classNamedElement: UMLClassNamedElement): UMLClassIRI =
    logger.debug(s"ontologyPrefix=$ontologyPrefix, classNamedElement=$classNamedElement ${Code.source}")
    classNamedElement match
      case classNamedElementType: UMLClassCurie =>
        val prefixNamespace = classNamedElementType.curieOption.get
        new UMLClassIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          classNamedElementType.curieOption.get.prefixReference.reference)
      case _ => new UMLClassIRI(ontologyPrefix.prefixIRI.iri + classNamedElement.getName)

case class UMLClassAttributeIRI(override val iri: String) extends UMLElementIRI

object UMLClassAttributeIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace,
            classNamedElement: UMLClassNamedElement,
            classAttributeNamedElement: UMLClassAttributeNamedElement): UMLClassAttributeIRI =
    logger.debug(s"ontologyPrefix.prefixIRI.iri = ${ontologyPrefix.prefixIRI.iri}, " +
      s"classAttributeNamedElement.getName = ${classAttributeNamedElement.getName} ${Code.source}")
    classAttributeNamedElement match
      case classAttributeIdType: UMLClassAttributeCurie =>
        val prefixNamespace = classAttributeIdType.curieOption.get
        new UMLClassAttributeIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          prefixNamespace.prefixReference.reference)
      case _ => new UMLClassAttributeIRI(ontologyPrefix.prefixIRI.iri + classAttributeNamedElement.getName(classNamedElement))


case class UMLEnumerationIRI(override val iri: String) extends UMLElementIRI

object UMLEnumerationIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace, 
            enumerationNamedElement: UMLEnumerationNamedElement): UMLEnumerationIRI =
    logger.debug(s"ontologyPrefix=$ontologyPrefix, enumerationNamedElement=$enumerationNamedElement ${Code.source}")
    enumerationNamedElement match
      case enumerationNamedElementType: UMLEnumerationCurie =>
        val prefixNamespace = enumerationNamedElementType.curieOption.get
        new UMLEnumerationIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          enumerationNamedElementType.curieOption.get.prefixReference.reference)
      case _ => new UMLEnumerationIRI(ontologyPrefix.prefixIRI.iri + enumerationNamedElement.getName)

case class UMLEnumerationValueIRI(override val iri: String) extends UMLElementIRI

object UMLEnumerationValueIRI:
  private val logger = Logger[this.type]

  def apply(ontologyPrefix: PrefixNamespace,
            enumerationNamedElement: UMLEnumerationNamedElement,
            enumerationValueNamedElement: UMLEnumerationValueNamedElement): UMLEnumerationValueIRI =
    logger.debug(s"ontologyPrefix.prefixIRI.iri = ${ontologyPrefix.prefixIRI.iri}, " +
      s"enumerationValueNamedElement.getName = ${enumerationValueNamedElement.getName} ${Code.source}")
    enumerationValueNamedElement match
      case enumerationValueIdType: UMLEnumerationValueCurie =>
        val prefixNamespace = enumerationValueIdType.curieOption.get
        new UMLEnumerationValueIRI(PrefixNamespace.getPrefixNamespace(prefixNamespace.prefixName).get.prefixIRI.iri +
          prefixNamespace.prefixReference.reference)
      case _ => new UMLEnumerationValueIRI(ontologyPrefix.prefixIRI.iri + enumerationValueNamedElement.getName(enumerationNamedElement))

case class OntologyIRI(ontologyIRI: String)

case class UMLClassDefinition(definition: String = "")

opaque type UMLInfinite <: Char = '*'

object UMLInfinite:
  def apply(): UMLInfinite = '*'

  def unapply(s: String): Boolean = s == "*"

opaque type UMLNonNegativeInteger <: Int = Int

object UMLNonNegativeInteger:
  def apply(n: Int): UMLNonNegativeInteger =
    require(n >= 0)
    n

  def unapply(s: String): Option[UMLNonNegativeInteger] =
    try
      val someInt: Int = s.toInt
      if someInt >= 0 then
        Some(UMLNonNegativeInteger(someInt))
      else
        None
    catch
      case e: Exception => None

sealed trait UMLCardinality

case class UMLInfiniteCardinality(infinite: UMLInfinite) extends UMLCardinality

case class UMLNonNegativeCardinality(nonNegativeInteger: UMLNonNegativeInteger) extends UMLCardinality

object UMLCardinality:
  private val logger = Logger[this.type]

  def apply(s: String): UMLCardinality =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UMLNonNegativeInteger(i) => UMLNonNegativeCardinality(i)
      case UMLInfinite() => UMLInfiniteCardinality(UMLInfinite())
      case _ => UMLNonNegativeCardinality(UMLNonNegativeInteger(1))

  def >=(c1: UMLCardinality, c2: UMLCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) => false
      case (UMLInfiniteCardinality(_), UMLNonNegativeCardinality(_)) => true
      case (UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) => false
      case (UMLNonNegativeCardinality(t1), UMLNonNegativeCardinality(t2)) => t1 >= t2

  def ==(c1: UMLCardinality, c2: UMLCardinality): Boolean =
    logger.debug(s"c1=$c1, c2=$c2 ${Code.source}")
    (c1, c2) match
      case (UMLInfiniteCardinality(_), UMLInfiniteCardinality(_)) => true
      case (UMLInfiniteCardinality(_), UMLNonNegativeCardinality(_)) => false
      case (UMLNonNegativeCardinality(_), UMLInfiniteCardinality(_)) => false
      case (UMLNonNegativeCardinality(t1), UMLNonNegativeCardinality(t2)) => t1 == t2

case class UMLMultiplicity(min: UMLCardinality = UMLCardinality("1"),
                           max: UMLCardinality = UMLCardinality("1"))

object UMLMultiplicity:
  private val logger = Logger[this.type]

  def apply(min: UMLCardinality, max: UMLCardinality): UMLMultiplicity =
    logger.debug(s"min=$min, max=$max ${Code.source}")
    require(>=(max, min),
      s"""Max cardinality must be greater or equal than min cardinality,
         but "min=$min" and "max=$max" found.""")
    new UMLMultiplicity(min, max)

case class UMLClassIdentity(className: UMLClassName,
                            classCurie: UMLClassCurie,
                            ontologyPrefix: PrefixNamespace):
  var _classIRI: UMLClassIRI = _
  var _classNamedElement: UMLClassNamedElement = _
  var _classLabel: String = _

  def classIRI: UMLClassIRI = _classIRI
  def classNamedElement: UMLClassNamedElement = _classNamedElement
  def classLabel: String = _classLabel

object UMLClassIdentity:
  private val logger = Logger[this.type]

  private val classIdentityByName: mutable.HashMap[UMLClassName, UMLClassIdentity] = mutable.HashMap[UMLClassName, UMLClassIdentity]()
  private val classIdentityByIRI: mutable.HashMap[UMLClassIRI, UMLClassIdentity] = mutable.HashMap[UMLClassIRI, UMLClassIdentity]()
  private val classIdentityByCurie: mutable.HashMap[UMLClassCurie, UMLClassIdentity] = mutable.HashMap[UMLClassCurie, UMLClassIdentity]()

  /**
   *
   * @param className
   * @param classCurie
   * @param ontologyPrefix
   * @return
   */
  def apply(className: UMLClassName = UMLClassName(),
            classCurie: UMLClassCurie = UMLClassCurie(),
            ontologyPrefix: PrefixNamespace): UMLClassIdentity =
    logger.debug(s"className = $className and classCurie=$classCurie ${Code.source}")
    require(classCurie.nonEmpty || className.nonEmpty, "A class must have either a curie or a name.")
    val classIdentity: UMLClassIdentity = new UMLClassIdentity(className, classCurie, ontologyPrefix)
    (className, classCurie.curieOption) match
      case (UMLClassName(""), Some(curie)) =>
        logger.debug(s"case (UmlClassName(\"\"), Some(curie)) ${Code.source}")
        populate(classCurie, ontologyPrefix, classIdentity, curie.prefixReference.reference)
        classIdentityByCurie += (classCurie -> classIdentity)
      case (UMLClassName(strClassName), Some(curie)) =>
        logger.debug(s"case (UmlClassName(strClassName), Some(curie)) ${Code.source}")
        populate(classCurie, ontologyPrefix, classIdentity, strClassName)
        classIdentityByCurie += (classCurie -> classIdentity)
        classIdentityByName += (className -> classIdentity)
      case (UMLClassName(strClassName), None) =>
        logger.debug(s"case (UmlClassName(strClassName), None) ${Code.source}")
        populate(className, ontologyPrefix, classIdentity, strClassName)
        classIdentityByName += (className -> classIdentity)

    classIdentityByIRI += (classIdentity.classIRI -> classIdentity)

    classIdentity


  private def populate(classNamedElement: UMLClassNamedElement,
                       ontologyPrefix: PrefixNamespace,
                       classIdentity: UMLClassIdentity,
                       label: String): Unit = {
    logger.debug(s"classNamedElement=$classNamedElement, ontologyPrefix=$ontologyPrefix, classIdentity=$classIdentity, " +
      s"label=$label, ${Code.source}")
    val classIRI: UMLClassIRI = UMLClassIRI(ontologyPrefix, classNamedElement)
    classIdentity._classNamedElement = classNamedElement
    classIdentity._classIRI = classIRI
    classIdentity._classLabel = label
  }

  def findClassNamedElement(s: String): Option[UMLClassIdentity] =
    logger.debug(s"s=$s ${Code.source}")
    val classNameOption = classIdentityByName.get(UMLClassName(s))
    var classIdentityOption: Option[UMLClassIdentity] = None

    if classNameOption.isDefined then
      logger.debug(s"classNameOption=${classNameOption.get} ${Code.source}")
      classIdentityOption = classNameOption
    else if Curie.isCurieBasedOnConfiguredPrefix(s) then
      logger.debug(s"s=$s ${Code.source}")
      classIdentityOption = classIdentityByCurie.get(UMLClassCurie(Some(Curie(s))))

    logger.debug(s"classIdentityOption=${classIdentityOption.getOrElse(None)} ${Code.source}")
    classIdentityOption

  def unapply(s: String): Option[UMLClassIdentity] = findClassNamedElement(s)

end UMLClassIdentity


case class UMLEnumerationIdentity(enumerationName: UMLEnumerationName,
                                  enumerationCurie: UMLEnumerationCurie,
                                  ontologyPrefix: PrefixNamespace):
  var enumerationIRI: UMLEnumerationIRI = _
  var enumerationNamedElement: UMLEnumerationNamedElement = _
  var enumerationLabel: String = _

object UMLEnumerationIdentity:
  private val logger = Logger[this.type]

  private var enumerationIdentityByName: mutable.HashMap[UMLEnumerationName, UMLEnumerationIdentity] = mutable.HashMap[UMLEnumerationName, UMLEnumerationIdentity]()
  private var enumerationIdentityByIRI: mutable.HashMap[UMLEnumerationIRI, UMLEnumerationIdentity] = mutable.HashMap[UMLEnumerationIRI, UMLEnumerationIdentity]()
  private var enumerationIdentityByCurie: mutable.HashMap[UMLEnumerationCurie, UMLEnumerationIdentity] = mutable.HashMap[UMLEnumerationCurie, UMLEnumerationIdentity]()

  def apply(enumerationName: UMLEnumerationName = UMLEnumerationName(),
            enumerationCurie: UMLEnumerationCurie = UMLEnumerationCurie(),
            ontologyPrefix: PrefixNamespace): UMLEnumerationIdentity =
    logger.debug(s"enumerationName = $enumerationName and enumerationCurie=$enumerationCurie ${Code.source}")
    require(enumerationCurie.nonEmpty || enumerationName.nonEmpty, "A enumeration must have either a curie or a name.")
    val enumerationIdentity: UMLEnumerationIdentity = new UMLEnumerationIdentity(enumerationName, enumerationCurie, ontologyPrefix)
    (enumerationName, enumerationCurie.curieOption) match
      case (UMLEnumerationName(""), Some(curie)) =>
        logger.debug(s"case (UmlEnumerationName(\"\"), Some(curie)) ${Code.source}")
        populate(enumerationCurie, ontologyPrefix, enumerationIdentity, curie.prefixReference.reference)
        enumerationIdentityByCurie += (enumerationCurie -> enumerationIdentity)
      case (UMLEnumerationName(strEnumerationName), Some(curie)) =>
        logger.debug(s"case (UmlEnumerationName(strEnumerationName), Some(curie)) ${Code.source}")
        populate(enumerationCurie, ontologyPrefix, enumerationIdentity, strEnumerationName)
        enumerationIdentityByCurie += (enumerationCurie -> enumerationIdentity)
        enumerationIdentityByName += (enumerationName -> enumerationIdentity)
      case (UMLEnumerationName(strEnumerationName), None) =>
        logger.debug(s"case (UmlEnumerationName(strEnumerationName), None) ${Code.source}")
        populate(enumerationName, ontologyPrefix, enumerationIdentity, strEnumerationName)
        enumerationIdentityByName += (enumerationName -> enumerationIdentity)

    enumerationIdentityByIRI += (enumerationIdentity.enumerationIRI -> enumerationIdentity)

    enumerationIdentity


  private def populate(enumerationNamedElement: UMLEnumerationNamedElement,
                       ontologyPrefix: PrefixNamespace,
                       enumerationIdentity: UMLEnumerationIdentity,
                       label: String): Unit = {
    logger.debug(s"enumerationNamedElement=$enumerationNamedElement, ontologyPrefix=$ontologyPrefix, enumerationIdentity=$enumerationIdentity, " +
      s"label=$label, ${Code.source}")
    val enumerationIRI: UMLEnumerationIRI = UMLEnumerationIRI(ontologyPrefix, enumerationNamedElement)
    enumerationIdentity.enumerationNamedElement = enumerationNamedElement
    enumerationIdentity.enumerationIRI = enumerationIRI
    enumerationIdentity.enumerationLabel = label
  }

  def findEnumerationNamedElement(s: String): Option[UMLEnumerationIdentity] =
    logger.debug(s"s=$s ${Code.source}")
    val enumerationNameOption = enumerationIdentityByName.get(UMLEnumerationName(s))
    var enumerationIdentityOption: Option[UMLEnumerationIdentity] = None

    if enumerationNameOption.isDefined then
      logger.debug(s"enumerationNameOption=${enumerationNameOption.get} ${Code.source}")
      enumerationIdentityOption = enumerationNameOption
    else if Curie.isCurieBasedOnConfiguredPrefix(s) then
      logger.debug(s"s=$s ${Code.source}")
      enumerationIdentityOption = enumerationIdentityByCurie.get(UMLEnumerationCurie(Some(Curie(s))))

    logger.debug(s"enumerationIdentityOption=${enumerationIdentityOption.getOrElse(None)} ${Code.source}")
    enumerationIdentityOption

  def unapply(s: String): Option[UMLEnumerationIdentity] = findEnumerationNamedElement(s)
end UMLEnumerationIdentity


sealed trait UMLClassDiagramElement

case class UMLClass(classIdentity: UMLClassIdentity,
                    classDefinition: UMLClassDefinition = UMLClassDefinition(),
                    classParentIds: UMLClassParentNamedElements = new UMLClassParentNamedElements(Set[UMLClassNamedElement]()))
  extends UMLClassDiagramElement


case class UMLClassAttributeDefinition(definition: String = "")

sealed trait UMLClassAttributeType
case class UMLXMLDataType(attributeType: SupportedDataType) extends UMLClassAttributeType
case class UMLClassIdentityType(attributeType: UMLClassIdentity) extends UMLClassAttributeType
case class UMLEnumerationIdentityType(attributeType: UMLEnumerationIdentity) extends UMLClassAttributeType
case class CurieBasedUMLClassAttributeType(attributeType: Curie) extends UMLClassAttributeType

//case class UndefinedUMLClassAttributeType() extends UMLClassAttributeType
object UndefinedUMLClassAttributeType extends UMLClassAttributeType


object UMLClassAttributeType:
  private val logger = Logger[this.type]

  def apply(s: String): UMLClassAttributeType =
    logger.debug(s"s=$s ${Code.source}")
    require(UMLClassIdentity.findClassNamedElement(s).nonEmpty ||
      UMLEnumerationIdentity.findEnumerationNamedElement(s).nonEmpty ||
      SupportedDataType.unapply(s).nonEmpty ||
      Curie.isCurieBasedOnConfiguredPrefix(s) ||
      s.isEmpty,
      s"""A class attribute must have a type that is either a class or enumeration, that has been specified,
        or an XML data type or an curie based on a known prefix. Prefixes are specified using the -x option when running uml2semantics.
        "$s" is not recognised as either a class or an enumeration or an XML data type.""")
    UMLClassIdentity.findClassNamedElement(s) match
      case Some(classNamedElement) => UMLClassIdentityType(classNamedElement)
      case None => UMLEnumerationIdentity.findEnumerationNamedElement(s) match
        case Some(enumerationNamedElement) => UMLEnumerationIdentityType(enumerationNamedElement)
        case None => SupportedDataType.unapply(s) match
          case Some(x) => UMLXMLDataType(x)
          case None =>
            val curieOption: Option[Curie] = Curie.unapply(s)
            curieOption match
              case Some(curie) => CurieBasedUMLClassAttributeType(curie)
              case None => UndefinedUMLClassAttributeType

  def unapply(s: String): Option[UMLClassAttributeType] =
    logger.debug(s"s=$s ${Code.source}")
    s match
      case UMLClassIdentity(className, classCurie, ontologyPrefix) => Some(UMLClassIdentityType(
        UMLClassIdentity(className, classCurie, ontologyPrefix)))
      case SupportedDataType(i) => Some(UMLXMLDataType(i))
      case _ => None

case class UMLClassAttribute(attributeIdentity: UMLClassAttributeIdentity,
                             typeOfAttribute: UMLClassAttributeType,
                             multiplicity: UMLMultiplicity,
                             definition: UMLClassAttributeDefinition = UMLClassAttributeDefinition())
  extends UMLClassDiagramElement

case class UMLEnumerationDefinition(definition: String = "")

case class UMLEnumeration(enumerationIdentity: UMLEnumerationIdentity,
                          definition: UMLEnumerationDefinition = UMLEnumerationDefinition())
  extends UMLClassDiagramElement


object UMLEnumeration:
  private val logger = Logger[this.type]

  private var enumerationsWithEnumerationValues: mutable.HashMap[UMLEnumerationIdentity, mutable.Set[UMLEnumerationValueIdentity]] =
    mutable.HashMap[UMLEnumerationIdentity, mutable.Set[UMLEnumerationValueIdentity]]()
  def cache(enumerationIdentity: UMLEnumerationIdentity, enumerationValueIdentity: UMLEnumerationValueIdentity): UMLEnumerationIRI =
    logger.debug(s"enumerationIdentity=$enumerationIdentity, enumerationValueIdentity=$enumerationValueIdentity ${Code.source}")
    var enumerationValues: mutable.Set[UMLEnumerationValueIdentity] =
      enumerationsWithEnumerationValues.getOrElse(enumerationIdentity, mutable.HashSet[UMLEnumerationValueIdentity]())
    enumerationValues += enumerationValueIdentity
    enumerationsWithEnumerationValues.put(enumerationIdentity, enumerationValues)
    logger.trace(s"enumerationsWithEnumerationValues.size = ${enumerationsWithEnumerationValues.size}")
    enumerationIdentity.enumerationIRI

  def find(enumerationIdentity: UMLEnumerationIdentity): Option[mutable.Set[UMLEnumerationValueIdentity]] =
    enumerationsWithEnumerationValues.get(enumerationIdentity)


case class UMLEnumerationValueDefinition(definition: String = "")

case class UMLEnumerationValue(valueIdentity: UMLEnumerationValueIdentity,
                               definition: UMLEnumerationValueDefinition = UMLEnumerationValueDefinition())
  extends UMLClassDiagramElement

object UMLEnumerationValue:
  private var enumerationValuesByIdentity: mutable.HashMap[UMLEnumerationValueIdentity, UMLEnumerationValue] =
    mutable.HashMap[UMLEnumerationValueIdentity, UMLEnumerationValue]()

  def apply(valueIdentity: UMLEnumerationValueIdentity,
            definition: UMLEnumerationValueDefinition): UMLEnumerationValue =
    val umlEnumerationValue = new UMLEnumerationValue(valueIdentity, definition)
    enumerationValuesByIdentity += (valueIdentity -> umlEnumerationValue)
    umlEnumerationValue

  def find(valueIdentity: UMLEnumerationValueIdentity): Option[UMLEnumerationValue] =
    enumerationValuesByIdentity.get(valueIdentity)

case class UMLClassDiagram(owlOntologyFile: File,
                           ontologyIRI: OntologyIRI,
                           ontologyPrefix: PrefixNamespace,
                           umlClasses: UMLClassesMap,
                           umlClassAttributes: UMLClassAttributesMap,
                           umlEnumerations: UMLEnumerationsMap,
                           umlEnumerationValues: UMLEnumerationValuesMap)


object UMLClassDiagram:
  private val logger = Logger[this.type]

  def apply(owlOntologyFile: File, ontologyIRI: OntologyIRI, ontologyPrefix: PrefixNamespace): UMLClassDiagram =
    logger.debug(s"owlOntologyFile=$owlOntologyFile, ontologyIRI=$ontologyIRI, ontologyPrefix=$ontologyPrefix ${Code.source}")
    new UMLClassDiagram(owlOntologyFile, ontologyIRI, ontologyPrefix, UMLClassesMap(Map()), UMLClassAttributesMap(Map()),
      UMLEnumerationsMap(Map()), UMLEnumerationValuesMap(Map()))

