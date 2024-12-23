package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.model.{PrefixNamespace, UMLCardinality, UMLClass, UMLClassAttribute, UMLClassAttributeCurie, UMLClassAttributeDefinition, UMLClassAttributeIdentity, UMLClassAttributeName, UMLClassAttributeType, UMLClassAttributesMap, UMLClassDefinition, UMLClassDiagram, UMLClassIdentity, UMLClassName, UMLClassNamedElement, UMLClassParentNamedElements, UMLClassesMap, UMLMultiplicity}
import org.uml2semantics.inline.Code
import org.w3c.dom.{Document, Node, NodeList}

import scala.jdk.CollectionConverters.*
import java.io.File
import scala.collection.{immutable, mutable}
import sourcecode.Text.generate

import java.util
import java.util.Collections
import java.util.stream.{Collectors, IntStream}
import javax.management.Query
import javax.xml.namespace.QName
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.xpath.{XPath, XPathConstants, XPathExpression, XPathExpressionException, XPathFactory, XPathFunction, XPathFunctionException, XPathFunctionResolver}

object XMIReader:

  def parseClasses(maybeXMIFile: Option[File], ontologyPrefix: PrefixNamespace): /*UMLClasses*/ Unit =
    val logger = Logger("parseClasses")
    logger.info("Start")

  def parseUMLClassDiagram(input: InputParameters): Option[UMLClassDiagram] =
    val logger = Logger("parseUMLClassDiagramFromXMI")
    logger.debug(s"start xmi parsing ${Code.source}")
    val ontologyPrefix: PrefixNamespace = PrefixNamespace(input.ontologyPrefix)

    val builderFactory: DocumentBuilderFactory = DocumentBuilderFactory.newInstance
    val builder: DocumentBuilder = builderFactory.newDocumentBuilder
    val xPath: XPath = XPathFactory.newInstance.newXPath
    xPath.setXPathFunctionResolver(DebugXPathFunctionResolver)
    val xmiDocument: Document = builder.parse(input.xmiFile.get)

    val classesResult: XMIClasses = parseClasses(ontologyPrefix, xPath, xmiDocument)
    logger.debug(s"classesResult = ${classesResult}")

    None

  private case class XMIClasses(classNodes: Seq[Node], umlClassesMap: UMLClassesMap)
//  private case class XMIClassAttributes(attributes: Seq[Node], umlClassAttributesMap: UMLClassAttributesMap)

  private def parseClasses(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): XMIClasses = {
    val UMLClassesQuery: String = "//element[@type='uml:Class']"
    val umlClassesMap: mutable.Map[UMLClassNamedElement, UMLClass] = mutable.Map()

    val classNodes: Seq[Node] = executeXPathQuery(xPath, xmiDocument, UMLClassesQuery)
    val DocumentationQuery: String = "properties/@documentation"

    val parentsByClassIdentityMap: mutable.Map[UMLClassIdentity, Set[String]] = mutable.Map()

    for classNode <- classNodes do {
      // @Todo: This currently does not support curies. Ideally we would support both curies and names.
      val className: String = classNode.getAttributes.getNamedItem("name").getNodeValue
      val umlClassName = UMLClassName(className)

      val classDefinition = executeXPathQuery(xPath, classNode, DocumentationQuery)
      val parents: Set[String] = extractParents(xPath, xmiDocument, classNode)

      val umlClass: UMLClass = UMLClass(
        UMLClassIdentity(className = umlClassName, ontologyPrefix = ontologyPrefix),
        UMLClassDefinition(classDefinition)
      )
      parentsByClassIdentityMap += umlClass.classIdentity -> parents

//      val attributes = extractAttributes(xPath, xmiDocument, umlClassName)

      umlClassesMap += umlClassName -> umlClass
    }

    for (umlClassName, umlClass) <- umlClassesMap do {
      val parents = parentsByClassIdentityMap(umlClass.classIdentity)
      val classParentIds = UMLClassParentNamedElements(parents)
      // @todo: Here we need to copy parentIds into the UMLClass, because we do not read classes in an order
      // that will ensure that parent classes are read before their children. We may be better off using a
      // Builder pattern to ensure instances are created with all necessary data.
      val updatedUmlClass = umlClass.copy(classParentIds = classParentIds)
      umlClassesMap.update(umlClassName, updatedUmlClass)
    }

    XMIClasses(classNodes, UMLClassesMap(umlClassesMap.toMap))
  }

//  private def parseAttributes(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document, xmiClasses: XMIClasses) : Unit = {
//
//  }

  private def extractParents(xPath: XPath, xmiDocument: Document, classNode: Node): immutable.Set[String] = {
    val logger = Logger("extractParents")
    val ParentsGeneralizationEndQuery: String = "links/Generalization/@end"
    val idref: String = executeXPathQuery(xPath, classNode, ParentsGeneralizationEndQuery)

    logger.debug(s"idref = ${idref}")
    val parents: mutable.Set[String] = mutable.Set[String]()
    if !idref.isBlank then
      val ParentsByIdrefQuery = s"//element[@idref='$idref']/@name"
      val parentNodes = executeXPathQuery(xPath, xmiDocument, ParentsByIdrefQuery)
      for parentNode <- parentNodes do
        parents += parentNode.getNodeValue

    logger.debug(s"parents = ${parents}")
    parents.toSet
  }

  private def extractAttributes(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document, 
                                className: UMLClassName): UMLClassAttributesMap = {

    val logger = Logger("extractAttributes")
    val umlClassAttributesMap: mutable.Map[UMLClassAttributeName, UMLClassAttribute] = mutable.Map()
    val AttributesQuery: String = s"//element[@type='uml:Class' and @name='${className.name}']//attribute/@name"
    val attributeNameNodes = executeXPathQuery(xPath, xmiDocument, AttributesQuery)
    val attributes: mutable.Set[String] = mutable.Set[String]()
    for attributeNode <- attributeNameNodes do
      val attributeName = attributeNode.getNodeValue
      val AttributesDetailsQuery: String =
        s"//element[@type='uml:Class' and @name='${className.name}']//attribute[@name='$attributeName']"
      val attributeDetailNodeList = executeXPathQueryAsNodeList(xPath, xmiDocument, AttributesDetailsQuery)
      val attributeType = executeXPathQuery(xPath, attributeDetailNodeList, "properties/@type")
      val attributeDocumentation = executeXPathQuery(xPath, attributeDetailNodeList, "documentation/@value")
      val attributeMultiplicityMin = executeXPathQuery(xPath, attributeDetailNodeList, "bounds/@lower")
      val attributeMultiplicityMax = executeXPathQuery(xPath, attributeDetailNodeList, "bounds/@upper")
      logger.debug(s"attributeName = ${attributeName}, attributeType = ${attributeType}, attributeMultiplicityMin = ${attributeMultiplicityMin}, attributeMultiplicityMax = ${attributeMultiplicityMax}")
      val umlClassAttributeIdentity = UMLClassAttributeIdentity(
        UMLClassName(className.name),
        UMLClassAttributeName(attributeName),
        UMLClassAttributeCurie(None),
        ontologyPrefix
      )
      
      val umlClassAttribute = UMLClassAttribute(
        umlClassAttributeIdentity,
        UMLClassAttributeType(attributeType),
        UMLMultiplicity(UMLCardinality(attributeMultiplicityMin), UMLCardinality(attributeMultiplicityMax)),
        UMLClassAttributeDefinition(attributeDocumentation)
      )
      umlClassAttributesMap += umlClassAttributeIdentity.attributeName -> umlClassAttribute

      attributes += attributeNode.getNodeValue

    logger.debug(s"attributes = ${attributes}")
    UMLClassAttributesMap(umlClassAttributesMap.toMap)
  }

  private def executeXPathQuery(xPath: XPath, document: Document, query: String) : Seq[Node] =
    nodeListToSeq(xPath.compile(query)
      .evaluate(document, XPathConstants.NODESET)
      .asInstanceOf[NodeList])

  private def executeXPathQueryAsNodeList(xPath: XPath, document: Document, query: String): NodeList =
    val logger = Logger("executeXPathQuery")
    logger.debug(s"Executing XPath query on document: query=$query")
    xPath.compile(query)
      .evaluate(document, XPathConstants.NODESET)
      .asInstanceOf[NodeList]

  private def executeXPathQuery(xPath: XPath, node: Node, query: String): String =
    xPath.compile(query).evaluate(node)

  private def executeXPathQuery(xPath: XPath, nodeList: NodeList, query: String) : String =
    val logger = Logger("executeXPathQuery")
    logger.debug(s"Executing XPath query on nodeList: query=$query")

    val results: Seq[String] = for {
      i <- 0 until nodeList.getLength
      node = nodeList.item(i)
    } yield xPath.compile(query).evaluate(node)

    logger.debug(s"results.size = ${results.size}")
    results.foreach(result => logger.debug(result))

    results.head


  private def nodeListToSeq(nodeList: NodeList) : Seq[Node] =
    IntStream.range(0, nodeList.getLength())
    .mapToObj(nodeList.item)
    .collect(Collectors.toList)
    .asScala
    .toSeq

object DebugXPathFunctionResolver extends XPathFunctionResolver {
  override def resolveFunction(functionName: QName, arity: Int): XPathFunction = {
    new XPathFunction {
      val logger = Logger("### evaluateFunction ###")
      override def evaluate(args: util.List[_]): AnyRef = {
        logger.debug(s"Evaluating function: ${functionName.getLocalPart} with arguments: ${args}")
        // Implement the actual function logic here
        null
      }
    }
  }
}

