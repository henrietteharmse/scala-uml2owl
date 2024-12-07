package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassDefinition, UMLClassDiagram, UMLClassIdentity, UMLClassName, UMLClassNamedElement, UMLClassParentNamedElements, UMLClassesMap}
import org.uml2semantics.inline.Code
import org.w3c.dom.{Document, Node, NodeList}

import scala.jdk.CollectionConverters.*
import java.io.File
import scala.collection.{immutable, mutable}
import sourcecode.Text.generate

import java.util.Collections
import java.util.stream.{Collectors, IntStream}
import javax.management.Query
import javax.xml.parsers.{DocumentBuilder, DocumentBuilderFactory}
import javax.xml.xpath.{XPath, XPathConstants, XPathExpression, XPathFactory}

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
    val xmiDocument: Document = builder.parse(input.xmiFile.get)

    val classesResult: XMIClassesResult = parseUMLClasses(ontologyPrefix, xPath, xmiDocument)
    logger.debug(s"classesResult = ${classesResult}")

    None



  private case class XMIClassesResult(classNodes: Seq[Node], umlClassesMap: UMLClassesMap)

  private def parseUMLClasses(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): XMIClassesResult = {
    val UMLClassesQuery: String = "//element[@type='uml:Class']"
    val umlClassesMap: mutable.Map[UMLClassNamedElement, UMLClass] = mutable.Map()

    val classNodes: Seq[Node] = executeXPathQuery(xPath, xmiDocument, UMLClassesQuery)
    val DocumentationQuery: String = "properties/@documentation"

    val parentsByClassIdentityMap: mutable.Map[UMLClassIdentity, Set[String]] = mutable.Map()

    for classNode <- classNodes do {
      val className: String = classNode.getAttributes.getNamedItem("name").getNodeValue
      val umlClassName = UMLClassName(className)

      val classDefinition = executeXPathQuery(xPath, classNode, DocumentationQuery)
      val parents: Set[String] = extractParents(xPath, xmiDocument, classNode)

      val umlClass: UMLClass = UMLClass(
        UMLClassIdentity(className = umlClassName, ontologyPrefix = ontologyPrefix),
        UMLClassDefinition(classDefinition)//,
//        UMLClassParentNamedElements(parents)
      )
      parentsByClassIdentityMap += umlClass.classIdentity -> parents

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

    XMIClassesResult(classNodes, UMLClassesMap(umlClassesMap.toMap))
  }

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

  private def executeXPathQuery(xPath: XPath, document: Document, query: String) : Seq[Node] =
    nodeListToSeq(xPath.compile(query)
      .evaluate(document, XPathConstants.NODESET)
      .asInstanceOf[NodeList])

  private def executeXPathQuery(xPath: XPath, node: Node, query: String) : String =
    xPath.compile(query)
      .evaluate(node)

  private def nodeListToSeq(nodeList: NodeList) : Seq[Node] =
    IntStream.range(0, nodeList.getLength())
    .mapToObj(nodeList.item)
    .collect(Collectors.toList)
    .asScala
    .toSeq

