package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
import org.uml2semantics.inline.Code
import org.uml2semantics.model.{PrefixNamespace, UMLClassName}
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
  
//  private def parseClasses(ontologyPrefix: PrefixNamespace, xPath: XPath, xmiDocument: Document): Unit = {
//    val UMLClassesQuery: String = "//element[@type='uml:Class']"
//    val classNodes: Seq[Node] = executeXPathQuery(xPath, xmiDocument, UMLClassesQuery)
//    
//    val DocumentationQuery: String = "properties/@documentation"
//    
//    for classNode <- classNodes do {
//      // @Todo: This currently does not support curies. Ideally we would support both curies and names.
//      val className: String = classNode.getAttributes.getNamedItem("name").getNodeValue
//      val umlClassName = UMLClassName(className)
//
//      val classDefinition = executeXPathQuery(xPath, classNode, DocumentationQuery)
//      val parents: Set[String] = extractParents(xPath, xmiDocument, classNode)
//
//      val umlClass: UMLClass = UMLClass(
//        UMLClassIdentity(className = umlClassName, ontologyPrefix = ontologyPrefix),
//        UMLClassDefinition(classDefinition)
//      )
//      parentsByClassIdentityMap += umlClass.classIdentity -> parents
//
//      //      val attributes = extractAttributes(xPath, xmiDocument, umlClassName)
//
//      umlClassesMap += umlClassName -> umlClass
//    }
//
//    for (umlClassName, umlClass) <- umlClassesMap do {
//      val parents = parentsByClassIdentityMap(umlClass.classIdentity)
//      val classParentIds = UMLClassParentNamedElements(parents)
//      // @todo: Here we need to copy parentIds into the UMLClass, because we do not read classes in an order
//      // that will ensure that parent classes are read before their children. We may be better off using a
//      // Builder pattern to ensure instances are created with all necessary data.
//      val updatedUmlClass = umlClass.copy(classParentIds = classParentIds)
//      umlClassesMap.update(umlClassName, updatedUmlClass)
//    }
//
//    XMIClasses(classNodes, UMLClassesMap(umlClassesMap.toMap))
//  }



//  private def extractParents(xPath: XPath, xmiDocument: Document, classNode: Node): immutable.Set[String] = {
//    val logger = Logger("extractParents")
//    val ParentsGeneralizationEndQuery: String = "links/Generalization/@end"
//    val idref: String = executeXPathQuery(xPath, classNode, ParentsGeneralizationEndQuery)
//
//    logger.debug(s"idref = ${idref}")
//    val parents: mutable.Set[String] = mutable.Set[String]()
//    if !idref.isBlank then
//      val ParentsByIdrefQuery = s"//element[@idref='$idref']/@name"
//      val parentNodes = executeXPathQuery(xPath, xmiDocument, ParentsByIdrefQuery)
//      for parentNode <- parentNodes do
//        parents += parentNode.getNodeValue
//
//    logger.debug(s"parents = ${parents}")
//    parents.toSet
//  }


  private def executeXPathQuery(xPath: XPath, document: Document, query: String): Seq[Node] =
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
  
  private def executeXPathQuery(xPath: XPath, nodeList: NodeList, query: String): String =
    val logger = Logger("executeXPathQuery")
    logger.debug(s"Executing XPath query on nodeList: query=$query")
  
    val results: Seq[String] = for {
      i <- 0 until nodeList.getLength
      node = nodeList.item(i)
    } yield xPath.compile(query).evaluate(node)
  
    logger.debug(s"results.size = ${results.size}")
    results.foreach(result => logger.debug(result))
  
    results.head
  
  
  private def nodeListToSeq(nodeList: NodeList): Seq[Node] =
    IntStream.range(0, nodeList.getLength())
      .mapToObj(nodeList.item)
      .collect(Collectors.toList)
      .asScala
      .toSeq

