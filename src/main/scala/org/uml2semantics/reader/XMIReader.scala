package org.uml2semantics.reader

import com.typesafe.scalalogging.Logger
import org.uml2semantics.InputParameters
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

//object XMIReader:


