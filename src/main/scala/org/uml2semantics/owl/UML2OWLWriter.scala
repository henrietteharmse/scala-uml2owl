package org.uml2semantics.owl

import com.typesafe.scalalogging.Logger
import org.semanticweb.owlapi.apibinding.OWLManager
import org.semanticweb.owlapi.formats.RDFXMLDocumentFormat
import org.semanticweb.owlapi.model.parameters.ChangeApplied
import org.semanticweb.owlapi.model.parameters.ChangeApplied.{NO_OPERATION, SUCCESSFULLY}
import org.semanticweb.owlapi.model.*
import org.semanticweb.owlapi.vocab.OWL2Datatype
import org.uml2semantics.model.SupportedDataType.getIRI
import org.uml2semantics.model.*
import org.uml2semantics.inline.Code

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer, Seq}
import scala.util.Try

import scala.jdk.CollectionConverters.*

class UML2OWLWriter(/*val umlClassDiagram: UMLClassDiagram*/):

end UML2OWLWriter
