package org.uml2semantics.model

trait UMLNamedElement:
  def isEmpty: Boolean
  def nonEmpty: Boolean
  def getName: String
  
  
trait MergableBuilder[T]:
  def merge(that: T): T