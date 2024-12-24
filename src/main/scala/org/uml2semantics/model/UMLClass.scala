package org.uml2semantics.model

import com.typesafe.scalalogging.Logger
import org.uml2semantics.inline.Code

import scala.annotation.targetName

import org.uml2semantics.model.UMLNamedElement

sealed trait UMLClassNamedElement extends UMLNamedElement:
  def name: String

case class UMLClassName(nameOption: Option[String] = None) extends UMLClassNamedElement:
  override def isEmpty: Boolean = nameOption.isEmpty || nameOption.get.isBlank
  override def name: String = nameOption.get

case class UMLClassCurie(curieOption: Option[Curie] = None) extends UMLClassNamedElement:
  override def isEmpty: Boolean = curieOption.isEmpty || curieOption.get.curie.isBlank
  override def name: String = curieOption.get.curie

case class UMLClassIdentity(name: UMLClassName, curie: UMLClassCurie, ontologyPrefix: PrefixNamespace)

case class UMLClassDefinition(definitionOption: Option[String] = None)

case class UMLClassParentNamedElements(setOfParentNamedElements: Set[UMLClassNamedElement])


enum CoveringConstraint:
  case Complete
  case Incomplete

enum DisjointConstraint:
  case Disjoint
  case Overlapping

case class UMLGeneralizationSet(generalizationSet: Set[UMLClassIdentity],
                                coveringConstraint: CoveringConstraint,
                                disjointConstraint: DisjointConstraint)

case class UMLClassSpecializations(setOfGeneralizationSets: Set[UMLGeneralizationSet])

case class UMLClass(classIdentity: UMLClassIdentity,
                    classDefinition: UMLClassDefinition = UMLClassDefinition(),
                    specializations: UMLClassSpecializations = UMLClassSpecializations(Set()))

object UMLClass:
  private val builderCache = BuilderCache()

  class BuilderCache:
    val buildersByClassCurie = scala.collection.mutable.Map[Curie, Builder]()
    val buildersByClassName = scala.collection.mutable.Map[String, Builder]()
    val buildersByClassIdentity = scala.collection.mutable.Map[UMLClassIdentity, Builder]()

  class Builder(prefixNamespaceOption: Option[PrefixNamespace], builderCache: BuilderCache):
    private var prefixNamespace: Option[PrefixNamespace] = prefixNamespaceOption
    private var name: Option[String] = None
    private var curie: Option[Curie] = None
    private var definition: Option[String] = None
    private var specializations: Set[UMLGeneralizationSet] = Set()

    def withName(name: String): Builder =
      if !name.isBlank then
        this.name = Some(name)
        builderCache.buildersByClassName += name -> this
      this

    def withCurie(curieAsString: String): Builder =
      this.curie = Curie.fromString(curieAsString)
      if this.curie.nonEmpty then
        builderCache.buildersByClassCurie += this.curie.get -> this
      this

    def withNameAndCurie(name: String, curie: Curie): Builder =
      this.name = Some(name)
      this.curie = Some(curie)
      if builderCache.buildersByClassName.contains(name) then
        builderCache.buildersByClassName.remove(name)
      if builderCache.buildersByClassCurie.contains(curie) then
        builderCache.buildersByClassCurie.remove(curie)
      builderCache.buildersByClassIdentity +=
        UMLClassIdentity(UMLClassName(this.name), UMLClassCurie(this.curie), prefixNamespace.get) -> this
      this

    def withDefinition(definition: String): Builder =
      this.definition = Some(definition)
      this

    def withSpecializations(specialization: UMLGeneralizationSet): Builder =
      this.specializations += specialization
      this

    def withSpecializations(specializations: Set[UMLClassIdentity]): Builder =
      val generalizationSet = UMLGeneralizationSet(specializations, CoveringConstraint.Incomplete, DisjointConstraint.Disjoint)
      specializations.foreach(specialization =>
        builderCache.buildersByClassIdentity += specialization -> this)
      this.specializations += generalizationSet
      this

    def build(): UMLClass =
      val umlClass = UMLClass(
        UMLClassIdentity(
          UMLClassName(name),
          UMLClassCurie(curie),
          prefixNamespace.get
        ),
        UMLClassDefinition(definition),
        UMLClassSpecializations(specializations)
      )

      umlClass
