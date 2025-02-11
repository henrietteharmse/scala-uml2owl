package org.uml2semantics.model.cache

import org.uml2semantics.model.UMLClassIdentity.ClassIdentityBuilder
import org.uml2semantics.model.{PrefixNamespace, UMLClass, UMLClassCurie, UMLClassIdentity, UMLClassName}

object ClassIdentityBuilderCache:
  private val buildersByClassName = scala.collection.mutable.Map[UMLClassName, ClassIdentityBuilder]()
  private val buildersByClassCurie = scala.collection.mutable.Map[UMLClassCurie, ClassIdentityBuilder]()
  private val classIdentityByClassName = scala.collection.mutable.Map[UMLClassName, UMLClassIdentity]()
  private val classIdentityByClassCurie = scala.collection.mutable.Map[UMLClassCurie, UMLClassIdentity]()

  def cacheUMLClassIdentity(className: UMLClassName, builder: ClassIdentityBuilder): Unit =
    if !classIdentityByClassName.contains(className) then
      val classIdentity = UMLClassIdentity(nameOption = Some(className), ontologyPrefix = builder.prefixNamespace)
      classIdentityByClassName += (className -> classIdentity)
      buildersByClassName += (className -> builder)

  def cacheUMLClassIdentity(classCurie: UMLClassCurie, builder: ClassIdentityBuilder): Unit =
    if !classIdentityByClassCurie.contains(classCurie) then
      val classIdentity = UMLClassIdentity(curieOption = Some(classCurie), ontologyPrefix = builder.prefixNamespace)
      classIdentityByClassCurie += (classCurie -> classIdentity)
      buildersByClassCurie += (classCurie -> builder)

  def cacheUMLClassIdentity(classIdentity: UMLClassIdentity, builder: ClassIdentityBuilder): UMLClassIdentity =
    classIdentity.nameOption.foreach(name =>
      classIdentityByClassName += (name -> classIdentity)
      buildersByClassName += (name -> builder)
    )

    classIdentity.curieOption.foreach(curie =>
      classIdentityByClassCurie += (curie -> classIdentity)
      buildersByClassCurie += (curie -> builder)
    )

    classIdentity

  def getUMLClassIdentity(className: UMLClassName): Option[UMLClassIdentity] =
    classIdentityByClassName.get(className)

  def getUMLClassIdentity(classCurie: UMLClassCurie): Option[UMLClassIdentity] =
    classIdentityByClassCurie.get(classCurie)

  /**
   * Retrieve or build a class identity based on the identifier. This method is used when parsing, for example TSV
   * files, where children of a class can be specified by either a name or a curie. Moreover, this method will create
   * the class if it does not exist in the cache, thereby giving users the most flexibility.
   *
   * @param identifier
   * @return
   */
  def retrieveOrBuildClassIdentity(identifier: String, prefixNamespace: PrefixNamespace): UMLClassIdentity = {
    val classIdentityBuilder = ClassIdentityBuilder(prefixNamespace)
    (UMLClassName(identifier), UMLClassCurie(identifier)) match
      case (Some(name), _) =>
        ClassIdentityBuilderCache.getUMLClassIdentity(name).getOrElse(
          classIdentityBuilder.withName(identifier).build)
      case (_, Some(curie)) =>
        ClassIdentityBuilderCache.getUMLClassIdentity(curie).getOrElse(
          classIdentityBuilder.withCurie(identifier).build)
      case _ => throw new IllegalArgumentException(s"Name = '$identifier' is neither a curie nor a name.")
  }



object ClassBuilderCache:
  private val buildersByClassIdentity = scala.collection.mutable.Map[UMLClassIdentity, UMLClass.ClassBuilder]()
  private val classesByClassIdentity = scala.collection.mutable.Map[UMLClassIdentity, UMLClass]()

  def cacheUMLClass(umlClass: UMLClass, builder: UMLClass.ClassBuilder): Unit =
    buildersByClassIdentity += (umlClass.classIdentity -> builder)
    classesByClassIdentity += (umlClass.classIdentity -> umlClass)