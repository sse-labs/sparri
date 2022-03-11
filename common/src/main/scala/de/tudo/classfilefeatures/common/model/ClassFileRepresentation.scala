package de.tudo.classfilefeatures.common.model

case class ClassFileRepresentation(flags: Int,
                                   majorVersion: Int,
                                   minorVersion: Int,
                                   thisTypeFqn: String,
                                   superTypeFqn: Option[String],
                                   interfacesFqn: Seq[String],
                                   methodRepresentations: Seq[MethodRepresentation],
                                   fieldRepresentations: Seq[FieldDefinitionRepresentation])
