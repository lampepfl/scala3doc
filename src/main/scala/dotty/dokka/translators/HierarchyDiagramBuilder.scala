package dotty.dokka

import org.jetbrains.dokka.model.Bound
import org.jetbrains.dokka.links.DRI
import dotty.dokka.model._
import dotty.dokka.model.api._


object HierarchyDiagramBuilder {
    def build(m: Member): HierarchyDiagram = {

        val mainVertex = Vertex(0, m.asLink)
        val mainType: LinkToType = null // workaround for error value -> is not a member of Null, but could be made available as an extension method.
        val mainTypeVertex: (LinkToType, Vertex) = (mainType -> mainVertex)
        val superTypesVertecies = (m.parents.values.flatMap(x => x).toSet
            .zipWithIndex.map { case (key, index) => key -> Vertex(index + 1, key) } + mainTypeVertex).toMap

        val supertypesEdges = m.parents.keys.flatMap(key =>
            m.parents(key).map(value => Edge(superTypesVertecies(key), superTypesVertecies(value)))
        ).toSeq

        val subtypesEdges = m.knownChildren.zipWithIndex.map { case (member, index) =>
            Edge(Vertex(index + superTypesVertecies.size, member), mainVertex)
        }

        HierarchyDiagram(supertypesEdges ++ subtypesEdges)
    }
}