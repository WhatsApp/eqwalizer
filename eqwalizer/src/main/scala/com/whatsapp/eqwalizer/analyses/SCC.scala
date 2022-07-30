/* Copyright (c) Meta Platforms, Inc. and affiliates. All rights reserved.
 *
 * This source code is licensed under the Apache 2.0 license found in
 * the LICENSE file in the root directory of this source tree.
 */

package com.whatsapp.eqwalizer.analyses

import scala.collection.mutable

// adapted from https://www.eclipse.org/viatra/
// https://git.eclipse.org/c/viatra/org.eclipse.viatra.git/tree/query/plugins/org.eclipse.viatra.query.runtime.base.itc/src/org/eclipse/viatra/query/runtime/base/itc/alg/misc/scc/SCC.java
// c530dfb1965ea9cd506cee5b9ec9508ffb716d4c
// $COVERAGE-OFF$
object SCC {
  private class SCCProperty(var index: Int, var lowlink: Int)

  def computeSCC[V](g: Map[V, Set[V]]): List[Set[V]] = {
    val allNodes = (g.keys ++ g.values.flatten).toSet
    var index: Int = 0
    val ret = new mutable.ListBuffer[Set[V]]

    // stores the lowlink and index information for the given node
    val nodeMap =
      mutable.Map.from(allNodes.iterator.map(_ -> new SCCProperty(0, 0)))

    // stores all target nodes of a given node - the list will be modified
    val targetNodeMap =
      mutable.Map.empty[V, mutable.Set[V]]

    // stores those target nodes for a given node which have not been visited
    val notVisitedMap =
      mutable.Map.empty[V, mutable.Set[V]]

    // stores the nodes during the traversal
    val nodeStack =
      mutable.Stack.empty[V]

    // stores the nodes which belong to an scc
    // (there can be many sccs in the stack at the same time)
    val sccStack =
      new mutable.Stack[V]

    var sink = false
    var finishedTraversal = true

    for (n <- allNodes) { // if the node has not been visited yet
      if (nodeMap(n).index == 0) {
        nodeStack.push(n)
        while (nodeStack.nonEmpty) {
          val currentNode: V = nodeStack.head
          sink = false
          finishedTraversal = false
          val prop = nodeMap(currentNode)
          if (nodeMap(currentNode).index == 0) {
            index += 1
            sccStack.push(currentNode)
            prop.index = index
            prop.lowlink = index
            notVisitedMap.put(currentNode, mutable.Set.empty[V])
            // storing the target nodes of the actual node
            targetNodeMap.update(currentNode, mutable.Set.from(g(currentNode)))
          }
          if (targetNodeMap.contains(currentNode)) {
            // remove node from stack, the exploration of its children has finished
            if (targetNodeMap(currentNode).isEmpty) {
              targetNodeMap.remove(currentNode)
              nodeStack.pop()
              for (targetNode <- g(currentNode)) {
                if (notVisitedMap(currentNode)(targetNode))
                  prop.lowlink = Math.min(prop.lowlink, nodeMap(targetNode).lowlink)
                else if (sccStack.contains(targetNode))
                  prop.lowlink = Math.min(prop.lowlink, nodeMap(targetNode).index)
              }
              finishedTraversal = true
            } else {
              val targetNode = targetNodeMap(currentNode).head
              targetNodeMap(currentNode).remove(targetNode)
              // if the targetNode has not yet been visited push it to the stack
              // and mark it in the notVisitedMap
              if (nodeMap(targetNode).index == 0) {
                notVisitedMap(currentNode).add(targetNode)
                nodeStack.push(targetNode)
              }
            }
          } else { // if currentNode has no target nodes
            nodeStack.pop()
            sink = true
          }
          // create scc if node is a sink or an scc has been found
          if ((sink || finishedTraversal) && (prop.lowlink == prop.index)) {
            var sc = Set.empty[V]
            var notCurrent = true
            while (notCurrent) {
              val targetNode = sccStack.pop()
              sc += targetNode
              notCurrent = targetNode != currentNode
            }
            ret.addOne(sc)
          }
        }
      }
    }
    ret.toList
  }
}
// $COVERAGE-ON$
