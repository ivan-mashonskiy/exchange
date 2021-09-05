package model

import scala.collection.immutable.TreeMap

case class State(clientsState: Map[ClientName, ClientInfo],
                 buyOrders: TreeMap[Price, Vector[ClientWithAmount]],
                 sellOrders: TreeMap[Price, Vector[ClientWithAmount]])
