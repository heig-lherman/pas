package Utils

import scala.collection.mutable

/** A simple castor Actor that manages a list of downstream actors and allows
  * for broadcasting events to them.
  *
  * @param cc
  *   the castor context
  * @param log
  *   the cask logger
  */
class WebsocketBroadcastChannelManager(implicit
    cc: castor.Context,
    log: cask.Logger,
) extends castor.SimpleActor[cask.Ws.Event]:

  // A map with weak references ensures the channel actors can be garbage
  // collected when they are no longer used or have been disconnected for a while,
  // respecting the castor specification.
  private val downstreams =
    mutable.WeakHashMap.empty[cask.WsChannelActor, Unit]

  /** Subscribe a new websocket channel actor to the channel manager. The actor
    * will receive all events sent to the channel manager.
    * @param actor
    *   the websocket channel actor to subscribe
    * @return
    *   the websocket actor that shall be returned as handler
    */
  def subscribe(actor: cask.WsChannelActor): cask.WsActor =
    downstreams += actor -> ()
    cask.WsActor {
      case cask.Ws.Close(_, _) =>
        unsubscribe(actor)
      case cask.Ws.ChannelClosed() =>
        unsubscribe(actor)
    }

  /** Unsubscribe a websocket channel actor from the channel manager. The actor
    * will no longer receive events sent to the channel manager.
    * @param actor
    *   the websocket channel actor to unsubscribe
    */
  def unsubscribe(actor: cask.WsChannelActor): Unit =
    downstreams -= actor

  /** Send an event to all subscribed websocket channel actors.
    *
    * @param msg
    *   the event to send
    */
  override def run(msg: cask.Ws.Event): Unit =
    downstreams.keys.foreach(_.send(msg))
