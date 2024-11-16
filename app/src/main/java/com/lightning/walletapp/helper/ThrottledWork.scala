package com.lightning.walletapp.helper

import rx.lang.scala.Subscription
import rx.lang.scala.{Observable => Obs}


abstract class ThrottledWork[T, V] {
  private var lastWork: Option[T] = Option.empty
  case class SubscriptionAndData(sub: Subscription, data: T)
  var subscriptionAndData: Option[SubscriptionAndData] = Option.empty

  def process(data: T, res: V): Unit
  private def doProcess(data: T, res: V): Unit = {
    // First nullify sunscription, then process callback
    subscriptionAndData = None
    process(data, res)
  }

  def error(error: Throwable): Unit
  private def doError(failure: Throwable): Unit = {
    // First nullify sunscription, then process error
    subscriptionAndData = None
    error(failure)
  }

  def work(input: T): Obs[V]
  def addWork(data: T): Unit = if (subscriptionAndData.isEmpty) {
    // Previous work has already finished or has never started, schedule a new one and then look if more work is added once this one is done
    val newSubscription = work(data).doOnSubscribe { lastWork = None }.doAfterTerminate { lastWork foreach addWork }.subscribe(res => doProcess(data, res), doError)
    subscriptionAndData = Some apply SubscriptionAndData(newSubscription, data)
  } else {
    // Current work has not finished yet
    // schedule new work once this is done
    lastWork = Some(data)
  }

  def replaceWork(data: T): Unit = if (subscriptionAndData.isEmpty) {
    // Previous work has already finished or was interrupted or has never started
    val newSubscription = work(data).subscribe(res => doProcess(data, res), doError)
    subscriptionAndData = Some apply SubscriptionAndData(newSubscription, data)
  } else {
    // Current work has not finished yet
    // disconnect subscription and start anew
    for (sad <- subscriptionAndData) sad.sub.unsubscribe
    subscriptionAndData = None
    replaceWork(data)
  }
}