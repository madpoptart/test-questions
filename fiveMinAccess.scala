# Given the following input files find the amount of time a given resources has been accessed the most in a five minute period.

  val logs1 = Array(
    Array("58523", "user_1", "resource_1"),
    Array("62314", "user_2", "resource_2"),
    Array("54001", "user_1", "resource_3"),
    Array("200", "user_6", "resource_5"),
    Array("215", "user_6", "resource_4"),
    Array("54060", "user_2", "resource_3"),
    Array("53760", "user_3", "resource_3"),
    Array("58522", "user_22", "resource_1"),
    Array("53651", "user_5", "resource_3"),
    Array("2", "user_6", "resource_1"),
    Array("100", "user_6", "resource_6"),
    Array("400", "user_7", "resource_2"),
    Array("100", "user_8", "resource_6"),
    Array("54359", "user_1", "resource_3")
  )

  val logs2 = Array(
    Array("300", "user_1", "resource_3"),
    Array("599", "user_1", "resource_3"),
    Array("900", "user_1", "resource_3"),
    Array("1199", "user_1", "resource_3"),
    Array("1200", "user_1", "resource_3"),
    Array("1201", "user_1", "resource_3"),
    Array("1202", "user_1", "resource_3")
  )

  val logs3 = Array(
    Array("300", "user_10", "resource_5")
  )


  println(getHighest(logs1))
  println(getHighest(logs2))
  println(getHighest(logs3))

/* Create a case class to hold information instead of in just a raw array */
case class Resource(accessTime: Int, user: String, resource: String)

/**
* Function to create a list of Resource case classes from double arrays 
*/
  def formatLogs(logs: Array[Array[String]]) = {
    logs.map(log => log.toList).toList
      .map(item => Resource(item.head.toInt, item.slice(1, 2).head, item.slice(2,3).head))
  }


/**
* Find the resource that has been accessed the most in a five minute period 
*/
  def getHighest(logs: Array[Array[String]]) = {
    val keyed = formatLogs(logs).groupBy(item => item.resource).map(kv => kv._1 -> kv._2.sortBy(_.accessTime))

    /**
    * Function to count the number of times the given testAccesss time has been accessed within five minutes of
    * the given access times. 
    */
    def getFiveMinCounts(testerAccessTime: Int, accessTimes: Seq[Int], count:Int =0): Int = {
      val times = accessTimes.map(time => Math.abs(time - testerAccessTime))

      if(times.isEmpty) 1
      else times.count(time => time <= 300)
    }

    val km = keyed.map(kv => kv._1 -> kv._2.map(resource => getFiveMinCounts(resource.accessTime, kv._2.filterNot(r => r == resource).map(_.accessTime))))
    val res = km.map(kv => kv._1 -> kv._2.max) /* Get the max value */
    res.maxBy(_._2) /* Get the max of the maxes to get the resource that has been accessed the most */

  }
