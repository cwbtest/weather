package com.cwb.weather

import scala.collection.mutable

case class CityData(name: String, lat: Float, lon: Float, alt: Int, summerMax: Float, summerMin: Float, autumnMax: Float,
  autumnMin: Float, winterMax: Float, winterMin: Float, springMax: Float, springMin: Float, baseHumidity: Int, basePressure: Int)
object CityWeatherDataCache {

  def loadData(): Map[String, CityData] = {
    val bufferedSource = io.Source.fromFile(getClass.getResource("/com/cwb/weather/weather.csv").getFile)
    val weatherMap = mutable.Map[String, CityData]()
    for (line <- bufferedSource.getLines) {
      if (!line.startsWith("#")) {
        val cols = line.split(",").map(_.trim)
        val cityData = CityData(cols(0), cols(1).toFloat, cols(2).toFloat, cols(3).toInt, cols(4).toFloat, cols(5).toFloat, cols(6).toFloat, cols(7).toFloat, cols(8).toFloat, cols(9).toFloat, cols(10).toFloat, cols(11).toFloat, cols(12).toInt, cols(13).toInt)
        weatherMap.put(cols(0), cityData)
      }
    }
    bufferedSource.close
    weatherMap.toMap
  }

  val data: Map[String, CityData] = loadData()

}
