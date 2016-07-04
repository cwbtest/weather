package com.cwb.weather

import com.cwb.weather.Constants._
import org.joda.time.DateTime

import scala.util.Random

case class WeatherParams(condition: ConditionEnums.Value, season: SeasonEnums.Value, baseMinTemp: Float,
  baseMaxTemp: Float, basePressure: Float, baseHumidity: Int, time: Long)

object WeatherGenerator {
  val TEMP_DIFF_RAIN: Int = 6
  val TEMP_VARIABILITY_FACTOR: Float = 5

  def getHourOfTheDay(time: Long): Int = {
    new DateTime(time).getHourOfDay
  }

  def getMinutesOfTheDay(time: Long): Int = {
    new DateTime(time).getMinuteOfDay
  }

  def getAvgMidNightTemp(params: WeatherParams): Float = {
    val tempVarianceOfTheDay = params.baseMaxTemp - params.baseMinTemp
    params.baseMinTemp + 0.40f * tempVarianceOfTheDay
  }

  def getAvgEarlyMorngTemp(params: WeatherParams): Float = {
    val tempVarianceOfTheDay = params.baseMaxTemp - params.baseMinTemp
    params.baseMinTemp + 0.15f * tempVarianceOfTheDay
  }

  def findGradient(params: WeatherParams): Float = {
    val tempVarianceOfTheDay = params.baseMaxTemp - params.baseMinTemp
    getHourOfTheDay(params.time) match {
      case it if 0 until 4 contains it => -(getAvgMidNightTemp(params) - getAvgEarlyMorngTemp(params)) / 4
      case it if 4 until 7 contains it => -(getAvgEarlyMorngTemp(params) - params.baseMinTemp) / 3
      case it if 7 until 15 contains it => (params.baseMaxTemp - params.baseMinTemp) / 8
      case it if 15 until 24 contains it => -(0.60f * tempVarianceOfTheDay) / 9
    }

  }

  def getMinsFromMidNight(time: Long): Float = {
    getHourOfTheDay(time) * 60.0f
  }

  def getMinsAfter(baseHour: Int, time: Long): Float = {
    getMinutesOfTheDay(time) - baseHour * 60.0f
  }

  def getBaseTemp(gradient: Float, params: WeatherParams): Float = {
    val RAIN_TEMP_DIFF = 3
    val baseTemp = getHourOfTheDay(params.time) match {
      case it if 0 until 4 contains it => getAvgMidNightTemp(params) + getMinsFromMidNight(params.time) * gradient / 60
      case it if 4 until 7 contains it => getAvgEarlyMorngTemp(params) + getMinsAfter(4, params.time) * gradient / 60
      case it if 7 until 15 contains it => params.baseMinTemp + getMinsAfter(7, params.time) * gradient / 60
      case it if 15 until 24 contains it =>
        val minsAfter15 = getMinsAfter(15, params.time)
        val valueUptoThisMin = minsAfter15 * gradient / 60
        //println(s"Mins after 15 :$minsAfter15 valueUptoThisMin : $valueUptoThisMin time:${params.time}")
        params.baseMaxTemp + valueUptoThisMin
    }
    params.condition match {
      case ConditionEnums.RAIN => baseTemp - RAIN_TEMP_DIFF
      case ConditionEnums.SNOW => -(baseTemp % 15)
      case _ => baseTemp

    }
  }

  def generateTemp(gradient: Float, weatherParams: WeatherParams): Float = {
    val baseTemp = getBaseTemp(gradient, weatherParams)
    val perMinVariance: Float = TEMP_VARIABILITY_FACTOR * gradient / 60
    val deviation = getRandom(Math.abs((perMinVariance * 100).asInstanceOf[Int])) / 100
    //println(s"baseMin:${weatherParams.baseMinTemp} baseMax: ${weatherParams.baseMaxTemp} gradient:$gradient baseTemp :$baseTemp perMinVariance : $perMinVariance deviation :$deviation")
    baseTemp + deviation
  }

  def generatePressure(temp: Float, params: WeatherParams): Float = {
    val PRESSURE_CONSTANT = 150
    params.condition match {
      case ConditionEnums.RAIN =>
        params.basePressure + Random.nextInt(20) - temp * 10 + PRESSURE_CONSTANT
      case ConditionEnums.SNOW =>
        params.basePressure + Random.nextInt(40) - temp * 10 + PRESSURE_CONSTANT
      case _ =>
        params.basePressure - Random.nextInt(10) - temp * 10 + PRESSURE_CONSTANT
    }
  }

  def generateHumidity(params: WeatherParams, temp: Float): Int = {
    params.condition match {
      case ConditionEnums.RAIN =>
        70 + Random.nextInt(30)
      case ConditionEnums.SNOW =>
        85 + Random.nextInt(15)
      case _ =>
        (params.baseHumidity + Random.nextInt(10)) % 100
    }
  }

  def getCondition(condition: ConditionEnums.Value, time: Long): String = {
    val dateTime = new DateTime(time)
    if (new DateTime(time).getHourOfDay > 18 || dateTime.getHourOfDay < 6) {
      if (condition.equals(ConditionEnums.SUNNY)) ConditionEnums.CLEAR.toString
      else condition.toString
    } else condition.toString
  }

  def generate(params: WeatherParams, cityData: CityData) = {
    val gradient = findGradient(params: WeatherParams)
    val temp = BigDecimal(generateTemp(gradient, params)).setScale(2, BigDecimal.RoundingMode.HALF_UP).toFloat
    val pressure = generatePressure(gradient, params)
    val humidity = generateHumidity(params, temp)
    val weatherRecord = new WeatherRecord(cityData.name, cityData.lat, cityData.lon, cityData.alt, new DateTime(params.time).toString, getCondition(params.condition, params.time), temp, pressure, humidity)
    println(weatherRecord.toString)

  }

  def getRandom(range: Int): Float = {
    if (range == 0) {
      0.0f
    } else {
      val abs = Random.nextInt(Math.abs(range) * 1000)
      if (abs % 2 == 0)
        -abs / 1000
      else
        abs / 1000
    }
  }
}

class WeatherRecord(city: String, lat: Float, lon: Float, alt: Float, date: String, condition: String, temp: Float, pressure: Float, humidity: Float) {

  val PIPE_SEPARATOR = "|"
  val COMMA_SEPARATOR = ","
  override def toString: String = {
    s"$city$PIPE_SEPARATOR$lat$COMMA_SEPARATOR$lon$COMMA_SEPARATOR$alt$PIPE_SEPARATOR$date$PIPE_SEPARATOR$condition$PIPE_SEPARATOR$temp$PIPE_SEPARATOR$pressure$PIPE_SEPARATOR$humidity"
  }

}
