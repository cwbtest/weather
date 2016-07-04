package com.cwb.weather

import com.cwb.weather.Constants.{ SeasonEnums, ConditionEnums }
import org.joda.time.DateTime

import scala.util.Random

case class Input(startDate: String, intervalInMins: Int,  noOfEntriesPerCity: Int)
object WeatherApp extends App{

  start

  def getCondition(input: Input): ConditionEnums.Value = {
    Random.nextInt(100) match {
      case it if 31 until 33 contains it  => ConditionEnums.SNOW
      case it if 1 until 20 contains it => ConditionEnums.RAIN
      case it if 20 until 30 contains it => ConditionEnums.CLOUDY

      case _ => ConditionEnums.SUNNY
    }
  }

  def getBaseTemp(season: SeasonEnums.Value, condition: ConditionEnums.Value, weatherHistoricalData: CityData, minOrMax: String): Float = {
    val seasonAvg = minOrMax match {
      case "Max" =>
        season match {
          case SeasonEnums.SUMMER => weatherHistoricalData.summerMax
          case SeasonEnums.AUTUMN => weatherHistoricalData.autumnMax
          case SeasonEnums.WINTER => weatherHistoricalData.winterMax
          case SeasonEnums.SPRING => weatherHistoricalData.springMax
        }
      case "Min" =>
        season match {
          case SeasonEnums.SUMMER => weatherHistoricalData.summerMin
          case SeasonEnums.AUTUMN => weatherHistoricalData.autumnMin
          case SeasonEnums.WINTER => weatherHistoricalData.winterMin
          case SeasonEnums.SPRING => weatherHistoricalData.springMin
        }
    }
    seasonAvg + (Random.nextInt(2) % 2 match {
      case 0 => Random.nextInt(3)
      case 1 => -Random.nextInt(2)
    })
  }

  def getWeatherCondition(input:Input) = {
    val startInMillis = new DateTime(input.startDate).getMillis
    var time = startInMillis
    CityWeatherDataCache.data foreach {
      case (city, weatherHistoricalData) =>
        var condition = getCondition(input)
        val season = getSeason(input)
        var maxBaseTemp = getBaseTemp(season, condition, weatherHistoricalData, "Max")
        var minBaseTemp = getBaseTemp(season, condition, weatherHistoricalData, "Min")
        var isNextDay = false
        var isNextWindow = false
        for (entryCount <- 1 to input.noOfEntriesPerCity) {
          val dateTime = new DateTime(time)
          val currDay = dateTime.getDayOfMonth
          val curHour = dateTime.getHourOfDay
          if (isNextDay) {
            maxBaseTemp = getBaseTemp(season, condition, weatherHistoricalData, "Max")
            minBaseTemp = getBaseTemp(season, condition, weatherHistoricalData, "Min")
            condition = getCondition(input)
            isNextDay = false
          }else if(isNextWindow){
            condition = getCondition(input)
            isNextWindow = false
          }
          val params = WeatherParams(condition, season, minBaseTemp, maxBaseTemp,
            weatherHistoricalData.basePressure, weatherHistoricalData.baseHumidity, time)
          WeatherGenerator.generate(params, weatherHistoricalData)
          time = time + input.intervalInMins * 60 * 1000
          val day = new DateTime(time).getDayOfMonth
          if (day != currDay) isNextDay = true
          val hour = new DateTime(time).getHourOfDay
          if (Math.abs(hour-curHour)>4) isNextWindow = true



        }
    }
  }

  def getSeason(input: Input): SeasonEnums.Value = {
    new DateTime(input.startDate).getMonthOfYear match {
      case it if 1 until 2 contains it => SeasonEnums.SUMMER
      case it if 3 until 5 contains it => SeasonEnums.AUTUMN
      case it if 6 until 8 contains it => SeasonEnums.WINTER
      case it if 9 until 11 contains it => SeasonEnums.SPRING
      case _ => SeasonEnums.SUMMER
    }
  }

  def start(): Unit = {
    println("----------------------------------------------------------------------------------------------------------")
    println("Welcome the CWB Weather Data Generator APP!!")
    println("----------------------------------------------------------------------------------------------------------")
    println("")
    println("Please enter the start date in ISO Format. Default -> 2013-07-12T00:00:00 : ")
    val date = scala.io.StdIn.readLine().isEmpty match{
      case true => "2016-07-12T00:00:00"
      case false => scala.io.StdIn.readLine().toString
    }
    println("Please enter the weather generation interval in mins. Default -> 120 : ")
    val interval = scala.io.StdIn.readLine().isEmpty match{
      case true => 120
      case false => scala.io.StdIn.readLine().toInt
    }
    println("Please enter the number of entries to generate per city. Default -> 10 : ")
    val entries = scala.io.StdIn.readLine().isEmpty match{
      case true => 10
      case false => scala.io.StdIn.readLine().toInt
    }
    val input = Input(date,interval,entries)
    println("Thank you!!. Please wait a moment. We are generating the weather forecast data...")
    Thread.sleep(1500)
    getWeatherCondition(input)
  }

}
