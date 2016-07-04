package com.cwb.weather

import org.specs2.mutable.Specification

object WeatherGeneratorTest extends Specification {

  "generateTemp" should {
    "generate Temperature for SUNNY at 9.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1000, 90, 1373772600000L)
      val temp = WeatherGenerator.generateTemp(1.25f,params)
      temp >= 12 &&  temp <= 13
    }
    "generate Temperature for RAIN at 18.30" in {
      val params = WeatherParams(Constants.ConditionEnums.RAIN, Constants.SeasonEnums.SPRING, 5f, 15f, 1000, 90, 1373634061000L)
      val temp = WeatherGenerator.generateTemp(-0.667f,params)
      temp >= 8 && temp <= 10
    }
    "generate Temperature for SNOW at 07.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SNOW, Constants.SeasonEnums.SPRING, 0f, 5f, 1000, 90, 1373634061000L)
      val temp = WeatherGenerator.generateTemp(-0.667f,params)
      temp >= -15 && temp <= 1
    }
  }

  "generatePressure" should {
    "generate Pressure for RAIN at 9.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1100, 90, 1373772600000L)
      val pressure = WeatherGenerator.generatePressure(9,params)
      pressure >= 1100 &&  pressure <= 1200
    }
    "generate Pressure for SUNNY at 18.30" in {
      val params = WeatherParams(Constants.ConditionEnums.RAIN, Constants.SeasonEnums.SPRING, 5f, 15f, 1000, 90, 1373634061000L)
      val pressure = WeatherGenerator.generatePressure(14.5f,params)
      pressure >= 1000 && pressure <= 1100
    }
    "generate Pressure for SNOW at 07.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SNOW, Constants.SeasonEnums.SPRING, 0f, 5f, 1000, 90, 1373634061000L)
      val pressure = WeatherGenerator.generatePressure(-5.2f,params)
      pressure >= 1200 && pressure <= 1250
    }
  }

  "generateHumidity" should {
    "generate Humidity for RAIN at 9.00" in {
      val params = WeatherParams(Constants.ConditionEnums.RAIN, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1100, 90, 1373772600000L)
      val humidity = WeatherGenerator.generateHumidity(params,9f)
      humidity >= 70 &&  humidity <= 100
    }
    "generate Humidity for SUNNY at 18.30" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 5f, 15f, 1000, 40, 1373634061000L)
      val humidity = WeatherGenerator.generateHumidity(params,14.5f)
      humidity >= 40 && humidity <= 75
    }
    "generate Humidity for SNOW at 07.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SNOW, Constants.SeasonEnums.SPRING, 0f, 5f, 1000, 90, 1373634061000L)
      val humidity = WeatherGenerator.generateHumidity(params,-5.2f)
      humidity >= 85 && humidity <= 100
    }
  }

  "findGradient" should {
    "find the gradient for temp at 00.30" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1000, 90, 1373655611000L)
      val gradient = WeatherGenerator.findGradient(params)
      gradient must_== -0.625f
    }
    "find the gradient for temp at 04.30" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1000, 90, 1373670011000L)
      val gradient = WeatherGenerator.findGradient(params)
      gradient must_== -0.500f
    }
    "find the gradient for temp at 09.00" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1000, 90, 1373772600000L)
      val gradient = WeatherGenerator.findGradient(params)
      gradient must_== 1.25f
    }
    "find the gradient for temp at 18.30" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SPRING, 10.0f, 20.0f, 1000, 90, 1373634061000L)
      val gradient = WeatherGenerator.findGradient(params)
      gradient must_== -0.6666667f
    }
  }
  "getBaseTemp" should {
    " Temperature at 09.00 in SUMMER when SUNNY should be" in {
      val params = WeatherParams(Constants.ConditionEnums.SUNNY, Constants.SeasonEnums.SUMMER, 10.0f, 20.0f, 1000, 90, 1373772600000L)
      val temp = WeatherGenerator.getBaseTemp(1.25f,params)
      temp must_== 12.5f
    }
    " Temperature at 10.00 in AUTUMN when RAIN should be" in {
      val params = WeatherParams(Constants.ConditionEnums.RAIN, Constants.SeasonEnums.WINTER, 10.0f, 20.0f, 1000, 90, 1373772600000L)
      val temp = WeatherGenerator.getBaseTemp(1.25f,params)
      temp must_== 9.5f
    }
    " Temperature at 16.30 in AUTUMN when SNOW should be" in {
      val params = WeatherParams(Constants.ConditionEnums.SNOW, Constants.SeasonEnums.WINTER, 10.0f, 20.0f, 1000, 90, 1373772600000L)
      val temp = WeatherGenerator.getBaseTemp(1.25f,params)
      temp <= -1
    }
  }



}
