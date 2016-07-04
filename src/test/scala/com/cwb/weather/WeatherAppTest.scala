package com.cwb.weather

import org.specs2.mutable.Specification

object WeatherAppTest extends Specification {

  "GenerateWeather" should {
    "generate Weather Data for all cities" in {
       WeatherApp.start()
      true
    }

  }

}