package com.cwb.weather

/**
  * Created by senthil.b on 29/06/16.
  */
object Constants {

  object ConditionEnums extends Enumeration{
    type Condition = Value
    val RAIN,SNOW,SUNNY,CLOUDY,CLEAR = Value
  }

  object SeasonEnums extends Enumeration{
    type Season = Value
    val SPRING,SUMMER,AUTUMN,WINTER = Value
  }

  object AreaTypeEnum extends Enumeration{
    type areaType = Value
    val COASTAL,MIDLAND = Value
  }

}
